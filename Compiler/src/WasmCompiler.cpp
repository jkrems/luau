#include "Luau/WasmCompiler.h"

#include <format>
#include <sstream>

#include <wasm-binary.h>
#include <wasm-builder.h>
#include <wasm.h>

#include "Luau/Ast.h"
#include "Luau/BuiltinDefinitions.h"
#include "Luau/Common.h"
#include "Luau/Frontend.h"
#include "Luau/Parser.h"
#include "Luau/ToString.h"
#include "Luau/TypeAttach.h"

#include "Builtins.h"
#include "ConstantFolding.h"
#include "Types.h"
#include "ValueTracking.h"
#include "WasmTableShape.h"

namespace Luau
{

using namespace Luau::Compile;

struct WasmFunction
{
    size_t id;
    wasm::Function* code;
};

struct Local
{
    uint8_t reg = 0;
    bool allocated = false;
    bool captured = false;
    uint32_t debugpc = 0;
    uint32_t allocpc = 0;
    wasm::Type type = wasm::Type::none;
    TypeId typeId = nullptr;
    wasm::Name name;
};

enum WasmIntrinsicType
{
    STRING_CONCAT,
    MEMORY_ALLOC,
    PRINT_STRING,
    NUMBER_TO_STRING,
    PRINT_F64,
    BOOL_TO_STRING,
    PRINT_BOOL,
    WASI__wasi_snapshot_preview1__fd_write,
    WASM_INTRINSIC_COUNT,
};

class WasmIntrinsics
{
public:
    static const int32_t RESERVED_HEAP = 148;
    // "true" + "false" = 9 bytes.
    static const int32_t TRUE_FALSE_INDEX = RESERVED_HEAP - strlen("truefalse");
    // "-inf" + "nan" = 7 bytes.
    static const int32_t INF_NAN_INDEX = TRUE_FALSE_INDEX - strlen("-infnan");

    WasmIntrinsics(wasm::Module& wasm, wasm::Builder& builder)
        : wasm(wasm)
        , builder(builder)
    {
    }

    wasm::Name get(WasmIntrinsicType type)
    {
        LUAU_ASSERT(type < WasmIntrinsicType::WASM_INTRINSIC_COUNT);

        auto m = cache.find(type);
        if (m != cache.end())
        {
            return m->second;
        }

        wasm::Function* f = create(type);
        cache[type] = f->name;
        return f->name;
    }

private:
    wasm::Module& wasm;
    wasm::Builder& builder;
    std::map<WasmIntrinsicType, wasm::Name> cache;
    wasm::Name memory = "memory";

    wasm::Function* create(WasmIntrinsicType type)
    {
        switch (type)
        {
        case WasmIntrinsicType::STRING_CONCAT:
        {
            wasm::Type params = wasm::Type({wasm::Type::i32, wasm::Type::i32, wasm::Type::i32, wasm::Type::i32});
            wasm::Type results = wasm::Type({wasm::Type::i32, wasm::Type::i32});
            wasm::Expression* body = builder.makeBlock(
                {// Calculate output string size: aLen + bLen.
                    builder.makeLocalSet(5, builder.makeBinary(wasm::BinaryOp::AddInt32, builder.makeLocalGet(1, wasm::Type::i32),
                                                builder.makeLocalGet(3, wasm::Type::i32))),
                    // Reserve memory for the output string.
                    builder.makeLocalSet(
                        4, builder.makeCall(get(WasmIntrinsicType::MEMORY_ALLOC), {builder.makeLocalGet(5, wasm::Type::i32)}, wasm::Type::i32)),
                    // Copy a -> out.
                    builder.makeMemoryCopy(builder.makeLocalGet(4, wasm::Type::i32), builder.makeLocalGet(0, wasm::Type::i32),
                        builder.makeLocalGet(1, wasm::Type::i32), memory, memory),
                    // Copy b -> out + len(a).
                    builder.makeMemoryCopy(builder.makeBinary(wasm::BinaryOp::AddInt32, builder.makeLocalGet(4, wasm::Type::i32),
                                               builder.makeLocalGet(1, wasm::Type::i32)),
                        builder.makeLocalGet(2, wasm::Type::i32), builder.makeLocalGet(3, wasm::Type::i32), memory, memory),
                    // Return (outPtr, outLen).
                    builder.makeReturn(builder.makeTupleMake(
                        std::vector<wasm::Expression*>{builder.makeLocalGet(4, wasm::Type::i32), builder.makeLocalGet(5, wasm::Type::i32)}))},
                results);
            wasm::Function* fn = wasm.addFunction(
                builder.makeFunction("__string_concat__", wasm::Signature{params, results}, {wasm::Type::i32, wasm::Type::i32}, body));
            fn->setLocalName(0, "aPtr");
            fn->setLocalName(1, "aLen");
            fn->setLocalName(2, "bPtr");
            fn->setLocalName(3, "bLen");
            fn->setLocalName(4, "outPtr");
            fn->setLocalName(5, "outLen");
            return fn;
        }

        case WasmIntrinsicType::WASI__wasi_snapshot_preview1__fd_write:
        {
            wasm::Function* fn = wasm.addFunction(builder.makeFunction(
                "fd_write", wasm::Signature{{wasm::Type::i32, wasm::Type::i32, wasm::Type::i32, wasm::Type::i32}, {wasm::Type::i32}}, {}));
            fn->module = "wasi_snapshot_preview1";
            fn->base = "fd_write";

            return fn;
        }

        case WasmIntrinsicType::PRINT_STRING:
        {
            wasm::Name fd_write = get(WasmIntrinsicType::WASI__wasi_snapshot_preview1__fd_write);
            wasm::Function* fn = wasm.addFunction(builder.makeFunction("print_string", wasm::Signature{{wasm::Type::i32, wasm::Type::i32}, {}}, {},
                builder.makeBlock({
                    // Write str ptr.
                    builder.makeStore(4, 0, 0, builder.makeConst(0), builder.makeLocalGet(0, wasm::Type::i32), wasm::Type::i32, memory),
                    // Write str len.
                    builder.makeStore(4, 4, 0, builder.makeConst(0), builder.makeLocalGet(1, wasm::Type::i32), wasm::Type::i32, memory),
                    // Write newline
                    builder.makeStore(4, 0, 0, builder.makeConst(8), builder.makeConst(20), wasm::Type::i32, memory),
                    builder.makeStore(4, 4, 0, builder.makeConst(8), builder.makeConst(1), wasm::Type::i32, memory),
                    builder.makeStore(1, 0, 0, builder.makeConst(20), builder.makeConst('\n'), wasm::Type::i32, memory),
                    builder.makeDrop(builder.makeCall(fd_write,
                        std::vector<wasm::Expression*>{// Write to fd=1 (stdout).
                            builder.makeConst<uint32_t>(1),
                            // iovs location in memory
                            builder.makeConst(0),
                            // #entries in iovs[]
                            builder.makeConst(1),
                            // Write written byte count after the iovs entries.
                            builder.makeConst<uint32_t>(16)},
                        wasm::Type::i32)),
                    builder.makeDrop(builder.makeCall(fd_write,
                        std::vector<wasm::Expression*>{// Write to fd=1 (stdout).
                            builder.makeConst<uint32_t>(1),
                            // iovs location in memory
                            builder.makeConst(8),
                            // #entries in iovs[]
                            builder.makeConst(1),
                            // Write written byte count after the iovs entries.
                            builder.makeConst<uint32_t>(16)},
                        wasm::Type::i32)),
                })));
            fn->setLocalName(0, "strPtr");
            fn->setLocalName(1, "strLen");
            return fn;
        }

        case WasmIntrinsicType::NUMBER_TO_STRING:
        {
            // This implements the algorithm described in:
            // https://blog.benoitblanchon.fr/lightweight-float-to-string/
            wasm.addDataSegment(
                builder.makeDataSegment("__number_to_string_static__", memory, false, builder.makeConst(INF_NAN_INDEX), "-infnan", 7));

            wasm::Name alloc = get(WasmIntrinsicType::MEMORY_ALLOC);
            // 3 * 7 digits (int32) + some additional chars.
            uint32_t maxStrLen = 28;

            wasm::Index value = 0;
            wasm::Index integralPart = 1;
            wasm::Index decimalPart = 2;
            wasm::Index exponent = 3;
            wasm::Index remainder = 7;

            wasm::Index outPtr = 4;
            wasm::Index outLen = 5;
            wasm::Index writePtr = 6;

            wasm::Index intVal = 8;
            wasm::Index intLen = 9;

            wasm::Type strType({wasm::Type::i32, wasm::Type::i32});

            wasm::Expression* handleInf = builder.makeReturn(builder.makeTupleMake(
                std::vector<wasm::Expression*>{builder.makeConst<uint32_t>(INF_NAN_INDEX + 1), builder.makeConst<uint32_t>(3)}));
            wasm::Expression* handleNegInf = builder.makeReturn(
                builder.makeTupleMake(std::vector<wasm::Expression*>{builder.makeConst<uint32_t>(INF_NAN_INDEX), builder.makeConst<uint32_t>(4)}));
            wasm::Expression* checkInf = builder.makeIf(builder.makeBinary(wasm::BinaryOp::EqFloat64, builder.makeLocalGet(value, wasm::Type::f64),
                                                            builder.makeConst<double>(std::numeric_limits<double>::infinity())),
                handleInf,
                builder.makeIf(builder.makeBinary(wasm::BinaryOp::EqFloat64, builder.makeLocalGet(value, wasm::Type::f64),
                                   builder.makeConst<double>(-std::numeric_limits<double>::infinity())),
                    handleNegInf, nullptr, wasm::Type::none),
                wasm::Type::none);

            wasm::Expression* handleNan = builder.makeReturn(builder.makeTupleMake(
                std::vector<wasm::Expression*>{builder.makeConst<uint32_t>(INF_NAN_INDEX + 4), builder.makeConst<uint32_t>(3)}));
            wasm::Expression* checkNan = builder.makeIf(builder.makeBinary(wasm::BinaryOp::EqFloat64, builder.makeLocalGet(value, wasm::Type::f64),
                                                            builder.makeLocalGet(value, wasm::Type::f64)),
                checkInf, handleNan, wasm::Type::none);

            wasm::Expression* positiveExpThreshold = builder.makeConst<double>(1e7);
            wasm::Block* positiveExpThresholdTransforms = builder.makeBlock();
            for (int factor = 256; factor > 0; factor = factor / 2)
            {
                wasm::Expression* expDiv = builder.makeConst<double>(ldexp(1.0, factor));
                positiveExpThresholdTransforms->list.push_back(builder.makeIf(
                    builder.makeBinary(wasm::BinaryOp::GeFloat64, builder.makeLocalGet(value, wasm::Type::f64), expDiv),
                    builder.makeBlock({// value /= 1e<factor>
                        builder.makeLocalSet(
                            value, builder.makeBinary(wasm::BinaryOp::DivFloat64, builder.makeLocalGet(value, wasm::Type::f64), expDiv)),

                        // exponent += <factor>
                        builder.makeLocalSet(exponent, builder.makeBinary(wasm::BinaryOp::AddInt32, builder.makeLocalGet(exponent, wasm::Type::i32),
                                                           builder.makeConst<uint32_t>(factor)))})));
            }

            wasm::Expression* negativeExpThreshold = builder.makeConst<double>(1e-5);
            wasm::Block* negativeExpThresholdTransforms = builder.makeBlock();
            for (int factor = 256; factor > 0; factor = factor / 2)
            {
                wasm::Expression* expTest = builder.makeConst<double>(ldexp(1.0, -(factor - 1)));
                wasm::Expression* expMul = builder.makeConst<double>(ldexp(1.0, factor));
                negativeExpThresholdTransforms->list.push_back(builder.makeIf(
                    builder.makeBinary(wasm::BinaryOp::LtFloat64, builder.makeLocalGet(value, wasm::Type::f64), expTest),
                    builder.makeBlock({// value /= 1e<factor>
                        builder.makeLocalSet(
                            value, builder.makeBinary(wasm::BinaryOp::MulFloat64, builder.makeLocalGet(value, wasm::Type::f64), expMul)),

                        // exponent += <factor>
                        builder.makeLocalSet(exponent, builder.makeBinary(wasm::BinaryOp::SubInt32, builder.makeLocalGet(exponent, wasm::Type::i32),
                                                           builder.makeConst<uint32_t>(factor)))})));
            }

            wasm::Function* fn = wasm.addFunction(builder.makeFunction("__number_to_string__", wasm::Signature{{wasm::Type::f64}, strType},
                {wasm::Type::i32, wasm::Type::i32, wasm::Type::i32, wasm::Type::i32, wasm::Type::i32, wasm::Type::i32, wasm::Type::f64,
                    wasm::Type::i32, wasm::Type::i32},
                builder.makeBlock(
                    {checkNan,
                        // Initialize outPtr and writePtr to the beginning of a freshly allocated bit of memory.
                        builder.makeLocalSet(
                            outPtr, builder.makeCall(alloc, std::vector<wasm::Expression*>{builder.makeConst<uint32_t>(maxStrLen)}, wasm::Type::i32)),
                        builder.makeLocalSet(writePtr, builder.makeLocalGet(outPtr, wasm::Type::i32)),

                        // if (num < 0) append('-'); num  = -num;
                        builder.makeIf(
                            builder.makeBinary(wasm::BinaryOp::LtFloat64, builder.makeLocalGet(value, wasm::Type::f64), builder.makeConst<double>(0)),
                            builder.makeBlock({builder.makeStore(1, 0, 0, builder.makeLocalGet(writePtr, wasm::Type::i32),
                                                   builder.makeConst<int32_t>('-'), wasm::Type::i32, memory),
                                builder.makeLocalSet(writePtr, builder.makeBinary(wasm::BinaryOp::AddInt32,
                                                                   builder.makeLocalGet(writePtr, wasm::Type::i32), builder.makeConst(1))),
                                builder.makeLocalSet(value, builder.makeBinary(wasm::BinaryOp::MulFloat64,
                                                                builder.makeLocalGet(value, wasm::Type::f64), builder.makeConst<double>(-1)))})),

                        /* <normalizeFloat> */
                        // if (value >= positiveExpThreshold) {...}
                        builder.makeIf(
                            builder.makeBinary(wasm::BinaryOp::GeFloat64, builder.makeLocalGet(value, wasm::Type::f64), positiveExpThreshold),
                            positiveExpThresholdTransforms),
                        // if (value > 0 && value <= negativeExpThreshold) {...}
                        builder.makeIf(
                            builder.makeBinary(wasm::BinaryOp::AndInt32,
                                builder.makeBinary(
                                    wasm::BinaryOp::GtFloat64, builder.makeLocalGet(value, wasm::Type::f64), builder.makeConst<double>(0.0)),
                                builder.makeBinary(wasm::BinaryOp::LeFloat64, builder.makeLocalGet(value, wasm::Type::f64), negativeExpThreshold)),
                            negativeExpThresholdTransforms),
                        /* </normalizeFloat> */

                        /* <splitFloat> */
                        // integralPart = (uint32_t)value;
                        builder.makeLocalSet(
                            integralPart, builder.makeUnary(wasm::UnaryOp::TruncUFloat64ToInt32, builder.makeLocalGet(value, wasm::Type::f64))),

                        // double remainder = value - integralPart;
                        builder.makeLocalSet(remainder,
                            builder.makeBinary(wasm::BinaryOp::SubFloat64, builder.makeLocalGet(value, wasm::Type::f64),
                                builder.makeUnary(wasm::UnaryOp::ConvertUInt32ToFloat64, builder.makeLocalGet(integralPart, wasm::Type::i32)))),

                        // remainder *= 1e9;
                        builder.makeLocalSet(remainder, builder.makeBinary(wasm::BinaryOp::MulFloat64,
                                                            builder.makeLocalGet(remainder, wasm::Type::f64), builder.makeConst<double>(1e9))),

                        // decimalPart = (uint32_t)remainder;
                        builder.makeLocalSet(
                            decimalPart, builder.makeUnary(wasm::UnaryOp::TruncUFloat64ToInt32, builder.makeLocalGet(remainder, wasm::Type::f64))),

                        // // rounding
                        // remainder -= decimalPart;
                        builder.makeLocalSet(remainder,
                            builder.makeBinary(wasm::BinaryOp::SubFloat64, builder.makeLocalGet(remainder, wasm::Type::f64),
                                builder.makeUnary(wasm::UnaryOp::ConvertUInt32ToFloat64, builder.makeLocalGet(decimalPart, wasm::Type::i32)))),
                        // if (remainder >= 0.5) { ... }
                        builder.makeIf(builder.makeBinary(wasm::BinaryOp::GeFloat64, builder.makeLocalGet(remainder, wasm::Type::f64),
                                           builder.makeConst<double>(0.5)),
                            builder.makeBlock(
                                {builder.makeLocalSet(decimalPart, builder.makeBinary(wasm::BinaryOp::AddInt32,
                                                                       builder.makeLocalGet(decimalPart, wasm::Type::i32), builder.makeConst(1))),
                                    builder.makeIf(builder.makeBinary(wasm::BinaryOp::GeUInt32, builder.makeLocalGet(decimalPart, wasm::Type::i32),
                                                       builder.makeConst<uint32_t>(1000000000)),
                                        builder.makeBlock({builder.makeLocalSet(decimalPart, builder.makeConst(0)),
                                            builder.makeLocalSet(
                                                integralPart, builder.makeBinary(wasm::BinaryOp::AddInt32,
                                                                  builder.makeLocalGet(integralPart, wasm::Type::i32), builder.makeConst(1))),
                                            builder.makeIf(builder.makeBinary(wasm::BinaryOp::AndInt32,
                                                               builder.makeBinary(wasm::BinaryOp::NeInt32,
                                                                   builder.makeLocalGet(exponent, wasm::Type::i32), builder.makeConst(0)),
                                                               builder.makeBinary(wasm::BinaryOp::GeUInt32,
                                                                   builder.makeLocalGet(integralPart, wasm::Type::i32), builder.makeConst(10))),
                                                builder.makeBlock({builder.makeLocalSet(exponent,
                                                                       builder.makeBinary(wasm::BinaryOp::AddInt32,
                                                                           builder.makeLocalGet(exponent, wasm::Type::i32), builder.makeConst(1))),
                                                    builder.makeLocalSet(integralPart, builder.makeConst(1))}))}))})),
                        /* </splitFloat> */

                        // writeInteger(integralPart);
                        builder.makeLocalSet(intVal, builder.makeLocalGet(integralPart, wasm::Type::i32)),
                        builder.makeLocalSet(intLen, builder.makeConst(0)),
                        builder.makeLoop("digit_count",
                            builder.makeBlock({
                                builder.makeLocalSet(intLen, builder.makeBinary(wasm::BinaryOp::AddInt32,
                                                                 builder.makeLocalGet(intLen, wasm::Type::i32), builder.makeConst(1))),
                                builder.makeLocalSet(intVal, builder.makeBinary(wasm::BinaryOp::DivUInt32,
                                                                 builder.makeLocalGet(intVal, wasm::Type::i32), builder.makeConst(10))),
                                builder.makeBreak("digit_count", nullptr,
                                    builder.makeBinary(wasm::BinaryOp::NeInt32, builder.makeLocalGet(intVal, wasm::Type::i32), builder.makeConst(0))),
                            })),
                        builder.makeLocalSet(writePtr, builder.makeBinary(wasm::BinaryOp::AddInt32, builder.makeLocalGet(writePtr, wasm::Type::i32),
                                                           builder.makeLocalGet(intLen, wasm::Type::i32))),
                        builder.makeLocalSet(intVal, builder.makeLocalGet(integralPart, wasm::Type::i32)),
                        builder.makeLocalSet(intLen, builder.makeConst(1)),
                        builder.makeLoop("digit_write",
                            builder.makeBlock({
                                builder.makeStore(1, 0, 0,
                                    builder.makeBinary(wasm::BinaryOp::SubInt32, builder.makeLocalGet(writePtr, wasm::Type::i32),
                                        builder.makeLocalGet(intLen, wasm::Type::i32)),
                                    builder.makeBinary(wasm::BinaryOp::AddInt32, builder.makeConst<uint32_t>('0'),
                                        builder.makeBinary(
                                            wasm::BinaryOp::RemUInt32, builder.makeLocalGet(intVal, wasm::Type::i32), builder.makeConst(10))),
                                    wasm::Type::i32, memory),
                                builder.makeLocalSet(intLen, builder.makeBinary(wasm::BinaryOp::AddInt32,
                                                                 builder.makeLocalGet(intLen, wasm::Type::i32), builder.makeConst(1))),
                                builder.makeLocalSet(intVal, builder.makeBinary(wasm::BinaryOp::DivUInt32,
                                                                 builder.makeLocalGet(intVal, wasm::Type::i32), builder.makeConst(10))),
                                builder.makeBreak("digit_write", nullptr,
                                    builder.makeBinary(wasm::BinaryOp::NeInt32, builder.makeLocalGet(intVal, wasm::Type::i32), builder.makeConst(0))),
                            })),

                        // if (decimalPart != 0) writeDecimals(decimalPart);
                        builder.makeIf(
                            builder.makeBinary(wasm::BinaryOp::NeInt32, builder.makeLocalGet(decimalPart, wasm::Type::i32), builder.makeConst(0)),
                            builder.makeBlock({
                                builder.makeStore(1, 0, 0, builder.makeLocalGet(writePtr, wasm::Type::i32), builder.makeConst<uint32_t>('.'),
                                    wasm::Type::i32, memory),
                                builder.makeLocalSet(intLen, builder.makeConst(9)),
                                builder.makeLoop("digit_count",
                                    builder.makeIf(builder.makeBinary(wasm::BinaryOp::AndInt32,
                                                       builder.makeBinary(wasm::BinaryOp::EqInt32,
                                                           builder.makeBinary(wasm::BinaryOp::RemUInt32,
                                                               builder.makeLocalGet(decimalPart, wasm::Type::i32), builder.makeConst(10)),
                                                           builder.makeConst(0)),
                                                       builder.makeBinary(wasm::BinaryOp::GtUInt32, builder.makeLocalGet(intLen, wasm::Type::i32),
                                                           builder.makeConst(0))),
                                        builder.makeBlock({
                                            builder.makeLocalSet(
                                                decimalPart, builder.makeBinary(wasm::BinaryOp::DivUInt32,
                                                                 builder.makeLocalGet(decimalPart, wasm::Type::i32), builder.makeConst(10))),
                                            builder.makeLocalSet(intLen, builder.makeBinary(wasm::BinaryOp::SubInt32,
                                                                             builder.makeLocalGet(intLen, wasm::Type::i32), builder.makeConst(1))),
                                            builder.makeBreak("digit_count"),
                                        }))),
                                builder.makeLocalSet(
                                    writePtr, builder.makeBinary(wasm::BinaryOp::AddInt32,
                                                  builder.makeBinary(wasm::BinaryOp::AddInt32, builder.makeLocalGet(writePtr, wasm::Type::i32),
                                                      builder.makeLocalGet(intLen, wasm::Type::i32)),
                                                  builder.makeConst(1))),
                                builder.makeLocalSet(integralPart, builder.makeConst(0)),
                                builder.makeLoop("digit_write",
                                    builder.makeIf(builder.makeBinary(wasm::BinaryOp::LtUInt32, builder.makeLocalGet(integralPart, wasm::Type::i32),
                                                       builder.makeLocalGet(intLen, wasm::Type::i32)),
                                        builder.makeBlock({
                                            builder.makeLocalSet(
                                                integralPart, builder.makeBinary(wasm::BinaryOp::AddInt32,
                                                                  builder.makeLocalGet(integralPart, wasm::Type::i32), builder.makeConst(1))),
                                            builder.makeStore(1, 0, 0,
                                                builder.makeBinary(wasm::BinaryOp::SubInt32, builder.makeLocalGet(writePtr, wasm::Type::i32),
                                                    builder.makeLocalGet(integralPart, wasm::Type::i32)),
                                                builder.makeBinary(wasm::BinaryOp::AddInt32, builder.makeConst<uint32_t>('0'),
                                                    builder.makeBinary(wasm::BinaryOp::RemUInt32, builder.makeLocalGet(decimalPart, wasm::Type::i32),
                                                        builder.makeConst(10))),
                                                wasm::Type::i32, memory),
                                            builder.makeLocalSet(
                                                decimalPart, builder.makeBinary(wasm::BinaryOp::DivUInt32,
                                                                 builder.makeLocalGet(decimalPart, wasm::Type::i32), builder.makeConst(10))),
                                            builder.makeBreak("digit_write"),
                                        }))),
                            })),

                        // if (exponent < 0) { ... }
                        builder.makeIf(
                            builder.makeBinary(wasm::BinaryOp::LtSInt32, builder.makeLocalGet(exponent, wasm::Type::i32), builder.makeConst(0)),
                            builder.makeBlock({
                                builder.makeStore(2, 0, 0, builder.makeLocalGet(writePtr, wasm::Type::i32), builder.makeConst(/* -e (LE) */ 0x2d65),
                                    wasm::Type::i32, memory),
                                builder.makeLocalSet(writePtr, builder.makeBinary(wasm::BinaryOp::AddInt32,
                                                                   builder.makeLocalGet(writePtr, wasm::Type::i32), builder.makeConst(2))),
                                builder.makeLocalSet(exponent, builder.makeBinary(wasm::BinaryOp::MulInt32,
                                                                   builder.makeLocalGet(exponent, wasm::Type::i32), builder.makeConst(-1))),

                                builder.makeLocalSet(intVal, builder.makeLocalGet(exponent, wasm::Type::i32)),
                                builder.makeLocalSet(intLen, builder.makeConst(0)),
                                builder.makeLoop("digit_count",
                                    builder.makeBlock({
                                        builder.makeLocalSet(intLen, builder.makeBinary(wasm::BinaryOp::AddInt32,
                                                                         builder.makeLocalGet(intLen, wasm::Type::i32), builder.makeConst(1))),
                                        builder.makeLocalSet(intVal, builder.makeBinary(wasm::BinaryOp::DivUInt32,
                                                                         builder.makeLocalGet(intVal, wasm::Type::i32), builder.makeConst(10))),
                                        builder.makeBreak("digit_count", nullptr,
                                            builder.makeBinary(
                                                wasm::BinaryOp::NeInt32, builder.makeLocalGet(intVal, wasm::Type::i32), builder.makeConst(0))),
                                    })),
                                builder.makeLocalSet(
                                    writePtr, builder.makeBinary(wasm::BinaryOp::AddInt32, builder.makeLocalGet(writePtr, wasm::Type::i32),
                                                  builder.makeLocalGet(intLen, wasm::Type::i32))),
                                builder.makeLocalSet(intVal, builder.makeLocalGet(exponent, wasm::Type::i32)),
                                builder.makeLocalSet(intLen, builder.makeConst(1)),
                                builder.makeLoop("digit_write",
                                    builder.makeBlock({
                                        builder.makeStore(1, 0, 0,
                                            builder.makeBinary(wasm::BinaryOp::SubInt32, builder.makeLocalGet(writePtr, wasm::Type::i32),
                                                builder.makeLocalGet(intLen, wasm::Type::i32)),
                                            builder.makeBinary(wasm::BinaryOp::AddInt32, builder.makeConst<uint32_t>('0'),
                                                builder.makeBinary(
                                                    wasm::BinaryOp::RemUInt32, builder.makeLocalGet(intVal, wasm::Type::i32), builder.makeConst(10))),
                                            wasm::Type::i32, memory),
                                        builder.makeLocalSet(intLen, builder.makeBinary(wasm::BinaryOp::AddInt32,
                                                                         builder.makeLocalGet(intLen, wasm::Type::i32), builder.makeConst(1))),
                                        builder.makeLocalSet(intVal, builder.makeBinary(wasm::BinaryOp::DivUInt32,
                                                                         builder.makeLocalGet(intVal, wasm::Type::i32), builder.makeConst(10))),
                                        builder.makeBreak("digit_write", nullptr,
                                            builder.makeBinary(
                                                wasm::BinaryOp::NeInt32, builder.makeLocalGet(intVal, wasm::Type::i32), builder.makeConst(0))),
                                    })),
                            }),

                            // if (exponent > 0) { ... }
                            builder.makeIf(
                                builder.makeBinary(wasm::BinaryOp::GtSInt32, builder.makeLocalGet(exponent, wasm::Type::i32), builder.makeConst(0)),
                                builder.makeBlock({
                                    builder.makeStore(
                                        1, 0, 0, builder.makeLocalGet(writePtr, wasm::Type::i32), builder.makeConst('e'), wasm::Type::i32, memory),
                                    builder.makeLocalSet(writePtr, builder.makeBinary(wasm::BinaryOp::AddInt32,
                                                                       builder.makeLocalGet(writePtr, wasm::Type::i32), builder.makeConst(1))),

                                    builder.makeLocalSet(intVal, builder.makeLocalGet(exponent, wasm::Type::i32)),
                                    builder.makeLocalSet(intLen, builder.makeConst(0)),
                                    builder.makeLoop("digit_count",
                                        builder.makeBlock({
                                            builder.makeLocalSet(intLen, builder.makeBinary(wasm::BinaryOp::AddInt32,
                                                                             builder.makeLocalGet(intLen, wasm::Type::i32), builder.makeConst(1))),
                                            builder.makeLocalSet(intVal, builder.makeBinary(wasm::BinaryOp::DivUInt32,
                                                                             builder.makeLocalGet(intVal, wasm::Type::i32), builder.makeConst(10))),
                                            builder.makeBreak("digit_count", nullptr,
                                                builder.makeBinary(
                                                    wasm::BinaryOp::NeInt32, builder.makeLocalGet(intVal, wasm::Type::i32), builder.makeConst(0))),
                                        })),
                                    builder.makeLocalSet(
                                        writePtr, builder.makeBinary(wasm::BinaryOp::AddInt32, builder.makeLocalGet(writePtr, wasm::Type::i32),
                                                      builder.makeLocalGet(intLen, wasm::Type::i32))),
                                    builder.makeLocalSet(intVal, builder.makeLocalGet(exponent, wasm::Type::i32)),
                                    builder.makeLocalSet(intLen, builder.makeConst(1)),
                                    builder.makeLoop("digit_write",
                                        builder.makeBlock({
                                            builder.makeStore(1, 0, 0,
                                                builder.makeBinary(wasm::BinaryOp::SubInt32, builder.makeLocalGet(writePtr, wasm::Type::i32),
                                                    builder.makeLocalGet(intLen, wasm::Type::i32)),
                                                builder.makeBinary(wasm::BinaryOp::AddInt32, builder.makeConst<uint32_t>('0'),
                                                    builder.makeBinary(wasm::BinaryOp::RemUInt32, builder.makeLocalGet(intVal, wasm::Type::i32),
                                                        builder.makeConst(10))),
                                                wasm::Type::i32, memory),
                                            builder.makeLocalSet(intLen, builder.makeBinary(wasm::BinaryOp::AddInt32,
                                                                             builder.makeLocalGet(intLen, wasm::Type::i32), builder.makeConst(1))),
                                            builder.makeLocalSet(intVal, builder.makeBinary(wasm::BinaryOp::DivUInt32,
                                                                             builder.makeLocalGet(intVal, wasm::Type::i32), builder.makeConst(10))),
                                            builder.makeBreak("digit_write", nullptr,
                                                builder.makeBinary(
                                                    wasm::BinaryOp::NeInt32, builder.makeLocalGet(intVal, wasm::Type::i32), builder.makeConst(0))),
                                        })),
                                }))),

                        // outLen = writePtr - outPtr
                        builder.makeLocalSet(outLen, builder.makeBinary(wasm::BinaryOp::SubInt32, builder.makeLocalGet(writePtr, wasm::Type::i32),
                                                         builder.makeLocalGet(outPtr, wasm::Type::i32))),
                        // return [outPtr, outLen];
                        builder.makeReturn(builder.makeTupleMake(std::vector<wasm::Expression*>{
                            builder.makeLocalGet(outPtr, wasm::Type::i32),
                            builder.makeLocalGet(outLen, wasm::Type::i32),
                        }))},
                    strType)));

            fn->setLocalName(value, "num");
            fn->setLocalName(integralPart, "integralPart");
            fn->setLocalName(decimalPart, "decimalPart");
            fn->setLocalName(exponent, "exponent");
            fn->setLocalName(outPtr, "outPtr");
            fn->setLocalName(outLen, "outLen");
            fn->setLocalName(writePtr, "writePtr");
            fn->setLocalName(remainder, "remainder");
            fn->setLocalName(intLen, "intLen");
            fn->setLocalName(intVal, "intVal");
            return fn;
        }

        case WasmIntrinsicType::PRINT_F64:
        {
            wasm::Name print_string = get(WasmIntrinsicType::PRINT_STRING);
            wasm::Name number_to_string = get(WasmIntrinsicType::NUMBER_TO_STRING);
            wasm::Function* fn = wasm.addFunction(builder.makeFunction("print_f64", wasm::Signature{{wasm::Type::f64}, {}}, {},
                builder.makeBlock({builder.makeCall(print_string,
                    std::vector<wasm::Expression*>{builder.makeCall(number_to_string,
                        std::vector<wasm::Expression*>{builder.makeLocalGet(0, wasm::Type::f64)}, wasm::Type({wasm::Type::i32, wasm::Type::i32}))},
                    wasm::Type::none)})));
            fn->setLocalName(0, "n");
            return fn;
        }

        case WasmIntrinsicType::BOOL_TO_STRING:
        {
            wasm.addDataSegment(
                builder.makeDataSegment("__bool_to_string_static__", memory, false, builder.makeConst(TRUE_FALSE_INDEX), "truefalse", 9));
            // The real answer here would be to introduce a data segment for the "true"/"false" strings.
            wasm::Function* fn = wasm.addFunction(builder.makeFunction("__bool_to_string__",
                wasm::Signature{{wasm::Type::i32}, {wasm::Type::i32, wasm::Type::i32}}, {wasm::Type::i32, wasm::Type::i32},
                builder.makeBlock({builder.makeIf(builder.makeUnary(wasm::UnaryOp::EqZInt32, builder.makeLocalGet(0, wasm::Type::i32)),
                    builder.makeTupleMake(
                        std::vector<wasm::Expression*>{builder.makeConst<uint32_t>(TRUE_FALSE_INDEX + 4), builder.makeConst<uint32_t>(5)}),
                    builder.makeTupleMake(
                        std::vector<wasm::Expression*>{builder.makeConst<uint32_t>(TRUE_FALSE_INDEX), builder.makeConst<uint32_t>(4)}))})));
            fn->setLocalName(0, "b");
            fn->setLocalName(1, "strPtr");
            fn->setLocalName(2, "strLen");
            return fn;
        }

        case WasmIntrinsicType::PRINT_BOOL:
        {
            wasm::Name bool_to_string = get(WasmIntrinsicType::BOOL_TO_STRING);
            wasm::Name print_string = get(WasmIntrinsicType::PRINT_STRING);
            wasm::Function* fn = wasm.addFunction(builder.makeFunction("print_bool", wasm::Signature{{wasm::Type::i32}, {}}, {},
                builder.makeBlock({builder.makeCall(print_string,
                    std::vector<wasm::Expression*>{builder.makeCall(bool_to_string,
                        std::vector<wasm::Expression*>{builder.makeLocalGet(0, wasm::Type::i32)}, wasm::Type({wasm::Type::i32, wasm::Type::i32}))},
                    wasm::Type::none)})));
            fn->setLocalName(0, "b");
            return fn;
        }

        case WasmIntrinsicType::MEMORY_ALLOC:
        {
            wasm.addGlobal(builder.makeGlobal("heapBase", wasm::Type::i32, builder.makeConst(RESERVED_HEAP), wasm::Builder::Mutability::Immutable));
            wasm.addGlobal(
                builder.makeGlobal("heap", wasm::Type::i32, builder.makeGlobalGet("heapBase", wasm::Type::i32), wasm::Builder::Mutability::Mutable));
            wasm::Function* fn =
                wasm.addFunction(builder.makeFunction("__memory_alloc__", wasm::Signature{{wasm::Type::i32}, {wasm::Type::i32}}, {wasm::Type::i32},
                    builder.makeBlock(
                        {// Store current heap pointer as the output.
                            builder.makeLocalSet(1, builder.makeGlobalGet("heap", wasm::Type::i32)),
                            // Increase pointer by len.
                            builder.makeGlobalSet("heap", builder.makeBinary(wasm::BinaryOp::AddInt32, builder.makeGlobalGet("heap", wasm::Type::i32),
                                                              builder.makeLocalGet(0, wasm::Type::i32))),
                            // Return pointer.
                            builder.makeReturn(builder.makeLocalGet(1, wasm::Type::i32))},
                        wasm::Type::i32)));
            fn->setLocalName(0, "len");
            fn->setLocalName(1, "ptr");
            return fn;
        }

        case WasmIntrinsicType::WASM_INTRINSIC_COUNT:
            return wasm.addFunction(builder.makeFunction(
                "__wasm_intrinsic_count__", wasm::Signature{}, {}, builder.makeConst<uint32_t>(WasmIntrinsicType::WASM_INTRINSIC_COUNT)));
        }
    }
};

class WasmRegister
{
public:
    WasmRegister(wasm::Name name, wasm::Type type, TypeId typeId, bool allocated = true)
        : name(name)
        , type(type)
        , typeId(typeId)
        , allocated(allocated)
    {
    }

    virtual ~WasmRegister() {}

    virtual wasm::Expression* get() const = 0;
    virtual wasm::Expression* set(wasm::Expression* value) const = 0;

    WasmRegister* deallocate()
    {
        allocated = false;
        return this;
    }

    WasmRegister* allocate()
    {
        allocated = true;
        return this;
    }

    wasm::Name name;
    wasm::Type type;
    TypeId typeId;
    bool allocated;
};

class WasmLocalRegister : public WasmRegister
{
public:
    WasmLocalRegister(
        wasm::Builder& builder, wasm::Name name, wasm::Type type, TypeId typeId, wasm::Index index, bool param = false, bool allocated = true)
        : WasmRegister(name, type, typeId, allocated)
        , builder(builder)
        , index(index)
        , param(param)
    {
    }
    virtual ~WasmLocalRegister() {}

    wasm::Expression* get() const override
    {
        return builder.makeLocalGet(index, type);
    }

    wasm::Expression* set(wasm::Expression* value) const override
    {
        return builder.makeLocalSet(index, value);
    }

    wasm::Index index;
    bool param;

protected:
    wasm::Builder& builder;
};

struct TableFieldNameRef
{
    wasm::Index index = 0;
    TypeId typeId;
    wasm::Type type;
};

struct UtilityFunction
{
    wasm::Name name;
    wasm::Type results;
};

struct WasmCompiler
{
public:
    WasmCompiler(wasm::Module& wasm, ModulePtr checkedModule, const CompileOptions& options)
        : wasm(wasm)
        , builder(this->wasm)
        , intrinsics(wasm, builder)
        , checkedModule(checkedModule)
        , options(options)
        , functions(nullptr)
        , locals(nullptr)
        , globals(AstName())
        , variables(nullptr)
        , constants(nullptr)
        , locstants(nullptr)
        , tableShapes(nullptr)
        , tableBindings(nullptr)
        , builtins(nullptr)
        , functionTypes(nullptr)
        , localTypes(nullptr)
        , exprTypes(nullptr)
        , builtinsFold(nullptr)
        , builtinTypes(options.vectorType)
        , utilityFunctions(AstName())
    {
    }

    void compile(const AstNameTable& names, AstStatBlock* root)
    {
        wasm.features = wasm::FeatureSet::All;

        wasm.addMemory(builder.makeMemory(mem, 1));
        wasm.addExport(builder.makeExport(mem, mem, wasm::ExternalKind::Memory));

        uint8_t mainFlags = 42;

        // since access to some global objects may result in values that change over time, we block imports from non-readonly tables
        assignMutable(globals, names, options.mutableGlobals);

        // this pass analyzes mutability of locals/globals and associates locals with their initial values
        trackValues(globals, variables, root);

        // this pass tracks which calls are builtins and can be compiled more efficiently
        analyzeBuiltins(builtins, globals, variables, options, root);

        // this pass analyzes constantness of expressions
        foldConstants(constants, variables, locstants, builtinsFold, builtinsFoldMathK, root);

        // this pass analyzes table assignments to estimate table shapes for initially empty tables
        predictWasmTableShapes(tableShapes, tableBindings, root);

        // gathers all functions with the invariant that all function references are to functions earlier in the list
        // for example, function foo() return function() end end will result in two vector entries, [0] = anonymous and [1] = foo
        std::vector<AstExprFunction*> functionList;
        WasmCompiler::FunctionVisitor functionVisitor(functionList);
        root->visit(&functionVisitor);

        buildTypeMap(functionTypes, localTypes, exprTypes, root, options.vectorType, builtinTypes, builtins, globals);

        for (AstExprFunction* expr : functionList)
            compileFunction(expr, 0);

        AstExprFunction main(root->location, /*generics= */ AstArray<AstGenericType>(), /*genericPacks= */ AstArray<AstGenericTypePack>(),
            /* self= */ nullptr, AstArray<AstLocal*>(), /* vararg= */ true, /* varargLocation= */ Luau::Location(), root, /* functionDepth= */ 0,
            /* debugname= */ AstName("__main__"));
        uint32_t mainid = compileFunction(&main, mainFlags);

        const WasmFunction* mainf = functions.find(&main);
        wasm.addExport(builder.makeExport("_start", mainf->code->name, wasm::ExternalKind::Function));

        if (wasm::Global* hb = wasm.getGlobalOrNull("heapBase"))
        {
            hb->init = builder.makeConst(heapBase);
        }
    }

private:
    struct FunctionVisitor : AstVisitor
    {
        std::vector<AstExprFunction*>& functions;
        bool hasTypes = false;

        FunctionVisitor(std::vector<AstExprFunction*>& functions)
            : functions(functions)
        {
            // preallocate the result; this works around std::vector's inefficient growth policy for small arrays
            functions.reserve(16);
        }

        bool visit(AstExprFunction* node) override
        {
            node->body->visit(this);

            for (AstLocal* arg : node->args)
                hasTypes |= arg->annotation != nullptr;

            // this makes sure all functions that are used when compiling this one have been already added to the vector
            functions.push_back(node);

            return false;
        }
    };

    struct RegScope
    {
        RegScope(WasmCompiler* self)
            : self(self)
            , oldTop(self->registers.size())
        {
        }

        // This ctor is useful to forcefully adjust the stack frame in case we know that registers after a certain point are scratch and can be
        // discarded
        RegScope(WasmCompiler* self, unsigned int top)
            : self(self)
            , oldTop(self->registers.size())
        {
            LUAU_ASSERT(top <= self->registers.size());
            self->registers.resize(top);
        }

        ~RegScope()
        {
            self->registers.resize(oldTop);
        }

        WasmCompiler* self;
        unsigned int oldTop;
    };

    wasm::Module& wasm;
    wasm::Builder builder;
    WasmIntrinsics intrinsics;
    ModulePtr checkedModule;
    CompileOptions options;
    wasm::Name mem = "memory";

    DenseHashMap<AstExprFunction*, WasmFunction> functions;
    DenseHashMap<AstLocal*, WasmRegister*> locals;
    DenseHashMap<AstName, Global> globals;
    DenseHashMap<AstLocal*, Variable> variables;
    DenseHashMap<AstExpr*, Constant> constants;
    DenseHashMap<AstLocal*, Constant> locstants;
    DenseHashMap<AstExprTable*, WasmTableShape> tableShapes;
    DenseHashMap<AstExprTable*, AstLocal*> tableBindings;
    DenseHashMap<AstExprCall*, int> builtins;
    DenseHashMap<AstExprFunction*, std::string> functionTypes;
    DenseHashMap<AstLocal*, LuauBytecodeType> localTypes;
    DenseHashMap<AstExpr*, LuauBytecodeType> exprTypes;

    DenseHashMap<AstName, UtilityFunction> utilityFunctions;
    Luau::Compile::BuiltinTypes builtinTypes;

    const DenseHashMap<AstExprCall*, int>* builtinsFold = nullptr;
    bool builtinsFoldMathK = false;

    std::vector<AstLocal*> localStack;
    std::vector<AstLocal*> upvals;
    std::vector<wasm::Block*> currentBlock;
    std::vector<std::unique_ptr<WasmRegister>> registers;

    uint32_t heapBase = WasmIntrinsics::RESERVED_HEAP;

    ScopePtr getScope(const Location& loc)
    {
        Location scopeLocation;
        ScopePtr scope = nullptr;
        for (const auto& s : checkedModule->scopes)
        {
            if (s.first.encloses(loc))
            {
                if (!scope || scopeLocation.encloses(s.first))
                {
                    scopeLocation = s.first;
                    scope = s.second;
                }
            }
        }

        return scope;
    }

    void startBlock()
    {
        currentBlock.push_back(builder.makeBlock());
    }

    void appendToBlock(wasm::Expression* expr)
    {
        currentBlock.back()->list.push_back(expr);
    }

    wasm::Block* finishBlock()
    {
        wasm::Block* expr = currentBlock.back();
        currentBlock.pop_back();
        return expr;
    }

    uint32_t addDataSegment(const char* data, size_t length, wasm::Name name = "")
    {
        uint32_t addr = heapBase;
        if (name.size() == 0)
        {
            std::stringstream ss;
            ss << "@" << addr;
            name = ss.str();
        }
        wasm::DataSegment* s = wasm.addDataSegment(builder.makeDataSegment(name, mem, false, builder.makeConst(addr), data, length));
        heapBase += length;
        return addr;
    }

    wasm::Type resolveTypeRef(AstType* astType)
    {
        TypeId typeId = checkedModule->astResolvedTypes[astType];

        if (!typeId)
        {
            printf("TODO: Missing type\n");
            return wasm::Type::f32;
        }

        return resolveTypeRef(typeId);
    }

    wasm::Type resolveTypeRef(const AstTypeList& typeList)
    {
        wasm::Tuple results;
        results.reserve(typeList.types.size);
        for (auto& res : typeList.types)
        {
            results.push_back(resolveTypeRef(res));
        }
        return wasm::Type(results);
    }

    wasm::Type resolveTypeRef(TypeId t)
    {
        // Resolve to "real" type first.
        t = follow(t);

        if (auto* pt = get<PrimitiveType>(t))
        {
            switch (pt->type)
            {
            case PrimitiveType::Boolean:
                return wasm::Type::i32;

            case PrimitiveType::Number:
                return wasm::Type::f64;

            case PrimitiveType::String:
                // Strings are (ptr, len) tuples for now.
                return wasm::Type({wasm::Type::i32, wasm::Type::i32});

            default:
                printf("TODO: Handle PrimitiveType %u\n", pt->type);
                break;
            }
        }
        else if (auto* tt = get<TableType>(t))
        {
            return wasm::Type(resolveTableTypeRef(t), wasm::Nullability::Nullable);
        }
        printf("TODO: Handle type %s\n", Luau::toString(t).c_str());
        return wasm::Type::f32;
    }

    wasm::Type resolveTypeRef(TypePackId pack)
    {
        const auto& [v, tail] = flatten(pack);

        wasm::Tuple results;
        results.reserve(v.size());
        for (auto& res : v)
        {
            results.push_back(resolveTypeRef(res));
        }

        return wasm::Type(results);
    }

    wasm::HeapType resolveTableTypeRef(TypeId ty)
    {
        // Resolve to "real" type first.
        ty = follow(ty);

        wasm::FieldList fields;

        if (auto* tab = get<TableType>(ty))
        {
            for (auto& prop : tab->props)
            {
                auto propType = prop.second.type();
                fields.push_back(wasm::Field(resolveTypeRef(propType), wasm::Mutability::Mutable));
            }
        }

        return wasm::Struct(fields);
    }

    std::optional<TableFieldNameRef> resolveTableFieldNameRef(TypeId type, AstName index)
    {
        const auto* tableType = get<TableType>(type);
        TableFieldNameRef fieldRef;
        for (auto& prop : tableType->props)
        {
            if (prop.first == index.value)
            {
                fieldRef.typeId = follow(prop.second.type());
                fieldRef.type = resolveTypeRef(fieldRef.typeId);
                return fieldRef;
            }
            ++fieldRef.index;
        }
        return std::optional<TableFieldNameRef>();
    }

    WasmRegister* pushRegLocal(AstNode* node, TypeId typeId, wasm::Type type, wasm::Name name = "", bool param = false)
    {
        wasm::Index index = registers.size();
        std::unique_ptr reg = std::make_unique<WasmLocalRegister>(builder, name, type, typeId, index, param);
        LUAU_ASSERT(reg->type.size() > 0);
        return registers.emplace_back(std::move(reg)).get();
    }

    WasmRegister* pushLocal(AstLocal* local, TypeId typeId, wasm::Type type, wasm::Name name = "")
    {
        WasmRegister* reg = pushRegLocal(nullptr, typeId, type, name, /* param= */ false);
        localStack.push_back(local);
        locals[local] = reg;
        return reg;
    }

    WasmRegister* pushParamLocal(AstLocal* local, TypeId typeId, wasm::Type type, wasm::Name name = "")
    {
        WasmRegister* reg = pushRegLocal(nullptr, typeId, type, name, /* param= */ true);
        localStack.push_back(local);
        locals[local] = reg;
        return reg;
    }

    void popLocals(size_t start)
    {
        for (size_t i = start; i < localStack.size(); ++i)
        {
            WasmRegister** l = locals.find(localStack[i]);
            LUAU_ASSERT(l);
            LUAU_ASSERT((*l)->allocated);

            (*l)->allocated = false;
        }

        localStack.resize(start);
    }

    uint32_t compileFunction(AstExprFunction* func, uint8_t protoflags)
    {
        LUAU_ASSERT(!functions.contains(func));
        LUAU_ASSERT(registers.empty() && localStack.empty() && upvals.empty());

        RegScope rs(this);

        uint32_t fid = 1000 + functions.size();
        WasmFunction& f = functions[func];
        f.id = fid;

        bool isMain = protoflags == 42;

        ScopePtr scope = getScope(func->body->location);
        if (!scope)
        {
            printf("TODO: Error - no scope\n");
            return -1;
        }

        uint8_t self = 0;
        uint8_t localsOffset = self + unsigned(func->args.size);

        // Determine function signature.
        wasm::Tuple paramsTuple;
        paramsTuple.reserve(func->args.size);
        for (auto& arg : func->args)
        {
            auto maybeType = scope->lookup(arg);
            LUAU_ASSERT(maybeType);
            TypeId argTypeId = follow(*maybeType);
            wasm::Type argType = resolveTypeRef(argTypeId);
            pushParamLocal(arg, argTypeId, argType, arg->name.value);
            paramsTuple.push_back(argType);
        }
        wasm::Type params(paramsTuple);

        wasm::Type results;
        if (isMain)
        {
            // These are the exports of the file, ignore.
            results = wasm::Type::none;
        }
        else if (func->returnAnnotation.has_value())
        {
            results = resolveTypeRef(*func->returnAnnotation);
        }
        else
        {
            results = resolveTypeRef(scope->returnType);
        }

        startBlock();

        AstStatBlock* stat = func->body;

        AstStatReturn* exportsReturn;
        for (size_t i = 0; i < stat->body.size; ++i)
        {
            AstStat* sub = stat->body.data[i];
            if (isMain && sub->is<AstStatReturn>())
            {
                exportsReturn = sub->as<AstStatReturn>();
                continue;
            }

            compileStat(stat->body.data[i]);
        }

        wasm::TypeList localTypes;
        wasm::Index expectedLocalTypesCount = registers.size() - params.size();
        localTypes.resize(expectedLocalTypesCount);
        for (auto& reg : registers)
        {
            LUAU_ASSERT(reg->allocated);
            if (WasmLocalRegister* lreg = dynamic_cast<WasmLocalRegister*>(reg.get()))
            {
                if (lreg->param)
                {
                    LUAU_ASSERT(lreg->index < params.size());
                    continue;
                }
                LUAU_ASSERT(reg->type.size() > 0);
                LUAU_ASSERT(lreg->index >= params.size());
                wasm::Index relativeIndex = lreg->index - params.size();
                LUAU_ASSERT(relativeIndex < expectedLocalTypesCount);
                localTypes[relativeIndex] = reg->type;
            }
        }

        for (auto& localType : localTypes)
        {
            LUAU_ASSERT(localType.size() > 0);
        }

        wasm::Expression* body = finishBlock();

        f.code = wasm.addFunction(builder.makeFunction(func->debugname.value, wasm::Signature(params, results), std::move(localTypes), body));

        for (auto& reg : registers)
        {
            if (WasmLocalRegister* lreg = dynamic_cast<WasmLocalRegister*>(reg.get()))
            {
                if (lreg->name.size() > 0)
                {
                    f.code->setLocalName(lreg->index, lreg->name);
                }
            }
        }

        popLocals(0);
        upvals.clear(); // note: instead of std::move above, we copy & clear to preserve capacity for future pushes
        registers.clear();

        if (exportsReturn && exportsReturn->list.size == 1)
        {
            if (AstExprTable* tab = (*exportsReturn->list.data)->as<AstExprTable>())
            {
                for (auto& item : tab->items)
                {
                    if (item.kind != AstExprTable::Item::Kind::Record)
                    {
                        continue;
                    }

                    AstExprConstantString* key = item.key->as<AstExprConstantString>();
                    AstExprLocal* value = item.value->as<AstExprLocal>();
                    if (key && value)
                    {
                        std::string exportName(key->value.begin(), key->value.end());
                        wasm.addExport(builder.makeExport(exportName.c_str(), value->local->name.value, wasm::ExternalKind::Function));
                    }
                }
            }
        }

        return fid;
    }

    void compileStat(AstStat* stat)
    {
        if (stat->is<AstStatLocalFunction>() || stat->is<AstStatFunction>())
        {
            // We compile these separately.
            return;
        }

        if (auto* loc = stat->as<AstStatLocal>())
        {
            for (auto* expr : loc->values)
            {
                appendToBlock(compileExpr(expr));
            }

            auto scope = getScope(loc->location);
            LUAU_ASSERT(scope);

            // Reverse assign
            for (auto it = loc->vars.rbegin(); it != loc->vars.rend(); ++it)
            {
                AstLocal* target = (*it);
                TypeId targetTypeId = follow(*scope->lookup(target));
                WasmRegister* l = pushLocal(target, targetTypeId, resolveTypeRef(targetTypeId));

                // 1. Find local.
                LUAU_ASSERT(l->allocated);

                // 2. Assign to local. We have to provide an expression.
                appendToBlock(l->set(builder.makeNop()));
            }
        }
        else if (auto* assign = stat->as<AstStatAssign>())
        {
            for (auto* expr : assign->values)
            {
                appendToBlock(compileExpr(expr));
            }

            auto scope = getScope(assign->location);
            LUAU_ASSERT(scope);

            for (auto it = assign->vars.rbegin(); it != assign->vars.rend(); ++it)
            {
                AstExpr* targetExpr = (*it);
                if (auto* targetLocal = targetExpr->as<AstExprLocal>())
                {
                    AstLocal* target = targetLocal->local;
                    WasmRegister* l = locals[target];
                    LUAU_ASSERT(l->allocated);
                    appendToBlock(l->set(builder.makeNop()));
                }
                else if (auto* propExpr = targetExpr->as<AstExprIndexName>())
                {
                    WasmRegister* tableRef = compileTableToLocal(propExpr);
                    LUAU_ASSERT(tableRef);

                    auto fieldRef = resolveTableFieldNameRef(tableRef->typeId, propExpr->index);
                    LUAU_ASSERT(fieldRef);

                    WasmRegister* reg = pushRegLocal(propExpr, fieldRef->typeId, fieldRef->type);
                    // Stack value in temporary local.
                    appendToBlock(reg->set(builder.makeNop()));

                    appendToBlock(builder.makeStructSet(fieldRef->index, tableRef->get(), reg->get()));
                }
                else
                {
                    printf(";; TODO: Assign to non-local expr\n");
                }
            }
        }
        else if (auto* compAssign = stat->as<AstStatCompoundAssign>())
        {
            wasm::Expression* right = compileExpr(compAssign->value);

            if (auto* targetLocal = compAssign->var->as<AstExprLocal>())
            {
                AstLocal* target = targetLocal->local;
                WasmRegister* l = locals[target];
                LUAU_ASSERT(l->allocated);

                wasm::Expression* left = l->get();
                wasm::Expression* newValue = toWasmBinaryExpr(compAssign->op, left, right);

                appendToBlock(l->set(newValue));
            }
            else if (auto* propExpr = compAssign->var->as<AstExprIndexName>())
            {
                WasmRegister* tableRef = compileTableToLocal(propExpr);
                LUAU_ASSERT(tableRef);

                auto fieldRef = resolveTableFieldNameRef(tableRef->typeId, propExpr->index);
                LUAU_ASSERT(fieldRef);

                wasm::Expression* getTable = tableRef->get();

                wasm::Expression* left = builder.makeStructGet(fieldRef->index, getTable, fieldRef->type);
                wasm::Expression* newValue = toWasmBinaryExpr(compAssign->op, left, right);

                appendToBlock(builder.makeStructSet(fieldRef->index, getTable, newValue));
            }
            else
            {
                printf(";; TODO: Assign to non-local expr\n");
            }
        }
        else if (auto* exprStat = stat->as<AstStatExpr>())
        {
            wasm::Expression* expr = compileExpr(exprStat->expr);

            // If this is a call, ensure we clean up the stack.
            if (auto* call = expr->cast<wasm::Call>())
            {
                // Ensure that we only do this if the return type is non-empty.
                if (call->type != wasm::Type::none)
                {
                    expr = builder.makeDrop(expr);
                }
            }
            appendToBlock(expr);
        }
        else if (auto* ret = stat->as<AstStatReturn>())
        {
            ScopePtr scope = getScope(ret->location);
            LUAU_ASSERT(scope);

            // The builder doesn't appear to support returning multiple values..?
            wasm::Block* result = builder.makeBlock();
            result->type = resolveTypeRef(scope->returnType);
            for (AstExpr* v : ret->list)
            {
                result->list.push_back(compileExpr(v));
            }
            appendToBlock(builder.makeReturn(result));
        }
        else if (auto* ifBr = stat->as<AstStatIf>())
        {
            wasm::Expression* condition = compileExpr(ifBr->condition);
            wasm::Expression* ifTrue = compileBody(ifBr->thenbody);
            wasm::Expression* ifFalse = compileBody(ifBr->elsebody);
            appendToBlock(builder.makeIf(condition, ifTrue, ifFalse));
        }
        else if (auto* typeDecl = stat->as<AstStatTypeAlias>())
        {
            // Ignore, nothing to be done.
        }
        else
        {
            printf(";; Unsupported statement: %d\n", stat->classIndex);
            appendToBlock(builder.makeDrop(builder.makeConst(42)));
        }
    }

    WasmRegister* compileTableToLocal(AstExprIndexName* indexName)
    {
        if (auto* refLocal = indexName->expr->as<AstExprLocal>())
        {
            AstLocal* target = refLocal->local;
            WasmRegister* l = locals[target];
            LUAU_ASSERT(l->allocated);

            return l;
        }
        else
        {
            printf(";; Create local for expression..?\n");
            return nullptr;
        }
    }

    wasm::Expression* compileBody(AstStat* body)
    {
        if (!body)
        {
            return nullptr;
        }

        startBlock();

        if (auto* statBlock = body->as<AstStatBlock>())
        {
            for (auto* subStat : statBlock->body)
            {
                compileStat(subStat);
            }
        }
        else
        {
            compileStat(body);
        }

        return finishBlock();
    }

    wasm::Expression* toWasmBinaryExpr(AstExprBinary::Op astOp, wasm::Expression* left, wasm::Expression* right)
    {
        bool isInt32CastOp = false;
        wasm::BinaryOp op;

        switch (astOp)
        {
        case AstExprBinary::Op::Add:
            op = wasm::BinaryOp::AddFloat64;
            break;

        case AstExprBinary::Op::Sub:
            op = wasm::BinaryOp::SubFloat64;
            break;

        case AstExprBinary::Op::Mul:
            op = wasm::BinaryOp::MulFloat64;
            break;

        case AstExprBinary::Op::Div:
            op = wasm::BinaryOp::DivFloat64;
            break;

        case AstExprBinary::Op::FloorDiv:
            op = wasm::BinaryOp::DivSInt32;
            isInt32CastOp = true;
            break;

        case AstExprBinary::Op::Mod:
            op = wasm::BinaryOp::RemSInt32;
            isInt32CastOp = true;
            break;

        case AstExprBinary::Op::Pow:
        {
            WasmRegister* leftReg = pushRegLocal(nullptr, nullptr, wasm::Type::f64);
            wasm::Expression* storeLeft = leftReg->set(left);
            WasmRegister* rightReg = pushRegLocal(nullptr, nullptr, wasm::Type::f64);
            wasm::Expression* storeRight = rightReg->set(right);

            left = leftReg->get();
            right = rightReg->get();

            wasm::Expression* fallback = builder.makeUnreachable();

            wasm::Expression* isPow2 = builder.makeBinary(wasm::BinaryOp::EqFloat64, right, builder.makeConst(2.0));
            wasm::Expression* pow2 = builder.makeBinary(wasm::BinaryOp::MulFloat64, left, left);

            wasm::Expression* isSqrt = builder.makeBinary(wasm::BinaryOp::EqFloat64, right, builder.makeConst(0.5));
            wasm::Expression* sqrt = builder.makeUnary(wasm::UnaryOp::SqrtFloat64, left);

            wasm::Expression* isNoop = builder.makeBinary(wasm::BinaryOp::EqFloat64, right, builder.makeConst(1.0));
            wasm::Expression* noop = left;

            return builder.makeBlock(
                {storeLeft, storeRight, builder.makeIf(isNoop, noop, builder.makeIf(isSqrt, sqrt, builder.makeIf(isPow2, pow2, fallback)))});
        }

        case AstExprBinary::Op::Concat:
        {
            // TODO: Handle implicit to-string.
            return builder.makeCall(intrinsics.get(WasmIntrinsicType::STRING_CONCAT), {left, right}, wasm::Type({wasm::Type::i32, wasm::Type::i32}));
        }

        case AstExprBinary::Op::CompareNe:
            op = wasm::BinaryOp::NeFloat64;
            break;

        case AstExprBinary::Op::CompareEq:
            op = wasm::BinaryOp::EqFloat64;
            break;

        case AstExprBinary::Op::CompareLt:
            op = wasm::BinaryOp::LtFloat64;
            break;

        case AstExprBinary::Op::CompareLe:
            op = wasm::BinaryOp::LeFloat64;
            break;

        case AstExprBinary::Op::CompareGt:
            op = wasm::BinaryOp::GtFloat64;
            break;

        case AstExprBinary::Op::CompareGe:
            op = wasm::BinaryOp::GeFloat64;
            break;

        case AstExprBinary::Op::And:
            op = wasm::BinaryOp::AndInt32;
            break;

        case AstExprBinary::Op::Or:
            op = wasm::BinaryOp::OrInt32;
            break;

        default:
            printf("Unhandled binary op: %d\n", astOp);
            op = wasm::BinaryOp::XorVec128;
            break;
        }

        if (isInt32CastOp)
        {
            // Convert operands to i32.
            left = builder.makeUnary(wasm::UnaryOp::TruncSFloat64ToInt32, left);
            right = builder.makeUnary(wasm::UnaryOp::TruncSFloat64ToInt32, right);
        }

        wasm::Expression* res = builder.makeBinary(op, left, right);

        if (isInt32CastOp)
        {
            return builder.makeUnary(wasm::UnaryOp::ConvertSInt32ToFloat64, res);
        }

        return res;
    }

    wasm::UnaryOp toWasmUnaryOp(AstExprUnary::Op op)
    {
        switch (op)
        {
        case AstExprUnary::Op::Minus:
            return wasm::UnaryOp::NegFloat64;

        case AstExprUnary::Op::Not:
            // `true` for 0, `false` otherwise.
            return wasm::UnaryOp::EqZInt32;

        default:
            printf("Unhandled unary op: %d\n", op);
            return wasm::UnaryOp::NegVecF32x4;
        }
    }

    AstExprFunction* getFunctionExpr(AstExpr* node)
    {
        if (AstExprLocal* expr = node->as<AstExprLocal>())
        {
            Variable* lv = variables.find(expr->local);

            if (!lv || lv->written || !lv->init)
                return nullptr;

            return getFunctionExpr(lv->init);
        }
        else if (AstExprGroup* expr = node->as<AstExprGroup>())
            return getFunctionExpr(expr->expr);
        else if (AstExprTypeAssertion* expr = node->as<AstExprTypeAssertion>())
            return getFunctionExpr(expr->expr);
        else
            return node->as<AstExprFunction>();
    }

    std::optional<wasm::Name> getUtilityFunction(const AstName& name)
    {
        if (utilityFunctions.contains(name))
        {
            return utilityFunctions[name].name;
        }

        if (strcmp("print_f64", name.value) == 0)
        {
            return intrinsics.get(WasmIntrinsicType::PRINT_F64);

            // UtilityFunction& uf = utilityFunctions[name];
            // uf.name = wasm::Name("print_f64");
            // wasm::Type params = wasm::Type::f64;
            // uf.results = wasm::Type::none;
            // wasm::Function* f = wasm.addFunction(builder.makeFunction(uf.name, wasm::Signature{params, uf.results}, {}));
            // f->module = "luau:util";
            // f->base = "print_number";
            // return uf.name;
        }
        else if (strcmp("print_string", name.value) == 0)
        {
            return intrinsics.get(WasmIntrinsicType::PRINT_STRING);
        }
        else if (strcmp("print_bool", name.value) == 0)
        {
            return intrinsics.get(WasmIntrinsicType::PRINT_BOOL);
        }

        return std::optional<wasm::Name>();
    }

    wasm::Expression* compileExpr(AstExpr* expr)
    {
        if (auto* grp = expr->as<AstExprGroup>())
        {
            return compileExpr(grp->expr);
        }
        else if (auto* loc = expr->as<AstExprLocal>())
        {
            AstLocal* target = loc->local;
            WasmRegister* l = locals[target];
            return l->get();
        }
        else if (auto* call = expr->as<AstExprCall>())
        {
            wasm::Name callee;
            wasm::Type returnType;

            if (AstExprFunction* calleeNode = getFunctionExpr(call->func))
            {
                auto& f = functions[calleeNode];
                callee = f.code->name;
                returnType = f.code->getResults();
            }
            else if (int builtinId = builtins[call])
            {
                std::cout << "Handle builtin: " << call->func << " (" << builtinId << ")\n";
                // printf("Could not find callee for call.\n");
                return builder.makeConst(1.41);
            }
            else if (AstExprGlobal* funcAstGlobal = call->func->as<AstExprGlobal>())
            {
                if (auto maybeCallee = getUtilityFunction(funcAstGlobal->name))
                {
                    callee = *maybeCallee;
                    returnType = wasm::Type::none;
                }
                else
                {
                    std::cout << "Handle potential global: " << call->func << " (" << funcAstGlobal->name.value << ")\n";
                    return builder.makeConst(1.42);
                }
            }
            else
            {
                std::cout << "Unknown function: " << call->func << "(" << call->func->classIndex << ")\n";
                return builder.makeConst(1.41);
            }

            std::vector<wasm::Expression*> args;
            args.reserve(call->args.size);
            for (auto* arg : call->args)
            {
                args.push_back(compileExpr(arg));
            }
            return builder.makeCall(callee, args, returnType);
        }
        else if (auto* num = expr->as<AstExprConstantNumber>())
        {
            return builder.makeConst(num->value);
        }
        else if (auto* b = expr->as<AstExprConstantBool>())
        {
            return builder.makeConst<int32_t>(b->value);
        }
        else if (auto* s = expr->as<AstExprConstantString>())
        {
            size_t length = s->value.size;
            uint32_t addr = addDataSegment(s->value.data, length);
            return builder.makeTupleMake(std::vector<wasm::Expression*>{
                builder.makeConst<uint32_t>(addr),
                builder.makeConst<uint32_t>(length),
            });
        }
        else if (auto* indexName = expr->as<AstExprIndexName>())
        {
            WasmRegister* tableRef = compileTableToLocal(indexName);
            LUAU_ASSERT(tableRef);

            auto fieldRef = resolveTableFieldNameRef(tableRef->typeId, indexName->index);
            LUAU_ASSERT(fieldRef);

            return builder.makeStructGet(fieldRef->index, tableRef->get(), fieldRef->type);
        }
        else if (auto* bin = expr->as<AstExprBinary>())
        {
            wasm::Expression* left = compileExpr(bin->left);
            wasm::Expression* right = compileExpr(bin->right);
            return toWasmBinaryExpr(bin->op, left, right);
        }
        else if (auto* unary = expr->as<AstExprUnary>())
        {
            return builder.makeUnary(toWasmUnaryOp(unary->op), compileExpr(unary->expr));
        }
        else if (auto* tab = expr->as<AstExprTable>())
        {
            // 1. Determine type of the table.
            TypeId tableType;

            if (AstLocal* binding = tableBindings[tab])
            {
                ScopePtr scope = getScope(binding->location);
                LUAU_ASSERT(scope);
                if (std::optional<TypeId> t = scope->lookup(binding))
                {
                    tableType = follow(*t);
                }
            }

            LUAU_ASSERT(tableType);

            // Simple case: Create with default values.
            if (tab->items.size == 0)
            {
                return builder.makeStructNew(resolveTableTypeRef(tableType), {});
            }

            // 2. Create table with given values.
            printf(";; TODO: Init table with values.\n");
        }
        printf(";; Unsupported expression: %d\n", expr->classIndex);
        return builder.makeConst(1.3);
    }
};

std::unique_ptr<wasm::Module> compileToWasm(SourceModule* sourceModule, ModulePtr checkedModule, const CompileOptions& options)
{
    auto wasm = std::make_unique<wasm::Module>();
    wasm->features = wasm::FeatureSet::All;

    AstStatBlock* root = sourceModule->root;

    WasmCompiler compiler(*wasm.get(), checkedModule, options);
    compiler.compile(*sourceModule->names, root);

    if (options.optimizationLevel >= 1)
    {
        // Convert to binary format & parse.
        wasm::BufferWithRandomAccess buffer;
        wasm::PassOptions passOptions;
        wasm::WasmBinaryWriter writer(wasm.get(), buffer, passOptions);
        writer.setNamesSection(true);
        writer.setEmitModuleName(true);
        writer.write();

        std::vector<char> bytes(buffer.begin(), buffer.end());
        wasm = std::make_unique<wasm::Module>();
        wasm::WasmBinaryReader reader(*wasm.get(), wasm::FeatureSet::All, bytes);
        reader.read();

        // Run binaryen optimization passes.
        wasm::PassRunner runner(wasm.get());
        runner.options.optimizeLevel = options.optimizationLevel;
        runner.addDefaultOptimizationPasses();
        runner.run();
    }

    return wasm;
}

static const std::string kDebugDefinitionLuaSrc = R"BUILTIN_SRC(
declare function print_bool(b: boolean): ()
declare function print_f64(n: number): ()
declare function print_string(s: string): ()
)BUILTIN_SRC";

static void registerDebugGlobals(Frontend& frontend, Luau::GlobalTypes& globals, bool typeCheckForAutocomplete = false)
{
    LoadDefinitionFileResult loadResult = frontend.loadDefinitionFile(
        frontend.globals, globals.globalScope, kDebugDefinitionLuaSrc, "@luau", /* captureComments */ false, typeCheckForAutocomplete);
    LUAU_ASSERT(loadResult.success);
}

static const std::string kWasiPreview1DefinitionLuaSrc = R"BUILTIN_SRC(
declare wasi_snapshot_preview1: {
    fd_write: (fd: number, iovs_ptr: number, iovs_len: number, written_ptr: number) -> number
}
)BUILTIN_SRC";

static void registerWasiGlobals(Frontend& frontend, Luau::GlobalTypes& globals, bool typeCheckForAutocomplete = false)
{
    LoadDefinitionFileResult loadResult = frontend.loadDefinitionFile(
        frontend.globals, globals.globalScope, kWasiPreview1DefinitionLuaSrc, "@luau", /* captureComments */ false, typeCheckForAutocomplete);
    LUAU_ASSERT(loadResult.success);
}

std::string compileToWasm(Frontend& frontend, const std::string& moduleName, const WasmCompileOptions& wasmOptions, const CompileOptions& options,
    const ParseOptions& parseOptions)
{
    // Force some necessary options.
    frontend.options.retainFullTypeGraphs = true;
    frontend.options.runLintChecks = true;

    registerBuiltinGlobals(frontend, frontend.globals);
    registerDebugGlobals(frontend, frontend.globals);
    registerWasiGlobals(frontend, frontend.globals);
    freeze(frontend.globals.globalTypes);

    CheckResult checks = frontend.check(moduleName);
    if (!checks.errors.empty())
    {
        const auto& checkError = checks.errors.front();
        std::string error = format(
            ":%d: %s", checkError.location.begin.line + 1, toString(checkError, Luau::TypeErrorToStringOptions{frontend.fileResolver}).c_str());

        return "(; error: " + error + ";)";
    }

    SourceModule* sourceModule = frontend.getSourceModule(moduleName);
    ModulePtr checkedModule = frontend.moduleResolver.getModule(moduleName);

    try
    {
        std::unique_ptr<wasm::Module> wasm = compileToWasm(sourceModule, checkedModule, options);

        if (wasmOptions.format == WasmOutputFormat::WAT)
        {
            std::ostringstream ss;
            ss << *wasm.get();
            return ss.str();
        }
        else
        {
            wasm::BufferWithRandomAccess buffer;
            wasm::PassOptions passOptions;
            wasm::WasmBinaryWriter writer(wasm.get(), buffer, passOptions);
            writer.setNamesSection(true);
            writer.setEmitModuleName(true);
            writer.write();

            return std::string(buffer.begin(), buffer.end());
        }
    }
    catch (CompileError& e)
    {
        std::string error = format(":%d: %s", e.getLocation().begin.line + 1, e.what());
        return "(; error: " + error + ";)";
    }
    catch (wasm::ParseException& e)
    {
        e.dump(std::cerr);
        throw e;
    }
}

struct StringContentsFileResolver : FileResolver
{
    std::string code;

    StringContentsFileResolver(const std::string& code)
        : code(code)
    {
    }

    std::optional<SourceCode> readSource(const ModuleName& name) override
    {
        return SourceCode{code, SourceCode::Type::Module};
    }
};

std::string compileToWasm(
    const std::string& source, const WasmCompileOptions& wasmOptions, const CompileOptions& options, const ParseOptions& parseOptions)
{
    StringContentsFileResolver fileResolver(source);
    NullConfigResolver configResolver;
    // Default to strict mode since we need good type info.
    configResolver.defaultConfig.mode = Mode::Strict;
    Frontend frontend(&fileResolver, &configResolver, {});

    std::string moduleName = "";

    return compileToWasm(frontend, moduleName, wasmOptions, options, parseOptions);
}

} // namespace Luau
