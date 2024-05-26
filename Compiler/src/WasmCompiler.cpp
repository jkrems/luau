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

            // case AstExprBinary::Op::Concat:
            //     op = wasm::BinaryOp::ConcatFloat64;
            //     break;

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
            UtilityFunction& uf = utilityFunctions[name];
            uf.name = wasm::Name("print_f64");
            // TODO: Change params type to be `...any`.
            // TODO: Consider implementing this in terms of Wasi..?
            wasm::Type params = wasm::Type::f64;
            uf.results = wasm::Type::none;
            wasm::Function* f = wasm.addFunction(builder.makeFunction(uf.name, wasm::Signature{params, uf.results}, {}));
            f->module = "luau:util";
            f->base = "print_number";
            return uf.name;
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
declare function print_f64(n: number): ()
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
