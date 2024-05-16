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
#include "TableShape.h"
#include "Types.h"
#include "ValueTracking.h"

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
};

enum UtilityFunctionType
{
    PRINT,
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
        predictTableShapes(tableShapes, root);

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
        wasm.addStart(mainf->code->name);
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
            , oldTop(self->regTop)
        {
        }

        // This ctor is useful to forcefully adjust the stack frame in case we know that registers after a certain point are scratch and can be
        // discarded
        RegScope(WasmCompiler* self, unsigned int top)
            : self(self)
            , oldTop(self->regTop)
        {
            LUAU_ASSERT(top <= self->regTop);
            self->regTop = top;
        }

        ~RegScope()
        {
            self->regTop = oldTop;
        }

        WasmCompiler* self;
        unsigned int oldTop;
    };

    wasm::Module& wasm;
    wasm::Builder builder;
    ModulePtr checkedModule;
    CompileOptions options;

    DenseHashMap<AstExprFunction*, WasmFunction> functions;
    DenseHashMap<AstLocal*, Local> locals;
    DenseHashMap<AstName, Global> globals;
    DenseHashMap<AstLocal*, Variable> variables;
    DenseHashMap<AstExpr*, Constant> constants;
    DenseHashMap<AstLocal*, Constant> locstants;
    DenseHashMap<AstExprTable*, TableShape> tableShapes;
    DenseHashMap<AstExprCall*, int> builtins;
    DenseHashMap<AstExprFunction*, std::string> functionTypes;
    DenseHashMap<AstLocal*, LuauBytecodeType> localTypes;
    DenseHashMap<AstExpr*, LuauBytecodeType> exprTypes;

    DenseHashMap<AstName, UtilityFunction> utilityFunctions;
    Luau::Compile::BuiltinTypes builtinTypes;

    const DenseHashMap<AstExprCall*, int>* builtinsFold = nullptr;
    bool builtinsFoldMathK = false;

    // compileFunction state, gets reset for every function
    unsigned int regTop = 0;
    unsigned int stackSize = 0;

    std::vector<AstLocal*> localStack;
    std::vector<AstLocal*> upvals;
    std::vector<wasm::Block*> currentBlock;

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

    uint8_t allocReg(AstNode* node, unsigned int count)
    {
        unsigned int top = regTop;

        regTop += count;
        stackSize = std::max(stackSize, regTop);

        return uint8_t(top);
    }

    void pushLocal(AstLocal* local, uint8_t reg, wasm::Type type)
    {
        localStack.push_back(local);

        Local& l = locals[local];

        LUAU_ASSERT(!l.allocated);

        l.reg = reg;
        l.allocated = true;
        l.type = type;
    }

    void popLocals(size_t start)
    {

        for (size_t i = start; i < localStack.size(); ++i)
        {
            Local* l = locals.find(localStack[i]);
            LUAU_ASSERT(l);
            LUAU_ASSERT(l->allocated);

            l->allocated = false;
        }

        localStack.resize(start);
    }

    uint32_t compileFunction(AstExprFunction* func, uint8_t protoflags)
    {
        LUAU_ASSERT(!functions.contains(func));
        LUAU_ASSERT(regTop == 0 && stackSize == 0 && localStack.empty() && upvals.empty());

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
        uint8_t args = allocReg(func, localsOffset);

        // Determine function signature.
        wasm::Tuple paramsTuple;
        paramsTuple.reserve(func->args.size);
        for (auto& arg : func->args)
        {
            wasm::Type argType = resolveTypeRef(arg->annotation);
            pushLocal(arg, args + paramsTuple.size(), argType);
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

        for (size_t i = 0; i < stat->body.size; ++i)
        {
            AstStat* sub = stat->body.data[i];
            if (isMain && sub->is<AstStatReturn>())
            {
                continue;
            }

            compileStat(stat->body.data[i]);
        }

        wasm::TypeList locals;
        // Find locals that have been declared, minus the known params.
        for (uint8_t li = args + localsOffset; li < localStack.size(); ++li)
        {
            AstLocal* astLocal = localStack[li];
            Local& l = this->locals[astLocal];
            LUAU_ASSERT(l.allocated);
            locals.push_back(l.type);
        }

        popLocals(0);

        wasm::Expression* body = finishBlock();

        f.code = wasm.addFunction(builder.makeFunction(func->debugname.value, wasm::Signature(params, results), std::move(locals), body));
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
            uint8_t regs = allocReg(loc, loc->vars.size);
            uint8_t varIdx = 0;
            for (auto it = loc->vars.rbegin(); it != loc->vars.rend(); ++it)
            {
                AstLocal* target = (*it);
                pushLocal(target, regs + varIdx, resolveTypeRef(*scope->lookup(target)));

                // 1. Find local.
                Local& l = locals[target];
                LUAU_ASSERT(l.allocated);

                // 2. Assign to local. We have to provide an expression.
                appendToBlock(builder.makeLocalSet(l.reg, builder.makeNop()));

                ++varIdx;
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
            if (!scope)
            {
                printf("TODO: Error - no scope\n");
                appendToBlock(builder.makeConst(13.7));
                return;
            }

            // The builder doesn't appear to support returning multiple values..?
            wasm::Block* result = builder.makeBlock();
            result->type = resolveTypeRef(scope->returnType);
            for (AstExpr* v : ret->list)
            {
                result->list.push_back(compileExpr(v));
            }
            appendToBlock(builder.makeReturn(result));
        }
        else
        {
            printf("Unsupported statement: %d\n", stat->classIndex);
            appendToBlock(builder.makeConst(42));
        }
    }

    wasm::BinaryOp toWasmBinaryOp(Luau::AstExprBinary::Op op)
    {
        switch (op)
        {
        case AstExprBinary::Op::Add:
            return wasm::BinaryOp::AddFloat64;

        default:
            return wasm::BinaryOp::XorVec128;
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

        if (strcmp("print", name.value) == 0)
        {
            UtilityFunction& uf = utilityFunctions[name];
            uf.name = wasm::Name("print");
            // TODO: Change params type to be `...any`.
            // TODO: Consider implementing this in terms of Wasi..?
            wasm::Type params = wasm::Type::f64;
            uf.results = wasm::Type::none;
            wasm::Function* f = wasm.addFunction(builder.makeFunction(uf.name, wasm::Signature{params, uf.results}, {}));
            f->module = "luau:util";
            f->base = "print";
            return uf.name;
        }

        return std::optional<wasm::Name>();
    }

    wasm::Expression* compileExpr(AstExpr* expr)
    {
        if (auto* loc = expr->as<AstExprLocal>())
        {
            AstLocal* target = loc->local;
            // 1. Find local.
            Local& l = locals[target];
            // 2. Determine the local type.
            wasm::Type lType = wasm::Type::f64;
            // 3. Get the local.
            return builder.makeLocalGet(l.reg, lType);
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
                    return builder.makeConst(1.41);
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
        else if (auto* bin = expr->as<AstExprBinary>())
        {
            wasm::Expression* left = compileExpr(bin->left);
            wasm::Expression* right = compileExpr(bin->right);
            return builder.makeBinary(toWasmBinaryOp(bin->op), left, right);
        }
        printf("Unsupported expression: %d\n", expr->classIndex);
        return builder.makeConst(1.3);
    }
};

void compileToWasmOrThrow(wasm::Module& wasm, SourceModule* sourceModule, ModulePtr checkedModule, const CompileOptions& options)
{
    AstStatBlock* root = sourceModule->root;

    WasmCompiler compiler(wasm, checkedModule, options);
    compiler.compile(*sourceModule->names, root);
}

std::string compileToWat(Frontend& frontend, const std::string& moduleName, const WasmCompileOptions& wasmOptions, const CompileOptions& options,
    const ParseOptions& parseOptions)
{
    // Force some necessary options.
    frontend.options.retainFullTypeGraphs = true;
    frontend.options.runLintChecks = true;

    registerBuiltinGlobals(frontend, frontend.globals);
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
        wasm::Module wasm;
        compileToWasmOrThrow(wasm, sourceModule, checkedModule, options);

        if (wasmOptions.format == WasmOutputFormat::WAT)
        {
            std::ostringstream ss;
            ss << wasm;
            return ss.str();
        }
        else
        {
            wasm::BufferWithRandomAccess buffer;
            wasm::WasmBinaryWriter writer(&wasm, buffer);
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

struct StaticStringFileResolver : FileResolver
{
    std::string code;

    StaticStringFileResolver(const std::string& code)
        : code(code)
    {
    }

    std::optional<SourceCode> readSource(const ModuleName& name) override
    {
        return SourceCode{code, SourceCode::Type::Module};
    }
};

std::string compileToWat(
    const std::string& source, const WasmCompileOptions& wasmOptions, const CompileOptions& options, const ParseOptions& parseOptions)
{
    StaticStringFileResolver fileResolver(source);
    NullConfigResolver configResolver;
    // Default to strict mode since we need good type info.
    configResolver.defaultConfig.mode = Mode::Strict;
    Frontend frontend(&fileResolver, &configResolver, {});

    std::string moduleName = "";

    return compileToWat(frontend, moduleName, wasmOptions, options, parseOptions);
}

} // namespace Luau
