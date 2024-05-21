// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#include "lua.h"
#include "lualib.h"
#include "luacode.h"

#include "Luau/Common.h"
#include "Luau/WasmCompiler.h"

#ifdef __EMSCRIPTEN__
#include <emscripten/bind.h>
#endif // __EMSCRIPTEN__

#include <string>

#include <string.h>

static void setupState(lua_State* L)
{
    luaL_openlibs(L);

    luaL_sandbox(L);
}

static std::string runCode(lua_State* L, const std::string& source)
{
    size_t bytecodeSize = 0;
    char* bytecode = luau_compile(source.data(), source.length(), nullptr, &bytecodeSize);
    int result = luau_load(L, "=stdin", bytecode, bytecodeSize, 0);
    free(bytecode);

    if (result != 0)
    {
        size_t len;
        const char* msg = lua_tolstring(L, -1, &len);

        std::string error(msg, len);
        lua_pop(L, 1);

        return error;
    }

    lua_State* T = lua_newthread(L);

    lua_pushvalue(L, -2);
    lua_remove(L, -3);
    lua_xmove(L, T, 1);

    int status = lua_resume(T, NULL, 0);

    if (status == 0)
    {
        int n = lua_gettop(T);

        if (n)
        {
            luaL_checkstack(T, LUA_MINSTACK, "too many results to print");
            lua_getglobal(T, "print");
            lua_insert(T, 1);
            lua_pcall(T, n, 0, 0);
        }

        lua_pop(L, 1); // pop T
        return std::string();
    }
    else
    {
        std::string error;

        lua_Debug ar;
        if (lua_getinfo(L, 0, "sln", &ar))
        {
            error += ar.short_src;
            error += ':';
            error += std::to_string(ar.currentline);
            error += ": ";
        }

        if (status == LUA_YIELD)
        {
            error += "thread yielded unexpectedly";
        }
        else if (const char* str = lua_tostring(T, -1))
        {
            error += str;
        }

        error += "\nstack backtrace:\n";
        error += lua_debugtrace(T);

        lua_pop(L, 1); // pop T
        return error;
    }
}

extern "C" const char* executeScript(const char* source)
{
    // setup flags
    for (Luau::FValue<bool>* flag = Luau::FValue<bool>::list; flag; flag = flag->next)
        if (strncmp(flag->name, "Luau", 4) == 0)
            flag->value = true;

    // create new state
    std::unique_ptr<lua_State, void (*)(lua_State*)> globalState(luaL_newstate(), lua_close);
    lua_State* L = globalState.get();

    // setup state
    setupState(L);

    // sandbox thread
    luaL_sandboxthread(L);

    // static string for caching result (prevents dangling ptr on function exit)
    static std::string result;

    // run code + collect error
    result = runCode(L, source);

    return result.empty() ? NULL : result.c_str();
}

#ifdef __EMSCRIPTEN__

class CompilationResult
{
public:
    CompilationResult() {}

    CompilationResult(const std::string& out)
        : output(out)
    {
    }

    emscripten::val getOutput()
    {
        return emscripten::val(emscripten::typed_memory_view(output.size(), output.data()));
    }

private:
    std::string output;
};

std::unique_ptr<CompilationResult> luauToWasm(std::string source, int optLevel = 1, bool wat = false)
{
    // setup flags
    for (Luau::FValue<bool>* flag = Luau::FValue<bool>::list; flag; flag = flag->next)
        if (strncmp(flag->name, "Luau", 4) == 0)
            flag->value = true;

    Luau::WasmCompileOptions wasmOpts;
    wasmOpts.format = wat ? Luau::WasmOutputFormat::WAT : Luau::WasmOutputFormat::WASM;
    Luau::CompileOptions compileOpts;
    compileOpts.optimizationLevel = optLevel;

    std::string wasm = Luau::compileToWasm(source, wasmOpts, compileOpts);

    return std::make_unique<CompilationResult>(wasm);
}

EMSCRIPTEN_BINDINGS(LuauWeb) {
    emscripten::function("luauToWasm", &luauToWasm);

    emscripten::class_<CompilationResult>("CompilationResult").constructor().function("getOutput", &CompilationResult::getOutput);
}

#endif // __EMSCRIPTEN__
