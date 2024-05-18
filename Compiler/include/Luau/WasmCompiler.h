// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#pragma once

#include <string>

#include "Luau/Frontend.h"
#include "Luau/ParseOptions.h"

#include "Compiler.h"

namespace Luau
{

enum WasmOutputFormat
{
    WASM,
    WAT,
};

struct WasmCompileOptions
{
    WasmOutputFormat format = WasmOutputFormat::WASM;
};

std::string compileToWasm(Frontend& frontend, const std::string& filename, const WasmCompileOptions& wasmOptions = {},
    const CompileOptions& options = {}, const ParseOptions& parseOptions = {});

std::string compileToWasm(
    const std::string& source, const WasmCompileOptions& wasmOptions = {}, const CompileOptions& options = {}, const ParseOptions& parseOptions = {});

} // namespace Luau
