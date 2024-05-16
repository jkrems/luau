// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#pragma once

#include <string>

#include "Luau/Frontend.h"
#include "Luau/ParseOptions.h"

#include "Compiler.h"

namespace Luau
{

std::string compileToWat(Frontend& frontend, const std::string& filename, const CompileOptions& options = {}, const ParseOptions& parseOptions = {});

std::string compileToWat(const std::string& source, const CompileOptions& options = {}, const ParseOptions& parseOptions = {});

} // namespace Luau
