// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#pragma once

#include "Luau/Ast.h"
#include "Luau/DenseHash.h"

namespace Luau
{
namespace Compile
{

struct WasmTableShape
{
    unsigned int arraySize = 0;
    unsigned int hashSize = 0;

    // Tracks if the shape is associated with a local.
    std::optional<AstName> boundToLocal;

    std::vector<AstName> names;
};

void predictWasmTableShapes(
    DenseHashMap<AstExprTable*, WasmTableShape>& shapes, DenseHashMap<AstExprTable*, AstLocal*>& tableBindings, AstNode* root);

} // namespace Compile
} // namespace Luau
