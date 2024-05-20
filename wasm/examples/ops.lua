-- Note: `..` (concat) is string-specific and not covered here.

local sub = 5  - 2;
local add = 5  + 2;
local mul = 5  * 2;
local div = 5  / 2;
local idv = 5 // 2;
local mod = 5  % 2;
local exp = 5  ^ 2;
local umi = -(2);

add  += 1;
sub  -= 1;
mul  *= 1;
div  /= 1;
idv //= 1;
mod  %= 1;
exp  ^= 1;

if sub > add then
  local gt = true;
elseif mul >= div then
  local gte = true;
elseif sub < add then
  local lte = true;
elseif mul <= div then
  local lte = true;
end

if sub == add then
  local eq = true;
elseif sub ~= add then
  local ne = true;
end

if true or false then
  local lor = true;
elseif true and true then
  local land = true;
end

if not false then
  local lnot = true;
else
  local lnot = false;
end
