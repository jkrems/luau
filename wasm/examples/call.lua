local function add(a: number, b: number)
  return a + b;
end

local function fn()
  local big = 40;
  return add(big, 2);
end

local l = fn();
print(l);

return {fn = fn};
