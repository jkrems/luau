local function opt(present: boolean, value: number)
    local t = {}
    t.present = present
    t.value = value
    return t
end

local v = opt(true, 4);
if v.present then
  print(v.value);
else
  print(-1);
end
