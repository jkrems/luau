local function opt(present: boolean, value: number)
  local t = {}
  t.present = present
  t.value = value
  return t
end

local v = opt(true, 4.25);
print_bool(v.present);
if v.present then
  print_f64(v.value);
else
  print_f64(-1);
end

return {opt = opt};
