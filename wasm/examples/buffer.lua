local function number_to_string(n: number)
  local b = buffer.create(10)
  buffer.writeu8(b, 0, --[[ H ]] 0x48)
  buffer.writeu8(b, 1, --[[ e ]] 0x65)
  buffer.writeu8(b, 2, --[[ l ]] 0x6c)
  buffer.writeu8(b, 3, --[[ l ]] 0x6c)
  buffer.writeu8(b, 4, --[[ o ]] 0x6f)
  print_string(buffer.tostring(b))
  return buffer.tostring(b)
end

return {number_to_string = number_to_string}
