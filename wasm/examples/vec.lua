type Vector2d = {x: number, y: number}

local function vec2d(x: number, y: number)
    local v = {};
    v.x = x;
    v.y = y;
    return v;
end

local function vecLen(v: Vector2d)
    return (v.x^2 + v.y^2) ^ 0.5;
end

local m: Vector2d = vec2d(3, 4);
print_f64(vecLen(m));
