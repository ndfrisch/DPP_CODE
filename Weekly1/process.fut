let process [n] (xs: [n]i32) (ys: [n]i32) =
    if n == 0 then
        []
    else
        map2 (\x y -> i32.abs(x) + i32.abs(y)) xs ys