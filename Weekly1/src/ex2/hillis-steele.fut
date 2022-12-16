def ilog2 (x: i64) = i64.i32 (63 - i64.clz x)

def hillis_steele [n] (xs: [n]i32) : [n]i32 =
    let m = ilog2 n
    in loop xs = copy xs for d in 0 ... m do
        map (\x -> if x < 2**d then xs[x] else xs[x] + xs[x - 2**d]) (iota(n))

def work_efficient [n] (xs: [n]i32) : [n]i32 =
    let m = ilog2 n
    
    let upswept =
        loop xs = copy xs for d in 0 ... m - 1 do
            map (\x -> if x % 2**(d+1) != 2**(d+1) - 1 then xs[x] 
                        else xs[x] + xs[x-2**d]) (iota(n))
    let upswept[n-1] = 0

    let downswept =
        loop xs = copy upswept for d in 0 ... m - 1 do
            map (\x -> 
                if x == 2**(m-1-d)-1 then xs[2*(2**(m-1-d))-1]
                else if x % 2**(m-d) != 2**(m-d) - 1 then xs[x] 
                else xs[x] + xs[x-2**(m-1-d)]) (iota(n))
    
    in downswept