-- ==
-- entry: test_segscan
-- random input { [100](i32, bool) }
-- random input { [1000](i32, bool) }
-- random input { [10000](i32, bool) }
-- random input { [100000](i32, bool) }
-- random input { [1000000](i32, bool) }
-- random input { [10000000](i32, bool) }

def segscan [n] 't (op: t -> t -> t) (ne: t) (arr: [n](t, bool)): [n]t =
    let temp = scan (\(v1, f1) (v2, f2) -> if f2 then (v2, f2) else ((op v1 v2), f1 || f2)) (ne, false) arr in
    let (retval, _) = unzip temp in
    retval

entry test_segscan = segscan (+) 0

def segreduce [n] 't (op: t -> t-> t) (ne: t) (arr: [n](t, bool)): []t =
    let temp = segscan op ne arr in
    let bits = map (\(_, f) -> if f then 1 else 0) arr in
    let indices = scan (+) (-1) bits in
    let retval = scatter (replicate (indices[n-1] + 1) ne) indices temp in
    retval

def reduce_by_index 'a [m] [n] (dest : *[m]a) (f : a -> a -> a) (ne : a) (is : [n]i64) (as : [n]a) : *[m]a =
    dest