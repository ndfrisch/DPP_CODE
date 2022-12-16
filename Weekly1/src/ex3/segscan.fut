-- ==
-- entry: test_segscan
-- random input { [100]i32 [100]bool }
-- random input { [1000]i32 [1000]bool }
-- random input { [10000]i32 [10000]bool }
-- random input { [100000]i32 [100000]bool }
-- random input { [1000000]i32 [1000000]bool }
-- random input { [10000000]i32 [10000000]bool }

def segscan [n] 't (op: t -> t -> t) (ne: t) (arr: [n](t, bool)): [n]t =
    let temp = scan (\(v1, f1) (v2, f2) -> if f2 then (v2, f2) else ((op v1 v2), f1 || f2)) (ne, false) arr in
    let (retval, _) = unzip temp in
    retval

entry test_segscan [n] (a: [n]i32) (b: [n]bool) = segscan (+) 0 (zip a b)

-- ==
-- entry: test_scan
-- random input { [100]i32 }
-- random input { [1000]i32 }
-- random input { [10000]i32 }
-- random input { [100000]i32 }
-- random input { [1000000]i32 }
-- random input { [10000000]i32 }

entry test_scan = scan (+) 0i32

-- ==
-- entry: test_segreduce
-- random input { [100]i32 [100]bool }
-- random input { [1000]i32 [1000]bool }
-- random input { [10000]i32 [10000]bool }
-- random input { [100000]i32 [100000]bool }
-- random input { [1000000]i32 [1000000]bool }
-- random input { [10000000]i32 [10000000]bool }

def segreduce [n] 't (op: t -> t-> t) (ne: t) (arr: [n](t, bool)): []t =
    let temp = segscan op ne arr in
    let indices = scan (+) (-1) (map (\(_, f) -> if f then 1 else 0) arr) in
    let retval = scatter (replicate (indices[n-1] + 1) ne) indices temp in
    retval

entry test_segreduce [n] (a: [n]i32) (b: [n]bool) = segreduce (+) 0 (zip a b)

-- ==
-- entry: test_reduce
-- random input { [100]i32 }
-- random input { [1000]i32 }
-- random input { [10000]i32 }
-- random input { [100000]i32 }
-- random input { [1000000]i32 }
-- random input { [10000000]i32 }

entry test_reduce = reduce (+) 0i32