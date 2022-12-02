-- ==
-- entry: test_process test_process_idx
-- random input { [100]i32 [100]i32 }
-- random input { [1000]i32 [1000]i32 }
-- random input { [10000]i32 [10000]i32 }
-- random input { [100000]i32 [100000]i32 }
-- random input { [1000000]i32 [1000000]i32 }
-- random input { [10000000]i32 [10000000]i32 }

def process (xs: []i32) (ys: []i32): i32 =
  reduce i32.max 0 (map2 (\x y -> i32.abs(x - y)) xs ys)

entry test_process = process