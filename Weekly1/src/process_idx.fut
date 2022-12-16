-- ==
-- entry: test_process_idx
-- random input { [100]i32 [100]i32 }
-- random input { [1000]i32 [1000]i32 }
-- random input { [10000]i32 [10000]i32 }
-- random input { [100000]i32 [100000]i32 }
-- random input { [1000000]i32 [1000000]i32 }
-- random input { [10000000]i32 [10000000]i32 }

def process_idx [n] (xs: [n]i32) (ys: [n]i32): (i32,i64) =
  reduce (\(x, y) (j, k) -> 
      if x > j then (x, y)
      else if x < j then (j, k)
      else if y > k then (x, y)
      else (j, k))
      (0, -1) 
      (zip (map i32.abs (map2 (-) xs ys))
      (iota n))

entry test_process_idx = process_idx