import "../../../diku-dk/cpprandom/random"
import "pkde"

def d_y a b = map2 (f32.-) a b |> map (flip (f32.**) 2f32) |> f32.sum |> f32.sqrt
module P = mk_pkde f32

-- ==
-- entry: bench_pkde_rank
-- random input { [100][10]f32 }
-- random input { [1000][10]f32 }
-- random input { [100][100]f32 }
-- random input { [1000][100]f32 }
entry bench_pkde_rank y_hat =
  P.rank d_y 0.15f32 y_hat
