import "../lib/github.com/jonesz/alrw-fut/jackknife_plus"
import "../lib/github.com/diku-dk/linalg/linalg"

module L = mk_linalg f32
module J = mk_jackknife_plus f32

-- ==
-- entry: bench_jackknife_plus_ols
-- random input { [100][10]f32 [100]f32 [10]f32 }
-- random input { [100][100]f32 [100]f32 [100]f32 }
entry bench_jackknife_plus_ols X Y x = 
	let A XY =
		let (X, Y) = unzip XY
		in L.ols X Y

	let mu = (L.dotprod)
	let F = zip X Y
	let r = J.fit A mu F
	in J.pred r mu 0.95f32 x
