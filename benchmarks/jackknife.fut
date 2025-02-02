import "../lib/github.com/jonesz/alrw-fut/np/jackknife"
import "../lib/github.com/diku-dk/linalg/linalg"

module L = mk_linalg f32

-- ==
-- entry: bench_jackknife_ols
-- random input { [100][10]f32 [100]f32 }
-- random input { [100][100]f32 [100]f32 }
entry bench_jackknife_ols X Y =
	let mf a =
		let (X, Y) = unzip a
		in L.ols X Y

	let F = zip X Y
	in jackknife_fit F mf
