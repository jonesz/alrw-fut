import "../../../diku-dk/cpprandom/random"

module dist = uniform_int_distribution i64 minstd_rand

def boot_fit 'a 'b [n][k] rng (B: i64) (F: [n]a) (mf: [k]a -> b) =

	-- TODO: When sampling, this produces a copy of 'a (which could be a massive vector, an i64, whatever);
	-- for an optimized version, we could just return indices...?
	let sample_F rng =
		let (rng, idx) = dist.rand (0, n) rng
		in (rng, F[idx])

	let fit rng =
		let (rngs, s_F) = unzip (map (sample_F) (minstd_rand.split_rng k rng))
		let T = mf s_F
		let rng = minstd_rand.join_rng rngs
		in (rng, T)

	let (rngs, w) = unzip (map (fit) (minstd_rand.split_rng B rng))
	let rng = minstd_rand.join_rng rngs
	in (rng, w)
