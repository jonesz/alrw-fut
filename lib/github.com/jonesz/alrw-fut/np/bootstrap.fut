import "../../../diku-dk/cpprandom/random"

module mk_boostrap(T: integral) = {
	type t = T.t

	let ne = T.i64 0
	module dist = uniform_int_distribution i64 minstd_rand

	def v_boot [z] rng (n: i64) (B: i64) (F: [z]t) (g: [n]t -> t) =	
		let sample_from_F rng =
			let (rng, idx) = dist.rand (0, z) rng
			in (rng, F[idx])

		let compute_g rng = 
			let (rngs, samples) = unzip (map (sample_from_F) (minstd_rand.split_rng n rng))
			let T_n = g samples
			let rng = minstd_rand.join_rng rngs
			in (rng, T_n)

		let (rngs, T_star) = unzip (map (compute_g) (minstd_rand.split_rng B rng))
		let rng = minstd_rand.join_rng rngs

		let v_boot =
			map (\b -> map (\r -> T_star[r]) (iota B) |> reduce (T.+) ne |> flip (T./) (T.i64 B) |> (T.-) T_star[b] |> flip (T.**) (T.i64 2))
		(iota B) |> reduce (T.+) ne |> flip (T./) (T.i64 B)
		
		in (rng, v_boot)
}

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
