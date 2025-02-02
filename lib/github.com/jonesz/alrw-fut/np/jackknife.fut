module mk_jackknife(T: integral) = {
	type t = T.t

	def jackknife [n] (x: [n]t) =
		let sum = reduce (T.+) (T.i64 0) x
		let e_t = (T./) sum (T.i64 n)
		let e_t_hat =
			-- TODO: For the below, we could compute some other statistic: quantile, median, etc.
			map ((T.-) sum) x |> map (flip (T./) (T.i64 (n - 1)))

			-- At this point, we have each T_{-i}; compute that mean to find T_hat.
			|> reduce (T.+) (T.i64 0)  |> flip (T./) (T.i64 n)

		-- the jackknife bias estimate: bjack = (n-1)(e_t_hat - e_t).
		in (T.i64 (n-1)) |> (T.*) ((T.-) e_t_hat e_t)
}

def jackknife_fit 'a 'b [n] (F: [n]a) (mf: [n-1]a -> b) : [n]b =
	let sample_F x =
		rotate x F |> tail
	in map (sample_F) (iota n) |> map (mf)
