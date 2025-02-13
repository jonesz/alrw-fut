module mk_pkde(R: real) = {
	type t = R.t

	def K u = -- (3.1) ~ exp(-1 * u^2)
		(R.**) u (R.i64 2i64) |> R.neg |> R.exp

	-- Rank the members of `sim_hat(x_i, B)` via a GKPDE.
	def rank 'a [B] dy sigma (y_hat: [B]a) =
		let mean [z] (xs: [z]t) =
			R.sum xs |> flip (R./) (R.i64 z)
		let inner y_b y_i = -- (3.1) ~ K(dy(y_b, y_i) / sigma)
			dy y_b y_i |> (R.*) (R.recip sigma) |> K

		in map (\y_b -> map (inner y_b) y_hat |> mean) y_hat
}
