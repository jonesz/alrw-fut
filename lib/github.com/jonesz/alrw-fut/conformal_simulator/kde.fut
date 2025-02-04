module mk_kde(T: real) = {
	type t = T.t
	def K u = T.i64 2i64 |> (T.**) u |> T.neg |> T.exp -- exp(-u^2)

	-- We utilize the arb type `a` because the inputs `y_b` and `y_l` can be multidimensional.
	-- TODO: That being said, I don't think a multi-dimensional output is very valuable...
	def lambda_i_b 'a [B] (d_y: a -> a -> t) (sigma: t) (y_l: [B]a) (y_b: a) : t =
		map (d_y y_b) y_l |> map (flip (T./) sigma) |> map (K) |> reduce (T.+) (T.i64 0i64) |> flip (T./) (T.i64 B)
}
