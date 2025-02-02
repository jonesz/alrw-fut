def jackknife_fit 'a 'b [n] (F: [n]a) (mf: [n-1]a -> b) : [n]b =
	let sample_F x =
		rotate x F |> tail
	in map (sample_F) (iota n) |> map (mf)
