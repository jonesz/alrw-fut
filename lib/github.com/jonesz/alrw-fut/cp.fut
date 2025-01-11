-- TODO: Is this truly the transductive case?
--| The default transductive conformal predictor.
module type cp = {
	--| `x` is an example with `p` features.
	type x [p]

	--| 'y' is an output with `q` features.
	type y [q]

	--| The output, prediction set type of the conformal predictor.
	type Y [q]

	--| The parameters for a specific algorithm.
	type param

	val predict [n][p][q] : param -> f32 -> [n]x[p] -> [n]y[q] -> [1]x[p] -> Y[q]
}
