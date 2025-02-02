def quantile 't [n] a (xs: [n]t) sort =
	let xs = sort xs
	let i = f32.i64 n |> (*) a |> f32.ceil |> i64.f32
	in xs[i]

--| An `a` length coverage interval.
def confidence_interval 't [n] a (xs: [n]t) sort =
	let xs = sort xs
	let l = (1f32 - a) |> flip (/) 2f32 |> f32.floor |> i64.f32
	let u = (1f32 - a) |> flip (/) 2f32 |> (+) a |> f32.ceil |> i64.f32
	in (xs[l], xs[u])
