import "../../../diku-dk/sorts/merge_sort"
import "../../../diku-dk/linalg/linalg"
import "pkde"

module mk_nested_min_covering (R: real) = {
  type r = R.t

  -- (4.2) ~ Compute the minimum covering radius for all the "top" values.
  -- TODO: This isn't the exact function: it's specifically `>=` for the `Q`'th
  -- quantile; instead we just drop the first `Q` values. I imagine there could be
  -- situations where the simulator outputs data that is exactly the same, thus has the
  -- same `lambda` value, and thus shouldn't be dropped...
  def min_covering 'a [B] d_y z (y_hat_sorted: [B]a) =
    let q = (1f32 - z) |> (f32.*) (f32.i64 B) |> f32.ceil |> i64.f32
    let b = drop q y_hat_sorted

    -- We'll enumerate over the entire cartesian product; compute the distance
    -- between the points and then find the maximum of those distances.
    in map (\b_i -> map (d_y b_i) b |> (R.maximum)) b |> (R.maximum)

  -- Compute the sequence of nested sets.
  def sequence d_y z lambda_b y_hat =
    let (_, y_hat_sorted) = zip lambda_b y_hat |> merge_sort_by_key (.0) (R.<=) |> unzip
    let radius = min_covering d_y z y_hat_sorted

    -- As these sets are nested, F_2 is a subset of F_1, etc. The radius is also constant
    -- across the whole filtration. Thus, we can move from F_B to F_0 and the first ball
    -- that contains `y` is the maximum. This is *only* possible because the radius is
    -- constant across the filtration.
    -- F_0 is Y, F_1 should have the first point dropped, ergo `drop 1`.
    in (radius, drop 1 y_hat_sorted)

  def cs 'a [B] d_y y (radius, y_hat_sorted) =
    -- `y` is contained within the ball if the distance is <= to the ball's radius.
    let y_in_ball b r = d_y b y |> (R.>=) r
    in map (flip y_in_ball radius) y_hat_sorted
      |> zip (iota B) |> filter (.1) |> map (.0)
      |> last                                    -- See the above note in the construction.
      |> (+) 1                                   -- Go from zero-indexing to one-indexing.
}
