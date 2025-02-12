import "../../../diku-dk/sorts/merge_sort"
import "../../../diku-dk/linalg/linalg"
import "pkde"

module type nested = {
  type r
  type seq 'a

  -- | Given a ranking and the set of points, produce the nested conformal set sequence.
  val sequence 'a [B] : [B]r -> [B]a -> seq a

  -- | Calculate the conformal score for some `y` and some nested conformal set sequence.
  val cs 'a [B] : (a -> a -> r) -> a -> seq a -> i64
}

module mk_nested
  (R: real)
  (P: {
    type r
    val z : f32
    val d_y 'a : a -> a -> r
  }
  with r = R.t) = {
  type r = R.t
  type ball = #none | #radius r

  -- (4.2) ~ Compute the minimum covering radius for all the "top" values.
  -- TODO: This isn't the exact function: it's specifically `>=` for the `Q`'th
  -- quantile; we just drop the first `Q` values, but there's situations where those
  -- all are equivalent and we shouldn't drop them...
  def min_covering 'a [B] (d_y: a -> a -> r) (y_hat_sorted: [B]a) : r =
    let q = (1f32 - P.z) |> (f32.*) (f32.i64 B) |> f32.ceil |> i64.f32
    let b = drop q y_hat_sorted
    -- We'll enumerate the entire set; compute the distance between the `b` points
    -- and then find the maximum of those distances.
    in map (\b_i -> map (d_y b_i) b |> (R.maximum)) b |> (R.maximum)

  -- Compute the sequence of nested sets.
  def sequence 'a [B] (lambda_b: [B]r) (y_hat: [B]a) : ([B]a, [B][B]ball) =
    let (lambda_b_sorted, y_hat_sorted) = zip lambda_b y_hat |> merge_sort_by_key (.0) (R.<=) |> unzip
    -- TODO: 1) There's a far more efficient manner to store this data.
    --       2) There's likely an indexing error here `iota` vs 1-starting-index...

    let radius = min_covering P.d_y y_hat_sorted

    in ( y_hat_sorted
       , map (\lambda_t ->
                -- the `t`'th order statistic...
                map (\lambda_i ->
                       if (R.>=) lambda_i lambda_t
                       then #radius radius
                       else #none)
                    lambda_b_sorted)
             lambda_b_sorted
       )

  def cs 'a [B] (dy: a -> a -> r) y ((y_hat, nested_sets): ([B]a, [B][B]ball)) =
    -- `y` is contained within the ball if the distance is <= to the ball's radius.
    let y_in_ball b r = dy b y |> (R.>=) r
    -- For all of the nested sets, compute whether each `y_hat` contains `y`.
    let (max_f, _) =
      map (\set ->
             map2 (\b_i r_i ->
                     match r_i
                     case #none -> false
                     case #radius rad -> y_in_ball b_i rad)
                  y_hat
                  set
             |> or)
          nested_sets
      |> -- To find the maximum index, zip the `bool`s with the index, then filter them,
         -- and take the last th remains
         zip (iota B)
      |> filter (.1)
      |> last
    -- TODO: What if the above filter returns nothing?

    in max_f
}
