--| The transductive conformal ridge regressor presented in ALRW (2.3.2 Basic CRR).
import "../../diku-dk/linalg/linalg"
import "../../diku-dk/sorts/radix_sort"
import "cp"

local module type integral_req = {
  type t

  -- TODO: `integral` gives us `num_bits`/`get_bit`; perhaps we could loosen to `real`; of note
  -- these are only needed for `radix_sort` and we could use a sort based on `ordered_field` ops.
  include integral with t = t
  include ordered_field with t = t
}

--| 2.3.2 Basic CRR.
module mk_crr (T: integral_req) : cp = {

  type x [p] = [p]T.t
  type y     = [1]T.t
  type Y     = (T.t, T.t)
  type param = {a: T.t}

  module L = mk_linalg T

  def predict [n] [p] (pm: param) eps X Y x =
    let a = pm.a
    let x = [x]

    let p_C =                                       -- eye - the "hat" matrix.
      let p_X = X ++ x
      let m =
        L.eye p |> L.matscale a                     -- aI
        |> L.matadd (L.matmul (transpose p_X) p_X)  -- X'X + aI
        |> L.inv                                    -- inv(X'X + aI)
      in L.matmul (L.matmul p_X m) (transpose p_X)  -- X inv(X'X + aI) X'
         |> L.matsub (L.eye (n + 1))                -- I - X inv(X'X + aI) X'

    -- (y_i, ..., y_n, 0)
    let p_A = L.matmul p_C (Y ++ [[T.i64 0i64]]) |> flatten
    -- (0_i, ..., 0_n, 1)
    let p_B = (transpose [L.veczeros n]) ++ [[T.i64 1i64]] |> L.matmul p_C |> flatten

    -- TODO: We could parameterize this by conformity score...
    let conf idx =

      let r =
        let top = (T.-) p_A[idx] p_A[n] -- p_A[idx] - p_A[n]
        let bot = (T.-) p_B[n] p_B[idx] -- p_B[n] - p_B[idx]
        in (T./) top bot                -- top / bot

      let cond = (T.>) ((T.-) p_B[n] p_B[idx]) (T.i64 0i64) -- p_B[n] - p_B[idx] > 0
      in if cond then (r, r) else (T.lowest, T.highest)     -- l = -inf; u = inf

    let (l, u) =
      map (\idx -> conf idx) (iota n) |> unzip              -- l = -inf; u = inf

    let l = radix_sort_float T.num_bits T.get_bit l
    let u = radix_sort_float T.num_bits T.get_bit u

    let l_idx = (eps / 2f32) * (f32.i64 n) |> f32.floor |> i64.f32          -- floor((eps/2)n)
    let u_idx = (1f32 - (eps / 2f32)) * (f32.i64 n) |> f32.ceil |> i64.f32  -- ceil((1f32-(eps/2))n)

    in (l[l_idx], u[u_idx])
}
