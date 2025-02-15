import "../../../../diku-dk/cpprandom/random"
import "../calibrate_predict"

module X_dist = uniform_real_distribution f32 minstd_rand
module Y_dist = normal_distribution f32 minstd_rand
module CS = mk_calibrate f32

-- (4.1) ~ the generative distribution from Lei and Wasserman (2014).
def sample_X rng = X_dist.rand (-1.5f32, 1.5f32) rng

def sample_Y rng x =
  let f x = (x - 1f32) |> flip (f32.**) 2f32 |> (f32.+) (x + 1f32)
  let g x = 2f32 * (x + 0.5f32) |> f32.sqrt |> (f32.*) (if x >= -0.5 then 1f32 else 0f32)
  let sigma_sqr x = f32.abs x |> (f32.+) 0.25f32
  let (rng, y_a) = Y_dist.rand {mean = ((f x) - (g x)), stddev = (sigma_sqr x)} rng
  let (rng, y_b) = Y_dist.rand {mean = ((f x) + (g x)), stddev = (sigma_sqr x)} rng
  in (rng, 0.5 * y_a + 0.5 * y_b)

-- The oracle example from secion 4.1; the simulator samples from the true conditional distribution.
-- `n` is the length of calibration set, `B` is the number of simulations to run, `m` is the size of the test set.
def oracle_dataset seed (n: i64) (B: i64) (m: i64) =
  -- TODO: Refactor this into a single function.
  let rng = minstd_rand.rng_from_seed seed
  let (rngs, X_calibrate) = map (sample_X) (minstd_rand.split_rng n rng) |> unzip
  let (rngs, Y_calibrate) = map2 (sample_Y) rngs X_calibrate |> unzip
  let (rngs, Y_calibrate_simulator) =
    map2 (\rng_i x_i ->
            let (rngs_i, Y_hat) = map (\rng_b -> sample_Y rng_b x_i) (minstd_rand.split_rng B rng_i) |> unzip
            let rng_i = minstd_rand.join_rng rngs_i
            in (rng_i, Y_hat))
         rngs
         X_calibrate
    |> unzip
  let rng = minstd_rand.join_rng rngs
  let (rngs, X_test) = map (sample_X) (minstd_rand.split_rng m rng) |> unzip
  let (rngs, Y_test) = map2 (sample_Y) rngs X_test |> unzip
  let (rngs, Y_test_simulator) =
    map2 (\rng_i x_i ->
            let (rngs_i, Y_hat) = map (\rng_b -> sample_Y rng_b x_i) (minstd_rand.split_rng B rng_i) |> unzip
            let rng_i = minstd_rand.join_rng rngs_i
            in (rng_i, Y_hat))
         rngs
         X_test
    |> unzip
  in ((X_calibrate, Y_calibrate, Y_calibrate_simulator), (X_test, Y_test_simulator, Y_test))

def d_y a b = (a - b) * (a - b) |> f32.sqrt

-- ==
-- entry: lw
-- input { 500i64 100i64 500i64 }
entry lw n B m =
  let z = 0.95f32
  let sigma = 0.025f32
  let ((_, Y_calibrate, Y_simulator), (_, Y_test_simulator, Y_test)) = oracle_dataset [123] n B m
  let idx = CS.calibrate d_y z sigma Y_calibrate Y_simulator |> conformal_cutoff 0.05f32
  let rs = map (CS.predict d_y z sigma idx) Y_test_simulator
  in map2 (\y_i rs_i ->
             let (radius, spheres) = rs_i
             let s_contains_y s = d_y s y_i |> (f32.<=) radius
             in map (s_contains_y) spheres |> or |> i64.bool)
          Y_test
          rs |> i64.sum |> f32.i64 |> flip (f32./) (f32.i64 m)
