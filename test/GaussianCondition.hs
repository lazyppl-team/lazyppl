module Main where

import Control.Exception (SomeException, displayException, evaluate, try)
import Control.Monad (forM, forM_, unless)
import Data.List (isPrefixOf, transpose)
import LazyPPL (Tree(..), randomTree, runProb)
import LazyPPL.Distributions (normal)
import LazyPPL.Distributions.GP (gp, wiener)
import LazyPPL.Distributions.GP.Pure (emptyGPState, sampleGP)
import System.Environment (getArgs)
import System.Random (mkStdGen)
import Test.QuickCheck
import Test.QuickCheck.Random (mkQCGen)

finite :: Double -> Bool
finite x = not (isNaN x || isInfinite x)

close :: Double -> Double -> Double -> Bool
close tolerance actual expected =
  finite actual && abs (actual - expected) <= tolerance

-- 2^-33 * cov x x is added to every variance, the first one included.
nugget :: Double
nugget = 2 ** (-33)

-- Runs gp's own sampler, sampleGP, on the draws zs we choose.
samplePath :: (Double -> Double -> Double) -> [Double] -> [Double] -> [Double]
samplePath cov = go emptyGPState
  where
    go _ [] [] = []
    go state (x:xs) (z:zs) =
      let (value,next) = sampleGP cov state x z
      in value : go next xs zs
    go _ _ _ = error "test point/innovation lengths differ"

-- Unit vectors give the columns of L (f = L z). L L^T should equal the kernel plus nugget, so no
-- second Cholesky is needed.
covarianceResidual :: [[Double]] -> Double
covarianceResidual kernel =
  let n = length kernel
      xs = map fromIntegral [0..n-1]
      cov x y = kernel !! round x !! round y
      columns = [samplePath cov xs [if j == k then 1 else 0 | j <- [0..n-1]]
                | k <- [0..n-1]]
      rows = transpose columns
      target = [[v + (if i == j then nugget*v else 0)
                | (j,v) <- zip [0::Int ..] row] | (i,row) <- zip [0..] kernel]
      actual = [[sum (zipWith (*) a b) | b <- rows] | a <- rows]
      norm values = sqrt (sum [v*v | row <- values, v <- row])
  in norm (zipWith (zipWith (-)) target actual) / norm target

-- Closed-form 2-by-2 Cholesky factor, so the reference needs no solver.
twoByTwo :: Property
twoByTwo = forAll (vectorOf 5 (choose (-0.5,0.5))) $ \values ->
  case values of
    [x,b,y,z,w] ->
      let a = 1+abs x; c = 1+abs y; q = 1+nugget
          cov i j = if i == j then (if i == 0 then a else c) else b
          actual = samplePath cov [0,1] [z,w]
          expected = [sqrt (a*q)*z, b/sqrt (a*q)*z + sqrt (c*q-b*b/(a*q))*w]
      in counterexample (show (actual,expected)) $
           and (zipWith (close 1e-12) actual expected)
    _ -> property False

-- A Householder reflection (https://en.wikipedia.org/wiki/Householder_transformation) gives
-- kernels with known eigenvalues, down to 1e-17 and 0. L L^T must reproduce
-- kernel + nugget*diag(kernel), dropping no mode.
knownSpectrum :: Property
knownSpectrum = forAll (chooseInt (3,10)) $ \n ->
  forAll (vectorOf n (choose (0.2,1))) $ \axis ->
  forAll (elements [0,1,2::Int]) $ \kind ->
    let norm = sum (map (\x -> x*x) axis)
        q = [[(if i == j then 1 else 0)-2*a*b/norm
             | (j,b) <- zip [0..] axis] | (i,a) <- zip [0::Int ..] axis]
        eigenvalues = 2 : replicate (n-1) (case kind of 0 -> 0.5; 1 -> 1e-17; _ -> 0)
        kernel = [[sum (zipWith3 (\d a b -> d*a*b) eigenvalues row col)
                  | col <- q] | row <- q]
        residual = covarianceResidual kernel
        -- 64*n*eps covers dot-product rounding and Cholesky's usual n-dependent error constant.
        bound = 64 * fromIntegral n * encodeFloat 1 (-52)
    in counterexample (show (kind,residual,bound)) (finite residual && residual <= bound)

residual :: Property
residual = forAll (chooseInt (2,10)) $ \n ->
  forAll (vectorOf n (vectorOf n (choose (-1,1)))) $ \a ->
    let kernel = [[sum (zipWith (*) row col)+(if i == j then 1 else 0)
                  | (j,col) <- zip [0..] a] | (i,row) <- zip [0::Int ..] a]
        r = covarianceResidual kernel
    in counterexample (show r) (finite r && r <= 64*fromIntegral n*encodeFloat 1 (-52))

-- A diagonal covariance s gives independent draws with standard deviation sqrt(s*(1+nugget)).
scaleCase :: [Double] -> Property
scaleCase scales = forAll (elements scales) $ \s ->
  forAll (vectorOf 4 (choose (-2,2))) $ \zs ->
    let cov x y = if x == y then s else 0
        actual = map (/sqrt s) (samplePath cov [0..3] zs)
        expected = map (*(sqrt (1+nugget))) zs
    in counterexample (show (s,actual,expected)) $
         and (zipWith (close 1e-12) actual expected)

smallScale, largeScale :: Property
smallScale = scaleCase [1e-160,1e-200,1e-250]
largeScale = scaleCase [1e8,1e160,1e200,1e250]

-- gp uses its normal draws in order of first query, so we force each value before asking for
-- the next.
draw :: (Double -> Double -> Double) -> [Double] -> Int -> IO [Double]
draw cov order seed = do
  let f = runProb (gp cov) (randomTree (mkStdGen seed))
  mapM (evaluate . f) order

-- Both query orders must match the specified law and each other. For zero-mean Gaussians the
-- standard error of a mean of X_i X_j is sqrt((Cii*Cjj+Cij^2)/N), and six of them keep false
-- failures rare.
orderIndependence :: Property
orderIndependence = forAll (choose (-0.8,0.8)) $ \rho ->
  forAll (chooseInt (1,1000000)) $ \firstSeed -> ioProperty $ do
    let cov x y = if x == y then (if x == 0 then 1 else 2) else rho
        count = 8192
        q = 1+nugget
        expected = [q,rho,2*q]
        errors = [sqrt (2*q*q/fromIntegral count),
                  sqrt ((2*q*q+rho*rho)/fromIntegral count),
                  sqrt (8*q*q/fromIntegral count)]
        moments pairs = [sum [a*a | [a,_] <- pairs]/fromIntegral count,
                         sum [a*b | [a,b] <- pairs]/fromIntegral count,
                         sum [b*b | [_,b] <- pairs]/fromIntegral count]
    forward <- mapM (draw cov [0,1]) [firstSeed..firstSeed+count-1]
    backward <- mapM (fmap reverse . draw cov [1,0]) [firstSeed+count..firstSeed+2*count-1]
    let a = moments forward; b = moments backward
        matches xs = and (zipWith3 (\v target se -> close (6*se) v target) xs expected errors)
        agrees = and (zipWith3 (\v w se -> close (6*sqrt 2*se) v w) a b errors)
    pure $ counterexample (show (rho,a,b,expected,errors)) (matches a && matches b && agrees)

expectError :: String -> (Double -> Double -> Double) -> [Double] -> IO ()
expectError name cov order = do
  outcome <- try (draw cov order 1) :: IO (Either SomeException [Double])
  case outcome of
    Left err | name `isPrefixOf` displayException err -> pure ()
    _ -> fail ("Expected " ++ name ++ ", got " ++ show outcome)

invalidCovariance :: IO ()
invalidCovariance = expectError "GPCovarianceNotPositiveDefinite"
  (\x y -> if x == y then 100 else 100.3) [0,1]

namedErrors :: IO ()
namedErrors = do
  invalidCovariance
  expectError "GPCovarianceNotPositiveDefinite" (\_ _ -> -1) [0]
  forM_ [0/0,1/0,negate (1/0)] $ \bad -> do
    expectError "GPCovarianceNotFinite" (\_ _ -> bad) [0]
    -- Only cov 0 1 is bad, an entry the factor never reads, yet the error must fire.
    expectError "GPCovarianceNotFinite" (\x y -> if x == 0 && y == 1 then bad else 1) [0,1]
  let zeroCross x y = if x == y then (if x == 0 then 0 else 1) else 0.5
      twoZeros x y = if x == y then 0 else 0.5
  forM_ [[0,1],[1,0]] $ \order -> do
    expectError "GPCovarianceNotPositiveDefinite" zeroCross order
    expectError "GPCovarianceNotPositiveDefinite" twoZeros order

wienerBridge :: IO ()
wienerBridge = forM_ (zip [20261029,20261030] [[2,1,3],[-2,-1,-3]]) $ \(seed,order) -> do
  -- Each path gets its own tree, so the compiler cannot share one runProb wiener expression
  -- and its memo table.
  let tree = randomTree (mkStdGen seed)
      f = runProb wiener tree
      -- The table starts at (0,0). A new point takes its normal draw from child 1 + table size
      -- of the input tree.
      z i = case tree of
        Tree _ children -> runProb (normal 0 1) (children !! i)
      go _ [] = pure ()
      go table (x:xs) = do
        let lower = filter ((<x) . fst) table
            upper = filter ((>x) . fst) table
            (mu,sd) = case (lower,upper) of
              ([],_) -> let (b,v) = minimum upper in (v,sqrt (b-x))
              (_,[]) -> let (a,v) = maximum lower in (v,sqrt (x-a))
              _ -> let (a,v) = maximum lower; (b,w) = minimum upper
                   in (v+(x-a)*(w-v)/(b-a),sqrt ((b-x)*(x-a)/(b-a)))
            expected = mu+sd*z (1+length table)
        actual <- evaluate (f x)
        unless (close 1e-12 actual expected) (fail (show (x,actual,expected)))
        go ((x,actual):table) xs
  go [(0,0)] order

runProperty :: Int -> Property -> IO ()
runProperty count prop = do
  result <- quickCheckWithResult stdArgs {maxSuccess=count,replay=Just(mkQCGen 20261029,0)} prop
  unless (isSuccess result) (fail "Gaussian process property failed")

allChecks :: IO ()
allChecks = do
  namedErrors
  zeros <- draw (\_ _ -> 0) [0,1,-1,2] 1
  unless (zeros == replicate 4 0) (fail ("Zero covariance: " ++ show zeros))
  -- Covariance 1 - 2^-53 leaves the kernel nearly singular, yet the second pivot squared stays
  -- at q - a^2/q, about 2^-32.
  let a = 1-encodeFloat 1 (-53)
      tinyMode x y = if x == y then 1 else a
      [_,innovation] = samplePath tinyMode [0,1] [0,1]
      q = toRational (1+nugget)
      expectedV = fromRational (q-toRational a*toRational a/q)
  unless (close (1e-5*expectedV) (innovation*innovation) expectedV) $
    fail ("Small spectral component: " ++ show (innovation*innovation,expectedV))
  -- Close points of the constant kernel have conditional variance q - 1/q, but the same key
  -- gives its stored value.
  let [_,near] = samplePath (\_ _ -> 1) [0,1e-6] [0,1]
      nearVariance = fromRational (q-1/q)
  unless (close (1e-5*nearVariance) (near*near) nearVariance) $
    fail ("Nearby re-observation: " ++ show (near*near,nearVariance))
  let f = runProb (gp (\_ _ -> 1)) (randomTree (mkStdGen 9))
  first <- evaluate (f 0)
  _ <- evaluate (f 1e-6)
  again <- evaluate (f 0)
  unless (first == again) (fail "Repeated key changed")
  grid <- draw (\x y -> exp (negate ((x-y)^ (2::Int))/0.1))
               [fromIntegral i*0.02 | i <- [0..150::Int]] 7
  unless (length grid == 151 && all finite grid) (fail "RBF grid failed")
  wienerBridge
  runProperty 512 twoByTwo
  runProperty 100 knownSpectrum
  runProperty 100 residual
  runProperty 100 smallScale
  runProperty 100 largeScale
  runProperty 24 orderIndependence

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> allChecks
    ["invalid"] -> invalidCovariance
    ["spectrum"] -> runProperty 512 knownSpectrum
    ["small"] -> runProperty 100 smallScale
    ["large"] -> runProperty 100 largeScale
    ["order"] -> runProperty 24 orderIndependence
    ["bridge"] -> wienerBridge
    _ -> fail "Expected no arguments, invalid, spectrum, small, large or order"
