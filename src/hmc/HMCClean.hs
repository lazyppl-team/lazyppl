import NagataMapNum as AutoDiff
import Data.Map hiding (map, take)
import System.Random
import Data.Random.Normal
import Debug.Trace

data Gaussian a = Gaussian {mu :: a , sigma :: a} deriving (Show)

logGaussian1D :: Floating a => Gaussian a -> a -> a
logGaussian1D (Gaussian mu sigma) x = -(x - mu)*(x - mu) / (2*sigma*sigma) + (-1/2) * log (2 * pi * sigma*sigma)

logThreeGaussians1D :: Floating a => a -> a
logThreeGaussians1D x = log $ 1/3 * (
        exp (logGaussian1D (Gaussian 1 1) x) + 
        exp (logGaussian1D (Gaussian 3 0.5) x) + 
        exp (logGaussian1D (Gaussian 4 2) x)
    )

v :: Floating a => a -> a
v x = -(logThreeGaussians1D x)

-- v :: Floating a => a -> a
-- v x = -(logGaussian1D (Gaussian 0 1) x)

autoDiff :: (Floating d, Ord d) => (Expr Vars) -> d -> d
autoDiff expr x0 = (tangent (forwardAD (\_ -> x0) X expr)) ! X

dV_dq :: (Floating a, Ord a) => a -> a
dV_dq = autoDiff (v (Var X))

k :: Floating a => a -> a -> a
k q p = 1/2 * (p ** 2)

h :: Floating a => a -> a -> a
h q p = k q p + v q

data LFConfig a = LFConfig {eps :: a , leapfrogSteps :: Int , selected :: Int}

updateQP :: (Floating a, Ord a, Show a) => (a -> a) -> a -> (a, a) -> (a, a)
updateQP dV_dq eps (q, p) = let
        p_half = p - eps/2 * (dV_dq q) -- half step
        q' = q + eps * p_half -- full step
        p' = p_half - eps/2 * (dV_dq q') -- finish half step
    in (q', p')

simpleLeapfrog :: (Floating a, Ord a, Show a) => (a -> a) -> (LFConfig a) -> (a, a) -> (a, a)
simpleLeapfrog dV_dq (LFConfig eps steps selected) (q, p) = let
        qps = take steps $ iterate (updateQP dV_dq eps) (q, p)
        qs = map fst qps
        ps = map snd qps
    in (last qs, last ps)

data HMCConfig a = HMCConfig {seed :: Int, lfc :: LFConfig a}

hmcProposal :: (Floating a, Ord a, Show a) => (LFConfig a) -> (a, a) -> (a, a)
hmcProposal lfc (q, p) = let
        (q', p') = simpleLeapfrog dV_dq lfc (q, p)
    in (q', -p')

hmcAcceptReject :: (RandomGen g, Floating a, Ord a, Random a, Show a) => LFConfig a -> ((a, a), g) -> ((a, a), g)
hmcAcceptReject lfc ((q, _), rng) = let
        (p, rng') = normal rng -- assume the proposal distribution for momentum is N(0, 1)
        (new_q, new_p) = hmcProposal lfc (q, p)
        (u, rng'') = randomR (0.0, 1.0) rng'
        (q', p') = if (log u) < (-(h new_q new_p) + (h q p)) then (new_q, new_p) else (q, p)
    in ((q', p'), rng'')

hmc1D :: (Floating a, Ord a, Random a, Show a) => (HMCConfig a) -> [(a, a)]
hmc1D (HMCConfig seed lfc) = let 
        -- set the seed for reproducibility
        rng = mkStdGen seed
        -- starting values
        q = 1.0
        p = 1.0
    in map fst $ iterate (hmcAcceptReject lfc) ((q, p), rng)

main :: IO ()
main = print $ take 10000 $ hmc1D (HMCConfig {seed=42, lfc=(LFConfig {eps=(0.05 :: Double), leapfrogSteps=40, selected=1})})

-- main :: IO ()
-- main = print $ [(let s = 8 * q/1000 - 4 in (s, v s)) | q <- [1,2..1000]]

-- main :: IO ()
-- main = print $ [(let s = 8 * q/1000 - 4 in (s, v s, dV_dq s)) | q <- [1,2..1000]]

-- main :: IO ()
-- main = print "HelloWorld"