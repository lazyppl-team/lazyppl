{-| Single-site [Metropolis-Hastings](https://en.wikipedia.org/wiki/Metropolis-Hastings_algorithm).

@mh1 m@ gets out an infinite lazy stream of (result,weight) pairs from the
unnormalized measure @m@, the first of which is the initial draw. At each step
after that, we randomly change one site (a node in the random tree), and then
accept or reject the change. If rejected, we repeat the previous pair.

Use 'drop' for burn-in and @LazyPPL.every@ for thinning. -}
module LazyPPL.SingleSite (mh1) where

import LazyPPL.SingleSite.Internal (mh1)
