# Changelog for lazyppl-singlesite

## 0.1.0

- The single-site Metropolis-Hastings sampler `mh1` was part of `lazyppl` 1.0.1. From `lazyppl` 1.1.0 it is in this package, so that `lazyppl` no longer depends on `ghc-heap`.
- Import `LazyPPL.SingleSite` as before and depend on `lazyppl-singlesite`, which works with `lazyppl ==1.1.*`.
- `lazyppl-singlesite` depends on `ghc-heap >=9.6.1 && <9.15` and is tested with GHC 9.6.7, 9.12.4 and 9.14.1.
- `mh1` changes one random choice per step. It finds the random choices that a model has evaluated by inspecting GHC's heap, so garbage collection can change which ones it finds.
- `mh1` rejects a proposal when the reverse site is absent from the proposed tree's evaluated sites. Otherwise, while the current weight is zero, it accepts every proposal.
- `mh1` accepts a move when its uniform draw is below the acceptance ratio. It used to accept on equality, so a uniform draw of 0 accepted a move to weight 0.
- `mh1` draws its proposed labels from the open interval (0,1) and takes its generator from `newStdGen`. It raises an error when a total log weight overflows to positive infinity.
