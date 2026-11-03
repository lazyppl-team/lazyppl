# Changelog for lazyppl

## 1.1.0

### Added

- `lazyppl-examples`: separate examples and plotting package
- Playground: interactive browser models
- Example pages: locally served fonts and icons

### Changed

- Build: Cabal and GHC 9.6.7, 9.12.4, 9.14.1
- `gp`, `wiener`: pure Haskell without LAPACK
- `mh`, `mhirreducible`: lower stream memory retention
- `memrec`: `Ord` keys without `Show`
- Integer `memoize`: restricted to `Prob`
- Distributions: validate inputs and parameter domains
- `beta`, `dirichlet`: reject unsupported shape parameters
- Random labels and acceptance draws: open interval (0,1)
- `mh`, `mhirreducible`, `weightedsamples`, `wis`: separate generator per run
- `superuniformbounded`: bounded graph angle sampling

### Fixed

- `wis`: resample from all weighted samples
- Integer `memoize`: return values at their own indices
- `mh`, `mhirreducible`, `wis`: zero and invalid weights
- `mh1`: reject proposals without reverse sites
- `beta`, `dirichlet`: sampling at numerical boundaries
- `beta`, `dirichlet`: reject unsupported custom tree labels
- `categorical`: rounding with finite and infinite streams
- `categorical`: accept finite probabilities with a rounding-sized shortfall
- Dirichlet processes: small-stick accumulation
- `poisson`: sampling at extreme labels
- `ibp`: first customer's dish count
- `gp`: initial and conditional distributions
- `wiener`: variance at extreme times
- `normalLogPdf`, `betaLogPdf`, `dirichletLogPdf`: extreme-input accuracy
- `binomialLogPmf`: large-count accuracy
- `gammaLogPdf`, `poissonLogPmf`: extreme-input accuracy
- Memoization, Gaussian processes and counters: concurrent state
- `uniformbounded`, `uniformBoundedLogPdf`: overflowing bound differences
- Playground: retry failed WebAssembly downloads
- Playground: validate results before plotting
- Mondrian playground: reject overlaps and duplicate identifiers
- GraphDemo: graph probabilities and figure edges
- Example pages: correct maximum-likelihood sample labels
- Example pages: syntax colours in dark mode
- Website generation: preserve prior site on build failure

### Removed

- `LazyPPL.SingleSite`: moved to `lazyppl-singlesite`
- `lazyppl`: `hmatrix`, LAPACK and plotting dependencies

## 1.0.1

- Add support for GHC 9.8 and 9.10
- Drop upper bounds on dependencies (except `base`)
- Add log-density / log-PMF functions: `normalLogPdf`, `gammaLogPdf`, etc.
- Export `scoreLog` and `scoreProductLog` (already defined in 1.0, now visible)
- Remove `monad-extras` dependency

## 1.0

- Initial Hackage release
