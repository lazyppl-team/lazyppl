# Changelog for lazyppl

## 1.0.1 — 2026-03-28

- Support GHC 9.6, 9.8, and 9.10 (retaining GHC 8.10 backward compatibility)
- Widen dependency bounds: ghc-heap, deepseq, containers, hmatrix, random
- Remove monad-extras dependency (inline iterateM)
- Remove 11 unused dependencies
- Add log-density scoring functions (normalLogPdf, gammaLogPdf, etc.)
- Export scoreLog and scoreProductLog
- Add test suite (logpdf-test)
- Add CI via GitHub Actions (GHC 8.10, 9.6, 9.8, 9.10)

## 1.0

- Initial Hackage release
