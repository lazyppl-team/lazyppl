# <img style="height:45px" src="https://user-images.githubusercontent.com/8027127/223598298-21dd4207-612d-4b4e-be9c-4daa2ae2de5b.png" /> LazyPPL with MALA 

This is an implementation of [MALA](https://en.wikipedia.org/wiki/Metropolis-adjusted_Langevin_algorithm) for the [LazyPPL](https://lazyppl-team.github.io) non-parametric probabilistic programming library, starting from uniform distributions on the circle rather than the Lebesgue measure on the real line. So overall this is a high/infinite dimensional torus. 

Here are some examples, using the same source code as the original LazyPPL examples. 

## Linear regression 

500 samples with no thinning:

![](https://raw.githubusercontent.com/lazyppl-team/lazyppl/refs/heads/torus-experiments/images/mala-linear-reg.svg) | ![](https://raw.githubusercontent.com/lazyppl-team/lazyppl/refs/heads/torus-experiments/images/grw-linear-reg.svg) | ![](images/lmh-linear-reg.svg)
MALA | Gaussian random walk | Original LazyPPL 

## Piecewise linear regression 

2000 samples with no thinning:

![]((https://raw.githubusercontent.com/lazyppl-team/lazyppl/refs/heads/torus-experiments/images/mala/mala-piecewiseconst-reg.svg) | ![]((https://raw.githubusercontent.com/lazyppl-team/lazyppl/refs/heads/torus-experiments/images/mala/grw-piecewiseconst-reg.svg) | ![]((https://raw.githubusercontent.com/lazyppl-team/lazyppl/refs/heads/torus-experiments/images/mala/lmh-piecewiseconst-reg.svg)
MALA | Gaussian random walk | Original LazyPPL 

## Testing samples in one dimension

1 million samples:

![]((https://raw.githubusercontent.com/lazyppl-team/lazyppl/refs/heads/torus-experiments/images/mala/test-uniform.svg) | ![]((https://raw.githubusercontent.com/lazyppl-team/lazyppl/refs/heads/torus-experiments/images/mala/test-normal.svg)
Uniform | Standard Gaussian
