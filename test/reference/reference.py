"""Reference values for the Beta and Dirichlet samplers and the log densities.

Run from the package root:

    uv run --with mpmath==1.4.1 python -I test/reference/reference.py [--jobs N]

It writes the tables beside this file, which test/Api.hs reads. Every value is
computed by mpmath at high precision from the definition of the distribution:
the incomplete Beta function (mpmath's betainc, or mpmath's quad of the
density), mpmath's loggamma and logarithms. None comes from the Haskell code.

Tables:

  beta-quantiles.tsv  For Beta(a,b) and a label u, the exact u-quantile x and
      1 - x, and for each the interval of doubles that the samplers may return:
      every double whose distribution function differs from u by at most
      EPS * min(u, 1 - u), joined with the two doubles next to the exact value.
      A row marked "reject" names an input rejected by both samplers; it has no
      quantile intervals.
  dirichlet-sticks.tsv  dirichlet as at the tree whose every label is u: the
      exact stick-breaking coordinates, each with a relative tolerance built
      from the same intervals.
  beta-laws.tsv  Distribution functions at grids of points, for law checks of
      two general Beta shapes and the four nonparametric stick laws.
  log-densities.tsv  Log densities and masses, with the exact value rounded to
      the nearest double (or an infinity where it leaves the double range).
"""

import argparse
import math
import os
import struct
import sys
from multiprocessing import Pool

import mpmath as mp

# mpmath 1.4.1 is the latest stable release (PyPI, 15 March 2026). The tables
# are reproducible only for the version that wrote them.
MPMATH_VERSION = "1.4.1"
if mp.__version__ != MPMATH_VERSION:
    sys.exit(f"reference.py needs mpmath {MPMATH_VERSION}, found {mp.__version__}")

HERE = os.path.dirname(os.path.abspath(__file__))

# Relative tolerance on the probability of a returned quantile.
EPS = "1e-9"

MAXD = sys.float_info.max


# Doubles and exact values -------------------------------------------------

def exact(v):
    """The exact value of a double or an integer, as an mpf."""
    return mp.mpf(v)


def down(v):
    """The largest double at most v (v an mpf in the double range)."""
    if v <= 0:
        return 0.0 if v == 0 else -math.inf
    f = float(mp.nstr(v, 25, strip_zeros=False))
    while f > 0 and exact(f) > v:
        f = math.nextafter(f, -math.inf)
    while math.nextafter(f, math.inf) <= MAXD and exact(math.nextafter(f, math.inf)) <= v:
        f = math.nextafter(f, math.inf)
    return f


def up(v):
    """The smallest double at least v."""
    f = down(v)
    return f if exact(f) == v else math.nextafter(f, math.inf)


def nearest(v):
    """The double nearest to v, ties to even, with infinities beyond the range:
    from MAXD plus half its spacing 2^970 up, IEEE rounding gives infinity."""
    if abs(v) >= exact(MAXD) + mp.mpf(2) ** 970:
        return math.copysign(math.inf, v)
    if v < 0:
        return -nearest(-v)
    lo, hi = down(v), up(v)
    if lo == hi:
        return lo
    dlo, dhi = v - exact(lo), exact(hi) - v
    if dlo < dhi:
        return lo
    if dhi < dlo:
        return hi
    # The even one has an even significand, the last bit of its encoding,
    # subnormal doubles included.
    return lo if struct.unpack("<Q", struct.pack("<d", lo))[0] % 2 == 0 else hi


def show(f):
    """A double as Haskell's read accepts it, exactly."""
    if f == math.inf:
        return "Infinity"
    if f == -math.inf:
        return "-Infinity"
    return repr(float(f))


# The Beta distribution ----------------------------------------------------

def logbeta(a, b):
    return mp.loggamma(a) + mp.loggamma(b) - mp.loggamma(a + b)


def logpdf(a, b, x, y):
    """log density of Beta(a,b) at x, with y = 1 - x given exactly."""
    return (a - 1) * mp.log(x) + (b - 1) * mp.log(y) - logbeta(a, b)


# Below this smaller parameter the tails come from mpmath's betainc, the
# hypergeometric series; above it both parameters exceed 1, the density is
# log-concave, and the tails come from mpmath's quad of the density.
SERIES_MAX = 100


def lower_by_quad(a, b, x, y):
    """P(X <= x) for X ~ Beta(a,b), a, b > 1, x at most the mode, by quadrature.

    Below x the density is at most f(x) exp(-h (x - t)), with h the slope of
    log f at x (log-concavity), so the pieces [x - 2^k s, x - 2^(k-1) s], with s
    the smaller of 1/h and the standard deviation, each see the density fall by
    a bounded factor, and the sum stops once that bound makes the rest
    negligible.
    """
    lb = logbeta(a, b)
    f = lambda t: mp.exp((a - 1) * mp.log(t) + (b - 1) * mp.log1p(-t) - lb)
    h = (a - 1) / x - (b - 1) / y
    sd = mp.sqrt(a * b / ((a + b) ** 2 * (a + b + 1)))
    s = min(1 / h, sd) if h > 0 else sd
    points = [x]
    k = 0
    while True:
        p = x - s * mp.mpf(2) ** k
        if p <= 0:
            points.append(mp.mpf(0))
            break
        points.append(p)
        if h > 0 and h * (x - p) > 400 + mp.mp.dps * 2.31 and p * mp.exp(-h * (x - p)) < s * mp.mpf(10) ** (-mp.mp.dps - 20):
            break
        k += 1
    points.reverse()
    return mp.quad(f, points)


def series_tails(p, q, z):
    """(P(Z <= z), P(Z > z)) for Z ~ Beta(p,q), by mpmath's betainc in z. Its
    hypergeometric series has terms summing in magnitude to at most (1+z)^(q-1)
    and a value at least (1-z)^(q-1), so the working precision gains that many
    digits; the complement is formed at that precision, which doubles until the
    complement has 30 correct digits."""
    cancel = 0.4343 * 2 * float(mp.atanh(z)) * max(float(q) - 1, 0)
    extra = int(cancel) + 30
    while True:
        with mp.extradps(extra):
            lower = mp.betainc(p, q, 0, z, regularized=True)
            upper = 1 - lower
            if upper > mp.mpf(10) ** (int(cancel) + 30 - mp.mp.dps):
                return lower, upper
        extra *= 2


# Above this many digits of cancellation the series is not used.
CANCEL_MAX = 6000


def tails(a, b, x, y):
    """(P(X <= x), P(X > x)) for X ~ Beta(a,b), with y = 1 - x exactly.

    One tail is computed directly and the other as its complement, at a working
    precision where the complement keeps 30 correct digits; by quadrature, the
    tail computed directly is the one beyond the mode, so the other is at least
    about 1/2.
    """
    if min(a, b) <= SERIES_MAX:
        # Evaluate the series on the side with less estimated cancellation.
        # Either side gives both tails, and the other remains a fallback when
        # a series argument or its cancellation exceeds the supported range.
        near = (a, b, x, False) if x <= y else (b, a, y, True)
        far = (b, a, y, True) if x <= y else (a, b, x, False)
        choices = []
        for p, q, z, mirrored in (near, far):
            if z <= mp.mpf("0.9"):
                cancel = 0.4343 * 2 * float(mp.atanh(z)) * max(float(q) - 1, 0)
                if cancel <= CANCEL_MAX:
                    choices.append((cancel, p, q, z, mirrored))
        if choices:
            _, p, q, z, mirrored = min(choices, key=lambda choice: choice[0])
            v, c = series_tails(p, q, z)
            return (c, v) if mirrored else (v, c)
        raise RuntimeError(f"no series route for Beta({a},{b}) at {x}")
    mode = (a - 1) / (a + b - 2)
    if x <= mode:
        lower = lower_by_quad(a, b, x, y)
        return lower, 1 - lower
    upper = lower_by_quad(b, a, y, x)
    return 1 - upper, upper


def quantile(a, b, u):
    """The exact u-quantile of Beta(a,b) as (x, y = 1 - x, f(x)), with the
    smaller of x and y found first, on the log scale, from the smaller of the two
    probabilities u and 1 - u, each exact."""
    u = exact(u)
    half = mp.mpf(1) / 2
    lower_half, _ = tails(a, b, half, half)
    if u <= lower_half:
        p, q, small_is_x, below, above = a, b, True, u, 1 - u
    else:
        p, q, small_is_x, below, above = b, a, False, 1 - u, u
    # z ~ Beta(p,q), z <= 1/2, with P(Z <= z) = below and P(Z > z) = above; t = log z.
    use_above = above < below
    def g(t):
        z = mp.exp(t)
        lower, upper = tails(p, q, z, 1 - z)
        if use_above:
            return mp.log(above) - mp.log(upper)
        return mp.log(lower) - mp.log(below)
    if g(mp.log(half)) <= 0:
        z = half
    else:
        # From the mean upward until the tail reaches the target, then downward
        # from there, so no evaluation lies far beyond the distribution's bulk.
        mean = p / (p + q)
        sd = mp.sqrt(p * q / ((p + q) ** 2 * (p + q + 1)))
        k = 4
        while g(mp.log(min(half, mean + k * sd))) < 0:
            k *= 2
        hi = mp.log(min(half, mean + k * sd))
        lo = hi - 1
        while g(lo) >= 0:
            lo = hi - 2 * (hi - lo)
        # mpmath's Anderson-Bjorck method keeps the root bracketed in [lo, hi],
        # and findroot checks the residual at the root it returns.
        z = mp.exp(mp.findroot(g, (lo, hi), solver="anderson", maxsteps=400))
    w = 1 - z
    x, y = (z, w) if small_is_x else (w, z)
    return x, y, mp.exp(logpdf(a, b, x, y)), small_is_x


def certify_bounds(lo, hi, value, a, b, target, use_lower):
    """Move a non-adjacent endpoint inward if its exact tail misses the tolerance."""
    adjacent = (down(value), up(value))
    with mp.workdps(80):
        p, q, t = exact(a), exact(b), exact(target)
        delta = mp.mpf(EPS) * t
        def accepted(v):
            if v in adjacent:
                return True
            x = exact(v)
            if x == 0:
                lower, upper = mp.mpf(0), mp.mpf(1)
            elif x == 1:
                lower, upper = mp.mpf(1), mp.mpf(0)
            else:
                lower, upper = tails(p, q, x, 1 - x)
            probability = lower if use_lower else upper
            return abs(probability - t) <= delta
        while not accepted(lo):
            lo = math.nextafter(lo, math.inf)
        while not accepted(hi):
            hi = math.nextafter(hi, -math.inf)
    return lo, hi


def quantile_row(case):
    a, b, u = case
    analytic = a == 1 or b == 1
    if not (in_domain(a, b) or analytic) or (u < 2.0 ** -53 and not analytic):
        return [show(a), show(b), show(u), "reject"]
    with mp.workdps(40):
        x, y, density, small_is_x = quantile(exact(a), exact(b), u)
        s = min(exact(u), 1 - exact(u))
        delta = mp.mpf(EPS) * s
        x_low, y_high, _, _ = quantile(exact(a), exact(b), exact(u) - delta)
        x_high, y_low, _, _ = quantile(exact(a), exact(b), exact(u) + delta)
        xlo, xhi = min(up(x_low), down(x)), max(down(x_high), up(x))
        ylo, yhi = min(up(y_low), down(y)), max(down(y_high), up(y))
        # If the entire smaller-share range is below 2^-53, its complement's
        # probability interval, clipped to [0,1], and the adjacent doubles of
        # the exact complement fit in [nextafter(1,0), 1].
        z_high = x_high if small_is_x else y_high
        near_one_needed = z_high < mp.mpf(2) ** -53
        if near_one_needed:
            near_one = (math.nextafter(1.0, 0.0), 1.0)
            if small_is_x:
                ylo, yhi = near_one
            else:
                xlo, xhi = near_one
        if exact(u) <= mp.mpf("0.5"):
            if not (near_one_needed and not small_is_x):
                xlo, xhi = certify_bounds(xlo, xhi, x, a, b, u, True)
            if not (near_one_needed and small_is_x):
                ylo, yhi = certify_bounds(ylo, yhi, y, b, a, u, False)
        else:
            target = 1 - exact(u)
            if not (near_one_needed and not small_is_x):
                xlo, xhi = certify_bounds(xlo, xhi, x, a, b, target, False)
            if not (near_one_needed and small_is_x):
                ylo, yhi = certify_bounds(ylo, yhi, y, b, a, target, True)
        return [show(a), show(b), show(u), show(xlo), show(xhi), show(ylo), show(yhi),
                "x" if small_is_x else "y", mp.nstr(x if small_is_x else y, 20),
                show(nearest(delta / density))]


def stick_row(case):
    """dirichlet as at the constant label u: coordinate i is the remaining
    stick times the share v_i, the u-quantile of Beta(a_i, a_(i+1) + ...)."""
    alphas, u = case
    with mp.workdps(40):
        rest = mp.mpf(1)
        rest_low = rest_high = mp.mpf(1)
        coords = []
        s = min(exact(u), 1 - exact(u))
        for i, a in enumerate(alphas[:-1]):
            later = sum(exact(c) for c in alphas[i + 1:])
            v, w, _, _ = quantile(exact(a), later, u)
            delta = mp.mpf(EPS) * s
            v_low, w_high, _, _ = quantile(exact(a), later, exact(u) - delta)
            v_high, w_low, _, _ = quantile(exact(a), later, exact(u) + delta)
            coords.append((rest * v, rest_low * v_low, rest_high * v_high))
            rest, rest_low, rest_high = rest * w, rest_low * w_low, rest_high * w_high
        coords.append((rest, rest_low, rest_high))
        ulps = mp.mpf(2) ** -53
        cells = []
        for i, (c, lower, upper) in enumerate(coords):
            slack = (4 * i + 6) * ulps
            lo = min(down(lower * (1 - slack)), down(c))
            hi = max(up(upper * (1 + slack)), up(c))
            cells += [show(lo), show(hi)]
        return [",".join(show(a) for a in alphas), show(u)] + cells


def law_row(case):
    """P(X <= x) for X ~ Beta(a,b) at a double x."""
    a, b, x = case
    with mp.workdps(40):
        lower, _ = tails(exact(a), exact(b), exact(x), 1 - exact(x))
        return [show(a), show(b), show(x), show(nearest(lower))]


# Log densities -------------------------------------------------------------

def log_density(case):
    name, args = case[0], case[1:]
    with mp.workdps(400):
        if name == "poissonLogPmf":
            lam, k = exact(args[0]), args[1]
            if k == 0:
                v = -lam
            else:
                v = k * mp.log(lam) - lam - mp.loggamma(k + 1)
        elif name == "gammaLogPdf":
            k, theta, x = map(exact, args)
            v = (k - 1) * mp.log(x) - x / theta - k * mp.log(theta) - mp.loggamma(k)
        elif name == "betaLogPdf":
            a, b, x = map(exact, args)
            v = logpdf(a, b, x, 1 - x)
        elif name == "binomialLogPmf":
            n, p, k = args
            p = exact(p)
            v = (mp.loggamma(n + 1) - mp.loggamma(k + 1) - mp.loggamma(n - k + 1)
                 + (k * mp.log(p) if k > 0 else 0) + ((n - k) * mp.log1p(-p) if n > k else 0))
        elif name == "dirichletLogPdf":
            alphas, xs = map(lambda l: [exact(v) for v in l], args)
            assert sum(xs) == 1, xs
            v = (mp.loggamma(sum(alphas)) - sum(mp.loggamma(a) for a in alphas)
                 + sum((a - 1) * mp.log(x) for a, x in zip(alphas, xs)))
        else:
            raise ValueError(name)
        return [name, " ".join(render(a) for a in args), show(nearest(v))]


def render(arg):
    if isinstance(arg, list):
        return "[" + ",".join(show(v) for v in arg) + "]"
    if isinstance(arg, int):
        return str(arg)
    return show(arg)


# Cases -----------------------------------------------------------------------

LABELS = [2.0 ** -53, 1e-300, 1e-100, 1e-20, 1e-8, 0.001, 0.25, 0.5, 0.75, 0.999,
          1 - 1e-8, 1 - 2.0 ** -30, 1 - 2.0 ** -53]
PARAMETERS = [1e-10, 1e-8, 1e-7, 1e-6, 1e-5, 1e-4, 1e-3, 0.0018614780417212304, 0.01, 0.1,
              0.5, 1.0, 2.0, 30.0, 53.5, 100.0, 1000.0, 1e4, 1e5, 1e6, 1e7, 1e8, 1e10]
# Beta(a,1) at the least label of the default generator for a around 53, where
# the quantile passes 1/2, the mirror cases Beta(1,b), a subnormal quantile, and
# a large shape.
BRANCH_CASES = [(a, 1.0, 2.0 ** -53) for a in [52.0, 53.0, 54.0, 100.0, 1000.0, 1e4]] + [
    (1.0, b, 1 - 2.0 ** -53) for b in [52.0, 53.0, 54.0, 100.0, 1000.0, 1e4]] + [
    (0.0018614780417212304, 1.0, 0.25), (1e16, 1e16, 0.25), (1e16, 1e16, 0.75)]


def in_domain(a, b):
    """The documented domain of dirichlet [a,b] and the general case of beta a b
    (src/LazyPPL/Distributions.hs); beta also takes Beta(a,1) and Beta(1,b)."""
    return min(a, b) >= PARAM_LEAST and a + b <= SUM_MAX


def edge_cases():
    """The domain's corners and the doubles just outside them, at every label:
    the least parameter beside a sum at the bound, two halves of the bound, and
    pairs one double below the least parameter or one double above the sum."""
    least, most = PARAM_LEAST, SUM_MAX
    inside = [(least, least), (least, 1.0), (least, most - least), (1.0, most - 1.0), (most / 2, most / 2)]
    below = math.nextafter(least, 0)
    outside = [(below, 1.0), (below, most - least), (most / 2, math.nextafter(most / 2, math.inf))]
    pairs = inside + outside
    return [(a, b, u) for a, b in pairs + [(b, a) for a, b in pairs if a != b] for u in LABELS]


def random_cases(count, seed=20261101):
    """Parameters drawn log-uniformly inside the domain, each with a label of LABELS, so the
    check covers more than the grid's points."""
    import random
    rng = random.Random(seed)
    cases = []
    while len(cases) < count:
        a = 10 ** rng.uniform(math.log10(PARAM_LEAST), math.log10(SUM_MAX))
        b = 10 ** rng.uniform(math.log10(PARAM_LEAST), math.log10(SUM_MAX))
        if in_domain(a, b):
            cases.append((float(f"{a:.6g}"), float(f"{b:.6g}"), rng.choice(LABELS)))
    return cases


def quantile_cases():
    grid = [(a, b, u) for a in PARAMETERS for b in PARAMETERS for u in LABELS]
    extra = BRANCH_CASES + edge_cases() + random_cases(400)
    return list(dict.fromkeys(grid + extra))


def stick_cases(least, sum_max):
    third = sum_max / 3
    vectors = [[1.0, 2.0, 3.0], [3.0, 2.0, 1.0], [2.0, 3.0, 1.0],
               [0.5, 0.5, 0.5, 0.5], [0.001, 1.0, 5.0], [5.0, 1.0, 0.001], [1.0, 5.0, 0.001],
               [third, third, third], [least, least, least], [least, sum_max - 2 * least, least],
               [30.0, 0.01, 0.5, 2.0]]
    return [(v, u) for v in vectors for u in [2.0 ** -53, 0.25, 0.5, 0.75, 1 - 2.0 ** -53]]


def law_cases(shapes):
    """Points for the law checks: 1201 points from the mean minus six standard
    deviations to the mean plus six where both parameters exceed SERIES_MAX
    (the law is then close to normal), and the quantiles at the levels k/1000
    otherwise. Closed forms select the points for the stick laws; law_row still
    computes their expected distribution functions independently with betainc.
    """
    rows = []
    for a, b in shapes:
        with mp.workdps(40):
            a_, b_ = exact(a), exact(b)
            if min(a, b) > SERIES_MAX:
                mean = a_ / (a_ + b_)
                sd = mp.sqrt(a_ * b_ / ((a_ + b_) ** 2 * (a_ + b_ + 1)))
                xs = {nearest(mean + sd * mp.mpf(z) / 100) for z in range(-600, 601)}
            elif b == 1:
                xs = {nearest(mp.exp(mp.log(exact(k / 1000)) / a_)) for k in range(1, 1000)}
            elif a == 1:
                xs = {nearest(-mp.expm1(mp.log1p(-exact(k / 1000)) / b_)) for k in range(1, 1000)}
            else:
                xs = {nearest(quantile(a_, b_, k / 1000)[0]) for k in range(1, 1000)}
        rows += [(a, b, x) for x in sorted(xs) if 0 < x < 1]
    return rows


def density_cases():
    cases = []
    lams = [1e-320, 1e-300, 1e-10, 0.5, 1.0, 3.0, 10.0, 1e6, 1e16, 1e32, 1e100, 1e300, 3e307, 1e308, MAXD]
    ks = [0, 1, 2, 10, 1000000, 1000001, 10 ** 16, 2 ** 53 + 1, 10 ** 32, 10 ** 100, 10 ** 300, 3 * 10 ** 307, 10 ** 308]
    for lam in lams:
        cases += [("poissonLogPmf", lam, k) for k in ks]
        t = int(lam)
        if t > 1:
            spread = max(1, math.isqrt(t))
            cases += [("poissonLogPmf", lam, k) for k in [t - 3 * spread, t - 1, t, t + 1, t + 3 * spread]]
    cases += [("poissonLogPmf", 1.25e307, int(1e308)), ("poissonLogPmf", 1e308, 2 * 10**308),
              ("poissonLogPmf", MAXD, int(MAXD) + 2**971), ("poissonLogPmf", MAXD, 3 * 10**308)]
    shapes = [1e-300, 1e-10, 0.5, 1.0, 2.0, 1000.0, 1e16, 1e300, 1e308]
    scales = [1e-300, 1e-10, 1.0, 8.0, 1e10, 1e300]
    points = [1e-320, 1e-300, 1e-10, 1.0, 3.0, 1e10, 1e300, 1e308]
    cases += [("gammaLogPdf", k, th, x) for k in shapes for th in scales for x in points]
    cases += [("gammaLogPdf", 3e307, 1.0, 3e307), ("gammaLogPdf", 1e308, 8.0, 1e308), ("gammaLogPdf", 1e16, 1.0, 1e16)]
    # Near the centre with large parameters, where a rounded x / theta, n * p or
    # (a + b - 2) * x moves the deviance: z standard deviations from the centre.
    zs = [0.5, 3.0, 30.0]
    for k in [1e6, 1e8, 1e12, 1e20, 1e100, 1e300]:
        for theta in [3.0, 1e-7]:
            cases += [("gammaLogPdf", k, theta, theta * (k - 1 + z * math.sqrt(k))) for z in zs]
    cases += [("gammaLogPdf", 1e300, 1e-300, 1.0), ("gammaLogPdf", 1e308, 0.5, 1e308),
              ("gammaLogPdf", MAXD, 0.9, MAXD)]
    for n in [10 ** 6, 10 ** 8, 10 ** 12, 2 ** 62]:
        for p in [0.3, 1 / 3, 0.7]:
            sd = math.sqrt(n * p * (1 - p))
            cases += [("binomialLogPmf", n, p, round(n * p + z * sd)) for z in zs]
    for a, b in [(1e6, 3e6), (1e8, 3e8), (1e12, 1e12 + 5), (1e100, 2e100), (1e300, 3e300)]:
        mode = (a - 1) / (a + b - 2)
        sd = math.sqrt(a) * math.sqrt(b) / (a + b) / math.sqrt(a + b + 1)
        cases += [("betaLogPdf", a, b, mode + z * sd) for z in zs]
    concentrations = [1e-300, 1e-100, 1e-10, 1e-3, 0.5, 1.0, 1.5, 2.0, 3.0, 10.0, 1000.0, 1e8, 1e16, 1e100, 1e300, 8e307]
    xs = [1e-300, 1e-20, 2.0 ** -53, 1e-3, 0.25, 0.5, 0.75, 1 - 2.0 ** -53]
    cases += [("betaLogPdf", a, b, x) for a in concentrations for b in concentrations for x in xs]
    cases += [("betaLogPdf", 1.0, 1e20, 1e-20), ("betaLogPdf", 2.0, 2.0, 0.5)]
    ns = [1, 10, 1000, 1000000, 10 ** 16, 2 ** 62, 2 ** 63 - 1]
    ps = [0.0, 1e-300, 1e-16, 1e-3, 0.3, 0.5, 0.7, 1 - 2.0 ** -53, 1.0]
    for n in ns:
        for p in ps:
            for k in sorted({0, 1, n // 2, n - 1, n, min(n, max(0, round(n * p)))}):
                if (p == 0 and k > 0) or (p == 1 and k < n):
                    continue
                cases.append(("binomialLogPmf", n, p, k))
    cases += [("dirichletLogPdf", al, x) for al, x in [
        ([1e200, 1e200], [0.5, 0.5]), ([1e16, 1e16], [0.5, 0.5]), ([1e300, 1e300], [0.5, 0.5]),
        ([1.0, 1.0, 1.0], [0.25, 0.25, 0.5]), ([2.0, 3.0, 5.0], [0.125, 0.375, 0.5]),
        ([0.001, 0.001, 0.002], [0.25, 0.25, 0.5]), ([1e-300, 1.0, 1e300], [0.25, 0.25, 0.5]),
        ([0.5, 0.5, 0.5, 0.5], [0.0625, 0.1875, 0.25, 0.5]), ([1e8, 3e8, 4e8], [0.125, 0.375, 0.5]),
        ([1e8, 3e8, 4e8], [0.125 + 2.0 ** -20, 0.375 - 2.0 ** -20, 0.5]),
        ([1e12, 3e12, 4e12], [0.125 + 2.0 ** -24, 0.375 - 2.0 ** -24, 0.5])]]
    # Each case once, at its first place; repr tells the cases apart, lists included.
    unique = {}
    for case in cases:
        unique.setdefault(repr(case), case)
    return list(unique.values())


# Output ----------------------------------------------------------------------

def write(name, header, rows):
    path = os.path.join(HERE, name)
    with open(path, "w") as out:
        out.write(f"# Written by test/reference/reference.py with mpmath {MPMATH_VERSION}.\n")
        out.write("# " + "\t".join(header) + "\n")
        for row in rows:
            out.write("\t".join(row) + "\n")
    print(f"wrote {path}: {len(rows)} rows", flush=True)


def self_checks():
    """The tail and quantile routines against closed forms and each other."""
    with mp.workdps(40):
        def close(v, ref, name, rel=mp.mpf("1e-25")):
            if abs(v - ref) > rel * abs(ref):
                sys.exit(f"self-check failed: {name}: {v} against {ref}")
        for a, x in [(0.001, 1e-300), (30.0, 0.7), (1e8, 0.9999999), (2.5, 0.5)]:
            a_, x_ = exact(a), exact(x)
            lower, upper = tails(a_, mp.mpf(1), x_, 1 - x_)
            close(lower, x_ ** a_, f"Beta({a},1) lower tail at {x}")
            close(upper, -mp.expm1(a_ * mp.log(x_)), f"Beta({a},1) upper tail at {x}")
            lower, upper = tails(mp.mpf(1), a_, x_, 1 - x_)
            close(upper, (1 - x_) ** a_, f"Beta(1,{a}) upper tail at {x}")
        a, b, x = mp.mpf("1e-8"), mp.mpf(10000), mp.mpf("0.5")
        _, upper = tails(a, b, x, 1 - x)
        direct_upper = mp.betainc(b, a, 0, 1 - x, regularized=True)
        close(upper, direct_upper, "asymmetric Beta upper tail")
        lower, _ = tails(mp.mpf("0.5"), mp.mpf("0.5"), mp.mpf("0.3"), mp.mpf("0.7"))
        close(lower, 2 / mp.pi * mp.asin(mp.sqrt(mp.mpf("0.3"))), "Beta(1/2,1/2) at 0.3")
        # The two methods agree where both apply.
        # The two methods agree where both apply, including betainc's far side
        # (an argument near 1) for a large parameter.
        for a, b, x in [(150.0, 150.0, 0.45), (150.0, 400.0, 0.2), (120.0, 130.0, 0.3),
                        (101.5, 1e6, 7e-5), (101.5, 1e6, 1.4e-4)]:
            a_, b_, x_ = exact(a), exact(b), exact(x)
            mode = (a_ - 1) / (a_ + b_ - 2)
            by_quad = lower_by_quad(a_, b_, x_, 1 - x_) if x_ <= mode else 1 - lower_by_quad(b_, a_, 1 - x_, x_)
            by_series = series_tails(a_, b_, x_)[0]
            by_series_far = series_tails(b_, a_, 1 - x_)[1]
            close(by_quad, by_series, f"quad against betainc for Beta({a},{b}) at {x}")
            close(by_quad, by_series_far, f"quad against betainc's far side for Beta({a},{b}) at {x}")
        # Closed forms: Beta(a,1) has the quantile u^(1/a), Beta(1,b) has 1 - (1-u)^(1/b).
        for a, u in [(1000.0, 1e-300), (100.0, 2.0 ** -53), (0.001, 1e-300), (1e8, 1 - 2.0 ** -53), (30.0, 0.25)]:
            x, y, _, _ = quantile(exact(a), mp.mpf(1), u)
            close(x, exact(u) ** (1 / exact(a)), f"Beta({a},1) quantile at {u}", mp.mpf("1e-30"))
            x, y, _, _ = quantile(mp.mpf(1), exact(a), u)
            close(y, (1 - exact(u)) ** (1 / exact(a)), f"Beta(1,{a}) complement of the quantile at {u}", mp.mpf("1e-30"))
        for a, b, u in [(0.5, 3.0, 1e-300), (1e6, 1e6, 0.25), (1.0, 0.001, 0.25), (200.0, 1.0, 2.0 ** -53)]:
            x, y, _, small_is_x = quantile(exact(a), exact(b), u)
            lower, upper = tails(exact(a), exact(b), x, y)
            target = exact(u) if small_is_x else 1 - exact(u)
            close(lower if small_is_x else upper, target, f"quantile of Beta({a},{b}) at {u}", mp.mpf("1e-30"))
        row = quantile_row((1000.0, 1.0, 2.0 ** -53))
        lo, hi = map(float, row[3:5])
        bad = 0.9639298077416072
        u = mp.mpf(2) ** -53
        assert abs(exact(bad) ** 1000 - u) > mp.mpf(EPS) * u
        assert not lo <= bad <= hi, "out-of-tolerance endpoint accepted"
        quantile_value = u ** (mp.mpf(1) / 1000)
        assert all(lo <= v <= hi for v in (down(quantile_value), up(quantile_value)))
        assert lo <= math.exp(math.log(float(u)) / 1000) <= hi
        sparse = quantile_row((0.001, 0.001, 0.999))
        assert list(map(float, sparse[3:5])) == [math.nextafter(1.0, 0.0), 1.0]
    print("self-checks passed", flush=True)


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("--jobs", type=int, default=os.cpu_count())
    parser.add_argument("--only", choices=["quantiles", "sticks", "laws", "densities"], action="append")
    parser.add_argument("--probe", action="store_true", help="time a few quantile rows and stop")
    args = parser.parse_args()
    only = set(args.only or ["quantiles", "sticks", "laws", "densities"])
    self_checks()
    if args.probe:
        import time
        for case in [(0.5, 3.0, 0.25), (1e-10, 1e-10, 0.25), (100.0, 1.0, 2.0 ** -53), (1e8, 53.5, 0.25),
                     (53.5, 1e8, 1e-300), (1e4, 1e4, 0.25), (1e6, 1e6, 1e-300), (1e8, 1e8, 0.75),
                     (1e10, 1e10, 0.25), (1e16, 1e16, 0.25)]:
            start = time.time()
            row = quantile_row(case)
            print(f"{time.time() - start:7.2f}s", "\t".join(row), flush=True)
        return
    with Pool(args.jobs) as pool:
        if "densities" in only:
            write("log-densities.tsv", ["function", "arguments", "exact log density, nearest double"],
                  pool.map(log_density, density_cases(), chunksize=4))
        if "quantiles" in only:
            write("beta-quantiles.tsv",
                  ["a", "b", "u", "x low or reject", "x high", "1-x low", "1-x high", "smaller side",
                   "smaller side exact", "linearised half-width (informational)"],
                  pool.map(quantile_row, quantile_cases(), chunksize=1))
        if "sticks" in only:
            write("dirichlet-sticks.tsv", ["alphas", "u", "coordinate 1 low", "coordinate 1 high", "..."],
                  pool.map(stick_row, stick_cases(PARAM_LEAST, SUM_MAX), chunksize=1))
        if "laws" in only:
            write("beta-laws.tsv", ["a", "b", "x", "P(X <= x), nearest double"],
                  pool.map(law_row, law_cases(LAW_SHAPES), chunksize=8))


# The supported domain of the Beta and Dirichlet samplers, measured with the
# table of this script (src/LazyPPL/Distributions.hs, betaLeast). The edge
# cases, the random cases, two of the stick vectors and the law shape
# (1000,1000), the largest symmetric one inside, come from it.
PARAM_LEAST = 1e-3
SUM_MAX = 2000.0
# Beta(1,b) and Beta(a,1) at a concentration of 1e-4 and of 1e4: the sticks of
# the Dirichlet process (Beta(1,alpha)) and of newRestaurantS (Beta(alpha,1)).
LAW_SHAPES = [(2.5, 3.5), (1000.0, 1000.0), (1.0, 1e-4), (1.0, 1e4), (1e-4, 1.0), (1e4, 1.0)]

if __name__ == "__main__":
    main()
