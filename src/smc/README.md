# Statistical Model Checking

This directory contains the experimental Statistical Model Checking (SMC)
engine for Kind 2.

The engine estimates bounded violation probabilities of Lustre invariant
properties by sampling input traces and checking the resulting bounded
executions with an SMT solver.

> **Status:** this feature is experimental. The interface, scope, and
> performance may still change.

## Usage

A basic SMC run can be started with:

```sh
kind2 --enable SMC --smc_runs 1000 --smc_steps 30 model.lus
```
where `--smc_runs` is the number of acceptes trace samples, and `--smc_steps` is the
execution horizon. By default, all inputs are randomly generated with respect to a uniform
distribution specified by `--smc_TYPE_min` and `--smc_TYPE_max` (except for booleans).

Additionnally, input distributions can be provided using an external configuration
file:

```sh
kind2 --enable SMC --smc_runs 1000 --smc_steps 30 --smc_input inputs.json model.lus
```

For example:

```json
{
  "fault": {
    "distribution": "bernoulli",
    "p": 0.01
  },

  "command": {
    "distribution": "uniform_int",
    "min": "0",
    "max": "10"
  },

  "noise": {
    "distribution": "uniform_real",
    "min": "-1.0",
    "max": "1.0"
  },

  "enabled": true
}
```

A JSON literal denotes a fixed input value. Inputs not listed in the file use
the default sampler for their type and inferred range.

Currently supported distributions include:

- `uniform`
- `bernoulli`
- `uniform_int`
- `uniform_real`

See `kind2 --help` for the complete list of SMC options.

## Semantics

For an invariant property `P` and a horizon `K`, SMC estimates the probability
that `P` is violated at least once during the bounded execution:

$$
p_K = \Pr\left[ \exists k \in [0, K - 1],\, \neg P(k) \right]
$$

Each accepted trace counts as one statistical sample.

If a sampled input trace is incompatible with the transition-system
constraints, it is rejected and does not count as an observation. The
resulting probability is therefore conditioned on feasibility over the
specified horizon (`K`).

Ordinary inputs are sampled at each logical instant. Constant inputs are
sampled once per trace.

Only explicitly sampled inputs are probabilistic. Values left logically
underspecified and resolved by the SMT solver are not assigned a probability
distribution by the current implementation.

## Estimators

### Fixed-sample estimation

The fixed estimator performs a user-specified number of $N$ accepted runs
and computes the observed violation frequency.

$$
\hat{p} = \frac{V}{N}
$$

where $V$ is the number of traces violating the property.

For a requested absolute precision $\varepsilon$, Kind2 reports the
Hoeffding confidence bound

$$
\Pr\left[
\left|\hat{p} - p_K\right| < \varepsilon
\right]
\ge
1 - 2 e^{-2N\varepsilon^2}
$$

For example:

```sh
kind2 --enable SMC \
  --smc_estimator fixed \ 
  --smc_runs 1000 \ # N
  --smc_steps 30 \ # K
  --smc_precision 0.05 \ # epsilon
  model.lus
```

### APMC estimation

With APMC, the user specifies the desired absolute precision $\varepsilon$ and
confidence $\gamma$. The required number of accepted samples is computed automatically as:

$$
N =
\left\lceil
  \frac
    {\ln\left(2/(1 - \gamma)\right)}
    {2\varepsilon^2}
\right\rceil
$$

For example:
```sh
kind2 --enable SMC \
  --smc_estimator apmc \
  --smc_steps 30 \ # K
  --smc_confidence 0.95 \ # gamma
  --smc_precision 0.05 \ # epsilon
  model.lus
```

### SPRT property testing

SPRT performs a sequential hypothesis test on the property violation
probability.

It can be enabled using:

```sh
--smc_estimator sprt
```

The following options are available:

```text
--smc_threshold <float>
--smc_delta <float>
--smc_alpha <float>
--smc_beta <float>
```

For a threshold \(\theta\) and an indifference parameter \(\delta\), SPRT
tests:

\[
P(\mathrm{violation}) \le \theta - \delta
\]

against:

\[
P(\mathrm{violation}) \ge \theta + \delta.
\]

`--smc_alpha` and `--smc_beta` specify the two error bounds.

`--smc_runs` specifies the maximum number of accepted samples. If neither
hypothesis is accepted before this limit, the result is reported as
`Inconclusive`.

Example:

```sh
kind2 --enable SMC \
  --smc_estimator sprt \
  --smc_runs 10000 \
  --smc_steps 20 \
  --smc_threshold 0.10 \
  --smc_delta 0.02 \
  --smc_alpha 0.05 \
  --smc_beta 0.05 \
  model.lus
```

This tests whether the bounded violation probability is below `0.08` or
above `0.12`.

## Current features

- fixed-sample Monte Carlo estimation with Hoeffding bounds
- APMC-style estimation
- per-input probability distributions
- fixed input values
- rejection sampling for constrained executions
- reproducible random sampling with specified seed
- incremental or one-shot SMT execution

The two SMT modes can be selected with:

```text
--smc_solver_mode incremental
```

or:

```text
--smc_solver_mode oneshot
```

Their relative performance is model dependent.


## Limitations

The implementation is still experimental.

In particular:

- only invariant properties are currently handled;
- statistical samples are executed sequentially;
- performance on simulation-oriented SMC benchmarks is still under active
  optimization;
- logical nondeterminism does not yet have solver-independent probabilistic
  semantics;
- SPRT is not yet implemented.
