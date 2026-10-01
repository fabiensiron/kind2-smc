import os
import math
import re
import subprocess
from pathlib import Path

import pytest


ROOT = Path(__file__).resolve().parent


# ---------------------------------------------------------------------------
# Kind2 executable
# ---------------------------------------------------------------------------

def find_kind2():
    """Find the Kind2 executable.

    The KIND2 environment variable can be used to override the executable.
    Otherwise, walk up from the test directory and look for a dune build.
    """

    if kind2 := os.environ.get("KIND2"):
        path = Path(kind2).expanduser().resolve()

        if not path.exists():
            raise RuntimeError(
                f"KIND2 points to a non-existing executable: {path}"
            )

        return path

    for parent in (ROOT, *ROOT.parents):
        candidates = [
            parent / "_build/default/src/kind2.exe",
            parent / "_build/default/src/kind2",
        ]

        for candidate in candidates:
            if candidate.exists():
                return candidate

    raise RuntimeError(
        "Could not find the Kind2 executable. "
        "Build Kind2 first or set the KIND2 environment variable."
    )


KIND2 = find_kind2()


# ---------------------------------------------------------------------------
# Output parsing
# ---------------------------------------------------------------------------

ANSI_ESCAPE = re.compile(
    r"\x1b(?:"
    r"\[[0-?]*[ -/]*[@-~]"
    r"|"
    r"[@-_]"
    r")"
)

def clean_output(output):
    """Remove ANSI colors/styles from Kind2 output."""
    return ANSI_ESCAPE.sub("", output)


def run_smc(model, *, params=[], runs, steps):
    model_path = ROOT / model

    assert model_path.exists(), (
        f"Missing Lustre model: {model_path}"
    )

    cmd = [
        str(KIND2),
        "--enable", "SMC",
        "--smc_runs", str(runs),
        "--smc_steps", str(steps),
        str(model_path),
    ]
    cmd += params

    proc = subprocess.run(
        cmd,
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE,
        text=True,
        timeout=120,
    )

    assert proc.returncode in (0, 30), (
        f"Kind2 failed with return code {proc.returncode}\n"
        f"Command:\n  {' '.join(cmd)}\n\n"
        f"stdout:\n{proc.stdout}\n\n"
        f"stderr:\n{proc.stderr}"
    )

    # Normally the result is on stdout. Keeping stderr in the returned
    # string also makes the tests tolerant of changes in Kind2's logging
    # destination.
    return clean_output(
        proc.stdout + "\n" + proc.stderr
    )


def probability(output, property_name):
    """Extract the estimated violation probability of a property."""

    pattern = (
        rf"Property\s+{re.escape(property_name)}\s*:"
        rf".*?"
        rf"Estimate\s*:\s*"
        rf"([0-9.eE+-]+)"
    )

    match = re.search(
        pattern,
        output,
        flags=re.DOTALL,
    )

    assert match is not None, (
        f"Could not find probability for property "
        f"{property_name!r} in output:\n\n{output}"
    )

    return float(match.group(1))

def confidence(output):
    match = re.search(
        r"Confidence\s*:\s*>=\s*([0-9.eE+-]+)%",
        output,
    )

    assert match is not None, (
        f"Could not find confidence in output:\n\n{output}"
    )

    return float(match.group(1)) / 100.0


def precision(output):
    match = re.search(
        r"Precision\s*:\s*[±+-]?\s*([0-9.eE+-]+)",
        output,
    )

    assert match is not None, (
        f"Could not find precision in output:\n\n{output}"
    )

    return float(match.group(1))

def sample_counts(output):
    """Extract generated, accepted and rejected sample counts."""

    generated = re.search(
        r"Generated\s*:\s*(\d+)",
        output,
    )

    accepted = re.search(
        r"Accepted\s*:\s*(\d+)",
        output,
    )

    rejected = re.search(
        r"Rejected\s*:\s*(\d+)",
        output,
    )

    assert generated is not None, output
    assert accepted is not None, output
    assert rejected is not None, output

    return (
        int(generated.group(1)),
        int(accepted.group(1)),
        int(rejected.group(1)),
    )


def assert_close(actual, expected, tolerance):
    assert abs(actual - expected) <= tolerance, (
        f"expected {expected:.6f} ± {tolerance:.6f}, "
        f"got {actual:.6f}"
    )

def hoeffding(runs, epsilon):
    delta = 2.0 * math.exp(-2.0 * runs * epsilon * epsilon)
    return max(0.0, min(1.0, 1.0 - delta))

def apmc_runs(precision, confidence):
    delta = 1.0 - confidence

    return math.ceil(
        math.log(2.0 / delta)
        / (2.0 * precision * precision)
    )

def property_block(output, property_name):
    """Extract the report block corresponding to one property."""

    start = re.search(
        rf"Property\s+{re.escape(property_name)}\s*:",
        output,
    )

    assert start is not None, (
        f"Could not find property {property_name!r} "
        f"in output:\n\n{output}"
    )

    remaining = output[start.end():]

    next_property = re.search(
        r"\n\s*Property\s+[^:\n]+\s*:",
        remaining,
    )

    if next_property is None:
        end = len(output)
    else:
        end = (
            start.end()
            + next_property.start()
        )

    return output[start.start():end]



def property_violation_counts(output, property_name):
    """Return (violations, samples) for one property."""

    block = property_block(
        output,
        property_name,
    )

    match = re.search(
        r"Violations\s*:\s*(\d+)\s*/\s*(\d+)",
        block,
    )

    assert match is not None, (
        f"Could not find violation counts for "
        f"{property_name!r} in:\n\n{block}"
    )

    return (
        int(match.group(1)),
        int(match.group(2)),
    )


def sprt_decision(output, property_name):
    """Extract the textual SPRT decision."""

    block = property_block(
        output,
        property_name,
    )

    match = re.search(
        r"Decision\s*:\s*([^\r\n]+)",
        block,
    )

    assert match is not None, (
        f"Could not find SPRT decision for "
        f"{property_name!r} in:\n\n{block}"
    )

    return match.group(1).strip()


def sprt_log_lr(output, property_name):
    """Extract the final SPRT log likelihood ratio."""

    block = property_block(
        output,
        property_name,
    )

    match = re.search(
        r"Log LR\s*:\s*([0-9.eE+-]+)",
        block,
    )

    assert match is not None, (
        f"Could not find SPRT log likelihood ratio for "
        f"{property_name!r} in:\n\n{block}"
    )

    return float(match.group(1))

def sprt_parameters(threshold, delta, alpha, beta):
    p_low = threshold - delta
    p_high = threshold + delta

    lower_bound = math.log(
        beta / (1.0 - alpha)
    )

    upper_bound = math.log(
        (1.0 - beta) / alpha
    )

    return (
        p_low,
        p_high,
        lower_bound,
        upper_bound,
    )


def sprt_constant_stopping_samples(
    *,
    violation,
    threshold,
    delta,
    alpha,
    beta,
):
    """Exact stopping time for an all-true or all-false observation stream."""

    (
        p_low,
        p_high,
        lower_bound,
        upper_bound,
    ) = sprt_parameters(
        threshold,
        delta,
        alpha,
        beta,
    )

    if violation:
        increment = math.log(
            p_high / p_low
        )

        return math.ceil(
            upper_bound / increment
        )

    increment = math.log(
        (1.0 - p_high)
        / (1.0 - p_low)
    )

    return math.ceil(
        lower_bound / increment
    )

# ---------------------------------------------------------------------------
# Deterministic regression tests
# ---------------------------------------------------------------------------

@pytest.mark.parametrize(
    "steps, expected",
    [
        (1, 0.0),
        (2, 0.0),
        (3, 1.0),
        (4, 1.0),
    ],
)
def test_horizon(steps, expected):
    output = run_smc(
        "horizon.lus",
        runs=10,
        steps=steps,
    )

    assert probability(
        output,
        "count_lt_2",
    ) == expected


@pytest.mark.parametrize(
    "steps, expected",
    [
        (1, 0.0),
        (2, 0.0),
        (3, 0.0),
        (5, 0.0),
    ],

)
def test_any_constraint(steps, expected):
    """The solver must always choose an `any` value satisfying its constraint."""

    output = run_smc(
        "any_deterministic.lus",
        runs=10,
        steps=steps,
    )

    assert probability(
        output,
        "any_constraint",
    ) == expected


# ---------------------------------------------------------------------------
# Statistical regression tests
# ---------------------------------------------------------------------------

@pytest.mark.parametrize(
    "steps",
    [
        (1),
        (2),
        (3),
        (5),
    ],

)
def test_boolean_horizon(steps):
    #steps = 5

    # Failure occurs if x = true at least once:
    #
    #   P(failure) = 1 - (1/2)^K
    #
    expected = 1.0 - 0.5 ** steps

    output = run_smc(
        "boolean.lus",
        runs=2000,
        steps=steps,
    )

    assert_close(
        probability(output, "never_x"),
        expected=expected,
        tolerance=0.025,
    )

@pytest.mark.parametrize(
    "steps",
    [
        (1),
        (2),
        (5),
        (10),
    ],

)
def test_constant_is_sampled_once(steps):
    """A const input must be sampled once for the whole execution."""

    output = run_smc(
        "const.lus",
        runs=2000,
        steps=steps,
    )

    # Since x is constant over the complete trace:
    #
    #   P(failure) = P(x = true) = 1/2
    #
    # If x were incorrectly re-sampled at each cycle this would instead
    # approach 1 - 2^-10 ~= 0.999.
    assert_close(
        probability(output, "never_x"),
        expected=0.5,
        tolerance=0.05,
    )

@pytest.mark.parametrize(
    "steps",
    [
        (1),
        (2),
        (5),
        (10),
    ],
)
def test_integer_range(steps):
    expected = 1.0 - (9.0 / 10.0) ** steps
    output = run_smc(
        "range.lus",
        params=["--smc_int_min", "0", "--smc_int_max", "9"],
        runs=2000,
        steps=1,
    )

    # x is uniform in [0,9].
    #
    # Property fails exactly when x = 0.
    assert_close(
        probability(output, "nonzero"),
        expected=0.1,
        tolerance=0.04,
    )

@pytest.mark.parametrize(
    "steps",
    [
        (1),
        (2),
        (5),
        (10),
    ],
)
def test_integer_subrange(steps):
    expected = 1.0 - (9.0 / 10.0) ** steps
    output = run_smc(
        "subrange.lus",
        runs=2000,
        steps=1,
    )

    # x is uniform in [0,9].
    #
    # Property fails exactly when x = 0.
    assert_close(
        probability(output, "nonzero"),
        expected=0.1,
        tolerance=0.04,
    )


def fibonacci(n):
    a, b = 0, 1

    for _ in range(n):
        a, b = b, a + b

    return a

@pytest.mark.parametrize(
    "steps",
    [
        (1),
        (2),
        (3),
        (5),
    ],
)
def test_consecutive_true(steps):

    # The number of Boolean sequences of length K without two
    # consecutive true values is F_(K+2).
    #
    # Hence:
    #
    #   P(failure) = 1 - F_(K+2) / 2^K
    #
    expected = (
        1.0
        - fibonacci(steps + 2)
        / (2.0 ** steps)
    )

    output = run_smc(
        "stateful.lus",
        runs=2000,
        steps=steps,
    )

    assert_close(
        probability(
            output,
            "no_consecutive_true",
        ),
        expected=expected,
        tolerance=0.04,
    )

@pytest.mark.parametrize(
    "steps",
    [
        (1),
        (2),
    ],
)
def test_rejection_sampling(steps):
    output = run_smc(
        "assumes.lus",
        params=["--smc_int_min", "0", "--smc_int_max", "9"],
        runs=2000,
        steps=steps,
    )

    generated, accepted, rejected = sample_counts(output)

    assert accepted == 2000

    assert generated == accepted + rejected

    # x,y are uniform in [0,9].
    #
    # There are 100 possible pairs and 45 satisfying x < y.
    #
    # Therefore:
    #
    #   P(accepted) = 45 / 100 = 0.45
    #
    acceptance_probability = (
        accepted / generated
    )

    expected = 0.45 ** steps

    assert_close(
        acceptance_probability,
        expected=expected,
        tolerance=0.05,
    )

    # Among the 45 legal pairs:
    #
    #   (0,1), ..., (0,9)
    #
    # are the 9 pairs for which x = 0.
    #
    # Thus:
    #
    #   P(x = 0 | x < y) = 9 / 45 = 0.2
    #
    expected = 1.0 - 0.8 ** steps
    assert_close(
        probability(
            output,
            "nonzero_x",
        ),
        expected=expected,
        tolerance=0.05,
    )


@pytest.mark.parametrize(
    "steps",
    [
        (1),
        (2),
    ],
)
def test_rejection_sampling_subrange(steps):
    output = run_smc(
        "subrange_assumes.lus",
        runs=2000,
        steps=steps,
    )

    generated, accepted, rejected = sample_counts(output)

    assert accepted == 2000

    assert generated == accepted + rejected

    # x,y are uniform in [0,9].
    #
    # There are 100 possible pairs and 45 satisfying x < y.
    #
    # Therefore:
    #
    #   P(accepted) = 45 / 100 = 0.45
    #
    acceptance_probability = (
        accepted / generated
    )

    expected = 0.45 ** steps

    assert_close(
        acceptance_probability,
        expected=expected,
        tolerance=0.05,
    )

    # Among the 45 legal pairs:
    #
    #   (0,1), ..., (0,9)
    #
    # are the 9 pairs for which x = 0.
    #
    # Thus:
    #
    #   P(x = 0 | x < y) = 9 / 45 = 0.2
    #
    expected = 1.0 - 0.8 ** steps
    assert_close(
        probability(
            output,
            "nonzero_x",
        ),
        expected=expected,
        tolerance=0.05,
    )


@pytest.mark.parametrize(
    "steps",
    [
        (1),
        (2),
        (3),
        (5),
    ],

)
def test_any_is_solver_completed(steps):
    expected = 1.0 - 2.0 ** (-steps)
    output = run_smc(
        "any.lus",
        runs=2000,
        steps=steps,
    )

    # The random input x determines which constraint the solver must
    # satisfy for the `any` expression. The `any` value itself is not
    # sampled probabilistically by SMC.
    assert_close(
        probability(
            output,
            "any_follows_x",
        ),
        expected=expected,
        tolerance=0.05,
    )

# ---------------------------------------------------------------------------
# Estimators regression tests
# ---------------------------------------------------------------------------

def test_fixed_estimator_confidence():
    runs = 1000
    epsilon = 0.05
    output = run_smc(
        "boolean.lus",
        runs=runs,
        steps=1,
        params=["--smc_precision", str(epsilon)],
    )

    expected = hoeffding(runs, epsilon)

    assert_close(
        confidence(output),
        expected,
        tolerance=1e-4
    )
    assert_close(
        precision(output),
        epsilon,
        tolerance=1e-12
    )

def test_fixed_estimator_vacuous_bound():
    runs = 1000
    epsilon = 0.01

    output = run_smc(
        "boolean.lus",
        runs=runs,
        steps=1,
        params=["--smc_precision", str(epsilon)],
    )

    assert confidence(output) == 0.0

def test_apmc_mode():
    output = run_smc(
       "boolean.lus",
        runs=1,# unused
        steps=1,
        params=["--smc_precision", str(0.05), "--smc_estimator", "apmc", "--smc_confidence", str(0.95)]
    )

    generated, accepted, rejected = sample_counts(output)

    assert accepted == 738

    assert_close(
        probability(output, "never_x"),
        expected=0.5,
        tolerance=0.02,
    )

# ---------------------------------------------------------------------------
# Input distributions regression tests
# ---------------------------------------------------------------------------

def test_input_distributions(tmp_path):
    input_file = tmp_path / "distributions.json"
    input_file.write_text(
        """
{
  "b": {
    "distribution": "bernoulli",
    "p": 0.2
  },
  "i": {
    "distribution": "uniform_int",
    "min": "0",
    "max": "9"
  },
  "r": {
    "distribution": "uniform_real",
    "min": "-1.0",
    "max": "1.0"
  }
}
"""
    )

    output = run_smc(
        "distributions.lus",
        params=[
            "--smc_input", str(input_file),
            "--smc_seed", "42",
        ],
        runs=2000,
        steps=1,
    )

    # b ~ Bernoulli(0.2)
    # Property fails iff b = true.
    assert_close(
        probability(output, "bernoulli"),
        expected=0.2,
        tolerance=0.04,
    )

    # i ~ Uniform([0,9])
    # Property fails iff i = 0.
    assert_close(
        probability(output, "uniform_int"),
        expected=0.1,
        tolerance=0.04,
    )

    # r ~ Uniform([-1,1])
    # Property fails iff r < 0.
    assert_close(
        probability(output, "uniform_real"),
        expected=0.5,
        tolerance=0.05,
    )


def test_fixed_and_uniform_input(tmp_path):
    input_file = tmp_path / "fixed_uniform.json"
    input_file.write_text(
        """
{
  "fixed": true,
  "random": {
    "distribution": "uniform"
  }
}
"""
    )

    output = run_smc(
        "fixed_uniform.lus",
        params=[
            "--smc_input", str(input_file),
            "--smc_seed", "42",
        ],
        runs=2000,
        steps=1,
    )

    # Fixed to true, so `not fixed` always fails.
    assert probability(output, "fixed") == 1.0

    # Uniform Boolean input.
    assert_close(
        probability(output, "uniform"),
        expected=0.5,
        tolerance=0.05,
    )

def test_sprt_boundaries_and_early_stopping(tmp_path):
    threshold = 0.10
    delta = 0.02
    alpha = 0.05
    beta = 0.05

    input_file = tmp_path / "sprt_fixed.json"

    input_file.write_text(
"""
{
  "low_fault": false,
  "high_fault": true
}
"""
    )

    output = run_smc(
        "sprt.lus",
        runs=1000,
        steps=1,
        params=[
            "--smc_estimator", "sprt",
            "--smc_threshold", str(threshold),
            "--smc_delta", str(delta),
            "--smc_alpha", str(alpha),
            "--smc_beta", str(beta),
            "--smc_input", str(input_file),
        ],
    )

    (p_low, p_high, lower_bound, upper_bound) = sprt_parameters(threshold, delta, alpha, beta)

    expected_low_samples = (
        sprt_constant_stopping_samples(
            violation=False,
            threshold=threshold,
            delta=delta,
            alpha=alpha,
            beta=beta,
        )
    )

    expected_high_samples = (
        sprt_constant_stopping_samples(
            violation=True,
            threshold=threshold,
            delta=delta,
            alpha=alpha,
            beta=beta,
        )
    )

    # With these parameters:
    #
    #   H_low  : P(violation) <= 0.08
    #   H_high : P(violation) >= 0.12
    #
    # An always-violated property reaches H_high after 8 samples.
    # A never-violated property reaches H_low after 67 samples.
    assert expected_high_samples == 8
    assert expected_low_samples == 67

    low_violations, low_samples = (
        property_violation_counts(
            output,
            "low_probability",
        )
    )

    high_violations, high_samples = (
        property_violation_counts(
            output,
            "high_probability",
        )
    )

    # low_fault is fixed to false:
    # low_probability is never violated.
    assert low_violations == 0
    assert low_samples == expected_low_samples

    # high_fault is fixed to true:
    # high_probability is violated on every sample.
    assert high_violations == high_samples
    assert high_samples == expected_high_samples

    # The two estimators must stop independently.
    #
    # In particular, high_probability must stop receiving observations
    # after sample 8 even though the global SMC loop continues until
    # low_probability terminates at sample 67.
    assert high_samples < low_samples

    assert sprt_decision(
        output,
        "low_probability",
    ) == f"P(violation) <= {p_low:g}"

    assert sprt_decision(
        output,
        "high_probability",
    ) == f"P(violation) >= {p_high:g}"

    # Check that the reported likelihood ratios actually crossed
    # the expected Wald boundaries.
    assert (
        sprt_log_lr(
            output,
            "low_probability",
        )
        <= lower_bound
    )

    assert (
        sprt_log_lr(
            output,
            "high_probability",
        )
        >= upper_bound
    )

    generated, accepted, rejected = (
        sample_counts(output)
    )

    # The global run continues until the slowest property estimator
    # terminates.
    assert accepted == expected_low_samples
    assert generated == accepted
    assert rejected == 0

def test_sprt_inconclusive_at_max_runs(tmp_path):
    threshold = 0.10
    delta = 0.02
    alpha = 0.05
    beta = 0.05

    # Five samples are insufficient to reach either boundary:
    #
    #   always violated: upper boundary needs 8
    #   never violated : lower boundary needs 67
    #
    max_runs = 5

    input_file = tmp_path / "sprt_fixed.json"

    input_file.write_text(
"""
{
  "low_fault": false,
  "high_fault": true
}
"""
    )

    output = run_smc(
        "sprt.lus",
        runs=max_runs,
        steps=1,
        params=[
            "--smc_estimator", "sprt",
            "--smc_threshold", str(threshold),
            "--smc_delta", str(delta),
            "--smc_alpha", str(alpha),
            "--smc_beta", str(beta),
            "--smc_input", str(input_file),
        ],
    )

    low_violations, low_samples = (
        property_violation_counts(
            output,
            "low_probability",
        )
    )

    high_violations, high_samples = (
        property_violation_counts(
            output,
            "high_probability",
        )
    )

    assert low_samples == max_runs
    assert high_samples == max_runs

    assert low_violations == 0
    assert high_violations == max_runs

    assert sprt_decision(output, "low_probability", ) == "Inconclusive"

    assert sprt_decision(output, "high_probability", ) == "Inconclusive"

    (_p_low, _p_high, lower_bound, upper_bound,) = sprt_parameters(threshold, delta, alpha, beta,)

    low_lr = sprt_log_lr(output, "low_probability", )

    high_lr = sprt_log_lr(output, "high_probability", )

    # Neither test has crossed a decision boundary.
    assert lower_bound < low_lr < upper_bound
    assert lower_bound < high_lr < upper_bound

    generated, accepted, rejected = (
        sample_counts(output)
    )

    assert generated == max_runs
    assert accepted == max_runs
    assert rejected == 0
