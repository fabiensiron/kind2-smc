import os
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
        rf"probability\s*=\s*"
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
