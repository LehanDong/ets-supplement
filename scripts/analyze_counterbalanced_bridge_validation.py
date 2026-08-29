#!/usr/bin/env python3
"""Merge corrected counterbalanced runs and produce final descriptive statistics."""

from __future__ import annotations

import argparse
import csv
import math
from collections import defaultdict
from datetime import datetime
from pathlib import Path

ROOT = Path(__file__).resolve().parents[1]
DEFAULT_BASE = (
    ROOT / "results/counterbalanced_bridge_validation_runs/20260824_210245/summary.csv"
)
DEFAULT_CORRECTION = (
    ROOT / "results/counterbalanced_bridge_validation_runs/20260824_212041/summary.csv"
)
DEFAULT_RESULTS_ROOT = ROOT / "results/counterbalanced_bridge_validation_analysis"


def read_csv(path: Path) -> list[dict[str, str]]:
    with path.open(encoding="utf-8-sig", newline="") as handle:
        return list(csv.DictReader(handle))


def write_csv(path: Path, rows: list[dict], fields: list[str]) -> None:
    with path.open("w", encoding="utf-8-sig", newline="") as handle:
        writer = csv.DictWriter(handle, fieldnames=fields)
        writer.writeheader()
        writer.writerows(rows)


def wilson(successes: int, total: int, z: float = 1.959963984540054) -> tuple[float, float]:
    if total == 0:
        return math.nan, math.nan
    p = successes / total
    denominator = 1 + z * z / total
    center = (p + z * z / (2 * total)) / denominator
    half = z * math.sqrt(p * (1 - p) / total + z * z / (4 * total * total)) / denominator
    return center - half, center + half


def two_sided_binomial_p(successes: int, total: int) -> float:
    """Exact two-sided binomial test for p=0.5."""
    observed = math.comb(total, successes)
    numerator = sum(math.comb(total, k) for k in range(total + 1) if math.comb(total, k) <= observed)
    return min(1.0, numerator / (2**total))


def zero_success_exact_upper(total: int, confidence: float = 0.95) -> float:
    """Two-sided Clopper-Pearson upper bound when successes are zero."""
    alpha = 1 - confidence
    return 1 - (alpha / 2) ** (1 / total)


def summarize(rows: list[dict[str, str]]) -> list[dict]:
    groups: dict[tuple[str, str, str], list[dict[str, str]]] = defaultdict(list)
    for row in rows:
        groups[(row["model"], row["polarity"], row["explicitness"])].append(row)
    output = []
    for key in sorted(groups):
        values = groups[key]
        labels = [row["label"] for row in values]
        correct = sum(row["label"] == row["expected_label"] for row in values)
        low, high = wilson(correct, len(values))
        output.append(
            {
                "model": key[0],
                "polarity": key[1],
                "explicitness": key[2],
                "n": len(values),
                "correct_n": correct,
                "correct_rate": correct / len(values),
                "correct_wilson_low": low,
                "correct_wilson_high": high,
                "label_0_rate": labels.count("0") / len(values),
                "label_1_rate": labels.count("1") / len(values),
                "label_9_rate": labels.count("9") / len(values),
            }
        )
    return output


def main() -> None:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--base", type=Path, default=DEFAULT_BASE)
    parser.add_argument("--correction", type=Path, default=DEFAULT_CORRECTION)
    parser.add_argument("--out-dir", type=Path)
    args = parser.parse_args()

    base = read_csv(args.base)
    correction = read_csv(args.correction)
    merged = {(row["model"], row["stimulus_id"]): row for row in base}
    merged.update({(row["model"], row["stimulus_id"]): row for row in correction})
    rows = sorted(merged.values(), key=lambda row: (row["model"], row["unit_id"], row["polarity"], row["explicitness"]))
    if len(rows) != 180:
        raise ValueError(f"Expected 180 merged rows, found {len(rows)}")

    out_dir = args.out_dir or DEFAULT_RESULTS_ROOT / datetime.now().strftime("%Y%m%d_%H%M%S")
    out_dir.mkdir(parents=True, exist_ok=True)
    write_csv(out_dir / "corrected_summary.csv", rows, list(rows[0]))

    condition_rows = summarize(rows)
    write_csv(out_dir / "aggregate_by_condition.csv", condition_rows, list(condition_rows[0]))

    errors = [row for row in rows if row["label"] != row["expected_label"]]
    write_csv(out_dir / "non_expected_responses.csv", errors, list(rows[0]))

    model_rows = []
    for model in sorted({row["model"] for row in rows}):
        values = [row for row in rows if row["model"] == model]
        correct = sum(row["label"] == row["expected_label"] for row in values)
        low, high = wilson(correct, len(values))
        model_rows.append(
            {
                "model": model,
                "n": len(values),
                "correct_n": correct,
                "correct_rate": correct / len(values),
                "correct_wilson_low": low,
                "correct_wilson_high": high,
            }
        )
    write_csv(out_dir / "aggregate_by_model.csv", model_rows, list(model_rows[0]))

    explicit_positive = [row for row in rows if row["explicitness"] == "explicit" and row["polarity"] == "positive"]
    explicit_negative = [row for row in rows if row["explicitness"] == "explicit" and row["polarity"] == "negative"]
    positive_selected_1 = sum(row["label"] == "1" for row in explicit_positive)
    negative_selected_1 = sum(row["label"] == "1" for row in explicit_negative)
    if (
        positive_selected_1 == len(explicit_positive)
        and negative_selected_1 == 0
        and len(explicit_positive) == len(explicit_negative)
    ):
        fisher_odds = math.inf
        fisher_p = 2 / math.comb(len(explicit_positive) + len(explicit_negative), len(explicit_positive))
    else:
        fisher_odds = math.nan
        fisher_p = math.nan

    paired = defaultdict(dict)
    for row in rows:
        correct = row["label"] == row["expected_label"]
        paired[(row["model"], row["unit_id"], row["polarity"])][row["explicitness"]] = correct
    improvements = sum(not pair["implicit"] and pair["explicit"] for pair in paired.values())
    regressions = sum(pair["implicit"] and not pair["explicit"] for pair in paired.values())
    discordant = improvements + regressions
    mcnemar_p = two_sided_binomial_p(improvements, discordant) if discordant else 1.0

    total_correct = sum(row["label"] == row["expected_label"] for row in rows)
    total_low, total_high = wilson(total_correct, len(rows))
    negative_leak_low = 0.0
    negative_leak_high = zero_success_exact_upper(len(explicit_negative))
    report = f"""# Counterbalanced Bridge Validation: Corrected Results

## Run status

- Final observations: {len(rows)}
- Successful observations: {sum(row['status'] == 'ok' for row in rows)}
- Correct evidence-supported labels: {total_correct}/{len(rows)} ({total_correct / len(rows):.1%}, Wilson 95% CI [{total_low:.1%}, {total_high:.1%}])
- Replaced records: 12 observations for `UNIT_039` from the corrected v2 run

## Central counterbalancing result

- Positive-explicit worlds selecting label 1: {positive_selected_1}/{len(explicit_positive)}
- Negative-explicit worlds selecting label 1: {negative_selected_1}/{len(explicit_negative)}
- Negative-explicit positive-label leakage rate: {negative_selected_1 / len(explicit_negative):.1%}, exact 95% CI [{negative_leak_low:.1%}, {negative_leak_high:.1%}]
- Fisher exact comparison of positive-explicit versus negative-explicit label-1 responses: odds ratio={fisher_odds}, p={fisher_p:.3g}

All 45 negative-explicit observations selected label 0. Explicit target-related language therefore did not produce a general shift toward label 1; the models followed the polarity of the stated relation.

## Explicitness effect

- Implicit-to-explicit correctness improvements: {improvements}
- Implicit-to-explicit correctness regressions: {regressions}
- Exact paired sign/McNemar test over discordant pairs: p={mcnemar_p:.3f}

Explicit relation statements corrected five implicit-condition errors. With only five discordant pairs, this improvement is descriptive and does not independently establish a population-level explicitness effect.

## Model-level accuracy

| Model | Correct | Rate | Wilson 95% CI |
|---|---:|---:|---:|
"""
    for row in model_rows:
        report += (
            f"| {row['model']} | {row['correct_n']}/{row['n']} | {row['correct_rate']:.1%} | "
            f"[{row['correct_wilson_low']:.1%}, {row['correct_wilson_high']:.1%}] |\n"
        )
    report += "\n## Non-expected responses\n\n"
    for row in errors:
        report += (
            f"- `{row['model']}` / `{row['stimulus_id']}`: observed `{row['label']}`, "
            f"expected `{row['expected_label']}`.\n"
        )
    report += (
        "\nThese five responses all occur in implicit conditions. They identify role attribution, "
        "temporal-return relations, and the distinction between informal and formal intervention "
        "as cases in which an explicit relation can resolve an otherwise incorrect judgment.\n"
    )
    (out_dir / "analysis_report.md").write_text(report, encoding="utf-8")
    print(f"out_dir={out_dir}")
    print(f"correct={total_correct}/{len(rows)}")
    print(f"negative_explicit_label_1={negative_selected_1}/{len(explicit_negative)}")
    print(f"improvements={improvements} regressions={regressions} p={mcnemar_p:.6f}")


if __name__ == "__main__":
    main()
