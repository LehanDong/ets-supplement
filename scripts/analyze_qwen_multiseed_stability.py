#!/usr/bin/env python3
"""Analyze Qwen pure-bridge stability across generation seeds."""

from __future__ import annotations

import argparse
import csv
import datetime as dt
import json
import math
import random
import statistics
import sys
from collections import Counter, defaultdict
from pathlib import Path
from typing import Any, Optional, Sequence


ROOT = Path(__file__).resolve().parents[1]
if str(ROOT) not in sys.path:
    sys.path.insert(0, str(ROOT))

from tools import analyze_evidentiary_threshold_signature as base  # noqa: E402


DEFAULT_RUN_ROOT = ROOT / "results/pure_bridge_15_qwen_llamacpp_runs"
DEFAULT_OUT_ROOT = ROOT / "results/qwen_multiseed_stability"
DEFAULT_SEEDS = (829, 830, 831)


def read_manifest(path: Path) -> dict:
    return json.loads(path.read_text(encoding="utf-8"))


def discover_runs(run_root: Path, seeds: Sequence[int]) -> dict[int, Path]:
    candidates: dict[int, list[Path]] = defaultdict(list)
    for directory in sorted(run_root.glob("*")):
        manifest_path = directory / "run_manifest.json"
        summary_path = directory / "summary.csv"
        if not manifest_path.exists() or not summary_path.exists():
            continue
        manifest = read_manifest(manifest_path)
        if manifest.get("smoke") or int(manifest.get("planned_calls", 0)) != 150:
            continue
        seed = manifest.get("generation_seed")
        if seed is None:
            continue
        rows = base.read_csv(summary_path)
        if len(rows) != 75 or any(row.get("status") != "ok" for row in rows):
            continue
        candidates[int(seed)].append(directory)
    return {seed: candidates[seed][-1] for seed in seeds if candidates.get(seed)}


def logsumexp_pair(a: float, b: float) -> float:
    high = max(a, b)
    return high + math.log(math.exp(a - high) + math.exp(b - high))


def add_non1(row: dict) -> dict:
    result = dict(row)
    for modality in ("natural", "probe"):
        lp0 = base.as_float(result.get(f"{modality}_logprob_0"))
        lp1 = base.as_float(result.get(f"{modality}_logprob_1"))
        lp9 = base.as_float(result.get(f"{modality}_logprob_9"))
        result[f"{modality}_margin_1_vs_non1"] = (
            lp1 - logsumexp_pair(lp0, lp9)
            if lp0 is not None and lp1 is not None and lp9 is not None
            else None
        )
    return result


def bootstrap_ci(values: Sequence[float], seed: int, n_boot: int = 20000) -> tuple[Optional[float], Optional[float]]:
    if not values:
        return None, None
    rng = random.Random(seed)
    n = len(values)
    means = []
    for _ in range(n_boot):
        means.append(sum(values[rng.randrange(n)] for _ in range(n)) / n)
    return base.percentile(means, 0.025), base.percentile(means, 0.975)


def paired_values(rows: Sequence[dict], modality: str, metric: str, high: str = "B4", low: str = "B1") -> list[float]:
    by_unit: dict[str, dict[str, dict]] = defaultdict(dict)
    for row in rows:
        by_unit[row["unit_id"]][row["level"]] = row
    col = f"{modality}_{metric}"
    values = []
    for levels in by_unit.values():
        if high not in levels or low not in levels:
            continue
        a = base.as_float(levels[high].get(col))
        b = base.as_float(levels[low].get(col))
        if a is not None and b is not None:
            values.append(a - b)
    return values


def summarize_seed(seed: int, run_dir: Path, rows: Sequence[dict]) -> dict:
    margin_19 = paired_values(rows, "probe", "margin_1_9")
    margin_non1 = paired_values(rows, "probe", "margin_1_vs_non1")
    ci19 = bootstrap_ci(margin_19, seed=seed)
    ci_non1 = bootstrap_ci(margin_non1, seed=seed + 1000)

    threshold_points = []
    for row in rows:
        label = base.as_int(row.get("probe_label"))
        if label in (0, 1, 9):
            threshold_points.append((base.LEVEL_STRENGTH[row["level"]], int(label == 1)))
    threshold = base.fit_binary_threshold(threshold_points)

    result: dict[str, Any] = {
        "generation_seed": seed,
        "run_dir": str(run_dir),
        "observations_n": len(rows),
        "natural_probe_agreement_n": sum(str(r.get("natural_probe_label_match", "")).lower() == "true" for r in rows),
        "natural_probe_agreement_rate": base.safe_div(
            sum(str(r.get("natural_probe_label_match", "")).lower() == "true" for r in rows), len(rows)
        ),
        "probe_zero_label_n": sum(r.get("probe_label") == "0" for r in rows),
        "probe_exact_margin_1_9_n": sum(base.as_float(r.get("probe_margin_1_9")) is not None for r in rows),
        "probe_exact_margin_1_vs_non1_n": sum(base.as_float(r.get("probe_margin_1_vs_non1")) is not None for r in rows),
        "probe_tau": threshold.get("tau"),
        "probe_k": threshold.get("k"),
        "probe_B4_minus_B1_margin_1_9_mean": base.mean(margin_19),
        "probe_B4_minus_B1_margin_1_9_ci_low": ci19[0],
        "probe_B4_minus_B1_margin_1_9_ci_high": ci19[1],
        "probe_B4_minus_B1_margin_1_9_positive_n": sum(value > 0 for value in margin_19),
        "probe_B4_minus_B1_margin_1_vs_non1_mean": base.mean(margin_non1),
        "probe_B4_minus_B1_margin_1_vs_non1_ci_low": ci_non1[0],
        "probe_B4_minus_B1_margin_1_vs_non1_ci_high": ci_non1[1],
        "probe_B4_minus_B1_margin_1_vs_non1_positive_n": sum(value > 0 for value in margin_non1),
    }
    for level in ("B0", "B1", "B2", "B3", "B4"):
        subset = [row for row in rows if row["level"] == level]
        result[f"probe_label_1_rate_{level}"] = base.safe_div(sum(r.get("probe_label") == "1" for r in subset), len(subset))
        result[f"probe_mean_margin_1_9_{level}"] = base.mean(
            [value for value in (base.as_float(r.get("probe_margin_1_9")) for r in subset) if value is not None]
        )
        result[f"probe_mean_margin_1_vs_non1_{level}"] = base.mean(
            [value for value in (base.as_float(r.get("probe_margin_1_vs_non1")) for r in subset) if value is not None]
        )
    anomaly = next((r for r in rows if r["unit_id"] == "UNIT_027" and r["level"] == "B4"), None)
    if anomaly:
        result["life013_f1_b4_natural_label"] = anomaly.get("natural_label")
        result["life013_f1_b4_probe_label"] = anomaly.get("probe_label")
        result["life013_f1_b4_probe_margin_1_9"] = anomaly.get("probe_margin_1_9")
        result["life013_f1_b4_probe_margin_1_vs_non1"] = anomaly.get("probe_margin_1_vs_non1")
    return result


def observation_table(rows_by_seed: dict[int, list[dict]]) -> list[dict]:
    lookup: dict[tuple[int, str, str], dict] = {}
    for seed, rows in rows_by_seed.items():
        for row in rows:
            lookup[(seed, row["unit_id"], row["level"])] = row
    keys = sorted({(unit_id, level) for _, unit_id, level in lookup})
    output = []
    for unit_id, level in keys:
        row: dict[str, Any] = {"unit_id": unit_id, "level": level}
        natural_labels, probe_labels = [], []
        natural_sufficiency, natural_bridge_status = [], []
        probe_margins_19, probe_margins_non1 = [], []
        for seed in sorted(rows_by_seed):
            source = lookup[(seed, unit_id, level)]
            natural_label = source.get("natural_label")
            probe_label = source.get("probe_label")
            sufficiency = source.get("natural_sufficiency")
            bridge_status = source.get("natural_evidence_bridge_status")
            margin19 = base.as_float(source.get("probe_margin_1_9"))
            margin_non1 = base.as_float(source.get("probe_margin_1_vs_non1"))
            row[f"natural_label_seed_{seed}"] = natural_label
            row[f"probe_label_seed_{seed}"] = probe_label
            row[f"natural_sufficiency_seed_{seed}"] = sufficiency
            row[f"natural_bridge_status_seed_{seed}"] = bridge_status
            row[f"probe_margin_1_9_seed_{seed}"] = margin19
            row[f"probe_margin_1_vs_non1_seed_{seed}"] = margin_non1
            natural_labels.append(natural_label)
            probe_labels.append(probe_label)
            natural_sufficiency.append(sufficiency)
            natural_bridge_status.append(bridge_status)
            if margin19 is not None:
                probe_margins_19.append(margin19)
            if margin_non1 is not None:
                probe_margins_non1.append(margin_non1)
        row["natural_label_all_seeds_match"] = len(set(natural_labels)) == 1
        row["probe_label_all_seeds_match"] = len(set(probe_labels)) == 1
        row["natural_sufficiency_all_seeds_match"] = len(set(natural_sufficiency)) == 1
        row["natural_bridge_status_all_seeds_match"] = len(set(natural_bridge_status)) == 1
        row["probe_margin_1_9_sd_across_seeds"] = statistics.pstdev(probe_margins_19) if len(probe_margins_19) > 1 else None
        row["probe_margin_1_vs_non1_sd_across_seeds"] = statistics.pstdev(probe_margins_non1) if len(probe_margins_non1) > 1 else None
        output.append(row)
    return output


def pairwise_agreement(rows_by_seed: dict[int, list[dict]]) -> list[dict]:
    seeds = sorted(rows_by_seed)
    output = []
    for index, seed_a in enumerate(seeds):
        lookup_a = {(r["unit_id"], r["level"]): r for r in rows_by_seed[seed_a]}
        for seed_b in seeds[index + 1 :]:
            lookup_b = {(r["unit_id"], r["level"]): r for r in rows_by_seed[seed_b]}
            for modality in ("natural", "probe"):
                labels_a, labels_b, margins_a, margins_b = [], [], [], []
                for key in sorted(set(lookup_a) & set(lookup_b)):
                    labels_a.append(lookup_a[key].get(f"{modality}_label"))
                    labels_b.append(lookup_b[key].get(f"{modality}_label"))
                    a = base.as_float(lookup_a[key].get(f"{modality}_margin_1_9"))
                    b = base.as_float(lookup_b[key].get(f"{modality}_margin_1_9"))
                    if a is not None and b is not None:
                        margins_a.append(a)
                        margins_b.append(b)
                output.append(
                    {
                        "seed_a": seed_a,
                        "seed_b": seed_b,
                        "modality": modality,
                        "label_agreement_n": sum(a == b for a, b in zip(labels_a, labels_b)),
                        "label_agreement_rate": base.safe_div(sum(a == b for a, b in zip(labels_a, labels_b)), len(labels_a)),
                        "margin_1_9_pearson_r": base.pearson(margins_a, margins_b),
                    }
                )
    return output


def write_report(path: Path, seeds: Sequence[int], summaries: Sequence[dict], observations: Sequence[dict]) -> None:
    lines = [
        "# Qwen Multi-Seed Stability",
        "",
        f"Generation seeds: {', '.join(str(seed) for seed in seeds)}",
        "",
        "| seed | agreement | probe tau | probe k | B4-B1 1-vs-9 | positive cases | B4 F1 natural/probe |",
        "|---:|---:|---:|---:|---:|---:|---|",
    ]
    for row in summaries:
        lines.append(
            f"| {row['generation_seed']} | {base.fmt(base.as_float(row.get('natural_probe_agreement_rate')))} | "
            f"{base.fmt(base.as_float(row.get('probe_tau')))} | {base.fmt(base.as_float(row.get('probe_k')))} | "
            f"{base.fmt(base.as_float(row.get('probe_B4_minus_B1_margin_1_9_mean')))} | "
            f"{row.get('probe_B4_minus_B1_margin_1_9_positive_n', '')}/15 | "
            f"{row.get('life013_f1_b4_natural_label', '')}/{row.get('life013_f1_b4_probe_label', '')} |"
        )
    all_natural = sum(str(row.get("natural_label_all_seeds_match", "")).lower() == "true" for row in observations)
    all_probe = sum(str(row.get("probe_label_all_seeds_match", "")).lower() == "true" for row in observations)
    all_sufficiency = sum(
        str(row.get("natural_sufficiency_all_seeds_match", "")).lower() == "true" for row in observations
    )
    all_bridge = sum(
        str(row.get("natural_bridge_status_all_seeds_match", "")).lower() == "true" for row in observations
    )
    lines.extend(
        [
            "",
            f"- Natural label identical across all seeds: {all_natural}/{len(observations)}.",
            f"- Probe label identical across all seeds: {all_probe}/{len(observations)}.",
            f"- Natural sufficiency identical across all seeds: {all_sufficiency}/{len(observations)}.",
            f"- Natural bridge status identical across all seeds: {all_bridge}/{len(observations)}.",
            "- Primary stability criterion: B4-B1 remains positive in all 15 cases for every seed.",
            "- Trace-field variation should be reported separately from decision-signature stability.",
            "",
        ]
    )
    path.write_text("\n".join(lines), encoding="utf-8")


def main() -> int:
    parser = argparse.ArgumentParser(description="分析 Qwen 829/830/831 generation seed 稳定性")
    parser.add_argument("--run-root", type=Path, default=DEFAULT_RUN_ROOT)
    parser.add_argument("--seeds", nargs="+", type=int, default=list(DEFAULT_SEEDS))
    parser.add_argument("--out-dir", type=Path, default=None)
    args = parser.parse_args()

    runs = discover_runs(args.run_root, args.seeds)
    missing = [seed for seed in args.seeds if seed not in runs]
    if missing:
        raise SystemExit("缺少完整 generation seed 结果：" + ", ".join(str(seed) for seed in missing))

    rows_by_seed: dict[int, list[dict]] = {}
    summaries = []
    for seed in args.seeds:
        rows = [add_non1(row) for row in base.read_csv(runs[seed] / "summary.csv")]
        rows_by_seed[seed] = rows
        summaries.append(summarize_seed(seed, runs[seed], rows))

    observations = observation_table(rows_by_seed)
    pairwise = pairwise_agreement(rows_by_seed)
    out_dir = args.out_dir or DEFAULT_OUT_ROOT / dt.datetime.now().strftime("%Y%m%d_%H%M%S")
    out_dir.mkdir(parents=True, exist_ok=False)
    base.write_csv(out_dir / "seed_summary.csv", summaries)
    base.write_csv(out_dir / "observation_stability.csv", observations)
    base.write_csv(out_dir / "pairwise_seed_agreement.csv", pairwise)
    write_report(out_dir / "README.md", args.seeds, summaries, observations)
    manifest = {
        "created_at": dt.datetime.now().isoformat(timespec="seconds"),
        "script": str(Path(__file__).resolve()),
        "run_dirs": {str(seed): str(runs[seed]) for seed in args.seeds},
        "out_dir": str(out_dir),
    }
    (out_dir / "run_manifest.json").write_text(json.dumps(manifest, ensure_ascii=False, indent=2), encoding="utf-8")
    print(f"done: {out_dir}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
