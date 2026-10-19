#!/usr/bin/env python3
"""Table S4: candidate VE estimates excluded from the synthesis, by reason,
period, influenza type/subtype and WHO region.

Candidate estimates are the rows of the "Review data" sheet plus the rows of
the "Cleaning log" sheet that were not exact duplicates. Each excluded estimate
is assigned a single reason:

- VE or its upper 95% confidence limit >= 100%;
- non-finite reconstructed variance (no boundary value, but the variance of the
  log effect ratio cannot be computed from the reported confidence limits);
- no extractable 95% confidence interval;
- internally inconsistent point estimate and confidence interval.

Influenza type follows the Pathogen column and subtype/lineage follows the
Pathogen_specific column, as in Table 1.
"""

from __future__ import annotations

import os
import math
import re
from pathlib import Path
from statistics import NormalDist

import pandas as pd

ROOT = Path(__file__).resolve().parent
SOURCE = ROOT / "Data" / "review_data.xlsx"
OUT_DIR = Path(os.environ.get("VE_RESULTS_DIR", ROOT / "Results")) / "TableS4"

NUMBER = r"[-+]?(?:\d+(?:\.\d*)?|\.\d+)(?:[Ee][+-]?\d+)?"
BOUNDARY = "VE or upper 95% CL >=100%"
NON_FINITE = "Non-finite variance"
MISSING_CI = "Missing CI"
INCONSISTENT = "Inconsistent VE and CI"
INCLUDED = "Included"
REASONS = [BOUNDARY, NON_FINITE, MISSING_CI, INCONSISTENT]

REGIONS = {
    "African Region (AFRO)": "AFRO",
    "Region of the Americas (AMRO)": "AMRO",
    "Eastern Mediterranean Region (EMRO)": "EMRO",
    "European Region (EURO)": "EURO",
    "South-East Asia Region (SEARO)": "SEARO",
    "Western Pacific Region (WPRO)": "WPRO",
    "Multi-region": "Multi-region",
}
TYPES = {"IAV": "IAV", "IBV": "IBV", "No distinction": "Unknown"}
SUBTYPES = {
    "A(H1N1)": "A(H1N1)",
    "A(pH1N1)": "A(H1N1)",
    "A(H3N2)": "A(H3N2)",
    "B(Yamagata)": "B/Yamagata",
    "B(Victoria)": "B/Victoria",
}
PERIODS = {"pre": "Pre-pandemic", "pan/post": "Pandemic/post-pandemic"}


def normalise_ci(text) -> str:
    ci = str(text).replace("−", "-").replace("–", "-").replace("—", "-")
    ci = re.sub(r"(?<=\d),(?=\d)", "", ci)
    # A hyphen following a number or percent sign is a range separator, not
    # the sign of the upper VE bound.
    return re.sub(r"(?<=[0-9%])\s*-\s*(?=[0-9])", " to ", ci)


def review_reason(row) -> str:
    """Reason for a row of the Review data sheet."""
    if row["Use in logOR synthesis"] == "Yes":
        return INCLUDED
    ci = normalise_ci(row["CI"])
    ve = float(row["VE"]) * 100
    # Infinite confidence limits are kept when testing the boundary condition.
    tokens = [float(x) for x in re.findall(r"[-+]?inf(?:inity)?|" + NUMBER, ci, flags=re.I)]
    upper = tokens[-1] if len(tokens) >= 3 else math.nan
    if ve >= 100 or upper >= 100:
        return BOUNDARY
    # Variance reconstructed as in functions.R.
    finite = [float(x) for x in re.findall(NUMBER, ci)]
    lower, upper = finite[-2:] if len(finite) >= 3 else (math.nan, math.nan)
    ratio_lower, ratio_upper = 1 - upper / 100, 1 - lower / 100
    variance = math.nan
    if ratio_lower > 0 and ratio_upper > 0:
        variance = (
            (math.log(ratio_upper) - math.log(ratio_lower))
            / (2 * NormalDist().inv_cdf(0.975))
        ) ** 2
    if not math.isfinite(variance) or variance <= 0:
        return NON_FINITE
    assert "internally inconsistent" in str(row["Source audit note"]), row
    return INCONSISTENT


def cleaning_reason(action: str) -> str | None:
    """Reason for a row of the Cleaning log sheet; exact duplicates return None."""
    if action == "Removed: exact duplicate":
        return None
    if action == "Removed: CI completely missing":
        return MISSING_CI
    if action == "Removed: excluded from the systematic review (no valid adjusted logOR)":
        return BOUNDARY
    raise ValueError(action)


def load_candidates() -> pd.DataFrame:
    columns = ["Pathogen", "Pathogen_specific", "WHO Region", "Epi"]
    review = pd.read_excel(SOURCE, sheet_name="Review data")
    review["reason"] = review.apply(review_reason, axis=1)
    cleaning = pd.read_excel(SOURCE, sheet_name="Cleaning log")
    cleaning["reason"] = cleaning["Action"].map(cleaning_reason)
    boundary_log = cleaning[cleaning["reason"].eq(BOUNDARY)]
    for ci in boundary_log["CI"]:
        upper = [float(x) for x in re.findall(NUMBER, normalise_ci(ci))][-1]
        assert upper >= 100, ci
    cleaning = cleaning[cleaning["reason"].notna()]
    candidates = pd.concat(
        [review[columns + ["reason"]], cleaning[columns + ["reason"]]],
        ignore_index=True,
    )
    for column in columns:
        candidates[column] = candidates[column].map(
            lambda x: x.strip() if isinstance(x, str) else x
        )
    candidates["period"] = candidates["Epi"].map(PERIODS)
    candidates["type"] = candidates["Pathogen"].map(TYPES)
    candidates["subtype"] = candidates["Pathogen_specific"].map(SUBTYPES)
    candidates["region"] = candidates["WHO Region"].map(REGIONS)
    assert candidates[["period", "type", "region"]].notna().all().all()
    return candidates


def summarise(data: pd.DataFrame, section: str, group: str) -> dict:
    counts = {reason: int(data["reason"].eq(reason).sum()) for reason in REASONS}
    total = len(data)
    numerical = counts[BOUNDARY] + counts[NON_FINITE]
    excluded = sum(counts.values())
    return {
        "Section": section,
        "Group": group,
        "Candidate estimates": total,
        "VE or upper 95% CL >=100%": counts[BOUNDARY],
        "Non-finite variance": counts[NON_FINITE],
        "Numerical exclusions": numerical,
        "Numerical exclusions, %": 100 * numerical / total if total else math.nan,
        "Missing CI": counts[MISSING_CI],
        "Inconsistent VE and CI": counts[INCONSISTENT],
        "All exclusions": excluded,
        "All exclusions, %": 100 * excluded / total if total else math.nan,
        "Included": int(data["reason"].eq(INCLUDED).sum()),
    }


def main() -> None:
    candidates = load_candidates()
    rows = [summarise(candidates, "All candidate estimates", "All candidate estimates")]
    for period in PERIODS.values():
        rows.append(summarise(candidates[candidates["period"].eq(period)], "Period", period))
    for group, column in [
        ("IAV", "type"), ("A(H1N1)", "subtype"), ("A(H3N2)", "subtype"),
        ("IBV", "type"), ("B/Yamagata", "subtype"), ("B/Victoria", "subtype"),
        ("Unknown", "type"),
    ]:
        rows.append(
            summarise(candidates[candidates[column].eq(group)], "Influenza type/subtype", group)
        )
    for region in ["AFRO", "AMRO", "EMRO", "EURO", "SEARO", "WPRO", "Multi-region"]:
        rows.append(summarise(candidates[candidates["region"].eq(region)], "WHO region", region))
    table = pd.DataFrame(rows)

    overall = table.iloc[0]
    assert overall["Candidate estimates"] - overall["All exclusions"] == overall["Included"]
    for section in ("Period", "WHO region"):
        part = table[table["Section"].eq(section)]
        assert part["Candidate estimates"].sum() == overall["Candidate estimates"], section
        assert part["All exclusions"].sum() == overall["All exclusions"], section

    OUT_DIR.mkdir(parents=True, exist_ok=True)
    out = OUT_DIR / "TableS4_excluded_estimates_by_reason.csv"
    table.to_csv(out, index=False, encoding="utf-8-sig")
    print(table.to_string(index=False))
    print(out)


if __name__ == "__main__":
    main()
