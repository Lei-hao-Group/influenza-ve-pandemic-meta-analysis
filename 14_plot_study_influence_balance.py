#!/usr/bin/env python3
"""Plot complete leave-one-study-out and study-balance sensitivity results."""

from __future__ import annotations

from pathlib import Path

import matplotlib.pyplot as plt
import numpy as np
import pandas as pd
from matplotlib import font_manager
from matplotlib.lines import Line2D


ROOT = Path(__file__).resolve().parents[1]
OUT_DIR = ROOT / "Additional_Analyses" / "32_study_influence_balance_summary"
SOURCE = OUT_DIR / "study_influence_balance_plot_data.csv"
STEM = OUT_DIR / "complete_leave_one_study_and_balance_plot"

ORDER = ["Overall", "0-17 years", "A/H3N2", "IBV", "B/Victoria"]
DISPLAY = {
    "0-17 years": "0–17 years",
    "A/H3N2": "A(H3N2)",
}
FONT_SIZE = 11


def register_arial() -> None:
    """Register the installed Microsoft Arial files with Matplotlib."""
    font_dir = Path("/usr/share/fonts/truetype/msttcorefonts")
    font_paths = sorted(font_dir.glob("*[Aa]rial*.ttf"))
    if not font_paths:
        raise RuntimeError(f"Arial font files were not found in {font_dir}")
    for font_path in font_paths:
        font_manager.fontManager.addfont(font_path)


def main() -> None:
    register_arial()
    data = pd.read_csv(SOURCE)
    data["Subgroup"] = pd.Categorical(data["Subgroup"], ORDER, ordered=True)
    data = data.sort_values("Subgroup").reset_index(drop=True)
    if data["Subgroup"].astype(str).tolist() != ORDER:
        raise RuntimeError("Unexpected subgroup content/order in plot data")

    plt.rcParams.update(
        {
            "font.family": "Arial",
            "font.size": FONT_SIZE,
            "axes.linewidth": 0.8,
            "pdf.fonttype": 42,
            "ps.fonttype": 42,
        }
    )
    fig, ax = plt.subplots(figsize=(7.25, 4.15))
    y = np.arange(len(data))[::-1]

    for index, row in data.iterrows():
        yi = y[index]
        if index % 2 == 1:
            ax.axhspan(yi - 0.48, yi + 0.48, color="#f5f7f9", zorder=0)

        # Envelope of all deletion-specific 95% CI limits. This is not a new CI.
        ax.hlines(
            yi,
            row["loo_ci_lower_min"],
            row["loo_ci_upper_max"],
            color="#a9cce3",
            linewidth=6.0,
            alpha=0.78,
            zorder=1,
        )
        ax.hlines(
            yi,
            row["loo_effect_min"],
            row["loo_effect_max"],
            color="#1f77b4",
            linewidth=2.6,
            zorder=2,
        )
        ax.plot(
            [row["loo_effect_min"], row["loo_effect_max"]],
            [yi, yi],
            linestyle="none",
            marker="|",
            markersize=8,
            markeredgewidth=1.5,
            color="#1f77b4",
            zorder=3,
        )

        ax.errorbar(
            row["primary_effect"],
            yi + 0.17,
            xerr=np.array(
                [[row["primary_effect"] - row["primary_lower"]],
                 [row["primary_upper"] - row["primary_effect"]]]
            ),
            fmt="o",
            color="#202020",
            ecolor="#202020",
            markersize=4.5,
            elinewidth=1.15,
            capsize=2.5,
            zorder=5,
        )
        ax.errorbar(
            row["balanced_effect"],
            yi - 0.17,
            xerr=np.array(
                [[row["balanced_effect"] - row["balanced_lower"]],
                 [row["balanced_upper"] - row["balanced_effect"]]]
            ),
            fmt="D",
            color="#b03a2e",
            ecolor="#b03a2e",
            markersize=4.2,
            elinewidth=1.15,
            capsize=2.5,
            zorder=5,
        )
        ax.text(
            1.015,
            yi,
            f"{int(row['loo_ci_excludes_null_n'])}/{int(row['studies_omitted'])}",
            transform=ax.get_yaxis_transform(),
            va="center",
            ha="left",
            fontsize=FONT_SIZE,
            color="#333333",
            clip_on=False,
        )

    low = min(data["loo_ci_lower_min"].min(), data["balanced_lower"].min())
    high = max(data["loo_ci_upper_max"].max(), data["balanced_upper"].max())
    padding = max(4.0, 0.06 * (high - low))
    ax.set_xlim(np.floor((low - padding) / 5) * 5, np.ceil((high + padding) / 5) * 5)
    ax.axvline(0, color="#666666", linestyle=(0, (3, 2)), linewidth=0.9, zorder=0)
    ax.set_yticks(y)
    ax.set_yticklabels([DISPLAY.get(value, value) for value in ORDER])
    ax.set_ylim(-0.65, len(data) - 0.35)
    ax.set_xlabel(
        "Adjusted relative period effect, %",
        fontsize=FONT_SIZE,
        fontweight="bold",
    )
    ax.text(
        1.015,
        1.02,
        "95% CIs excluding 0, n/N",
        transform=ax.transAxes,
        ha="left",
        va="bottom",
        fontsize=FONT_SIZE,
        fontweight="bold",
        clip_on=False,
    )

    handles = [
        Line2D([0], [0], marker="o", color="#202020", lw=1.15, markersize=4.5,
               label="Primary estimate (95% CI)"),
        Line2D([0], [0], color="#1f77b4", lw=2.6,
               label="Leave-one-study-out estimate range"),
        Line2D([0], [0], color="#a9cce3", lw=6.0,
               label="Envelope of leave-one-study-out 95% CI limits"),
        Line2D([0], [0], marker="D", color="#b03a2e", lw=1.15, markersize=4.2,
               label="Study-contribution-balanced estimate (95% CI)"),
    ]
    ax.legend(
        handles=handles,
        loc="upper center",
        bbox_to_anchor=(0.5, -0.18),
        ncol=2,
        frameon=False,
        fontsize=FONT_SIZE,
        handlelength=2.7,
        columnspacing=1.6,
    )
    ax.spines["top"].set_visible(False)
    ax.spines["right"].set_visible(False)
    ax.grid(axis="x", color="#dddddd", linewidth=0.55, zorder=0)
    ax.set_axisbelow(True)
    fig.subplots_adjust(left=0.16, right=0.82, top=0.94, bottom=0.29)

    outputs = {
        "png": {"dpi": 600},
        "pdf": {},
        "tiff": {"dpi": 600, "pil_kwargs": {"compression": "tiff_lzw"}},
    }
    for extension, kwargs in outputs.items():
        path = STEM.with_suffix(f".{extension}")
        fig.savefig(path, bbox_inches="tight", facecolor="white", **kwargs)
        print(f"Wrote {path}")
    plt.close(fig)

if __name__ == "__main__":
    main()
