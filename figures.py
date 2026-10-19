#!/usr/bin/env python3
"""Figures S2-S4. Run after descriptive.R and sensitivity.R."""

import argparse
import csv
import hashlib
import json
import os
from pathlib import Path

import matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt
import matplotlib.colors as mcolors
from matplotlib import font_manager
from matplotlib.lines import Line2D
from matplotlib.ticker import PercentFormatter
import numpy as np
import pandas as pd
from PIL import Image

ROOT = Path(__file__).resolve().parent
RESULTS = Path(os.environ.get("VE_RESULTS_DIR", ROOT / "Results"))


def register_arial():
    for path in Path("/usr/share/fonts/truetype/msttcorefonts").glob("*[Aa]rial*.ttf"):
        font_manager.fontManager.addfont(path)


def save_figure(fig, stem, tight=False):
    stem.parent.mkdir(parents=True, exist_ok=True)
    for extension in ("pdf", "png", "tiff"):
        options = {"pil_kwargs": {"compression": "tiff_lzw"}} if extension == "tiff" else {}
        fig.savefig(stem.with_suffix("." + extension), dpi=600,
                    bbox_inches="tight" if tight else None, facecolor="white", **options)
    with Image.open(stem.with_suffix(".tiff")) as image:
        image.convert("RGB").save(str(stem.with_suffix(".tiff")), compression="tiff_lzw", dpi=(600, 600))
    plt.close(fig)


def figure_s2():
    NOS = ROOT / "Data" / "Supplementary Data_Characteristics and Quality score.xlsx"
    TND = ROOT / "Data" / "case_control_test_negative_classification.csv"
    OUT = RESULTS / "FigureS2" / "Figure S2"

    for font_file in (
        "/usr/share/fonts/truetype/msttcorefonts/Arial.ttf",
        "/usr/share/fonts/truetype/msttcorefonts/Arial_Bold.ttf",
    ):
        if Path(font_file).exists():
            font_manager.fontManager.addfont(font_file)

    ITEMS = ["S1", "S2", "S3", "S4", "C", "E1", "E2", "E3"]
    CC_LABELS = [
        "S1  Case definition",
        "S2  Representative cases",
        "S3  Control selection",
        "S4  Control definition",
        "C1  Comparability",
        "E1  Exposure ascertainment",
        "E2  Same ascertainment method",
        "E3  Non-response rate",
    ]
    COHORT_LABELS = [
        "S1  Exposed cohort",
        "S2  Non-exposed cohort",
        "S3  Exposure ascertainment",
        "S4  Outcome absent at baseline",
        "C1  Comparability",
        "O1  Outcome assessment",
        "O2  Follow-up duration",
        "O3  Follow-up completeness",
    ]
    CATEGORIES = ("Maximum stars", "One of two stars", "No star")
    COLORS = {"Maximum stars": "#90D4C9", "One of two stars": "#FFCB8F", "No star": "#E3DED3"}


    def load_groups():
        cols = ["id", "study", *ITEMS, "total"]
        cc = pd.read_excel(NOS, "NOS case-control", header=None, skiprows=2, names=cols).dropna(subset=["id"])
        co = pd.read_excel(NOS, "NOS cohort", header=None, skiprows=2, names=cols).dropna(subset=["id"])
        cc["id"] = cc["id"].astype(int)
        tnd = pd.read_csv(TND)[["article_id", "test_negative_design"]]
        cc = cc.merge(tnd, left_on="id", right_on="article_id", how="left", validate="one_to_one")
        assert cc["test_negative_design"].notna().all()
        for df in (cc, co):
            vals = df[ITEMS]
            assert vals.isin([0, 1, 2]).all().all()
            assert (vals.drop(columns="C") <= 1).all().all()
            has_total = df["total"].notna()
            assert (vals.sum(axis=1)[has_total] == df.loc[has_total, "total"]).all()
            df["score"] = vals.sum(axis=1)
        groups = [
            ("Test-negative case–control studies", cc[cc["test_negative_design"] == "yes"], CC_LABELS),
            ("Other case–control studies", cc[cc["test_negative_design"] == "no"], CC_LABELS),
            ("Cohort studies", co, COHORT_LABELS),
        ]
        assert [len(g[1]) for g in groups] == [305, 18, 26]
        return groups


    def item_counts(df, item):
        v = df[item]
        if item == "C":
            return {"Maximum stars": (v == 2).sum(), "One of two stars": (v == 1).sum(), "No star": (v == 0).sum()}
        return {"Maximum stars": (v == 1).sum(), "One of two stars": 0, "No star": (v == 0).sum()}


    def draw():
        plt.rcParams.update({"font.family": "Arial", "font.size": 10, "axes.labelsize": 11,
                             "axes.titlesize": 11, "axes.linewidth": 0.95, "pdf.fonttype": 42})
        groups = load_groups()
        fig, axes = plt.subplots(4, 1, figsize=(183 / 25.4, 240 / 25.4), layout="constrained",
                                  gridspec_kw={"height_ratios": (1, 1, 1, 0.5)})
        rows = []

        for i, (ax, (name, df, labels)) in enumerate(zip(axes[:3], groups)):
            n = len(df)
            ge7 = int((df["score"] >= 7).sum())
            y = range(len(ITEMS))
            left = [0.0] * len(ITEMS)
            counts = [item_counts(df, it) for it in ITEMS]
            for cat in CATEGORIES:
                pct = [c[cat] * 100.0 / n for c in counts]
                bars = ax.barh(y, pct, left=left, height=0.64, color=COLORS[cat],
                               edgecolor="white", linewidth=0.5, label=cat, zorder=3)
                for bar, p in zip(bars, pct):
                    if p >= 8:
                        ax.text(bar.get_x() + bar.get_width() / 2, bar.get_y() + bar.get_height() / 2,
                                f"{p:.0f}%", ha="center", va="center", color="#111111",
                                fontsize=10, zorder=4)
                left = [a + b for a, b in zip(left, pct)]
                for it, c, p in zip(labels, counts, pct):
                    rows.append([name, n, it, cat, int(c[cat]), f"{p:.4f}"])

            for boundary in (3.5, 4.5):
                ax.axhline(boundary, color="#8C8C8C", lw=0.85 * 0.7, zorder=2)
            ax.set_yticks(list(y), labels)
            ax.invert_yaxis()
            ax.set_xlim(0, 100)
            ax.set_xticks(range(0, 101, 25))
            ax.tick_params(axis="y", length=0)
            ax.set_title(f"{name} (n = {n}; NOS ≥7: {ge7}/{n})", loc="left", fontweight="bold")
            ax.spines[["top", "right"]].set_visible(False)
            ax.spines["left"].set_visible(False)
            if i < 2:
                ax.tick_params(axis="x", labelbottom=False)
            else:
                ax.set_xlabel("Studies (%)")

        # Overall score distribution across all designs.
        scores = pd.concat([g[1]["score"] for g in groups])
        n_all = len(scores)
        score_range = list(range(int(scores.min()), 10))
        score_counts = [int((scores == s).sum()) for s in score_range]
        ax = axes[3]
        score_pct = [c * 100.0 / n_all for c in score_counts]
        ax.bar(score_range, score_pct, width=0.65, color=COLORS["Maximum stars"], zorder=3)
        for x, c, p in zip(score_range, score_counts, score_pct):
            ax.text(x, p + 1.5, f"{c} ({p:.1f}%)", ha="center", va="bottom",
                    fontsize=10)
        ax.axvline(6.5, color="#8C8C8C", lw=0.85, ls="--", zorder=2)
        ax.set_xticks(score_range)
        ax.set_xlim(score_range[0] - 0.5, 9.5)
        ax.set_ylim(0, 55)
        ax.set_yticks([0, 25, 50])
        ax.set_xlabel("NOS score (0–9)")
        ax.set_ylabel("Studies (%)")
        ax.set_title(f"Overall NOS score (all studies, n = {n_all}; NOS ≥7: "
                     f"{int((scores >= 7).sum())}/{n_all})", loc="left", fontweight="bold")
        ax.spines[["top", "right"]].set_visible(False)
        for s_, c, p in zip(score_range, score_counts, score_pct):
            rows.append(["All studies", n_all, "Overall NOS score", f"{s_} points", c, f"{p:.4f}"])

        handles, lbls = axes[0].get_legend_handles_labels()
        fig.legend(handles, lbls, loc="outside upper center", ncols=3, frameon=False)
        save_figure(fig, OUT)

        with open(OUT.parent / "FigureS2_source_data.csv", "w", newline="", encoding="utf-8-sig") as fh:
            w = csv.writer(fh)
            w.writerow(["design", "n_studies", "item", "rating", "count", "percent"])
            w.writerows(rows)



    draw()


def figure_s3():
    OUT_DIR = RESULTS / "FigureS3"
    SOURCE = OUT_DIR / "FigureS3_plot_data.csv"
    STEM = OUT_DIR / "Figure S3"

    ORDER = ["Overall", "0-17 years", "A/H3N2", "IBV", "B/Victoria"]
    DISPLAY = {
        "0-17 years": "6 mo–17 y",
        "A/H3N2": "A(H3N2)",
    }
    FONT_SIZE = 11


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

        save_figure(fig, STEM, tight=True)


    main()


def figure_s4():
    SOURCE = RESULTS / 'TableS19/covariate_data.csv'
    PROVENANCE = SOURCE.parent / 'review_data.md5'
    WORKBOOK = ROOT / 'Data/review_data.xlsx'
    OUT = RESULTS / 'FigureS4'
    OUT.mkdir(parents=True, exist_ok=True)
    workbook_md5 = hashlib.md5(WORKBOOK.read_bytes()).hexdigest()
    if workbook_md5 != PROVENANCE.read_text().strip():
        raise RuntimeError('review_data.xlsx changed; rerun descriptive.R first.')
    data = pd.read_csv(SOURCE)
    assert len(data) == 8440
    periods = ['Pre-pandemic', 'Pandemic/Post-Pandemic']
    totals = data.groupby('period').size().reindex(periods)
    assert totals.tolist() == [6346, 2094]

    # Reuse Figure 3's blue/green/purple and the NOS figure's orange/neutral.
    blue, teal, orange, purple, gray = (
        '#3D5A91', '#008F7A', '#FFCB8F', '#A66A91', '#E3DED3')
    # WHO region colours also used in maps.R.
    who_colors = dict(AMRO='#E3D2E7', EURO='#C5DFED', WPRO='#FCCECA',
                      SEARO='#D3D4E8', AFRO='#FFFAD3', EMRO='#DBF1D6')
    panels = [
        ('Age group', 'age_display', [
            ('All ages', 'All ages', blue), ('0-17 years', '6 months–17 years', teal),
            ('18-64 years', '18–64 years', orange), ('≥65 years', '≥65 years', purple),
            ('Mixed age groups', 'Mixed age groups', gray)]),
        ('Vaccine type', 'vaccine_type_display', [
            ('Trivalent', 'Trivalent', blue), ('Quadrivalent', 'Quadrivalent', teal),
            ('Other', 'Other', orange), ('Unknown', 'Unknown', gray)]),
        ('Influenza type', 'pathogen_display', [
            ('IAV', 'Influenza A', blue), ('IBV', 'Influenza B', purple),
            ('Unknown', 'Unspecified', gray)]),
        ('WHO region', 'region_short', [
            ('AMRO', 'AMRO', who_colors['AMRO']), ('EURO', 'EURO', who_colors['EURO']),
            ('WPRO', 'WPRO', who_colors['WPRO']), ('SEARO', 'SEARO', who_colors['SEARO']),
            ('AFRO', 'AFRO', who_colors['AFRO']), ('EMRO', 'EMRO', who_colors['EMRO']),
            ('Multi-region', 'Multi-region', gray)]),
        ('Clinical outcome', 'outcome_display', [
            ('Outpatient', 'Outpatient', blue), ('Inpatient', 'Inpatient', teal),
            ('Severe and Death', 'Severe illness/death', purple), ('Unknown', 'Unknown', gray)])
    ]
    records = []
    for title, column, categories in panels:
        assert set(data[column].unique()) == {c[0] for c in categories}
        for category, label, color in categories:
            for period in periods:
                subset = data.loc[data[column].eq(category) & data.period.eq(period)]
                records.append(dict(covariate=title, variable=column, category=category,
                                    display_label=label, period=period,
                                    studies=subset.study_id.nunique(), estimates=len(subset),
                                    period_total_estimates=int(totals[period]),
                                    percent=100 * len(subset) / totals[period]))
    table = pd.DataFrame(records)
    assert np.allclose(table.groupby(['covariate', 'period']).percent.sum(), 100)
    table.to_csv(OUT / 'FigureS4_covariate_period_counts_percentages.csv', index=False, encoding='utf-8-sig')

    # Register Arial files that Matplotlib does not find by itself on Linux. Without
    # Arial, Matplotlib falls back to its default sans-serif font.
    for font_path in sorted(Path('/usr/share/fonts/truetype/msttcorefonts').glob('*[Aa]rial*.ttf')):
        font_manager.fontManager.addfont(font_path)
    plt.rcParams.update({'font.family': 'Arial', 'font.size': 10,
                         'pdf.fonttype': 42, 'ps.fonttype': 42, 'svg.fonttype': 'none',
                         'axes.linewidth': .5, 'text.color': '#292C31',
                         'axes.labelcolor': '#292C31', 'xtick.color': '#292C31',
                         'ytick.color': '#292C31'})
    # Figure 3 uses a 230-mm canvas; retain that width so all five panels are legible.
    fig, axes = plt.subplots(1, 5, figsize=(230/25.4, 115/25.4), sharey=True)
    fig.subplots_adjust(left=.073, right=.988, top=.90, bottom=.30, wspace=.48)
    for i, (ax, (title, column, categories)) in enumerate(zip(axes, panels)):
        bottom = np.zeros(2)
        for category, label, color in categories:
            rows = table.loc[table.variable.eq(column) & table.category.eq(category)].set_index('period')
            values = rows.loc[periods, 'percent'].to_numpy()
            ax.bar([0, 1], values, width=.70, bottom=bottom, color=color,
                   edgecolor='white', linewidth=.45, label=label, zorder=3)
            r, g, b = mcolors.to_rgb(color)
            text_color = 'white' if .2126*r + .7152*g + .0722*b < .57 else '#20252A'
            for x, height, base in zip([0, 1], values, bottom):
                if height >= 5:
                    ax.text(x, base + height/2, f'{height:.1f}%', ha='center', va='center',
                            fontsize=8.5, color=text_color, zorder=4)
            bottom += values
        ax.set_title(title, loc='center', fontsize=10, fontweight='bold', pad=10)
        ax.set_ylim(0, 100)
        ax.set_xlim(-.6, 1.6)
        ax.set_xticks([0, 1], ['Pre-P', 'P/Post-P'], fontsize=8.5)
        ax.set_yticks([0, 25, 50, 75, 100])
        ax.yaxis.set_major_formatter(PercentFormatter(100, decimals=0))
        ax.tick_params(axis='x', direction='out', length=3.2, width=.6, pad=4,
                       bottom=True, top=False)
        ax.tick_params(axis='y', direction='out', length=3.2, width=.6, pad=3,
                       labelsize=8.5, left=True, right=False, labelleft=True)
        for side in ['top', 'right']:
            ax.spines[side].set_visible(False)
        for side in ['left', 'bottom']:
            ax.spines[side].set_visible(True)
            ax.spines[side].set_color('#292C31')
            ax.spines[side].set_linewidth(.6)
            ax.spines[side].set_capstyle('projecting')
        ax.spines['left'].set_bounds(0, 100)
        handles, labels = ax.get_legend_handles_labels()
        # Center each legend below its own panel; wrap long category names.
        wraps = {'Mixed age groups': 'Mixed age\ngroups',
                 '6 months–17 years': '6 months–\n17 years',
                 'Multi-region': 'Multi-\nregion',
                 'Severe illness/death': 'Severe illness/\ndeath'}
        labels = [wraps.get(label, label) for label in labels]
        columns = 2 if column in ['age_display', 'region_short'] else 1
        if columns == 2:
            # Matplotlib fills columns first; reorder so readers follow rows in
            # the original bottom-to-top stack order.
            order = list(range(0, len(labels), 2)) + list(range(1, len(labels), 2))
            handles = [handles[j] for j in order]
            labels = [labels[j] for j in order]
        ax.legend(handles, labels, loc='upper center', bbox_to_anchor=(.5, -.135),
                  frameon=False, ncol=columns, fontsize=8, handlelength=.9,
                  handleheight=.8, labelspacing=.45, columnspacing=.8,
                  handletextpad=.45, borderaxespad=0)
    axes[0].set_ylabel('VE estimates (%)', fontsize=11, fontweight='bold', labelpad=7)
    # Left-align each panel letter with the left edge of its own y tick labels.
    fig.canvas.draw()
    renderer = fig.canvas.get_renderer()
    for i, ax in enumerate(axes):
        tick_x0 = min(label.get_window_extent(renderer).x0 for label in ax.get_yticklabels())
        x = ax.transAxes.inverted().transform((tick_x0, 0))[0]
        ax.text(x, 1.10, chr(97+i), transform=ax.transAxes,
                fontsize=11, fontweight='bold', ha='left', va='center')
    save_figure(fig, OUT / 'Figure S4')

    caption = '''Covariate composition of the overall analysis sample by study period. Panels show age group (a), vaccine type (b), influenza type (c), WHO region (d), and clinical outcome (e). Each bar represents 100% of the source vaccine-effectiveness (VE) estimates within the corresponding period: 6,346 estimates from 273 studies in 2010–2019 (Pre-P) and 2,094 estimates from 76 studies in 2020–2025 (P/Post-P). Percentages are unweighted proportions of source estimates before within-model aggregation, not inverse-variance weights, proportions of participants, or proportions of unique studies. One study may contribute estimates to multiple categories; study counts therefore need not sum across categories. Unknown/unspecified categories are retained. Nonzero segments below 5% are shown without numeric labels; exact counts and percentages, including zeros, are provided in the accompanying CSV. Influenza type follows the overall model coding (IAV, IBV, unspecified); B/Yamagata is not a separate level in this panel. This figure describes marginal covariate composition and does not establish joint common support or positivity. AMRO, Region of the Americas; EURO, European Region; WPRO, Western Pacific Region; SEARO, South-East Asia Region; AFRO, African Region; EMRO, Eastern Mediterranean Region.
    '''
    assert data.groupby('period').study_id.nunique().reindex(periods).tolist() == [273, 76]
    (OUT / 'FigureS4_caption.txt').write_text(caption)
    (OUT / 'FigureS4_provenance.json').write_text(json.dumps({
        'source': str(SOURCE), 'source_sha256': hashlib.sha256(SOURCE.read_bytes()).hexdigest(),
        'workbook_md5': workbook_md5, 'n_source_estimates': len(data),
        'period_counts': totals.to_dict(), 'denominator': 'unweighted pre-aggregation source effect estimates'
    }, indent=2))
    print(OUT)
    print(table.loc[table.covariate.eq('Vaccine type'), ['category', 'period', 'studies', 'estimates', 'percent']].to_string(index=False))


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("figures", nargs="*", metavar="S2|S3|S4")
    args = parser.parse_args()
    if any(name not in ["S2", "S3", "S4"] for name in args.figures):
        parser.error("Choose S2, S3 or S4.")
    register_arial()
    for name in args.figures or ["S2", "S3", "S4"]:
        with plt.rc_context():
            {"S2": figure_s2, "S3": figure_s3, "S4": figure_s4}[name]()
