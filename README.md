# Asymétrie d'Information & Efficience Informationnelle des Marchés Financiers

> Empirical study reproducing and extending the methodology of **El Bouazizi (2018)**,  
> applied to **4 international stock markets** over the period **2018–2025**.  
> Module: Modèles Mathématiques pour la Finance

---

##Research Question

Does **information asymmetry** (measured by the price range F_P)  
lead to **market inefficiency** (measured by abnormal returns RTA)?

**Model tested:**

$$RTA_t = \alpha + \beta \cdot F\_P_t + \varepsilon_t$$

If **β > 0 and statistically significant** → information asymmetry  
degrades informational efficiency. This is what El Bouazizi (2018)  
confirmed on the Casablanca Stock Exchange. We extend this to 4 markets.

---

## Markets & Sample

| Market | Index | Ticker | Type | Stocks |
|--------|-------|--------|------|--------|
| 🇺🇸 USA | S&P 500 | `^GSPC` | Developed | 10 |
| 🇫🇷 France | CAC 40 | `^FCHI` | Developed | 10 |
| 🇧🇷 Brazil | Bovespa | `^BVSP` | Emerging | 8 |
| 🇹🇷 Turkey | BIST 100 | `XU100.IS` | Emerging | 10 |

**Period:** January 2018 — December 2025  
**Source:** Yahoo Finance via `quantmod` (R)  
**Total:** 38 stocks across 4 markets, ~8,000 observations per variable

---

## Project Structure
asymetrie-information-marches-financiers/
│
├── data/
│   ├── usa/
│   │   └── raw_usa.RData
│   ├── france/
│   │   └── raw_france.RData
│   ├── bresil/
│   │   └── raw_bresil.RData
│   ├── turquie/
│   │   └── raw_turquie.RData
│   ├── market_models.RData       # OLS results — alpha, beta, R2
│   └── rta_fp.RData              # RTA and F_P variables
│
├── output/
│   └── tables/
│       ├── market_model_params.csv
│       ├── descriptive_stats.csv
│       ├── correlations.csv
│       ├── correlations_period.csv
│       └── full_dataset.csv
│
├── Markdown file for project Asymeterie Inf....R   # main script (all steps)
├── README.md
└── .gitignore

---

## Step 2 Results — Market Model OLS

**Model:** $R_{it} = \alpha_i + \beta_i \cdot R_{Mt} + \varepsilon_{it}$ — estimated by OLS (Sharpe, 1963)

| Market | Stocks | Significant | Avg Beta (β) | Avg R² |
|--------|--------|-------------|-------------|--------|
| 🇺🇸 USA | 10 | **10/10** | 0.9495 | 0.4178 |
| 🇫🇷 France | 10 | **10/10** | 1.0410 | 0.4753 |
| 🇧🇷 Brazil | 8 | **8/8** | 0.9147 | 0.3923 |
| 🇹🇷 Turkey | 10 | **10/10** | 1.0196 | 0.4650 |

All 38 models are significant at the **1% level** (Prob>F = 0.000).  
Brazil has the lowest R² (0.39) — consistent with higher firm-level noise in emerging markets.

---

## Step 3 Results — RTA & F_P

### Descriptive Statistics

| Market | Obs | Avg RTA | Sd RTA | Avg F_P | Sd F_P |
|--------|-----|---------|--------|---------|--------|
| 🇺🇸 USA | 2,009 | 0.009515 | 0.004980 | 3.6019 | 1.60 |
| 🇫🇷 France | 2,045 | 0.008721 | 0.004250 | 3.5293 | 1.55 |
| 🇧🇷 Brazil | 1,985 | 0.012157 | 0.006040 | 0.9791 | 0.35 |
| 🇹🇷 Turkey | 1,972 | 0.013450 | 0.005960 | 1.9480 | 2.01 |

Emerging markets (Brazil, Turkey) show **higher average RTA** — consistent  
with the theory that less efficient markets generate more abnormal returns.

### Correlation RTA ~ F_P by Market

| Market | Correlation (r) | P-value | Interpretation |
|--------|----------------|---------|----------------|
| 🇺🇸 USA | **0.524** | 0.000 | Moderate positive — developed market |
| 🇫🇷 France | **0.553** | 0.000 | Moderate positive — developed market |
| 🇧🇷 Brazil | **0.627** | 0.000 | Strong positive — emerging market |
| 🇹🇷 Turkey | **0.118** | 0.000 | Weak — explained by hyperinflation |

### Correlation by Sub-Period

| Market | P1: 2018-2019 | P2: 2020-2021 | P3: 2022-2025 |
|--------|--------------|--------------|--------------|
| 🇺🇸 USA | 0.539 | **0.691** | 0.469 |
| 🇫🇷 France | 0.518 | **0.710** | 0.497 |
| 🇧🇷 Brazil | 0.598 | 0.626 | 0.622 |
| 🇹🇷 Turkey | **0.658** | 0.554 | **0.038** |

**Key observations:**
- For USA and France, the COVID period (2020-2021) shows the **strongest correlation** — crisis amplifies information asymmetry, exactly as the article predicts
- Brazil is **consistently high** across all periods — persistent structural inefficiency typical of emerging markets
- Turkey's collapse to **r = 0.038** in 2022-2025 is explained by **hyperinflation** (inflation peaked at 85% in 2022): the price spread (F_P) exploded due to currency effects rather than information asymmetry, breaking the relationship temporarily

---

## How to Reproduce

### Requirements

```r
install.packages(c("quantmod", "tidyverse", "ggplot2",
                   "broom", "kableExtra", "gridExtra"))
```

### Run

Open the main R script and run all steps sequentially.  
Data is downloaded automatically from Yahoo Finance.

---

##Reference

> **El Bouazizi, N. (2018).** Analyse de la relation entre l'asymétrie d'information
> et l'efficience informationnelle des marchés financiers : Étude empirique sur
> les sociétés cotées à la Bourse de Casablanca.
> *Finance & Finance Internationale*, N°11.

- Fama, E.F. (1970). Efficient Capital Markets. *Journal of Finance*, 25(2).
- Grossman & Stiglitz (1980). On the Impossibility of Informationally Efficient Markets. *AER*, 70(3).
- Kyle, S.A. (1985). Continuous Auctions and Insider Trading. *Econometrica*, 53(6).
- Sharpe, W.F. (1963). A Simplified Model of Portfolio Analysis. *Management Science*, 9(2).

---

##Author

**Malek Dhaouadi**  
Module: Modèles Mathématiques pour la Finance — 2025/2026
