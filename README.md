# Asymétrie d'Information & Efficience Informationnelle des Marchés Financiers

> Empirical study reproducing and extending the methodology of **El Bouazizi (2018)**,  
> applied to **4 international stock markets** over the period **2018–2025**.

---

## Project Overview

This project examines the relationship between **information asymmetry** and **informational efficiency** across four financial markets of varying development levels:

| Market | Index | Benchmark Ticker | Type |
|--------|-------|-----------------|------|
| 🇺🇸 USA | S&P 500 | `^GSPC` | Developed |
| 🇫🇷 France | CAC 40 | `^FCHI` | Developed |
| 🇧🇷 Brazil | Bovespa | `^BVSP` | Emerging |
| 🇹🇷 Turkey | BIST 100 | `XU100.IS` | Emerging |

**Research question:**  
Does a wider bid-ask spread (information asymmetry) lead to higher abnormal returns (market inefficiency)?

**Main model tested:**
$$RTA_t = \alpha + \beta \cdot F\_P_t + \varepsilon_t$$

If **β > 0 and significant** → information asymmetry degrades market efficiency.

---

## Progress

- [x] **Step 1** — Data downloaded from Yahoo Finance
- [x] **Step 2** — Market model estimated by OLS
- [ ] **Step 3** — Compute RTA and bid-ask spread
- [ ] **Step 4** — Linear regression RTA ~ F_P
- [ ] **Step 5** — Write R Markdown report
- [ ] **Step 6** — Build presentation

---

## Step 2 Results — Market Model (OLS)

Estimated model per stock: **Rit = αi + βi · RMt + εit**  
All 38 models are statistically significant at the 1% level.

| Market | Stocks | Significant | Avg Beta (β) | Avg R² |
|--------|--------|-------------|-------------|--------|
| USA 🇺🇸 | 10 | 10/10 | 0.9495 | 0.4178 |
| France 🇫🇷 | 10 | 10/10 | 1.0410 | 0.4753 |
| Brazil 🇧🇷 | 8 | 8/8 | 0.9147 | 0.3923 |
| Turkey 🇹🇷 | 10 | 10/10 | 1.0196 | 0.4650 |
| **Total** | **38** | **38/38** | — | — |

**Key observations:**
- France has the highest avg beta (1.041), meaning CAC 40 stocks are slightly more sensitive to market movements than others
- Brazil has the lowest R² (0.39), suggesting more stock-specific noise — consistent with an emerging market where firm-level information asymmetry is higher
- All markets show R² between 0.39 and 0.48, which is typical for daily return data

---

## How to Reproduce

### Requirements

- R ≥ 4.0
- RStudio

### Install packages

```r
install.packages(c("quantmod", "tidyverse", "ggplot2", 
                   "broom", "kableExtra", "gridExtra"))
```

### Run in order

```r
source("scripts/01_import_data.R")   # ~2 min (downloads data)
source("scripts/02_market_model.R")  # ~30 sec
source("scripts/03_rta_fp.R")        # coming soon
source("scripts/04_regression.R")    # coming soon
```

Then open `rapport.Rmd` and press **Knit** (`Ctrl + Shift + K`).

---

##  Reference

> El Bouazizi, N. (2018). *Analyse de la relation entre l'asymétrie d'information  
> et l'efficience informationnelle des marchés financiers : Étude empirique sur  
> les sociétés cotées à la Bourse de Casablanca.*  
> Finance & Finance Internationale, N°11, juillet 2018.

**Other key references:**
- Fama, E.F. (1970). Efficient Capital Markets. *Journal of Finance*, 25(2).
- Grossman & Stiglitz (1980). On the Impossibility of Informationally Efficient Markets. *AER*, 70(3).
- Kyle, S.A. (1985). Continuous Auctions and Insider Trading. *Econometrica*, 53(6).
- Sharpe, W.F. (1963). A Simplified Model of Portfolio Analysis. *Management Science*, 9(2).

---

## Author

**Malek**  
Module: Modèles Mathématiques pour la Finance  
Academic Year: 2025–2026