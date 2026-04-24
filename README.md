# Asymétrie d'Information et Efficience Informationnelle des Marchés Financiers

Extension de l'étude d'El Bouazizi (2018) à quatre marchés internationaux sur la période 2018-2025.

## Présentation du projet

Ce projet analyse la relation entre l'asymétrie d'information et l'inefficience informationnelle sur quatre marchés financiers:

- États-Unis (S&P 500)
- France (CAC 40)
- Brésil (Bovespa)
- Turquie (BIST 100)

L'idée centrale est de tester si une hausse du proxy d'asymétrie d'information s'accompagne d'une hausse de l'inefficience informationnelle.

Le rapport final est disponible dans [report_asymetrie_info.Rmd](report_asymetrie_info.Rmd).

## Question de recherche et hypothèse

Le modèle estimé est:

$$
RTA_t = \alpha + \beta \cdot F_{P,t} + \varepsilon_t
$$

avec:

- $RTA$: rentabilité anormale (proxy d'inefficience informationnelle)
- $F_P$: fourchette de prix quotidienne (proxy d'asymétrie d'information)

Hypothèse testée: **$\beta > 0$**.

## Données et périmètre

- **Source principale:** Yahoo Finance (via `quantmod`)
- **Fenêtre temporelle:** 2018-01-01 à 2025-12-31
- **Échantillon actions:**
  - USA: 10 titres
  - France: 10 titres
  - Brésil: 8 titres
  - Turquie: 10 titres

Les données brutes et objets intermédiaires sont stockés dans le dossier [data/](data).

## Méthodologie

Le workflow empirique se déroule en 4 étapes:

1. **Collecte des données de marché**
- Téléchargement des séries de prix (actions + indices de référence) depuis Yahoo Finance.
- Sauvegarde par marché dans des fichiers `.RData`.

2. **Estimation du modèle de marché (Sharpe, 1963)**
- Régression OLS titre par titre:
  $$
  R_{it} = \alpha_i + \beta_i \cdot R_{Mt} + \varepsilon_{it}
  $$
- Sauvegarde des paramètres (`alpha`, `beta`, `R2`, significativité).

3. **Construction des variables d'analyse**
- Calcul de `RTA`:
  - rendement observé moins rendement théorique issu du modèle de marché
  - valeur absolue agrégée au niveau marché
- Calcul de `F_P`:
  - fourchette quotidienne `High - Low`, agrégée au niveau marché
- Création d'un panel combiné et de sous-périodes:
  - P1: 2018-2019 (pré-COVID)
  - P2: 2020-2021 (COVID)
  - P3: 2022-2025 (post-COVID)

4. **Régressions et diagnostics**
- Régressions `RTA ~ F_P` sur période complète et sous-périodes.
- Diagnostics: autocorrélation, hétéroscédasticité (Spearman), normalité (Shapiro-Wilk).
- Production des figures et tables de synthèse.

Le script principal qui exécute le pipeline est [Markdown file for project Asymeterie Informationnelle.R](Markdown%20file%20for%20project%20Asymeterie%20Informationnelle.R).

## Résultats principaux

- Relation positive et significative entre `RTA` et `F_P` sur les marchés étudiés.
- Effet plus marqué sur le Brésil.
- Amplification de la relation durant la période COVID (2020-2021) pour plusieurs marchés.
- Rupture nette en Turquie après 2022 (effondrement de la corrélation), interprétée comme un effet de régime macroéconomique (hyperinflation et forte instabilité monétaire).

## Structure du dépôt

```text
asymetrie-information-marches-financiers/
|-- asymetrie-information-marches-financiers.Rproj
|-- Markdown file for project Asymeterie Informationnelle.R
|-- report_asymetrie_info.Rmd
|-- article-scientifique.pdf
|-- data/
|   |-- market_models.RData
|   |-- regression_results.RData
|   |-- rta_fp.RData
|   |-- bresil/raw_bresil.RData
|   |-- france/raw_france.RData
|   |-- turquie/raw_turquie.RData
|   |-- usa/raw_usa.RData
|-- output/
|   |-- figures/
|   |   |-- figure1_*.png
|   |   |-- figure2_*.png
|   |   |-- figure3_beta_coefficients.png
|   |   |-- figure4_subperiod_beta.png
|   |   |-- figure5_*.png
|   |   |-- figure6_turkey_anomaly.png
|   |-- tables/
|       |-- descriptive_stats.csv
|       |-- correlations.csv
|       |-- correlations_period.csv
|       |-- market_model_params.csv
|       |-- regression_full_period.csv
|       |-- regression_subperiod.csv
|       |-- regression_diagnostics.csv
|       |-- full_dataset.csv
```

## Prérequis

- R (>= 4.2 recommandé)
- Packages R:
  - `quantmod`
  - `tidyverse`
  - `broom`
  - `dplyr`
  - `ggplot2`
  - `knitr`
  - `kableExtra`
  - `rmarkdown`

Installation rapide:

```r
install.packages(c(
  "quantmod", "tidyverse", "broom", "dplyr", "ggplot2",
  "knitr", "kableExtra", "rmarkdown"
))
```

## Reproduire l'analyse

### 1) Exécuter le pipeline de calcul

Depuis RStudio ou une session R:

```r
source("Markdown file for project Asymeterie Informationnelle.R")
```

Cette étape:

- télécharge les données
- estime les modèles
- calcule `RTA` et `F_P`
- génère les résultats dans [output/tables/](output/tables) et [output/figures/](output/figures)
- met à jour les objets `.RData` dans [data/](data)

### 2) Générer le rapport final

```r
rmarkdown::render("report_asymetrie_info.Rmd")
```

Le document HTML généré présente les tableaux, graphiques et interprétations.

## Fichiers clés

- [report_asymetrie_info.Rmd](report_asymetrie_info.Rmd): rapport scientifique complet.
- [Markdown file for project Asymeterie Informationnelle.R](Markdown%20file%20for%20project%20Asymeterie%20Informationnelle.R): script de construction de la base analytique et des résultats.
- [output/tables/](output/tables): export CSV des résultats statistiques et économétriques.
- [output/figures/](output/figures): visualisations finales.

## Interprétation académique

Le projet confirme globalement la logique de la microstructure de marché: lorsque l'asymétrie d'information augmente (proxée par la fourchette de prix), l'inefficience informationnelle augmente également (proxée par la rentabilité anormale). Les contrastes entre marchés développés et émergents, ainsi que l'anomalie turque post-2022, soulignent l'importance du contexte macroéconomique dans l'interprétation des proxys.

## Références théoriques principales

- El Bouazizi, N. (2018). Analyse de la relation entre l'asymétrie d'information et l'efficience informationnelle des marchés financiers.
- Fama, E. F. (1970). Efficient Capital Markets.
- Grossman, S. J., & Stiglitz, J. E. (1980). On the Impossibility of Informationally Efficient Markets.
- Kyle, A. S. (1985). Continuous Auctions and Insider Trading.
- Sharpe, W. F. (1963). A Simplified Model of Portfolio Analysis.

## Contact

Pour toute question académique ou méthodologique concernant ce projet, contacter l'auteur: **Malek Dhaouadi**.
mail: malek.dhaouadi@esprit.tn
