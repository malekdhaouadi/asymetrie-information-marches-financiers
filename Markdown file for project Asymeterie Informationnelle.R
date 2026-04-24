# importation de données depuis Yahoo Finance
cat("Markets : USA | France | Brazil | Turkey\n")
install.packages(c("quantmod", "tidyverse"))
library(quantmod)
library(tidyverse)
library(broom)

date_start <- "2018-01-01"
date_end <- "2025-12-31"

#USA S&P 500 benchmark: ^GSPC
# World's most liquid and efficient market
tickers_usa <- c("AAPL", "MSFT", "JPM", "JNJ", "XOM",  "KO",   "BA",  "GS", "PG", "AMZN")

# FRANCE CAC 40 benchmark: ^FCHI 
# Developed European market
tickers_fra <- c("MC.PA",  "TTE.PA", "SAN.PA",
                 "BNP.PA", "AIR.PA", "OR.PA",
                 "SU.PA",  "DG.PA",  "RI.PA",
                 "CAP.PA")

# BRAZIL BOVESPA BENCHMARK: ^BVSP
# Emerging market — Latin America
#Medium Expected efficiency
tickers_bra <- c("PETR4.SA", "VALE3.SA", "ITUB4.SA",
                 "BBDC4.SA", "ABEV3.SA", "WEGE3.SA",
                 "SUZB3.SA", "RENT3.SA")

#TURKEY BIST 100 benchmark: XU100.IS
#Emerging market
# High inflation, high volatility = strong information asymmetry
tickers_tur <- c("GARAN.IS",  # Garanti Bank
                 "AKBNK.IS",  # Akbank
                 "EREGL.IS",  # Eregli Steel
                 "BIMAS.IS",  # BIM (retail)
                 "THYAO.IS",  # Turkish Airlines
                 "ASELS.IS",  # Aselsan (defense)
                 "KCHOL.IS",  # Koc Holding
                 "SISE.IS",   # Sisecam (glass)
                 "TUPRS.IS",  # Tupras (oil refinery)
                 "FROTO.IS")  # Ford Otosan


getSymbols(tickers_usa,
           src        = "yahoo",
           from       = date_start,
           to         = date_end,
           auto.assign = TRUE)

getSymbols("^GSPC",
           src        = "yahoo",
           from       = date_start,
           to         = date_end,
           auto.assign = TRUE)

getSymbols(tickers_fra,
           src        = "yahoo",
           from       = date_start,
           to         = date_end,
           auto.assign = TRUE)

getSymbols("^FCHI",
           src        = "yahoo",
           from       = date_start,
           to         = date_end,
           auto.assign = TRUE)

getSymbols(tickers_bra,
           src        = "yahoo",
           from       = date_start,
           to         = date_end,
           auto.assign = TRUE)

getSymbols("^BVSP",
           src        = "yahoo",
           from       = date_start,
           to         = date_end,
           auto.assign = TRUE)

getSymbols(tickers_tur,
           src         = "yahoo",
           from        = date_start,
           to          = date_end,
           auto.assign = TRUE)

getSymbols("XU100.IS",
           src         = "yahoo",
           from        = date_start,
           to          = date_end,
           auto.assign = TRUE)


#CHECK
cat("S&P 500 (USA):\n");   print(head(GSPC, 3))
cat("CAC 40 (France):\n"); print(head(FCHI, 3))
cat("Bovespa (Brazil):\n");print(head(BVSP, 3))
cat("BIST 100 (Turkey): last date =", 
    as.character(tail(index(XU100.IS), 1)), "\n")
#creation des dossiers si ça n'existe pas
dir.create("data/usa",    showWarnings = FALSE, recursive = TRUE)
dir.create("data/france", showWarnings = FALSE, recursive = TRUE)
dir.create("data/bresil", showWarnings = FALSE, recursive = TRUE)
dir.create("data/turquie",showWarnings = FALSE, recursive = TRUE)



save(AAPL, MSFT, JPM, JNJ, XOM, KO, BA, GS, PG, AMZN, GSPC,
     file = "data/usa/raw_usa.RData")

save(MC.PA, TTE.PA, SAN.PA, BNP.PA, AIR.PA,
     OR.PA,  SU.PA,  DG.PA,  RI.PA,  CAP.PA, FCHI,
     file = "data/france/raw_france.RData")

save(PETR4.SA, VALE3.SA, ITUB4.SA, BBDC4.SA,
     ABEV3.SA,  WEGE3.SA, SUZB3.SA, RENT3.SA, BVSP,
     file = "data/bresil/raw_bresil.RData")

save(GARAN.IS, AKBNK.IS, EREGL.IS, BIMAS.IS, THYAO.IS,
     ASELS.IS, KCHOL.IS, SISE.IS,  TUPRS.IS, FROTO.IS,
     XU100.IS,
     file = "data/turquie/raw_turquie.RData")



#Etape 2: Market Model Estimation

load("data/usa/raw_usa.RData")
load("data/france/raw_france.RData")
load("data/bresil/raw_bresil.RData")
load("data/turquie/raw_turquie.RData")

# Market model estimation (OLS) for all 4 markets
# Sharpe (1963): Rit = alpha_i + beta_i * RMt + epsilon_it

#Fonctionnement: ça prend une liste de tout les ticker objects + market index
# ça retourne une liste avec model, alpha, beta, R2, probF, data

estimate_market_model <- function(tickers, index_obj, market_name) {
  cat("estimating: ", market_name, "\n")
  
  Rmt <- na.omit(diff(log(Ad(index_obj))))
  colnames(Rmt) <- "Rmt"
  
  results <- list()
  for (tk in tickers) {
    #prend srock object from env 
    stock_obj <- get(tk)
    Rit <- na.omit(diff(log(Ad(stock_obj))))
    colnames(Rit) <- "Rit"
    df <- merge(Rit, Rmt) |> as.data.frame() |> na.omit()
    
    # OLS regression: Rit = alpha + beta * Rmt + epsilon
    model <- lm(Rit ~ Rmt, data = df)
    s     <- summary(model)
    
    #F test p-value
    f     <- s$fstatistic
    probF <- pf(f[1], f[2], f[3], lower.tail = FALSE)
    
    
    results[[tk]] <- list(
      model  = model,
      alpha  = coef(model)[1],
      beta   = coef(model)[2],
      R2     = s$r.squared,
      probF  = probF,
      sig    = ifelse(probF < 0.01,  "*** (1%)",
                      ifelse(probF < 0.05,  "**  (5%)",
                             ifelse(probF < 0.10,  "*   (10%)",
                                    "Not significant"))),
      data   = df,
      nobs   = nrow(df)
    )
    
    cat("  ", tk,
        "| alpha =", round(coef(model)[1], 6),
        "| beta =",  round(coef(model)[2], 4),
        "| R2 =",    round(s$r.squared, 4),
        "|", results[[tk]]$sig, "\n")
  }
  
  return(results)
}



#pour chaque marché
cat("Markets : USA | France | Brazil | Turkey\n")
res_usa <- estimate_market_model(tickers_usa, GSPC,     "USA — S&P 500")
res_fra <- estimate_market_model(tickers_fra, FCHI,     "France — CAC 40")
res_bra <- estimate_market_model(tickers_bra, BVSP,     "Brazil — Bovespa")
res_tur <- estimate_market_model(tickers_tur, XU100.IS, "Turkey — BIST 100")

#Table de sommaire
make_params_table <- function(res, market) {
  do.call(rbind, lapply(names(res), function(tk) {
    data.frame(
      Market      = market,
      Ticker      = tk,
      Alpha       = round(res[[tk]]$alpha, 6),
      Beta        = round(res[[tk]]$beta,  4),
      R2          = round(res[[tk]]$R2,    4),
      Prob_F      = round(res[[tk]]$probF, 4),
      Significant = res[[tk]]$sig,
      N_obs       = res[[tk]]$nobs,
      stringsAsFactors = FALSE
    )
  }))
}

params_all <- rbind(
  make_params_table(res_usa, "USA"),
  make_params_table(res_fra, "France"),
  make_params_table(res_bra, "Brazil"),
  make_params_table(res_tur, "Turkey")
)


print(params_all, row.names = FALSE)
dir.create("output/tables", showWarnings = FALSE, recursive = TRUE)

write.csv(params_all,
          "output/tables/market_model_params.csv",
          row.names = FALSE)

save(res_usa, res_fra, res_bra, res_tur,
     params_all,
     tickers_usa, tickers_fra, tickers_bra, tickers_tur,
     file = "data/market_models.RData")


#Résumé par marché

for (mkt in unique(params_all$Market)) {
  sub  <- params_all[params_all$Market == mkt, ]
  nsig <- sum(!grepl("Not significant", sub$Significant))
  cat(mkt, ":",
      nsig, "/", nrow(sub),
      "significant models |",
      "avg beta =", round(mean(sub$Beta), 4),
      "| avg R2 =",  round(mean(sub$R2),  4), "\n")
}


#étape 3
# Compute RTA (Abnormal Return) and F_P (Bid-Ask Spread)

#ABNORMAL RETURN (RTA)
# RTA = Observed Return - Theoretical Return
# Theoretical Return = alpha + beta * Market Return

compute_rta <- function(res, index_obj, market_name) {
  
  cat("\nComputing RTA for", market_name, "\n")
  # Market log-returns
  Rmt <- na.omit(diff(log(Ad(index_obj))))
  colnames(Rmt) <- "Rmt"
  Rmt_df <- data.frame(
    Date = as.Date(index(Rmt)),
    Rmt  = as.numeric(Rmt)
  )
  rta_per_stock <- list()
  
  for (tk in names(res)) {
    
    r     <- res[[tk]]
    alpha <- r$alpha
    beta  <- r$beta
    # Observed log-returns
    stock_obj <- get(tk)
    Rit <- na.omit(diff(log(Ad(stock_obj))))
    Rit_df <- data.frame(
      Date = as.Date(index(Rit)),
      Rit  = as.numeric(Rit)
    )
    
    # Merge observed returns with market returns
    df <- merge(Rit_df, Rmt_df, by = "Date") |> na.omit()
    
    # Theoretical return from market model
    df$RTT <- alpha + beta * df$Rmt
    
    # Abnormal return = absolute difference
    df$RTA <- abs(df$Rit - df$RTT)
    
    rta_per_stock[[tk]] <- df[, c("Date", "RTA")]
    cat("  ", tk, "— avg RTA =",
        round(mean(df$RTA, na.rm = TRUE), 6), "\n")
}
  
  # Average RTA across all stocks by date
  all_rta <- Reduce(function(a, b) merge(a, b, by = "Date"),
                    rta_per_stock)
  
  dates      <- all_rta$Date
  rta_values <- rowMeans(all_rta[, -1], na.rm = TRUE)
  
  result <- data.frame(Date = dates, RTA = rta_values)
  cat(market_name, "— total obs =", nrow(result),
      "| overall avg RTA =",
      round(mean(result$RTA, na.rm = TRUE), 6), "\n")
  
  return(result)
} 

#RTA pour chaque marché
rta_usa <- compute_rta(res_usa, GSPC,     "USA")
rta_fra <- compute_rta(res_fra, FCHI,     "France")
rta_bra <- compute_rta(res_bra, BVSP,     "Brazil")
rta_tur <- compute_rta(res_tur, XU100.IS, "Turkey")



#BID-ASK SPREAD / PRICE RANGE (F_P)
# F_P = Daily High - Daily Low
# Proxy for information asymmetry

compute_fp <- function(tickers, market_name) {
  
  cat("\nComputing F_P for", market_name, "...\n")
  
  fp_per_stock <- list()
  
  for (tk in tickers) {
    
    stock_obj <- get(tk)
    
    hi <- Hi(stock_obj)
    lo <- Lo(stock_obj)
    fp <- na.omit(hi - lo)
    
    fp_df <- data.frame(
      Date = as.Date(index(fp)),
      FP   = as.numeric(fp)
    )
    
    fp_per_stock[[tk]] <- fp_df
    cat("  ", tk, "— avg F_P =",
        round(mean(fp_df$FP, na.rm = TRUE), 4), "\n")
}
  
  # Average F_P across all stocks by date
  all_fp <- Reduce(function(a, b) merge(a, b, by = "Date"),
                   fp_per_stock)
  
  dates     <- all_fp$Date
  fp_values <- rowMeans(all_fp[, -1], na.rm = TRUE)
  
  result <- data.frame(Date = dates, FP = fp_values)
  cat(market_name, "— total obs =", nrow(result),
      "| overall avg F_P =",
      round(mean(result$FP, na.rm = TRUE), 4), "\n")
  
  return(result)
}

#F_P pour chaque marché
fp_usa <- compute_fp(tickers_usa, "USA")
fp_fra <- compute_fp(tickers_fra, "France")
fp_bra <- compute_fp(tickers_bra, "Brazil")
fp_tur <- compute_fp(tickers_tur, "Turkey")

#MERGE RTA + F_P INTO MODEL DATAFRAMES

make_model_df <- function(rta, fp, market) {
  df <- merge(rta, fp, by = "Date") |> na.omit()
  df$Market <- market
  # Add sub-period labels
  df$Period <- case_when(
    df$Date < as.Date("2020-01-01") ~ "P1: 2018-2019 (Pre-COVID)",
    df$Date < as.Date("2022-01-01") ~ "P2: 2020-2021 (COVID)",
    TRUE                            ~ "P3: 2022-2025 (Post-COVID)"
  )
  return(df)
}

df_usa <- make_model_df(rta_usa, fp_usa, "USA")
df_fra <- make_model_df(rta_fra, fp_fra, "France")
df_bra <- make_model_df(rta_bra, fp_bra, "Brazil")
df_tur <- make_model_df(rta_tur, fp_tur, "Turkey")
df_all <- rbind(df_usa, df_fra, df_bra, df_tur)
df_all$Market <- factor(df_all$Market,
                        levels = c("USA", "France",
                                   "Brazil", "Turkey"))

#stats descriptives
desc_stats <- df_all |>
  group_by(Market) |>
  summarise(
    N_obs      = n(),
    RTA_mean   = round(mean(RTA, na.rm = TRUE), 6),
    RTA_sd     = round(sd(RTA,   na.rm = TRUE), 6),
    RTA_min    = round(min(RTA,  na.rm = TRUE), 6),
    RTA_max    = round(max(RTA,  na.rm = TRUE), 6),
    FP_mean    = round(mean(FP,  na.rm = TRUE), 4),
    FP_sd      = round(sd(FP,    na.rm = TRUE), 4),
    FP_min     = round(min(FP,   na.rm = TRUE), 4),
    FP_max     = round(max(FP,   na.rm = TRUE), 4),
    .groups    = "drop"
  )

print(desc_stats, width = Inf)

#Correlation RTA et F_P pour chaque marché
cor_results <- df_all |>
  group_by(Market) |>
  summarise(
    Correlation = round(cor(RTA, FP, use = "complete.obs"), 4),
    P_value     = round(cor.test(RTA, FP)$p.value, 6),
    .groups     = "drop"
  )

print(cor_results)


cat("\n By market AND sub-period\n")
cor_by_period <- df_all |>
  group_by(Market, Period) |>
  summarise(
    N           = n(),
    Correlation = round(cor(RTA, FP, use = "complete.obs"), 4),
    .groups     = "drop"
  )

print(cor_by_period, n = Inf)

dir.create("output/tables", showWarnings = FALSE, recursive = TRUE)

write.csv(desc_stats,   "output/tables/descriptive_stats.csv",  row.names = FALSE)
write.csv(cor_results,  "output/tables/correlations.csv",        row.names = FALSE)
write.csv(cor_by_period,"output/tables/correlations_period.csv", row.names = FALSE)
write.csv(df_all,       "output/tables/full_dataset.csv",        row.names = FALSE)

save(df_usa, df_fra, df_bra, df_tur, df_all,
     rta_usa, rta_fra, rta_bra, rta_tur,
     fp_usa,  fp_fra,  fp_bra,  fp_tur,
     desc_stats, cor_results, cor_by_period,
     file = "data/rta_fp.RData")


#Résumé final
cat ("Summary")
for (mkt in levels(df_all$Market)) {
  sub <- df_all[df_all$Market == mkt, ]
  cor <- round(cor(sub$RTA, sub$FP, use = "complete.obs"), 4)
  cat(mkt, ": obs =", nrow(sub),
      "| avg RTA =", round(mean(sub$RTA), 6),
      "| avg F_P =", round(mean(sub$FP),  4),
      "| cor(RTA,FP) =", cor, "\n")
}

  
  
  
  

  
  
  

