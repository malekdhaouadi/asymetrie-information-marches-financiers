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

