# ============================================================
# Script 07:
# - bootstrap parametrico normale del VaR;
# - stima di bias ed errore standard bootstrap;
# - intervalli normale, percentile, basic e BCa;
# - salvataggio di tabelle e grafici.
# ============================================================


# ------------------------------------------------------------
# 1. Pacchetto necessario
# ------------------------------------------------------------

library(ggplot2)


# ------------------------------------------------------------
# 2. Impostazioni generali
# ------------------------------------------------------------

set.seed(123)

# Per prova veloce usare B <- 2000.
# Per il risultato finale usare B <- 100000.
B <- 100000

livelli_var <- c(0.95, 0.99)
alpha_ic <- 0.05


# ------------------------------------------------------------
# 3. Palette grafica
# ------------------------------------------------------------

colore_blu <- "#2563EB"
colore_azzurro <- "#A7C7E7"


# ------------------------------------------------------------
# 4. Percorsi dei file
# ------------------------------------------------------------

file_2018_2022 <- "data/processed/sp500_2018_2022_losses.csv"
file_2021_2025 <- "data/processed/sp500_2021_2025_losses.csv"

cartella_tabelle <- "output/tables"
cartella_figure <- "output/figures"

if (!dir.exists(cartella_tabelle)) {
  dir.create(cartella_tabelle, recursive = TRUE)
}

if (!dir.exists(cartella_figure)) {
  dir.create(cartella_figure, recursive = TRUE)
}


# ------------------------------------------------------------
# 5. Funzione per importare i dati elaborati
# ------------------------------------------------------------

importa_dati_elaborati <- function(percorso_file) {
  
  if (!file.exists(percorso_file)) {
    stop(
      paste0(
        "File non trovato: ", percorso_file,
        "\nEsegui prima lo script 01_importazione_e_rendimenti.R."
      )
    )
  }
  
  dati <- read.csv(
    file = percorso_file,
    stringsAsFactors = FALSE
  )
  
  dati$Date <- as.Date(dati$Date)
  
  return(dati)
}


# ------------------------------------------------------------
# 6. Funzione VaR parametrico normale
# ------------------------------------------------------------

var_normale <- function(perdite, livello) {
  
  mu_hat <- mean(perdite)
  sigma_hat <- sd(perdite)
  
  mu_hat + sigma_hat * qnorm(livello)
}


# ------------------------------------------------------------
# 7. Funzione per intervallo BCa
# ------------------------------------------------------------

intervallo_bca <- function(theta_boot, theta_hat, theta_jack, alpha = 0.05) {
  
  B <- length(theta_boot)
  
  # z0: correzione di bias
  prop_minori <- mean(theta_boot < theta_hat)
  
  # Evita valori esattamente 0 o 1
  prop_minori <- min(max(prop_minori, 1 / (2 * B)), 1 - 1 / (2 * B))
  
  z0 <- qnorm(prop_minori)
  
  # a: parametro di accelerazione, stimato tramite jackknife
  theta_jack_media <- mean(theta_jack)
  
  numeratore <- sum((theta_jack_media - theta_jack)^3)
  denominatore <- 6 * (sum((theta_jack_media - theta_jack)^2))^(3 / 2)
  
  if (denominatore == 0) {
    a <- 0
  } else {
    a <- numeratore / denominatore
  }
  
  z_alpha_1 <- qnorm(alpha / 2)
  z_alpha_2 <- qnorm(1 - alpha / 2)
  
  alpha_1_bca <- pnorm(
    z0 + (z0 + z_alpha_1) / (1 - a * (z0 + z_alpha_1))
  )
  
  alpha_2_bca <- pnorm(
    z0 + (z0 + z_alpha_2) / (1 - a * (z0 + z_alpha_2))
  )
  
  limite_inf <- as.numeric(
    quantile(theta_boot, probs = alpha_1_bca, type = 7)
  )
  
  limite_sup <- as.numeric(
    quantile(theta_boot, probs = alpha_2_bca, type = 7)
  )
  
  return(
    c(
      limite_inf = limite_inf,
      limite_sup = limite_sup,
      z0 = z0,
      a = a,
      alpha_1_bca = alpha_1_bca,
      alpha_2_bca = alpha_2_bca
    )
  )
}


# ------------------------------------------------------------
# 8. Funzione principale di bootstrap parametrico normale
# ------------------------------------------------------------

bootstrap_var_normale <- function(dati, nome_periodo, B, livelli) {
  
  perdite <- dati$loss
  n <- length(perdite)
  
  # Parametri stimati sul campione originale
  mu_hat <- mean(perdite)
  sigma_hat <- sd(perdite)
  
  # VaR normale stimato sul campione originale
  var_originale <- sapply(
    livelli,
    function(livello) var_normale(perdite, livello)
  )
  
  names(var_originale) <- paste0("VaR_", livelli)
  
  # Matrice delle repliche bootstrap
  repliche_boot <- matrix(
    NA,
    nrow = B,
    ncol = length(livelli)
  )
  
  colnames(repliche_boot) <- paste0("livello_", livelli)
  
  cat("\n")
  cat("Bootstrap parametrico normale:", nome_periodo, "\n")
  cat("Numero osservazioni:", n, "\n")
  cat("Numero repliche:", B, "\n")
  cat("Media stimata:", mu_hat, "\n")
  cat("Deviazione standard stimata:", sigma_hat, "\n")
  
  for (b in 1:B) {
    
    # Campione generato dalla normale stimata sui dati originali
    campione_boot <- rnorm(
      n = n,
      mean = mu_hat,
      sd = sigma_hat
    )
    
    # Su ogni campione bootstrap si ristimano media e deviazione standard
    # e poi si ricalcola il VaR normale
    repliche_boot[b, ] <- sapply(
      livelli,
      function(livello) var_normale(campione_boot, livello)
    )
    
    if (b %% 10000 == 0) {
      cat("Repliche completate:", b, "su", B, "\n")
    }
  }
  
  risultati <- data.frame()
  repliche_lunghe <- data.frame()
  
  for (j in seq_along(livelli)) {
    
    livello <- livelli[j]
    theta_hat <- var_originale[j]
    theta_boot <- repliche_boot[, j]
    
    # Jackknife: stime leave-one-out della statistica parametrica normale
    theta_jack <- numeric(n)
    
    for (i in 1:n) {
      theta_jack[i] <- var_normale(perdite[-i], livello)
    }
    
    # Bias ed errore standard bootstrap
    bias_boot <- mean(theta_boot) - theta_hat
    se_boot <- sd(theta_boot)
    
    # Intervallo normale bootstrap
    ic_normale_inf <- theta_hat - qnorm(1 - alpha_ic / 2) * se_boot
    ic_normale_sup <- theta_hat + qnorm(1 - alpha_ic / 2) * se_boot
    
    # Intervallo percentile
    ic_percentile <- as.numeric(
      quantile(
        theta_boot,
        probs = c(alpha_ic / 2, 1 - alpha_ic / 2),
        type = 7
      )
    )
    
    # Intervallo basic
    ic_basic_inf <- 2 * theta_hat - ic_percentile[2]
    ic_basic_sup <- 2 * theta_hat - ic_percentile[1]
    
    # Intervallo BCa
    ic_bca <- intervallo_bca(
      theta_boot = theta_boot,
      theta_hat = theta_hat,
      theta_jack = theta_jack,
      alpha = alpha_ic
    )
    
    risultati_j <- data.frame(
      periodo = nome_periodo,
      livello = paste0(round(100 * livello), "%"),
      confidenza = livello,
      B = B,
      media_perdite = mu_hat,
      deviazione_standard_perdite = sigma_hat,
      var_normale = theta_hat,
      media_bootstrap = mean(theta_boot),
      bias_bootstrap = bias_boot,
      errore_standard_bootstrap = se_boot,
      normale_inf = ic_normale_inf,
      normale_sup = ic_normale_sup,
      percentile_inf = ic_percentile[1],
      percentile_sup = ic_percentile[2],
      basic_inf = ic_basic_inf,
      basic_sup = ic_basic_sup,
      bca_inf = ic_bca["limite_inf"],
      bca_sup = ic_bca["limite_sup"],
      bca_z0 = ic_bca["z0"],
      bca_a = ic_bca["a"]
    )
    
    risultati <- rbind(risultati, risultati_j)
    
    repliche_j <- data.frame(
      periodo = nome_periodo,
      livello = paste0(round(100 * livello), "%"),
      confidenza = livello,
      replica = 1:B,
      var_bootstrap = theta_boot
    )
    
    repliche_lunghe <- rbind(repliche_lunghe, repliche_j)
  }
  
  return(
    list(
      risultati = risultati,
      repliche = repliche_lunghe
    )
  )
}


# ------------------------------------------------------------
# 9. Importazione dei dati
# ------------------------------------------------------------

dati_2018_2022 <- importa_dati_elaborati(file_2018_2022)
dati_2021_2025 <- importa_dati_elaborati(file_2021_2025)


# ------------------------------------------------------------
# 10. Esecuzione del bootstrap parametrico normale
# ------------------------------------------------------------

bootstrap_normale_2018_2022 <- bootstrap_var_normale(
  dati = dati_2018_2022,
  nome_periodo = "2018-2022",
  B = B,
  livelli = livelli_var
)

bootstrap_normale_2021_2025 <- bootstrap_var_normale(
  dati = dati_2021_2025,
  nome_periodo = "2021-2025",
  B = B,
  livelli = livelli_var
)


# ------------------------------------------------------------
# 11. Tabelle finali
# ------------------------------------------------------------

risultati_bootstrap_var_normale <- rbind(
  bootstrap_normale_2018_2022$risultati,
  bootstrap_normale_2021_2025$risultati
)

repliche_bootstrap_var_normale <- rbind(
  bootstrap_normale_2018_2022$repliche,
  bootstrap_normale_2021_2025$repliche
)

# Versione in percentuale per tabella leggibile
risultati_percentuali <- risultati_bootstrap_var_normale

colonne_da_percentuale <- c(
  "media_perdite",
  "deviazione_standard_perdite",
  "var_normale",
  "media_bootstrap",
  "bias_bootstrap",
  "errore_standard_bootstrap",
  "normale_inf",
  "normale_sup",
  "percentile_inf",
  "percentile_sup",
  "basic_inf",
  "basic_sup",
  "bca_inf",
  "bca_sup"
)

risultati_percentuali[, colonne_da_percentuale] <-
  100 * risultati_percentuali[, colonne_da_percentuale]

risultati_percentuali[, colonne_da_percentuale] <-
  round(risultati_percentuali[, colonne_da_percentuale], 4)

risultati_percentuali$bca_z0 <- round(risultati_percentuali$bca_z0, 4)
risultati_percentuali$bca_a <- round(risultati_percentuali$bca_a, 4)

rownames(risultati_percentuali) <- NULL


# ------------------------------------------------------------
# 12. Stampa a video
# ------------------------------------------------------------

cat("\n")
cat("============================================================\n")
cat("BOOTSTRAP PARAMETRICO NORMALE DEL VaR\n")
cat("Valori percentuali, dove applicabile\n")
cat("============================================================\n")

print(risultati_percentuali)


# ------------------------------------------------------------
# 13. Salvataggio tabelle
# ------------------------------------------------------------

write.csv(
  risultati_percentuali,
  file = file.path(
    cartella_tabelle,
    "bootstrap_var_parametrico_normale.csv"
  ),
  row.names = FALSE
)

write.csv2(
  risultati_percentuali,
  file = file.path(
    cartella_tabelle,
    "bootstrap_var_parametrico_normale_excel.csv"
  ),
  row.names = FALSE
)

write.csv(
  repliche_bootstrap_var_normale,
  file = file.path(
    cartella_tabelle,
    "repliche_bootstrap_var_parametrico_normale.csv"
  ),
  row.names = FALSE
)

saveRDS(
  repliche_bootstrap_var_normale,
  file = file.path(
    cartella_tabelle,
    "repliche_bootstrap_var_parametrico_normale.rds"
  )
)


# ------------------------------------------------------------
# 14. Funzione per grafici delle repliche bootstrap
# ------------------------------------------------------------

crea_grafico_bootstrap_normale <- function(repliche, risultati, nome_periodo, livello_var, nome_file) {
  
  dati_grafico <- repliche[
    repliche$periodo == nome_periodo &
      repliche$livello == livello_var,
  ]
  
  dati_grafico$var_bootstrap_pct <- 100 * dati_grafico$var_bootstrap
  
  risultato <- risultati[
    risultati$periodo == nome_periodo &
      risultati$livello == livello_var,
  ]
  
  var_originale_pct <- 100 * risultato$var_normale
  
  grafico <- ggplot(dati_grafico, aes(x = var_bootstrap_pct)) +
    geom_histogram(
      bins = 60,
      fill = colore_azzurro,
      color = "white"
    ) +
    geom_vline(
      xintercept = var_originale_pct,
      linewidth = 0.8,
      color = colore_blu
    ) +
    labs(
      x = "VaR bootstrap (%)",
      y = "Frequenza",
    ) +
    theme_minimal(base_size = 12) +
    theme(
      axis.title.x = element_text(face = "bold", margin = margin(t = 10)),
      axis.title.y = element_text(face = "bold", margin = margin(r = 10))
    )
  
  ggsave(
    filename = file.path(
      cartella_figure,
      paste0("bootstrap_var_normale_", nome_file, ".png")
    ),
    plot = grafico,
    width = 8,
    height = 5,
    dpi = 300
  )
  
  print(grafico)
}


# ------------------------------------------------------------
# 15. Creazione grafici bootstrap
# ------------------------------------------------------------

crea_grafico_bootstrap_normale(
  repliche = repliche_bootstrap_var_normale,
  risultati = risultati_bootstrap_var_normale,
  nome_periodo = "2018-2022",
  livello_var = "95%",
  nome_file = "2018_2022_95"
)

crea_grafico_bootstrap_normale(
  repliche = repliche_bootstrap_var_normale,
  risultati = risultati_bootstrap_var_normale,
  nome_periodo = "2018-2022",
  livello_var = "99%",
  nome_file = "2018_2022_99"
)

crea_grafico_bootstrap_normale(
  repliche = repliche_bootstrap_var_normale,
  risultati = risultati_bootstrap_var_normale,
  nome_periodo = "2021-2025",
  livello_var = "95%",
  nome_file = "2021_2025_95"
)

crea_grafico_bootstrap_normale(
  repliche = repliche_bootstrap_var_normale,
  risultati = risultati_bootstrap_var_normale,
  nome_periodo = "2021-2025",
  livello_var = "99%",
  nome_file = "2021_2025_99"
)


# ------------------------------------------------------------
# 16. Messaggio finale
# ------------------------------------------------------------

cat("\n")
cat("============================================================\n")
cat("BOOTSTRAP PARAMETRICO NORMALE COMPLETATO\n")
cat("============================================================\n")

cat("File creati:\n")
cat("- output/tables/bootstrap_var_parametrico_normale.csv\n")
cat("- output/tables/bootstrap_var_parametrico_normale_excel.csv\n")
cat("- output/tables/repliche_bootstrap_var_parametrico_normale.csv\n")
cat("- output/tables/repliche_bootstrap_var_parametrico_normale.rds\n")
cat("- output/figures/bootstrap_var_normale_2018_2022_95.png\n")
cat("- output/figures/bootstrap_var_normale_2018_2022_99.png\n")
cat("- output/figures/bootstrap_var_normale_2021_2025_95.png\n")
cat("- output/figures/bootstrap_var_normale_2021_2025_99.png\n")