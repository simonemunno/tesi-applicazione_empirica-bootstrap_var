# ============================================================
# Script 06:
# - stima parametrica normale del Value at Risk;
# - confronto con il VaR storico;
# - salvataggio della tabella dei risultati;
# - grafici delle perdite con VaR storico e VaR normale.
# ============================================================


# ------------------------------------------------------------
# 1. Pacchetto necessario
# ------------------------------------------------------------

library(ggplot2)


# ------------------------------------------------------------
# 2. Impostazioni generali
# ------------------------------------------------------------

livelli_var <- c(0.95, 0.99)


# ------------------------------------------------------------
# 3. Palette grafica
# ------------------------------------------------------------

colore_azzurro <- "#A7C7E7"
colore_storico <- "#0F766E"
colore_normale <- "#2563EB"


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
# 6. Funzione VaR storico
# ------------------------------------------------------------

var_storico <- function(perdite, livello) {
  as.numeric(
    quantile(perdite, probs = livello, type = 7)
  )
}


# ------------------------------------------------------------
# 7. Funzione VaR parametrico normale
# ------------------------------------------------------------

var_normale <- function(perdite, livello) {
  
  mu_hat <- mean(perdite)
  sigma_hat <- sd(perdite)
  
  var_hat <- mu_hat + sigma_hat * qnorm(livello)
  
  return(
    c(
      media = mu_hat,
      deviazione_standard = sigma_hat,
      var_normale = var_hat
    )
  )
}


# ------------------------------------------------------------
# 8. Funzione per calcolare i risultati su un periodo
# ------------------------------------------------------------

calcola_var_periodo <- function(dati, nome_periodo, livelli) {
  
  perdite <- dati$loss
  
  risultati <- data.frame()
  
  for (livello in livelli) {
    
    stima_normale <- var_normale(perdite, livello)
    stima_storica <- var_storico(perdite, livello)
    
    risultati_livello <- data.frame(
      periodo = nome_periodo,
      livello = paste0(round(100 * livello), "%"),
      confidenza = livello,
      media_perdite = stima_normale["media"],
      deviazione_standard_perdite = stima_normale["deviazione_standard"],
      var_storico = stima_storica,
      var_normale = stima_normale["var_normale"],
      differenza_normale_storico = stima_normale["var_normale"] - stima_storica
    )
    
    risultati <- rbind(risultati, risultati_livello)
  }
  
  return(risultati)
}


# ------------------------------------------------------------
# 9. Importazione dei dati
# ------------------------------------------------------------

dati_2018_2022 <- importa_dati_elaborati(file_2018_2022)
dati_2021_2025 <- importa_dati_elaborati(file_2021_2025)


# ------------------------------------------------------------
# 10. Calcolo VaR parametrico normale
# ------------------------------------------------------------

risultati_2018_2022 <- calcola_var_periodo(
  dati = dati_2018_2022,
  nome_periodo = "2018-2022",
  livelli = livelli_var
)

risultati_2021_2025 <- calcola_var_periodo(
  dati = dati_2021_2025,
  nome_periodo = "2021-2025",
  livelli = livelli_var
)

risultati_var_normale <- rbind(
  risultati_2018_2022,
  risultati_2021_2025
)


# ------------------------------------------------------------
# 11. Versione percentuale
# ------------------------------------------------------------

risultati_percentuali <- risultati_var_normale

colonne_da_percentuale <- c(
  "media_perdite",
  "deviazione_standard_perdite",
  "var_storico",
  "var_normale",
  "differenza_normale_storico"
)

risultati_percentuali[, colonne_da_percentuale] <-
  100 * risultati_percentuali[, colonne_da_percentuale]

risultati_percentuali[, colonne_da_percentuale] <-
  round(risultati_percentuali[, colonne_da_percentuale], 4)


# ------------------------------------------------------------
# 12. Stampa a video
# ------------------------------------------------------------

cat("\n")
cat("============================================================\n")
cat("VaR STORICO E VaR PARAMETRICO NORMALE\n")
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
    "var_parametrico_normale.csv"
  ),
  row.names = FALSE
)

write.csv2(
  risultati_percentuali,
  file = file.path(
    cartella_tabelle,
    "var_parametrico_normale_excel.csv"
  ),
  row.names = FALSE
)


# ------------------------------------------------------------
# 14. Funzione per grafico con VaR storico e VaR normale
# ------------------------------------------------------------

crea_grafico_var_normale <- function(dati, risultati, nome_periodo, livello_var, nome_file) {
  
  dati_grafico <- dati
  dati_grafico$loss_pct <- 100 * dati_grafico$loss
  
  risultato <- risultati[
    risultati$periodo == nome_periodo &
      risultati$livello == livello_var,
  ]
  
  var_storico_pct <- 100 * risultato$var_storico
  var_normale_pct <- 100 * risultato$var_normale
  
  grafico <- ggplot(dati_grafico, aes(x = loss_pct)) +
    geom_histogram(
      aes(y = after_stat(density)),
      bins = 60,
      fill = colore_azzurro,
      color = "white"
    ) +
    geom_density(
      linewidth = 0.8
    ) +
    geom_vline(
      xintercept = var_storico_pct,
      linewidth = 0.8,
      linetype = "dashed",
      color = colore_storico
    ) +
    geom_vline(
      xintercept = var_normale_pct,
      linewidth = 0.8,
      linetype = "dotted",
      color = colore_normale
    ) +
    labs(

      x = "Perdita relativa (%)",
      y = "Densità",
     
    ) +
    theme_minimal(base_size = 12) +
    theme(
      axis.title.x = element_text(face = "bold", margin = margin(t = 10)),
      axis.title.y = element_text(face = "bold", margin = margin(r = 10))
    )
  
  ggsave(
    filename = file.path(
      cartella_figure,
      paste0("var_normale_confronto_", nome_file, ".png")
    ),
    plot = grafico,
    width = 8,
    height = 5,
    dpi = 300
  )
  
  print(grafico)
}


# ------------------------------------------------------------
# 15. Creazione grafici
# ------------------------------------------------------------

crea_grafico_var_normale(
  dati = dati_2018_2022,
  risultati = risultati_var_normale,
  nome_periodo = "2018-2022",
  livello_var = "95%",
  nome_file = "2018_2022_95"
)

crea_grafico_var_normale(
  dati = dati_2018_2022,
  risultati = risultati_var_normale,
  nome_periodo = "2018-2022",
  livello_var = "99%",
  nome_file = "2018_2022_99"
)

crea_grafico_var_normale(
  dati = dati_2021_2025,
  risultati = risultati_var_normale,
  nome_periodo = "2021-2025",
  livello_var = "95%",
  nome_file = "2021_2025_95"
)

crea_grafico_var_normale(
  dati = dati_2021_2025,
  risultati = risultati_var_normale,
  nome_periodo = "2021-2025",
  livello_var = "99%",
  nome_file = "2021_2025_99"
)


# ------------------------------------------------------------
# 16. Messaggio finale
# ------------------------------------------------------------

cat("\n")
cat("============================================================\n")
cat("VaR PARAMETRICO NORMALE COMPLETATO\n")
cat("============================================================\n")

cat("File creati:\n")
cat("- output/tables/var_parametrico_normale.csv\n")
cat("- output/tables/var_parametrico_normale_excel.csv\n")
cat("- output/figures/var_normale_confronto_2018_2022_95.png\n")
cat("- output/figures/var_normale_confronto_2018_2022_99.png\n")
cat("- output/figures/var_normale_confronto_2021_2025_95.png\n")
cat("- output/figures/var_normale_confronto_2021_2025_99.png\n")