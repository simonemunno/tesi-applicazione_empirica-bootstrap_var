# ============================================================
# Script 04:
# - importazione dei dataset elaborati;
# - calcolo del Value at Risk storico;
# - salvataggio della tabella dei risultati;
# - grafici delle perdite con linee verticali del VaR storico.
# ============================================================


# ------------------------------------------------------------
# 1. Pacchetto necessario
# ------------------------------------------------------------

library(ggplot2)


# ------------------------------------------------------------
# 2. Palette grafica
# ------------------------------------------------------------

colore_blu <- "#2563EB"
colore_azzurro <- "#A7C7E7"
colore_petrolio <- "#0F766E"
colore_zero <- "#111827"
colore_var_95 <- "#0F766E"
colore_var_99 <- "#1D4ED8"


# ------------------------------------------------------------
# 3. Percorsi dei file
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
# 4. Funzione per importare i dati elaborati
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
  dati$loss_pct <- 100 * dati$loss
  
  return(dati)
}


# ------------------------------------------------------------
# 5. Funzione per calcolare il VaR storico
# ------------------------------------------------------------

calcola_var_storico <- function(dati, nome_periodo) {
  
  perdite <- dati$loss
  
  var_95 <- as.numeric(
    quantile(perdite, probs = 0.95, type = 7)
  )
  
  var_99 <- as.numeric(
    quantile(perdite, probs = 0.99, type = 7)
  )
  
  risultati <- data.frame(
    periodo = nome_periodo,
    livello = c("95%", "99%"),
    confidenza = c(0.95, 0.99),
    var_storico = c(var_95, var_99),
    var_storico_percentuale = 100 * c(var_95, var_99)
  )
  
  return(risultati)
}


# ------------------------------------------------------------
# 6. Importazione dei dati
# ------------------------------------------------------------

dati_2018_2022 <- importa_dati_elaborati(file_2018_2022)
dati_2021_2025 <- importa_dati_elaborati(file_2021_2025)


# ------------------------------------------------------------
# 7. Calcolo del VaR storico
# ------------------------------------------------------------

var_2018_2022 <- calcola_var_storico(
  dati = dati_2018_2022,
  nome_periodo = "2018-2022"
)

var_2021_2025 <- calcola_var_storico(
  dati = dati_2021_2025,
  nome_periodo = "2021-2025"
)

risultati_var_storico <- rbind(
  var_2018_2022,
  var_2021_2025
)

# Arrotondamento per stampa e tabelle
risultati_var_storico$var_storico <- round(
  risultati_var_storico$var_storico,
  6
)

risultati_var_storico$var_storico_percentuale <- round(
  risultati_var_storico$var_storico_percentuale,
  4
)


# ------------------------------------------------------------
# 8. Stampa dei risultati
# ------------------------------------------------------------

cat("\n")
cat("============================================================\n")
cat("VALUE AT RISK STORICO\n")
cat("============================================================\n")

print(risultati_var_storico)


# ------------------------------------------------------------
# 9. Salvataggio delle tabelle
# ------------------------------------------------------------

# CSV standard
write.csv(
  risultati_var_storico,
  file = file.path(
    cartella_tabelle,
    "var_storico.csv"
  ),
  row.names = FALSE
)

# CSV compatibile con Excel/Numbers in italiano
write.csv2(
  risultati_var_storico,
  file = file.path(
    cartella_tabelle,
    "var_storico_excel.csv"
  ),
  row.names = FALSE
)


# ------------------------------------------------------------
# 10. Funzione per grafico delle perdite con VaR storico
# ------------------------------------------------------------

crea_grafico_var_storico <- function(dati, risultati_var, nome_periodo, nome_file) {
  
  var_95_pct <- risultati_var$var_storico_percentuale[
    risultati_var$livello == "95%"
  ]
  
  var_99_pct <- risultati_var$var_storico_percentuale[
    risultati_var$livello == "99%"
  ]
  
  grafico <- ggplot(dati, aes(x = loss_pct)) +
    geom_histogram(
      bins = 50,
      fill = colore_azzurro,
      color = "white"
    ) +
    geom_vline(
      xintercept = var_95_pct,
      linewidth = 0.8,
      linetype = "dashed",
      color = colore_var_95
    ) +
    geom_vline(
      xintercept = var_99_pct,
      linewidth = 0.8,
      linetype = "dotted",
      color = colore_var_99
    ) +
    labs(
  
      x = "Perdita relativa (%)",
      y = "Frequenza"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      axis.title.x = element_text(face = "bold", margin = margin(t = 10)),
      axis.title.y = element_text(face = "bold", margin = margin(r = 10))
    )
  ggsave(
    filename = file.path(
      cartella_figure,
      paste0("var_storico_istogramma_", nome_file, ".png")
    ),
    plot = grafico,
    width = 8,
    height = 5,
    dpi = 300
  )
  
  print(grafico)
}


# ------------------------------------------------------------
# 11. Creazione grafici VaR storico
# ------------------------------------------------------------

crea_grafico_var_storico(
  dati = dati_2018_2022,
  risultati_var = var_2018_2022,
  nome_periodo = "2018-2022",
  nome_file = "2018_2022"
)

crea_grafico_var_storico(
  dati = dati_2021_2025,
  risultati_var = var_2021_2025,
  nome_periodo = "2021-2025",
  nome_file = "2021_2025"
)


# ------------------------------------------------------------
# 12. Messaggio finale
# ------------------------------------------------------------

cat("\n")
cat("============================================================\n")
cat("CALCOLO DEL VaR STORICO COMPLETATO\n")
cat("============================================================\n")

cat("File creati:\n")
cat("- output/tables/var_storico.csv\n")
cat("- output/tables/var_storico_excel.csv\n")
cat("- output/figures/var_storico_istogramma_2018_2022.png\n")
cat("- output/figures/var_storico_istogramma_2021_2025.png\n")