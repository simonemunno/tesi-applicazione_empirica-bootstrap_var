# ============================================================
# Script 03:
# - importazione dei dataset elaborati;
# - calcolo delle statistiche descrittive delle perdite;
# - salvataggio della tabella dei risultati.
# ============================================================


# ------------------------------------------------------------
# 1. Percorsi dei file
# ------------------------------------------------------------

file_2018_2022 <- "data/processed/sp500_2018_2022_losses.csv"
file_2021_2025 <- "data/processed/sp500_2021_2025_losses.csv"

cartella_tabelle <- "output/tables"

if (!dir.exists(cartella_tabelle)) {
  dir.create(cartella_tabelle, recursive = TRUE)
}


# ------------------------------------------------------------
# 2. Funzione per importare i dati elaborati
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
# 3. Funzioni per asimmetria e curtosi
# ------------------------------------------------------------

asimmetria <- function(x) {
  x <- x[!is.na(x)]
  n <- length(x)
  media <- mean(x)
  s <- sd(x)
  
  if (n < 3 || s == 0) {
    return(NA)
  }
  
  mean(((x - media) / s)^3)
}


curtosi <- function(x) {
  x <- x[!is.na(x)]
  n <- length(x)
  media <- mean(x)
  s <- sd(x)
  
  if (n < 4 || s == 0) {
    return(NA)
  }
  
  mean(((x - media) / s)^4)
}


# ------------------------------------------------------------
# 4. Funzione per calcolare le statistiche descrittive
# ------------------------------------------------------------

calcola_statistiche <- function(dati, nome_periodo) {
  
  perdite <- dati$loss
  
  tabella <- data.frame(
    periodo = nome_periodo,
    osservazioni = length(perdite),
    media = mean(perdite),
    deviazione_standard = sd(perdite),
    minimo = min(perdite),
    primo_quartile = as.numeric(quantile(perdite, 0.25, type = 7)),
    mediana = median(perdite),
    terzo_quartile = as.numeric(quantile(perdite, 0.75, type = 7)),
    massimo = max(perdite),
    asimmetria = asimmetria(perdite),
    curtosi = curtosi(perdite),
    quantile_95 = as.numeric(quantile(perdite, 0.95, type = 7)),
    quantile_99 = as.numeric(quantile(perdite, 0.99, type = 7))
  )
  
  return(tabella)
}


# ------------------------------------------------------------
# 5. Importazione dei due periodi
# ------------------------------------------------------------

dati_2018_2022 <- importa_dati_elaborati(file_2018_2022)
dati_2021_2025 <- importa_dati_elaborati(file_2021_2025)


# ------------------------------------------------------------
# 6. Calcolo delle statistiche descrittive
# ------------------------------------------------------------

statistiche_2018_2022 <- calcola_statistiche(
  dati = dati_2018_2022,
  nome_periodo = "2018-2022"
)

statistiche_2021_2025 <- calcola_statistiche(
  dati = dati_2021_2025,
  nome_periodo = "2021-2025"
)

statistiche_descrittive <- rbind(
  statistiche_2018_2022,
  statistiche_2021_2025
)


# ------------------------------------------------------------
# 7. Versione in percentuale per lettura e tesi
# ------------------------------------------------------------

statistiche_descrittive_percentuali <- statistiche_descrittive

colonne_percentuali <- c(
  "media",
  "deviazione_standard",
  "minimo",
  "primo_quartile",
  "mediana",
  "terzo_quartile",
  "massimo",
  "quantile_95",
  "quantile_99"
)

statistiche_descrittive_percentuali[, colonne_percentuali] <-
  100 * statistiche_descrittive_percentuali[, colonne_percentuali]


# Arrotondamento per una tabella più leggibile
statistiche_descrittive_percentuali[, colonne_percentuali] <-
  round(statistiche_descrittive_percentuali[, colonne_percentuali], 4)

statistiche_descrittive_percentuali$asimmetria <-
  round(statistiche_descrittive_percentuali$asimmetria, 4)

statistiche_descrittive_percentuali$curtosi <-
  round(statistiche_descrittive_percentuali$curtosi, 4)


# ------------------------------------------------------------
# 8. Stampa a video dei risultati
# ------------------------------------------------------------

cat("\n")
cat("============================================================\n")
cat("STATISTICHE DESCRITTIVE DELLE PERDITE\n")
cat("Valori espressi in percentuale, dove applicabile\n")
cat("============================================================\n")

print(statistiche_descrittive_percentuali)


# ------------------------------------------------------------
# 9. Salvataggio della tabella
# ------------------------------------------------------------

# Salvataggio in formato CSV standard
write.csv(
  statistiche_descrittive_percentuali,
  file = file.path(
    cartella_tabelle,
    "statistiche_descrittive_perdite.csv"
  ),
  row.names = FALSE
)

# Salvataggio in formato compatibile con Excel/Numbers in italiano
write.csv2(
  statistiche_descrittive_percentuali,
  file = file.path(
    cartella_tabelle,
    "statistiche_descrittive_perdite_excel.csv"
  ),
  row.names = FALSE
)


# ------------------------------------------------------------
# 10. Messaggio finale
# ------------------------------------------------------------

cat("\n")
cat("============================================================\n")
cat("TABELLA SALVATA CORRETTAMENTE\n")
cat("============================================================\n")

cat("File creato:\n")
cat("output/tables/statistiche_descrittive_perdite.csv\n")