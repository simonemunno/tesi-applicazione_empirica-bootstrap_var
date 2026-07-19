# ============================================================
# Script 01:
# - importazione dei dati dell'S&P 500;
# - controllo preliminare delle osservazioni;
# - calcolo dei rendimenti logaritmici;
# - costruzione della serie delle perdite;
# - salvataggio dei dataset elaborati.
# ============================================================


# ------------------------------------------------------------
# 1. Percorsi dei file
# ------------------------------------------------------------

# File originali scaricati da Stooq e salvati in data/raw/
file_2018_2022 <- "data/raw/sp500_2018_2022.csv"
file_2021_2025 <- "data/raw/sp500_2021_2025.csv"

# Cartella in cui verranno salvati i dati elaborati
cartella_output <- "data/processed"


# ------------------------------------------------------------
# 2. Controllo della cartella di output
# ------------------------------------------------------------

if (!dir.exists(cartella_output)) {
  dir.create(cartella_output, recursive = TRUE)
}

# ------------------------------------------------------------
# 3. Funzione per importare e preparare un dataset
# ------------------------------------------------------------

prepara_dati_sp500 <- function(percorso_file) {
  
  # Controllo che il file esista davvero
  if (!file.exists(percorso_file)) {
    stop(
      paste0(
        "File non trovato: ", percorso_file,
        "\nControlla che il nome del file sia corretto ",
        "e che sia nella cartella data/raw/."
      )
    )
  }
  
  # Importazione del file CSV
  dati <- read.csv(
    file = percorso_file,
    stringsAsFactors = FALSE,
    na.strings = c("", "NA")
  )
  
  # Elimina eventuali spazi dai nomi delle colonne
  names(dati) <- trimws(names(dati))
  
  # Controlla che esistano le colonne necessarie
  colonne_richieste <- c("Date", "Close")
  
  if (!all(colonne_richieste %in% names(dati))) {
    stop(
      paste0(
        "Nel file ", percorso_file,
        " mancano le colonne Date e/o Close."
      )
    )
  }
  
  # Manteniamo soltanto data e prezzo di chiusura
  dati <- dati[, c("Date", "Close")]
  
  # Trasformazione della colonna Date in una data leggibile da R
  dati$Date <- as.Date(dati$Date, format = "%Y-%m-%d")
  
  # Trasformazione esplicita del prezzo in formato numerico
  dati$Close <- as.numeric(dati$Close)
  
  # Controllo date mancanti o non leggibili
  if (any(is.na(dati$Date))) {
    stop(
      paste0(
        "Sono presenti date mancanti o non leggibili nel file: ",
        percorso_file
      )
    )
  }
  
  # Controllo prezzi mancanti
  if (any(is.na(dati$Close))) {
    stop(
      paste0(
        "Sono presenti prezzi di chiusura mancanti nel file: ",
        percorso_file
      )
    )
  }
  
  # Ordinamento cronologico crescente
  dati <- dati[order(dati$Date), ]
  
  # Controllo di eventuali date duplicate
  if (any(duplicated(dati$Date))) {
    
    warning(
      paste0(
        "Sono presenti date duplicate nel file ",
        percorso_file,
        ". Verrà mantenuta soltanto la prima osservazione di ogni data."
      )
    )
    
    dati <- dati[!duplicated(dati$Date), ]
  }
  
  # I prezzi devono essere strettamente positivi
  if (any(dati$Close <= 0)) {
    stop(
      paste0(
        "Sono presenti prezzi non positivi nel file: ",
        percorso_file
      )
    )
  }
  
  # Calcolo dei rendimenti logaritmici:
  # r_t = log(P_t / P_(t-1))
  dati$log_return <- c(
    NA,
    diff(log(dati$Close))
  )
  
  # Costruzione delle perdite relative:
  # L_t = -r_t
  dati$loss <- -dati$log_return
  
  # Il primo rendimento non può essere calcolato:
  # eliminiamo quindi la prima osservazione.
  dati <- dati[!is.na(dati$log_return), ]
  
  # Riordiniamo le colonne finali
  dati <- dati[, c("Date", "Close", "log_return", "loss")]
  
  return(dati)
}


# ------------------------------------------------------------
# 4. Elaborazione del primo periodo: 2018-2022
# ------------------------------------------------------------

dati_2018_2022 <- prepara_dati_sp500(
  percorso_file = file_2018_2022
)


# ------------------------------------------------------------
# 5. Elaborazione del secondo periodo: 2021-2025
# ------------------------------------------------------------

dati_2021_2025 <- prepara_dati_sp500(
  percorso_file = file_2021_2025
)


# ------------------------------------------------------------
# 6. Controlli preliminari del primo periodo
# ------------------------------------------------------------

cat("\n")
cat("============================================================\n")
cat("PERIODO 2018-2022\n")
cat("============================================================\n")

cat("Numero di osservazioni:", nrow(dati_2018_2022), "\n")
cat("Prima data:", as.character(min(dati_2018_2022$Date)), "\n")
cat("Ultima data:", as.character(max(dati_2018_2022$Date)), "\n\n")

cat("Prime sei righe del dataset:\n")
print(head(dati_2018_2022))

cat("\nRiepilogo delle principali variabili:\n")
print(summary(dati_2018_2022[, c("Close", "log_return", "loss")]))


# ------------------------------------------------------------
# 7. Controlli preliminari del secondo periodo
# ------------------------------------------------------------

cat("\n")
cat("============================================================\n")
cat("PERIODO 2021-2025\n")
cat("============================================================\n")

cat("Numero di osservazioni:", nrow(dati_2021_2025), "\n")
cat("Prima data:", as.character(min(dati_2021_2025$Date)), "\n")
cat("Ultima data:", as.character(max(dati_2021_2025$Date)), "\n\n")

cat("Prime sei righe del dataset:\n")
print(head(dati_2021_2025))

cat("\nRiepilogo delle principali variabili:\n")
print(summary(dati_2021_2025[, c("Close", "log_return", "loss")]))


# ------------------------------------------------------------
# 8. Salvataggio dei dataset elaborati
# ------------------------------------------------------------

write.csv(
  dati_2018_2022,
  file = file.path(
    cartella_output,
    "sp500_2018_2022_losses.csv"
  ),
  row.names = FALSE
)

write.csv(
  dati_2021_2025,
  file = file.path(
    cartella_output,
    "sp500_2021_2025_losses.csv"
  ),
  row.names = FALSE
)


# ------------------------------------------------------------
# 9. Messaggio finale
# ------------------------------------------------------------

cat("\n")
cat("============================================================\n")
cat("ELABORAZIONE COMPLETATA CORRETTAMENTE\n")
cat("============================================================\n")

cat("I file elaborati sono stati salvati nella cartella:\n")
cat("data/processed/\n\n")


cat("File creati:\n")
cat("- sp500_2018_2022_losses.csv\n")
cat("- sp500_2021_2025_losses.csv\n")