# ============================================================
# Script 08:
# - stima parametrica del Value at Risk con distribuzione t di Student;
# - stima dei parametri tramite massima verosimiglianza;
# - confronto con VaR storico e VaR parametrico normale;
# - salvataggio della tabella dei risultati;
# - grafici delle perdite con VaR storico, normale e t di Student.
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
colore_student <- "#7C3AED"


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
  
  mu_hat + sigma_hat * qnorm(livello)
}


# ------------------------------------------------------------
# 8. Stima dei parametri della t di Student tramite MLE
# ------------------------------------------------------------

stima_student <- function(perdite) {
  
  mu_iniziale <- mean(perdite)
  sigma_iniziale <- sd(perdite)
  nu_iniziale <- 6
  
  # Parametrizzazione:
  # par[1] = mu
  # par[2] = log(sigma)
  # par[3] = log(nu - 2)
  #
  # In questo modo imponiamo sigma > 0 e nu > 2.
  # La condizione nu > 2 garantisce varianza finita.
  
  log_verosimiglianza_negativa <- function(par) {
    
    mu <- par[1]
    sigma <- exp(par[2])
    nu <- 2 + exp(par[3])
    
    z <- (perdite - mu) / sigma
    
    log_densita <- dt(z, df = nu, log = TRUE) - log(sigma)
    
    return(-sum(log_densita))
  }
  
  stima <- optim(
    par = c(
      mu_iniziale,
      log(sigma_iniziale),
      log(nu_iniziale - 2)
    ),
    fn = log_verosimiglianza_negativa,
    method = "BFGS",
    control = list(
      maxit = 10000
    )
  )
  
  if (stima$convergence != 0) {
    warning("La procedura di ottimizzazione non è convergente.")
  }
  
  mu_hat <- stima$par[1]
  sigma_hat <- exp(stima$par[2])
  nu_hat <- 2 + exp(stima$par[3])
  
  return(
    c(
      media = mu_hat,
      scala = sigma_hat,
      gradi_liberta = nu_hat,
      logLik_negativa = stima$value,
      convergenza = stima$convergence
    )
  )
}


# ------------------------------------------------------------
# 9. Funzione VaR parametrico t di Student
# ------------------------------------------------------------

var_student <- function(parametri_student, livello) {
  
  mu_hat <- parametri_student["media"]
  sigma_hat <- parametri_student["scala"]
  nu_hat <- parametri_student["gradi_liberta"]
  
  mu_hat + sigma_hat * qt(livello, df = nu_hat)
}


# ------------------------------------------------------------
# 10. Funzione per calcolare i risultati su un periodo
# ------------------------------------------------------------

calcola_var_periodo <- function(dati, nome_periodo, livelli) {
  
  perdite <- dati$loss
  
  parametri_student <- stima_student(perdite)
  
  risultati <- data.frame()
  
  for (livello in livelli) {
    
    stima_storica <- var_storico(perdite, livello)
    stima_normale <- var_normale(perdite, livello)
    stima_t <- var_student(parametri_student, livello)
    
    risultati_livello <- data.frame(
      periodo = nome_periodo,
      livello = paste0(round(100 * livello), "%"),
      confidenza = livello,
      media_t = parametri_student["media"],
      scala_t = parametri_student["scala"],
      gradi_liberta_t = parametri_student["gradi_liberta"],
      convergenza = parametri_student["convergenza"],
      var_storico = stima_storica,
      var_normale = stima_normale,
      var_student = stima_t,
      differenza_student_storico = stima_t - stima_storica,
      differenza_student_normale = stima_t - stima_normale
    )
    
    risultati <- rbind(risultati, risultati_livello)
  }
  
  return(risultati)
}


# ------------------------------------------------------------
# 11. Importazione dei dati
# ------------------------------------------------------------

dati_2018_2022 <- importa_dati_elaborati(file_2018_2022)
dati_2021_2025 <- importa_dati_elaborati(file_2021_2025)


# ------------------------------------------------------------
# 12. Calcolo VaR parametrico t
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

risultati_var_student <- rbind(
  risultati_2018_2022,
  risultati_2021_2025
)

rownames(risultati_var_student) <- NULL


# ------------------------------------------------------------
# 13. Versione percentuale
# ------------------------------------------------------------

risultati_percentuali <- risultati_var_student

colonne_da_percentuale <- c(
  "media_t",
  "scala_t",
  "var_storico",
  "var_normale",
  "var_student",
  "differenza_student_storico",
  "differenza_student_normale"
)

risultati_percentuali[, colonne_da_percentuale] <-
  100 * risultati_percentuali[, colonne_da_percentuale]

risultati_percentuali[, colonne_da_percentuale] <-
  round(risultati_percentuali[, colonne_da_percentuale], 4)

risultati_percentuali$gradi_liberta_t <-
  round(risultati_percentuali$gradi_liberta_t, 4)

risultati_percentuali$convergenza <-
  as.integer(risultati_percentuali$convergenza)

rownames(risultati_percentuali) <- NULL


# ------------------------------------------------------------
# 14. Stampa a video
# ------------------------------------------------------------

cat("\n")
cat("============================================================\n")
cat("VaR STORICO, NORMALE E t DI STUDENT\n")
cat("Valori percentuali, dove applicabile\n")
cat("============================================================\n")

print(risultati_percentuali)


# ------------------------------------------------------------
# 15. Salvataggio tabelle
# ------------------------------------------------------------

write.csv(
  risultati_percentuali,
  file = file.path(
    cartella_tabelle,
    "var_parametrico_student.csv"
  ),
  row.names = FALSE
)

write.csv2(
  risultati_percentuali,
  file = file.path(
    cartella_tabelle,
    "var_parametrico_student_excel.csv"
  ),
  row.names = FALSE
)


# ------------------------------------------------------------
# 16. Funzione per grafico con VaR storico, normale e t
# ------------------------------------------------------------

crea_grafico_var_student <- function(dati, risultati, nome_periodo, livello_var, nome_file) {
  
  dati_grafico <- dati
  dati_grafico$loss_pct <- 100 * dati_grafico$loss
  
  risultato <- risultati[
    risultati$periodo == nome_periodo &
      risultati$livello == livello_var,
  ]
  
  var_storico_pct <- 100 * risultato$var_storico
  var_normale_pct <- 100 * risultato$var_normale
  var_student_pct <- 100 * risultato$var_student
  
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
    geom_vline(
      xintercept = var_student_pct,
      linewidth = 0.8,
      linetype = "dotdash",
      color = colore_student
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
      paste0("var_student_confronto_", nome_file, ".png")
    ),
    plot = grafico,
    width = 8,
    height = 5,
    dpi = 300
  )
  
  print(grafico)
}


# ------------------------------------------------------------
# 17. Creazione grafici
# ------------------------------------------------------------

crea_grafico_var_student(
  dati = dati_2018_2022,
  risultati = risultati_var_student,
  nome_periodo = "2018-2022",
  livello_var = "95%",
  nome_file = "2018_2022_95"
)

crea_grafico_var_student(
  dati = dati_2018_2022,
  risultati = risultati_var_student,
  nome_periodo = "2018-2022",
  livello_var = "99%",
  nome_file = "2018_2022_99"
)

crea_grafico_var_student(
  dati = dati_2021_2025,
  risultati = risultati_var_student,
  nome_periodo = "2021-2025",
  livello_var = "95%",
  nome_file = "2021_2025_95"
)

crea_grafico_var_student(
  dati = dati_2021_2025,
  risultati = risultati_var_student,
  nome_periodo = "2021-2025",
  livello_var = "99%",
  nome_file = "2021_2025_99"
)


# ------------------------------------------------------------
# 18. Messaggio finale
# ------------------------------------------------------------

cat("\n")
cat("============================================================\n")
cat("VaR PARAMETRICO t DI STUDENT COMPLETATO\n")
cat("============================================================\n")

cat("File creati:\n")
cat("- output/tables/var_parametrico_student.csv\n")
cat("- output/tables/var_parametrico_student_excel.csv\n")
cat("- output/figures/var_student_confronto_2018_2022_95.png\n")
cat("- output/figures/var_student_confronto_2018_2022_99.png\n")
cat("- output/figures/var_student_confronto_2021_2025_95.png\n")
cat("- output/figures/var_student_confronto_2021_2025_99.png\n")