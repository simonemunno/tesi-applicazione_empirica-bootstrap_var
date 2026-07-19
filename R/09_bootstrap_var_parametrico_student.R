# ============================================================
# Script 09:
# - bootstrap parametrico t di Student del VaR;
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

# ATTENZIONE:
# Questo script è più pesante degli altri perché ogni replica
# richiede una stima MLE della t di Student.

B <- 100000

livelli_var <- c(0.95, 0.99)
alpha_ic <- 0.05


# ------------------------------------------------------------
# 3. Palette grafica
# ------------------------------------------------------------

colore_blu <- "#7C3AED"
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
# 6. Stima MLE della t di Student
# ------------------------------------------------------------

stima_student <- function(perdite, parametri_iniziali = NULL) {
  
  if (is.null(parametri_iniziali)) {
    
    mu_iniziale <- mean(perdite)
    sigma_iniziale <- sd(perdite)
    nu_iniziale <- 6
    
  } else {
    
    mu_iniziale <- as.numeric(parametri_iniziali["media"])
    sigma_iniziale <- as.numeric(parametri_iniziali["scala"])
    nu_iniziale <- as.numeric(parametri_iniziali["gradi_liberta"])
    
    if (is.na(mu_iniziale) || is.na(sigma_iniziale) || is.na(nu_iniziale)) {
      mu_iniziale <- mean(perdite)
      sigma_iniziale <- sd(perdite)
      nu_iniziale <- 6
    }
  }
  
  sigma_iniziale <- max(sigma_iniziale, 1e-6)
  nu_iniziale <- max(nu_iniziale, 2.1)
  
  log_verosimiglianza_negativa <- function(par) {
    
    mu <- par[1]
    sigma <- exp(par[2])
    nu <- 2 + exp(par[3])
    
    z <- (perdite - mu) / sigma
    
    log_densita <- dt(z, df = nu, log = TRUE) - log(sigma)
    
    -sum(log_densita)
  }
  
  stima <- tryCatch(
    optim(
      par = c(
        mu_iniziale,
        log(sigma_iniziale),
        log(nu_iniziale - 2)
      ),
      fn = log_verosimiglianza_negativa,
      method = "BFGS",
      control = list(maxit = 5000)
    ),
    error = function(e) NULL
  )
  
  if (is.null(stima)) {
    return(
      c(
        media = NA_real_,
        scala = NA_real_,
        gradi_liberta = NA_real_,
        logLik_negativa = NA_real_,
        convergenza = 1
      )
    )
  }
  
  mu_hat <- as.numeric(stima$par[1])
  sigma_hat <- as.numeric(exp(stima$par[2]))
  nu_hat <- as.numeric(2 + exp(stima$par[3]))
  
  return(
    c(
      media = mu_hat,
      scala = sigma_hat,
      gradi_liberta = nu_hat,
      logLik_negativa = as.numeric(stima$value),
      convergenza = as.numeric(stima$convergence)
    )
  )
}


# ------------------------------------------------------------
# 7. Funzione VaR parametrico t di Student
# ------------------------------------------------------------

var_student <- function(parametri_student, livello) {
  
  mu_hat <- parametri_student["media"]
  sigma_hat <- parametri_student["scala"]
  nu_hat <- parametri_student["gradi_liberta"]
  
  mu_hat + sigma_hat * qt(livello, df = nu_hat)
}


# ------------------------------------------------------------
# 8. Funzione per intervallo BCa
# ------------------------------------------------------------

intervallo_bca <- function(theta_boot, theta_hat, theta_jack, alpha = 0.05) {
  
  theta_boot <- theta_boot[!is.na(theta_boot)]
  theta_jack <- theta_jack[!is.na(theta_jack)]
  
  B_eff <- length(theta_boot)
  
  prop_minori <- mean(theta_boot < theta_hat)
  prop_minori <- min(max(prop_minori, 1 / (2 * B_eff)), 1 - 1 / (2 * B_eff))
  
  z0 <- qnorm(prop_minori)
  
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
# 9. Funzione principale di bootstrap parametrico t
# ------------------------------------------------------------

bootstrap_var_student <- function(dati, nome_periodo, B, livelli) {
  
  perdite <- dati$loss
  n <- length(perdite)
  
  # Stima dei parametri sul campione originale
  parametri_hat <- stima_student(perdite)
  
  mu_hat <- parametri_hat["media"]
  sigma_hat <- parametri_hat["scala"]
  nu_hat <- parametri_hat["gradi_liberta"]
  
  var_originale <- sapply(
    livelli,
    function(livello) var_student(parametri_hat, livello)
  )
  
  names(var_originale) <- paste0("VaR_", livelli)
  
  repliche_boot <- matrix(
    NA,
    nrow = B,
    ncol = length(livelli)
  )
  
  colnames(repliche_boot) <- paste0("livello_", livelli)
  
  convergenze <- numeric(B)
  
  cat("\n")
  cat("Bootstrap parametrico t di Student:", nome_periodo, "\n")
  cat("Numero osservazioni:", n, "\n")
  cat("Numero repliche richieste:", B, "\n")
  cat("Media stimata:", mu_hat, "\n")
  cat("Scala stimata:", sigma_hat, "\n")
  cat("Gradi di libertà stimati:", nu_hat, "\n")
  
  for (b in 1:B) {
    
    # Campione generato dalla t stimata sui dati originali
    campione_boot <- mu_hat + sigma_hat * rt(
      n = n,
      df = nu_hat
    )
    
    # Ristima MLE sul campione bootstrap
    parametri_boot <- stima_student(
      campione_boot,
      parametri_iniziali = parametri_hat
    )
    
    convergenze[b] <- parametri_boot["convergenza"]
    
    if (!any(is.na(parametri_boot)) && parametri_boot["convergenza"] == 0) {
      
      repliche_boot[b, ] <- sapply(
        livelli,
        function(livello) var_student(parametri_boot, livello)
      )
    }
    
    if (b %% 5000 == 0) {
      cat("Repliche completate:", b, "su", B, "\n")
    }
  }
  
  risultati <- data.frame()
  repliche_lunghe <- data.frame()
  
  for (j in seq_along(livelli)) {
    
    livello <- livelli[j]
    theta_hat <- var_originale[j]
    theta_boot <- repliche_boot[, j]
    theta_boot_valide <- theta_boot[!is.na(theta_boot)]
    
    B_eff <- length(theta_boot_valide)
    
    # Jackknife: stime leave-one-out della statistica parametrica t
    theta_jack <- numeric(n)
    
    for (i in 1:n) {
      
      parametri_jack <- stima_student(
        perdite[-i],
        parametri_iniziali = parametri_hat
      )
      
      if (!any(is.na(parametri_jack)) && parametri_jack["convergenza"] == 0) {
        theta_jack[i] <- var_student(parametri_jack, livello)
      } else {
        theta_jack[i] <- NA
      }
    }
    
    # Bias ed errore standard bootstrap
    bias_boot <- mean(theta_boot_valide) - theta_hat
    se_boot <- sd(theta_boot_valide)
    
    # Intervallo normale bootstrap
    ic_normale_inf <- theta_hat - qnorm(1 - alpha_ic / 2) * se_boot
    ic_normale_sup <- theta_hat + qnorm(1 - alpha_ic / 2) * se_boot
    
    # Intervallo percentile
    ic_percentile <- as.numeric(
      quantile(
        theta_boot_valide,
        probs = c(alpha_ic / 2, 1 - alpha_ic / 2),
        type = 7
      )
    )
    
    # Intervallo basic
    ic_basic_inf <- 2 * theta_hat - ic_percentile[2]
    ic_basic_sup <- 2 * theta_hat - ic_percentile[1]
    
    # Intervallo BCa
    ic_bca <- intervallo_bca(
      theta_boot = theta_boot_valide,
      theta_hat = theta_hat,
      theta_jack = theta_jack,
      alpha = alpha_ic
    )
    
    risultati_j <- data.frame(
      periodo = nome_periodo,
      livello = paste0(round(100 * livello), "%"),
      confidenza = livello,
      B_richiesto = B,
      B_effettivo = B_eff,
      media_t = mu_hat,
      scala_t = sigma_hat,
      gradi_liberta_t = nu_hat,
      var_student = theta_hat,
      media_bootstrap = mean(theta_boot_valide),
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
      bca_a = ic_bca["a"],
      convergenze_ok = sum(convergenze == 0),
      convergenze_non_ok = sum(convergenze != 0)
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
# 10. Importazione dei dati
# ------------------------------------------------------------

dati_2018_2022 <- importa_dati_elaborati(file_2018_2022)
dati_2021_2025 <- importa_dati_elaborati(file_2021_2025)


# ------------------------------------------------------------
# 11. Esecuzione del bootstrap parametrico t
# ------------------------------------------------------------

bootstrap_student_2018_2022 <- bootstrap_var_student(
  dati = dati_2018_2022,
  nome_periodo = "2018-2022",
  B = B,
  livelli = livelli_var
)

bootstrap_student_2021_2025 <- bootstrap_var_student(
  dati = dati_2021_2025,
  nome_periodo = "2021-2025",
  B = B,
  livelli = livelli_var
)


# ------------------------------------------------------------
# 12. Tabelle finali
# ------------------------------------------------------------

risultati_bootstrap_var_student <- rbind(
  bootstrap_student_2018_2022$risultati,
  bootstrap_student_2021_2025$risultati
)

repliche_bootstrap_var_student <- rbind(
  bootstrap_student_2018_2022$repliche,
  bootstrap_student_2021_2025$repliche
)

# Versione in percentuale
risultati_percentuali <- risultati_bootstrap_var_student

colonne_da_percentuale <- c(
  "media_t",
  "scala_t",
  "var_student",
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

risultati_percentuali$gradi_liberta_t <-
  round(risultati_percentuali$gradi_liberta_t, 4)

risultati_percentuali$bca_z0 <- round(risultati_percentuali$bca_z0, 4)
risultati_percentuali$bca_a <- round(risultati_percentuali$bca_a, 4)

rownames(risultati_percentuali) <- NULL


# ------------------------------------------------------------
# 13. Stampa a video
# ------------------------------------------------------------

cat("\n")
cat("============================================================\n")
cat("BOOTSTRAP PARAMETRICO t DI STUDENT DEL VaR\n")
cat("Valori percentuali, dove applicabile\n")
cat("============================================================\n")

print(risultati_percentuali)


# ------------------------------------------------------------
# 14. Salvataggio tabelle
# ------------------------------------------------------------

write.csv(
  risultati_percentuali,
  file = file.path(
    cartella_tabelle,
    "bootstrap_var_parametrico_student.csv"
  ),
  row.names = FALSE
)

write.csv2(
  risultati_percentuali,
  file = file.path(
    cartella_tabelle,
    "bootstrap_var_parametrico_student_excel.csv"
  ),
  row.names = FALSE
)

write.csv(
  repliche_bootstrap_var_student,
  file = file.path(
    cartella_tabelle,
    "repliche_bootstrap_var_parametrico_student.csv"
  ),
  row.names = FALSE
)

saveRDS(
  repliche_bootstrap_var_student,
  file = file.path(
    cartella_tabelle,
    "repliche_bootstrap_var_parametrico_student.rds"
  )
)


# ------------------------------------------------------------
# 15. Funzione per grafici delle repliche bootstrap
# ------------------------------------------------------------

crea_grafico_bootstrap_student <- function(repliche, risultati, nome_periodo, livello_var, nome_file) {
  
  dati_grafico <- repliche[
    repliche$periodo == nome_periodo &
      repliche$livello == livello_var &
      !is.na(repliche$var_bootstrap),
  ]
  
  dati_grafico$var_bootstrap_pct <- 100 * dati_grafico$var_bootstrap
  
  risultato <- risultati[
    risultati$periodo == nome_periodo &
      risultati$livello == livello_var,
  ]
  
  var_originale_pct <- 100 * risultato$var_student
  
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
      paste0("bootstrap_var_student_", nome_file, ".png")
    ),
    plot = grafico,
    width = 8,
    height = 5,
    dpi = 300
  )
  
  print(grafico)
}


# ------------------------------------------------------------
# 16. Creazione grafici bootstrap
# ------------------------------------------------------------

crea_grafico_bootstrap_student(
  repliche = repliche_bootstrap_var_student,
  risultati = risultati_bootstrap_var_student,
  nome_periodo = "2018-2022",
  livello_var = "95%",
  nome_file = "2018_2022_95"
)

crea_grafico_bootstrap_student(
  repliche = repliche_bootstrap_var_student,
  risultati = risultati_bootstrap_var_student,
  nome_periodo = "2018-2022",
  livello_var = "99%",
  nome_file = "2018_2022_99"
)

crea_grafico_bootstrap_student(
  repliche = repliche_bootstrap_var_student,
  risultati = risultati_bootstrap_var_student,
  nome_periodo = "2021-2025",
  livello_var = "95%",
  nome_file = "2021_2025_95"
)

crea_grafico_bootstrap_student(
  repliche = repliche_bootstrap_var_student,
  risultati = risultati_bootstrap_var_student,
  nome_periodo = "2021-2025",
  livello_var = "99%",
  nome_file = "2021_2025_99"
)


# ------------------------------------------------------------
# 17. Messaggio finale
# ------------------------------------------------------------

cat("\n")
cat("============================================================\n")
cat("BOOTSTRAP PARAMETRICO t DI STUDENT COMPLETATO\n")
cat("============================================================\n")

cat("File creati:\n")
cat("- output/tables/bootstrap_var_parametrico_student.csv\n")
cat("- output/tables/bootstrap_var_parametrico_student_excel.csv\n")
cat("- output/tables/repliche_bootstrap_var_parametrico_student.csv\n")
cat("- output/tables/repliche_bootstrap_var_parametrico_student.rds\n")
cat("- output/figures/bootstrap_var_student_2018_2022_95.png\n")
cat("- output/figures/bootstrap_var_student_2018_2022_99.png\n")
cat("- output/figures/bootstrap_var_student_2021_2025_95.png\n")
cat("- output/figures/bootstrap_var_student_2021_2025_99.png\n")