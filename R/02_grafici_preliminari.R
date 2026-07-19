# ============================================================
# Script 02:
# - importazione dei dataset elaborati;
# - grafici preliminari dei prezzi;
# - grafici delle perdite giornaliere;
# - istogrammi delle perdite;
# - QQ-plot rispetto alla normale;
# - confronto tra le distribuzioni delle perdite.
# ============================================================


# ------------------------------------------------------------
# 1. Pacchetto necessario
# ------------------------------------------------------------

library(ggplot2)


colore_linea <- "#1F2937"       # grigio-blu scuro
colore_blu <- "#2563EB"         # blu sobrio
colore_azzurro <- "#A7C7E7"     # azzurro tenue
colore_petrolio <- "#0F766E"    # verde petrolio
colore_grigio <- "#4B5563"      # grigio freddo
colore_zero <- "#111827"        # quasi nero

# ------------------------------------------------------------
# 2. Percorsi dei file
# ------------------------------------------------------------

file_2018_2022 <- "data/processed/sp500_2018_2022_losses.csv"
file_2021_2025 <- "data/processed/sp500_2021_2025_losses.csv"

cartella_figure <- "output/figures"

if (!dir.exists(cartella_figure)) {
  dir.create(cartella_figure, recursive = TRUE)
}


# ------------------------------------------------------------
# 3. Funzione per importare i dati elaborati
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
  
  # Rendimenti e perdite in percentuale,
  # solo per rendere i grafici più leggibili.
  dati$log_return_pct <- 100 * dati$log_return
  dati$loss_pct <- 100 * dati$loss
  
  return(dati)
}


# ------------------------------------------------------------
# 4. Importazione dei due periodi
# ------------------------------------------------------------

dati_2018_2022 <- importa_dati_elaborati(file_2018_2022)
dati_2021_2025 <- importa_dati_elaborati(file_2021_2025)


# ------------------------------------------------------------
# 5. Funzione per creare i grafici di un periodo
# ------------------------------------------------------------

crea_grafici_periodo <- function(dati, nome_periodo, nome_file) {
  
  # ----------------------------------------------------------
  # Grafico 1: prezzi di chiusura
  # ----------------------------------------------------------
  
  grafico_prezzi <- ggplot(dati, aes(x = Date, y = Close)) +
    geom_line(linewidth = 0.45,  color = colore_blu) +
    labs(
    
      x = "Data",
      y = "Prezzo di chiusura"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      axis.title.x = element_text(face = "bold", margin = margin(t = 10)),
      axis.title.y = element_text(face = "bold", margin = margin(r = 10))
    )
  ggsave(
    filename = file.path(
      cartella_figure,
      paste0("prezzi_sp500_", nome_file, ".png")
    ),
    plot = grafico_prezzi,
    width = 10,
    height = 5,
    dpi = 300
  )
  
  
  # ----------------------------------------------------------
  # Grafico 2: rendimenti logaritmici giornalieri
  # ----------------------------------------------------------
  
  grafico_rendimenti <- ggplot(dati, aes(x = Date, y = log_return_pct)) +
    geom_hline(yintercept = 0, linewidth = 0.4,  color = colore_zero) +
    geom_line(linewidth = 0.35,  color = colore_grigio) +
    labs(
      x = "Data",
      y = "Rendimento logaritmico (%)"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      axis.title.x = element_text(face = "bold", margin = margin(t = 10)),
      axis.title.y = element_text(face = "bold", margin = margin(r = 10))
    )
  ggsave(
    filename = file.path(
      cartella_figure,
      paste0("rendimenti_sp500_", nome_file, ".png")
    ),
    plot = grafico_rendimenti,
    width = 10,
    height = 5,
    dpi = 300
  )
  
  
  # ----------------------------------------------------------
  # Grafico 3: perdite giornaliere
  # ----------------------------------------------------------
  
  grafico_perdite <- ggplot(dati, aes(x = Date, y = loss_pct)) +
    geom_hline(yintercept = 0, linewidth = 0.4,  color = colore_zero) +
    geom_line(linewidth = 0.35,  color = colore_grigio) +
    labs(
      x = "Data",
      y = "Perdita relativa (%)"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      axis.title.x = element_text(face = "bold", margin = margin(t = 10)),
      axis.title.y = element_text(face = "bold", margin = margin(r = 10))
    )
  ggsave(
    filename = file.path(
      cartella_figure,
      paste0("perdite_sp500_", nome_file, ".png")
    ),
    plot = grafico_perdite,
    width = 10,
    height = 5,
    dpi = 300
  )
  
  
  # ----------------------------------------------------------
  # Grafico 4: istogramma delle perdite
  # ----------------------------------------------------------
  
  grafico_istogramma <- ggplot(dati, aes(x = loss_pct)) +
    geom_histogram(
      bins = 50,
      fill = colore_azzurro,
      color = "white"
    ) +
    geom_vline(
      xintercept = 0,
      linetype = "dashed",
      linewidth = 0.5,
      color = colore_zero
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
      paste0("istogramma_perdite_", nome_file, ".png")
    ),
    plot = grafico_istogramma,
    width = 8,
    height = 5,
    dpi = 300
  )
  
  
  # ----------------------------------------------------------
  # Grafico 5: densità delle perdite
  # ----------------------------------------------------------
  
  grafico_densita <- ggplot(dati, aes(x = loss_pct)) +
    geom_density(linewidth = 0.8,  color = colore_blu) +
    geom_vline(
      xintercept = 0,
      linetype = "dashed",
      linewidth = 0.5
    ) +
    labs(
     
      x = "Perdita relativa (%)",
      y = "Densità"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      axis.title.x = element_text(face = "bold", margin = margin(t = 10)),
      axis.title.y = element_text(face = "bold", margin = margin(r = 10))
    )
  ggsave(
    filename = file.path(
      cartella_figure,
      paste0("densita_perdite_", nome_file, ".png")
    ),
    plot = grafico_densita,
    width = 8,
    height = 5,
    dpi = 300
  )
  
  
  # ----------------------------------------------------------
  # Grafico 6: QQ-plot rispetto alla normale
  # ----------------------------------------------------------
  
  grafico_qqplot <- ggplot(dati, aes(sample = loss_pct)) +
    stat_qq(size = 1.1, alpha = 0.65,  color = colore_blu) +
    stat_qq_line(linewidth = 0.6,  color = colore_zero) +
    labs(
      x = "Quantili teorici della normale",
      y = "Quantili empirici delle perdite (%)"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      axis.title.x = element_text(face = "bold", margin = margin(t = 10)),
      axis.title.y = element_text(face = "bold", margin = margin(r = 10))
    )
  ggsave(
    filename = file.path(
      cartella_figure,
      paste0("qqplot_perdite_", nome_file, ".png")
    ),
    plot = grafico_qqplot,
    width = 7,
    height = 6,
    dpi = 300
  )
  
  
  # Mostra i grafici nel pannello Plots di RStudio
  print(grafico_prezzi)
  print(grafico_rendimenti)
  print(grafico_perdite)
  print(grafico_istogramma)
  print(grafico_densita)
  print(grafico_qqplot)
}


# ------------------------------------------------------------
# 6. Creazione dei grafici per i due periodi
# ------------------------------------------------------------

crea_grafici_periodo(
  dati = dati_2018_2022,
  nome_periodo = "2018-2022",
  nome_file = "2018_2022"
)

crea_grafici_periodo(
  dati = dati_2021_2025,
  nome_periodo = "2021-2025",
  nome_file = "2021_2025"
)


# ------------------------------------------------------------
# 7. Grafico di confronto tra le densità delle perdite
# ------------------------------------------------------------

dati_2018_2022$periodo <- "2018-2022"
dati_2021_2025$periodo <- "2021-2025"

dati_confronto <- rbind(
  dati_2018_2022,
  dati_2021_2025
)

grafico_confronto_densita <- ggplot(
  dati_confronto,
  aes(x = loss_pct, color = periodo, fill = periodo)
) +
  geom_density(alpha = 0.15, linewidth = 0.8) +
  geom_vline(
    xintercept = 0,
    linetype = "dashed",
    linewidth = 0.5
  ) +
  labs(
    x = "Perdita relativa (%)",
    y = "Densità",
    color = "Periodo",
    fill = "Periodo"
  ) +
  scale_color_manual(
    values = c(
      "2018-2022" = colore_blu,
      "2021-2025" = colore_petrolio
    )
  ) +
  scale_fill_manual(
    values = c(
      "2018-2022" = colore_blu,
      "2021-2025" = colore_petrolio
    )
  ) +
  theme_minimal(base_size = 12) +
  theme(
    axis.title.x = element_text(face = "bold", margin = margin(t = 10)),
    axis.title.y = element_text(face = "bold", margin = margin(r = 10))
  )
ggsave(
  filename = file.path(
    cartella_figure,
    "confronto_densita_perdite.png"
  ),
  plot = grafico_confronto_densita,
  width = 9,
  height = 5,
  dpi = 300
)

print(grafico_confronto_densita)


# ------------------------------------------------------------
# 8. Grafico di confronto delle perdite nel tempo
# ------------------------------------------------------------

grafico_confronto_perdite <- ggplot(
  dati_confronto,
  aes(x = Date, y = loss_pct)
) +
  geom_hline(yintercept = 0, linewidth = 0.4, color = colore_zero) +
  geom_line(linewidth = 0.3, color = colore_grigio) +
  facet_wrap(~ periodo, ncol = 1, scales = "free_x") +
  labs(
    x = "Data",
    y = "Perdita relativa (%)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    axis.title.x = element_text(face = "bold", margin = margin(t = 10)),
    axis.title.y = element_text(face = "bold", margin = margin(r = 10))
  )
ggsave(
  filename = file.path(
    cartella_figure,
    "confronto_perdite_periodi.png"
  ),
  plot = grafico_confronto_perdite,
  width = 10,
  height = 7,
  dpi = 300
)

print(grafico_confronto_perdite)


# ------------------------------------------------------------
# 9. Messaggio finale
# ------------------------------------------------------------

cat("\n")
cat("============================================================\n")
cat("GRAFICI PRELIMINARI CREATI CORRETTAMENTE\n")
cat("============================================================\n")

cat("I grafici sono stati salvati nella cartella:\n")
cat("output/figures/\n")