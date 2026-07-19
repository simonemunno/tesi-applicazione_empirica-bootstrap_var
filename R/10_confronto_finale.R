# ============================================================
# Script 10:
# - confronto finale tra VaR storico, VaR normale e VaR t di Student;
# - costruzione di tabelle riassuntive;
# - grafici finali con VaR puntuali, errori standard e intervalli BCa.
# ============================================================


# ------------------------------------------------------------
# 1. Pacchetto necessario
# ------------------------------------------------------------

library(ggplot2)


# ------------------------------------------------------------
# 2. Percorsi dei file
# ------------------------------------------------------------

file_boot_storico <- "output/tables/bootstrap_var_storico.csv"
file_boot_normale <- "output/tables/bootstrap_var_parametrico_normale.csv"
file_boot_student <- "output/tables/bootstrap_var_parametrico_student.csv"

cartella_tabelle <- "output/tables"
cartella_figure <- "output/figures"

if (!dir.exists(cartella_tabelle)) {
  dir.create(cartella_tabelle, recursive = TRUE)
}

if (!dir.exists(cartella_figure)) {
  dir.create(cartella_figure, recursive = TRUE)
}


# ------------------------------------------------------------
# 3. Controllo esistenza dei file
# ------------------------------------------------------------

file_necessari <- c(
  file_boot_storico,
  file_boot_normale,
  file_boot_student
)

for (file in file_necessari) {
  if (!file.exists(file)) {
    stop(
      paste0(
        "File non trovato: ", file,
        "\nEsegui prima gli script 05, 07 e 09."
      )
    )
  }
}


# ------------------------------------------------------------
# 4. Importazione delle tabelle bootstrap
# ------------------------------------------------------------

boot_storico <- read.csv(
  file = file_boot_storico,
  stringsAsFactors = FALSE
)

boot_normale <- read.csv(
  file = file_boot_normale,
  stringsAsFactors = FALSE
)

boot_student <- read.csv(
  file = file_boot_student,
  stringsAsFactors = FALSE
)


# ------------------------------------------------------------
# 5. Costruzione tabella uniforme
# ------------------------------------------------------------

tabella_storico <- data.frame(
  periodo = boot_storico$periodo,
  livello = boot_storico$livello,
  metodo = "Storico",
  B = boot_storico$B,
  var = boot_storico$var_storico,
  bias_bootstrap = boot_storico$bias_bootstrap,
  errore_standard_bootstrap = boot_storico$errore_standard_bootstrap,
  normale_inf = boot_storico$normale_inf,
  normale_sup = boot_storico$normale_sup,
  percentile_inf = boot_storico$percentile_inf,
  percentile_sup = boot_storico$percentile_sup,
  basic_inf = boot_storico$basic_inf,
  basic_sup = boot_storico$basic_sup,
  bca_inf = boot_storico$bca_inf,
  bca_sup = boot_storico$bca_sup
)

tabella_normale <- data.frame(
  periodo = boot_normale$periodo,
  livello = boot_normale$livello,
  metodo = "Normale",
  B = boot_normale$B,
  var = boot_normale$var_normale,
  bias_bootstrap = boot_normale$bias_bootstrap,
  errore_standard_bootstrap = boot_normale$errore_standard_bootstrap,
  normale_inf = boot_normale$normale_inf,
  normale_sup = boot_normale$normale_sup,
  percentile_inf = boot_normale$percentile_inf,
  percentile_sup = boot_normale$percentile_sup,
  basic_inf = boot_normale$basic_inf,
  basic_sup = boot_normale$basic_sup,
  bca_inf = boot_normale$bca_inf,
  bca_sup = boot_normale$bca_sup
)

tabella_student <- data.frame(
  periodo = boot_student$periodo,
  livello = boot_student$livello,
  metodo = "t di Student",
  B = boot_student$B_effettivo,
  var = boot_student$var_student,
  bias_bootstrap = boot_student$bias_bootstrap,
  errore_standard_bootstrap = boot_student$errore_standard_bootstrap,
  normale_inf = boot_student$normale_inf,
  normale_sup = boot_student$normale_sup,
  percentile_inf = boot_student$percentile_inf,
  percentile_sup = boot_student$percentile_sup,
  basic_inf = boot_student$basic_inf,
  basic_sup = boot_student$basic_sup,
  bca_inf = boot_student$bca_inf,
  bca_sup = boot_student$bca_sup
)

confronto_finale <- rbind(
  tabella_storico,
  tabella_normale,
  tabella_student
)


# ------------------------------------------------------------
# 6. Ordinamento dei metodi e dei livelli
# ------------------------------------------------------------

confronto_finale$metodo <- factor(
  confronto_finale$metodo,
  levels = c("Storico", "Normale", "t di Student")
)

confronto_finale$livello <- factor(
  confronto_finale$livello,
  levels = c("95%", "99%")
)

confronto_finale$periodo <- factor(
  confronto_finale$periodo,
  levels = c("2018-2022", "2021-2025")
)

confronto_finale <- confronto_finale[
  order(
    confronto_finale$periodo,
    confronto_finale$livello,
    confronto_finale$metodo
  ),
]

rownames(confronto_finale) <- NULL


# ------------------------------------------------------------
# 7. Arrotondamento tabella finale
# ------------------------------------------------------------

colonne_numeriche <- c(
  "var",
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

confronto_finale[, colonne_numeriche] <-
  round(confronto_finale[, colonne_numeriche], 4)


# ------------------------------------------------------------
# 8. Tabelle compatte per la tesi
# ------------------------------------------------------------

tabella_var_puntuali <- confronto_finale[, c(
  "periodo",
  "livello",
  "metodo",
  "B",
  "var",
  "bias_bootstrap",
  "errore_standard_bootstrap"
)]

tabella_intervalli_bca <- confronto_finale[, c(
  "periodo",
  "livello",
  "metodo",
  "var",
  "bca_inf",
  "bca_sup"
)]

tabella_intervalli_tutti <- confronto_finale[, c(
  "periodo",
  "livello",
  "metodo",
  "var",
  "normale_inf",
  "normale_sup",
  "percentile_inf",
  "percentile_sup",
  "basic_inf",
  "basic_sup",
  "bca_inf",
  "bca_sup"
)]


# ------------------------------------------------------------
# 9. Stampa a video
# ------------------------------------------------------------

cat("\n")
cat("============================================================\n")
cat("CONFRONTO FINALE TRA METODI DI STIMA DEL VaR\n")
cat("Valori percentuali\n")
cat("============================================================\n")

print(tabella_var_puntuali)

cat("\n")
cat("============================================================\n")
cat("INTERVALLI BCa\n")
cat("Valori percentuali\n")
cat("============================================================\n")

print(tabella_intervalli_bca)


# ------------------------------------------------------------
# 10. Salvataggio tabelle
# ------------------------------------------------------------

write.csv(
  confronto_finale,
  file = file.path(
    cartella_tabelle,
    "confronto_finale_completo.csv"
  ),
  row.names = FALSE
)

write.csv2(
  confronto_finale,
  file = file.path(
    cartella_tabelle,
    "confronto_finale_completo_excel.csv"
  ),
  row.names = FALSE
)

write.csv(
  tabella_var_puntuali,
  file = file.path(
    cartella_tabelle,
    "confronto_var_puntuali.csv"
  ),
  row.names = FALSE
)

write.csv2(
  tabella_var_puntuali,
  file = file.path(
    cartella_tabelle,
    "confronto_var_puntuali_excel.csv"
  ),
  row.names = FALSE
)

write.csv(
  tabella_intervalli_bca,
  file = file.path(
    cartella_tabelle,
    "confronto_intervalli_bca.csv"
  ),
  row.names = FALSE
)

write.csv2(
  tabella_intervalli_bca,
  file = file.path(
    cartella_tabelle,
    "confronto_intervalli_bca_excel.csv"
  ),
  row.names = FALSE
)

write.csv(
  tabella_intervalli_tutti,
  file = file.path(
    cartella_tabelle,
    "confronto_intervalli_tutti.csv"
  ),
  row.names = FALSE
)

write.csv2(
  tabella_intervalli_tutti,
  file = file.path(
    cartella_tabelle,
    "confronto_intervalli_tutti_excel.csv"
  ),
  row.names = FALSE
)


# ------------------------------------------------------------
# 11. Grafico 1: confronto dei VaR puntuali
# ------------------------------------------------------------

grafico_var_puntuali <- ggplot(
  confronto_finale,
  aes(x = metodo, y = var)
) +
  geom_col(
    fill = "#A7C7E7",
    color = "white"
  ) +
  facet_grid(
    periodo ~ livello
  ) +
  labs(
    x = "Metodo di stima",
    y = "VaR (%)",
    
  ) +
  theme_minimal(base_size = 12) +
  theme(
    axis.title.x = element_text(face = "bold", margin = margin(t = 10)),
    axis.title.y = element_text(face = "bold", margin = margin(r = 10))
  )

ggsave(
  filename = file.path(
    cartella_figure,
    "confronto_var_puntuali.png"
  ),
  plot = grafico_var_puntuali,
  width = 9,
  height = 6,
  dpi = 300
)

print(grafico_var_puntuali)


# ------------------------------------------------------------
# 12. Grafico 2: confronto degli errori standard bootstrap
# ------------------------------------------------------------

grafico_errori_standard <- ggplot(
  confronto_finale,
  aes(x = metodo, y = errore_standard_bootstrap)
) +
  geom_col(
    fill = "#A7C7E7",
    color = "white"
  ) +
  facet_grid(
    periodo ~ livello
  ) +
  labs(
    x = "Metodo di stima",
    y = "Errore standard bootstrap (%)",
  ) +
  theme_minimal(base_size = 12) +
  theme(
    axis.title.x = element_text(face = "bold", margin = margin(t = 10)),
    axis.title.y = element_text(face = "bold", margin = margin(r = 10))
  )

ggsave(
  filename = file.path(
    cartella_figure,
    "confronto_errori_standard_bootstrap.png"
  ),
  plot = grafico_errori_standard,
  width = 9,
  height = 6,
  dpi = 300
)

print(grafico_errori_standard)


# ------------------------------------------------------------
# 13. Grafico 3: confronto degli intervalli BCa
# ------------------------------------------------------------

grafico_intervalli_bca <- ggplot(
  confronto_finale,
  aes(
    x = metodo,
    y = var,
    ymin = bca_inf,
    ymax = bca_sup
  )
) +
  geom_pointrange(
    linewidth = 0.7
  ) +
  facet_grid(
    periodo ~ livello
  ) +
  labs(
    x = "Metodo di stima",
    y = "VaR (%)",
  ) +
  theme_minimal(base_size = 12) +
  theme(
    axis.title.x = element_text(face = "bold", margin = margin(t = 10)),
    axis.title.y = element_text(face = "bold", margin = margin(r = 10))
  )

ggsave(
  filename = file.path(
    cartella_figure,
    "confronto_intervalli_bca.png"
  ),
  plot = grafico_intervalli_bca,
  width = 9,
  height = 6,
  dpi = 300
)

print(grafico_intervalli_bca)


# ------------------------------------------------------------
# 14. Grafico 4: intervalli BCa solo al 99%
# ------------------------------------------------------------

dati_99 <- confronto_finale[
  confronto_finale$livello == "99%",
]

grafico_intervalli_bca_99 <- ggplot(
  dati_99,
  aes(
    x = metodo,
    y = var,
    ymin = bca_inf,
    ymax = bca_sup
  )
) +
  geom_pointrange(
    linewidth = 0.7
  ) +
  facet_wrap(
    ~ periodo,
    nrow = 1
  ) +
  labs(
    x = "Metodo di stima",
    y = "VaR (%)",
  )
theme_minimal(base_size = 12) +
  theme(
    axis.title.x = element_text(face = "bold", size = 13, margin = margin(t = 10)),
    axis.title.y = element_text(face = "bold", size = 13, margin = margin(r = 10)),
    strip.text = element_text(face = "bold", size = 12)
  )
ggsave(
  filename = file.path(
    cartella_figure,
    "confronto_intervalli_bca_99.png"
  ),
  plot = grafico_intervalli_bca_99,
  width = 9,
  height = 5,
  dpi = 300
)

print(grafico_intervalli_bca_99)


# ------------------------------------------------------------
# 15. Messaggio finale
# ------------------------------------------------------------

cat("\n")
cat("============================================================\n")
cat("CONFRONTO FINALE COMPLETATO\n")
cat("============================================================\n")

cat("File creati:\n")
cat("- output/tables/confronto_finale_completo.csv\n")
cat("- output/tables/confronto_finale_completo_excel.csv\n")
cat("- output/tables/confronto_var_puntuali.csv\n")
cat("- output/tables/confronto_var_puntuali_excel.csv\n")
cat("- output/tables/confronto_intervalli_bca.csv\n")
cat("- output/tables/confronto_intervalli_bca_excel.csv\n")
cat("- output/tables/confronto_intervalli_tutti.csv\n")
cat("- output/tables/confronto_intervalli_tutti_excel.csv\n")
cat("- output/figures/confronto_var_puntuali.png\n")
cat("- output/figures/confronto_errori_standard_bootstrap.png\n")
cat("- output/figures/confronto_intervalli_bca.png\n")
cat("- output/figures/confronto_intervalli_bca_99.png\n")