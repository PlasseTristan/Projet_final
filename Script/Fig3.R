library(tidyverse)

# ==========================================================
# 1. SIMULATION
# ==========================================================
simuler_trajectoires_adultes <- function(matrice, n_iterations, n_annees = 100) {
  n_stades <- nrow(matrice)
  historique <- matrix(0, nrow = n_iterations, ncol = n_annees + 1)
  
  n_larves <- 0
  n_fry    <- 0
  
  for (i in 1:n_iterations) {
    mat_fec  <- fecondite_stochastique(matrice, n_annees)
    mat_surv <- survie_stochastique(matrice, n_annees)
    
    pop <- calculer_structure_initiale(matrice, 1000)
    pop[1:3] <- 0
    
    historique[i, 1] <- sum(pop[13:33])
    
    for (t in 1:n_annees) {
      M_t <- matrix(0, nrow = n_stades, ncol = n_stades)
      
      # Bug fix : 32 valeurs dans 32 slots
      diag(M_t[-1, -n_stades]) <- mat_surv[1:(n_stades - 1), t]
      
      s_an1_t <- prod(mat_surv[1:3, t])
      M_t[4, 13:33] <- mat_fec[13:33, t] * s_an1_t
      M_t[2,1] <- 0; M_t[3,2] <- 0; M_t[4,3] <- 0
      
      pop <- M_t %*% pop
      
      #pop[4] <- pop[4] + (n_larves * mat_surv[2, t] * mat_surv[3, t]) +
        (n_fry   * mat_surv[3, t])
      
      pop[4] <- pop[4] +(pop[1]*mat_surv[1,t]* mat_surv[2, t] * mat_surv[3, t])+ ((pop[2]+n_larves) * mat_surv[2, t] * mat_surv[3, t]) +
        ((pop[3]+n_fry)   * mat_surv[3, t])
      
      historique[i, t + 1] <- sum(pop[13:33])
    }
  }
  return(historique)
}

# ==========================================================
# 2. STATISTIQUES (quantiles directs sur 5000 simulations)
# ==========================================================
calculer_stats <- function(historique, nom_scenario) {
  seuils <- seq(0, 1000, by = 1)
  n_sims <- nrow(historique)
  
  temps_sous_seuil <- matrix(0, nrow = n_sims, ncol = length(seuils))
  for (j in seq_along(seuils)) {
    temps_sous_seuil[, j] <- rowMeans(historique < seuils[j])
  }
  
  data.frame(
    Threshold = seuils,
    Median    = apply(temps_sous_seuil, 2, median),
    Inf_70    = apply(temps_sous_seuil, 2, quantile, probs = 0.15),
    Sup_70    = apply(temps_sous_seuil, 2, quantile, probs = 0.85),
    Scenario  = factor(nom_scenario, levels = c("Null", "Endangered", "Measured"))
  )
}

# ==========================================================
# 3. EXÉCUTION
# ==========================================================
hist_null <- simuler_trajectoires_adultes(null_pred,  5000)
hist_end  <- simuler_trajectoires_adultes(endangered, 5000)
hist_meas <- simuler_trajectoires_adultes(measured,   5000)

df_final <- bind_rows(
  calculer_stats(hist_null, "Null"),
  calculer_stats(hist_end,  "Endangered"),
  calculer_stats(hist_meas, "Measured")
)

# ==========================================================
# 4. LISSAGE AVEC ANCRAGE À (0, 0)
# ==========================================================
df_final$Scenario <- factor(df_final$Scenario, levels = c("Measured", "Endangered", "Null"))

df_smoothed <- df_final %>%
  group_by(Scenario) %>%
  mutate(
    Median = predict(loess(Median ~ Threshold, span = 0.15)),
    Inf_70 = predict(loess(Inf_70 ~ Threshold, span = 0.15)),
    Sup_70 = predict(loess(Sup_70 ~ Threshold, span = 0.15)),
   
    # Empêche les valeurs négatives dues au lissage
    Median = pmax(Median, 0),
    Inf_70 = pmax(Inf_70, 0),
    Sup_70 = pmax(Sup_70, 0)
  ) %>%
  ungroup()

# ==========================================================
# 5. GRAPHIQUE
# ==========================================================
p <- ggplot(df_smoothed, aes(x = Threshold, group = Scenario)) +
  
  geom_ribbon(aes(ymin = Inf_70, ymax = Sup_70, fill = Scenario),
              alpha = 0.25, show.legend = FALSE) +
  
  geom_line(aes(y = Median, color = Scenario), linewidth = 1.1) +
  
  scale_color_manual(
    values = c("Null" = "black", "Endangered" = "red", "Measured" = "blue"),
    breaks = c("Null", "Endangered", "Measured")
  ) +
  scale_fill_manual(
    values = c("Null" = "black", "Endangered" = "red", "Measured" = "blue"),
    guide  = "none"
  ) +
  
  scale_y_continuous(
    limits = c(0, 1.01),
    breaks = seq(0, 1, 0.2),
    expand = c(0, 0),
    labels = function(x) sprintf("%.1f", x)
  ) +
  scale_x_continuous(
    limits = c(0, 1000),
    breaks = seq(0, 1000, 200),
    expand = c(0, 0)
  ) +
  
  theme_classic() +
  labs(
    x = "Threshold value (Number of mature individuals)",
    y = "Average probability of being below\nthreshold value in the next 100 years"
  ) +
  theme(
    text              = element_text(family = "serif"),
    panel.border      = element_rect(colour = "black", fill = NA, linewidth = 1),
    axis.line         = element_blank(),
    legend.position   = c(0.80, 0.25),
    legend.title      = element_blank(),
    legend.text       = element_text(size = 11),
    legend.key        = element_blank(),
    legend.background = element_rect(fill = "white", color = NA),
    axis.text         = element_text(color = "black", size = 11),
    axis.title        = element_text(size = 12),
    plot.margin       = margin(10, 20, 10, 10)
  )

print(p)
ggsave("figure3.png", plot = p, width = 6, height = 5, dpi = 300)