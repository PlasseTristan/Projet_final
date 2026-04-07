library(tidyverse)

calculer_data_scenario <- function(matrice, nom_scenario, n_iterations = 5000, n_annees = 100) {
  n_stades <- nrow(matrice)
  minima_matures <- numeric(n_iterations)
  
  # Paramètres d'ensemencement
  n_larves <- 500000
  n_fry <- 15000
  
  for (i in 1:n_iterations) {
    # 1. Génération des paramètres stochastiques
    mat_fec_stoch <- fecondite_stochastique(matrice, n_annees)
    mat_surv_stoch <- survie_stochastique(matrice, n_annees)
    
    # 2. Initialisation (Population initiale de 2000 adultes)
    pop_temp <- calculer_structure_initiale(matrice, 2000)
    pop_temp[1:3] <- 0
    
    matures_traj <- numeric(n_annees + 1)
    matures_traj[1] <- sum(pop_temp[13:33])
    
    for (t in 1:n_annees) {
      M_t <- matrix(0, nrow = n_stades, ncol = n_stades)
      diag(M_t[-1, -n_stades]) <- mat_surv_stoch[, t]
      
      # Logique de condensation Chevalier Cuivré
      s_an1_t <- prod(mat_surv_stoch[1:3, t])
      M_t[4, 13:33] <- mat_fec_stoch[13:33, t] * s_an1_t
      M_t[2,1] <- 0; M_t[3,2] <- 0; M_t[4,3] <- 0
      
      pop_temp <- M_t %*% pop_temp
      
      # Ensemencement stochastique
      survie_stock <- (n_larves * mat_surv_stoch[2,t] * mat_surv_stoch[3,t]) + (n_fry * mat_surv_stoch[3,t])
      pop_temp[4] <- pop_temp[4] + survie_stock
      
      matures_traj[t+1] <- sum(pop_temp[13:33])
    }
    minima_matures[i] <- min(matures_traj)
  }
  
  # 3. Calcul de la probabilité et CI 70% par Bootstrap
  seuils <- seq(0, 1000, by = 20)
  n_boot <- 100
  boot_matrix <- matrix(0, nrow = length(seuils), ncol = n_boot)
  
  for(b in 1:n_boot) {
    sample_minima <- sample(minima_matures, replace = TRUE)
    boot_matrix[, b] <- sapply(seuils, function(s) sum(sample_minima < s) / n_iterations)
  }
  
  return(data.frame(
    Seuil = seuils,
    Mediane = apply(boot_matrix, 1, median),
    Inf_70 = apply(boot_matrix, 1, quantile, probs = 0.15),
    Sup_70 = apply(boot_matrix, 1, quantile, probs = 0.85),
    Scenario = nom_scenario
  ))
}
# Génération des données pour les 3 scénarios
data_null       <- calculer_data_scenario(null_pred, "Null")
data_endangered <- calculer_data_scenario(endangered, "Endangered")
data_measured   <- calculer_data_scenario(measured, "Measured")

# Fusion des données
df_final <- rbind(data_null, data_endangered, data_measured)

# Création de la figure
ggplot(df_final, aes(x = Seuil, group = Scenario)) +
  # Rubans de confiance 70%
  geom_ribbon(aes(ymin = Inf_70, ymax = Sup_70, fill = Scenario), alpha = 0.25) +
  # Lignes de Médiane
  geom_line(aes(y = Mediane, color = Scenario), linewidth = 1) +
  
  # Couleurs fidèles à l'image originale (Noir, Rouge, Bleu)
  scale_color_manual(values = c("Null" = "black", "Endangered" = "red", "Measured" = "blue")) +
  scale_fill_manual(values = c("Null" = "black", "Endangered" = "red", "Measured" = "blue")) +
  
  # Configuration des axes
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2), expand = c(0,0)) +
  scale_x_continuous(limits = c(0, 1000), expand = c(0,0)) +
  
  # Esthétique
  theme_bw() +
  labs(
    x = "Threshold value (Number of mature individuals)",
    y = "Average probability of being below\nthreshold value in the next 100 years",
    title = "Comparison of Stochastic Population Scenarios",
    subtitle = "Lines: Median of 5000 iterations; Envelopes: 70% CIs"
  ) +
  theme(
    legend.position = c(0.85, 0.2),
    legend.background = element_rect(fill = "white", color = "black"),
    panel.grid.minor = element_blank()
  )