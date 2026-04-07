library(scales)


calculer_scenario <- function(matrice, n_iterations, n_annees, n_ini, stockage, nom_scenario) {
  n_larves <- stockage[1]
  n_fry <- stockage[2]
  resultats_matures <- matrix(0, nrow = n_annees + 1, ncol = n_iterations)
  n_stades <- nrow(matrice)
  
  for (i in 1:n_iterations) {
    mat_fec_stoch <- fecondite_stochastique(matrice, n_annees)
    mat_surv_stoch <- survie_stochastique(matrice, n_annees)
    
    sim_temp <- matrix(0, nrow = n_stades, ncol = n_annees + 1)
    pop_init <- calculer_structure_initiale(matrice, n_ini)
    pop_init[1:3] <- 0 
    sim_temp[, 1] <- pop_init
    
    resultats_matures[1, i] <- sum(sim_temp[13:33, 1])
    
    for (t in 1:n_annees) {
      M_t <- matrix(0, nrow = n_stades, ncol = n_stades)
      diag(M_t[-1, -n_stades]) <- mat_surv_stoch[, t]
      s_an1_t <- mat_surv_stoch[1, t] * mat_surv_stoch[2, t] * mat_surv_stoch[3, t]
      M_t[4, 13:33] <- mat_fec_stoch[13:33, t] * s_an1_t
      M_t[2,1] <- 0; M_t[3,2] <- 0; M_t[4,3] <- 0
      
      sim_temp[, t+1] <- M_t %*% sim_temp[, t]
      
      # Apport
      apport_stade4 <- (n_larves * mat_surv_stoch[2, t] * mat_surv_stoch[3, t]) + 
        (n_fry * mat_surv_stoch[3, t])
      sim_temp[4, t+1] <- sim_temp[4, t+1] + apport_stade4
      
      resultats_matures[t+1, i] <- sum(sim_temp[13:33, t+1])
    }
  }
  
  # Retourne un data.frame formaté
  data.frame(
    Annee = 0:n_annees,
    Mediane = apply(resultats_matures, 1, median),
    Inf_95 = apply(resultats_matures, 1, quantile, probs = 0.025),
    Sup_95 = apply(resultats_matures, 1, quantile, probs = 0.975),
    Scenario = nom_scenario
  )
}

# Définition des scénarios
stockage_nul      <- c(0, 0)
stockage_present  <- c(231185, 14868)
stockage_propose  <- c(500000, 15000)

# Calculs
res_nul     <- calculer_scenario(null_pred, 5000, 100, 2000, stockage_nul, "Nul (0)")
res_present <- calculer_scenario(null_pred, 5000, 100, 2000, stockage_present, "Actuel (231k/14k)")
res_propose <- calculer_scenario(null_pred, 5000, 100, 2000, stockage_propose, "Proposé (500k/15k)")

# Fusion
df_final <- bind_rows(res_nul, res_present, res_propose)

# Sécurité log
df_final <- df_final %>% mutate(across(c(Mediane, Inf_95, Sup_95), ~ifelse(.x <= 0.1, 0.1, .x)))

ggplot(df_final, aes(x = Annee, y = Mediane, color = Scenario, fill = Scenario)) +
  # 1. Intervalle de confiance (Ruban)
  # On précise que ymin et ymax ne servent que pour le ruban
  geom_ribbon(aes(ymin = Inf_95, ymax = Sup_95), alpha = 0.15, color = NA) +
  
  # 2. Lignes médianes 
  # Maintenant, il trouve "y" automatiquement dans l'aes principal
  geom_line(linewidth = 1) +
  
  # Configuration des axes et thèmes
  scale_y_log10(
    limits = c(1, 12000),
    # Utilise l'expression mathématique 10^x
    labels = label_log(), 
    # Force les coupures à chaque puissance de 10
    breaks = trans_breaks("log10", function(x) 10^x),
    oob = scales::squish
  ) +
  
  # Couleurs (Gris, Bleu, Orange)
  scale_color_manual(values = c("Nul (0)" = "red", 
                                "Actuel (231k/14k)" = "steelblue", 
                                "Proposé (500k/15k)" = "green")) +
  scale_fill_manual(values = c("Nul (0)" = "red", 
                               "Actuel (231k/14k)" = "steelblue", 
                               "Proposé (500k/15k)" = "green")) +
  
  theme_minimal() +
  labs(
    title = "Comparaison des scénarios d'ensemencement",
    subtitle = "Évolution de la population mature (Médiane et IC 95%)",
    x = "Années",
    y = "Abondance (échelle log)",
    color = "Scénario",
    fill = "Scénario"
  ) +
  theme(
    legend.position = "bottom",
    panel.grid.minor = element_blank()
  )
