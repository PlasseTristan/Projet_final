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
res_nul     <- calculer_scenario(null_pred, 5000, 100, 2000, stockage_nul, "No stocking")
res_present <- calculer_scenario(null_pred, 5000, 100, 2000, stockage_present, "Actual stocking")
res_propose <- calculer_scenario(null_pred, 5000, 100, 2000, stockage_propose, "Theoretical stocking")

# Fusion
df_final <- bind_rows(res_nul, res_present, res_propose)

# Sécurité log
df_final <- df_final %>% mutate(across(c(Mediane, Inf_95, Sup_95), ~ifelse(.x <= 0.1, 0.1, .x)))

ggplot(df_final, aes(x = Annee, y = Mediane, color = Scenario, fill = Scenario)) +
  # 1. Intervalle de confiance (Ruban)
  # On précise que ymin et ymax ne servent que pour le ruban
  geom_ribbon(aes(ymin = Inf_95, ymax = Sup_95), alpha = 0.2, color = NA) +
  
  # 2. Lignes médianes 
  geom_smooth(linewidth = 1, span = 0.2) +
  
  # Annotation "(a)" en haut à gauche
  annotate("text", x = 0, y = 40000, label = "(a)", size = 6, hjust = 0) +
  
  # Configuration des axes et thèmes
  scale_y_log10(
    limits = c(1, 50000),
    # Utilise l'expression mathématique 10^x
    labels = label_log(), 
    # Force les coupures à chaque puissance de 10
    breaks = trans_breaks("log10", function(x) 10^x),
    oob = scales::squish
  ) +
  scale_x_continuous(expand = c(0, 0)) +
  
  # Couleurs (Gris, Bleu, Orange)
  scale_color_manual(values = c("No stocking" = "#b00014", 
                                "Actual stocking" = "#29abe2", 
                                "Theoretical stocking" = "#43a047")) +
  scale_fill_manual(values = c("No stocking" = "#b00014", 
                               "Actual stocking" = "#29abe2", 
                               "Theoretical stocking" = "#43a047")) +
  
  theme_minimal() +
  labs(
    x = "Time(yr)",
    y = "Abundance",
    color = NULL,
    fill = NULL
    
  ) +
  theme(
    legend.position = c(0.3, 0.2), # Légende à l'intérieur (x, y)
    legend.background = element_blank(),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12, color = "black"),
    axis.line = element_line(linewidth = 0.8)
  )
