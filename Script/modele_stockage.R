library(tidyverse)

stockage_nul <- c(0,0)
stockage_present <-c(231185,14868)
stockage_propose<-c(500000,15000)

fig2 <- function(matrice, n_iterations, n_annees, n_ini, stockage) {
  # Paramètres d'ensemencement
  n_larves <- stockage[1] # Stade 2
  n_fry <- stockage[2]    # Stade 3
  
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
      
      # 1. Calcul de la survie cumulée pour la reproduction naturelle
      s_an1_t <- mat_surv_stoch[1, t] * mat_surv_stoch[2, t] * mat_surv_stoch[3, t]
      M_t[4, 13:33] <- mat_fec_stoch[13:33, t] * s_an1_t
      
      # Nettoyage des transitions intermédiaires (car on saute vers le stade 4)
      M_t[2,1] <- 0; M_t[3,2] <- 0; M_t[4,3] <- 0
      
      # 2. Projection de la population existante
      sim_temp[, t+1] <- M_t %*% sim_temp[, t]
      
      # 3. Ajout de l'ensemencement (Stocking)
      # On calcule combien de larves et fry ensemencés atteignent le stade 4
      survie_larves_vers_stade4 <- mat_surv_stoch[2, t] * mat_surv_stoch[3, t]
      survie_fry_vers_stade4 <- mat_surv_stoch[3, t]
      
      apport_stade4 <- (n_larves * survie_larves_vers_stade4) + (n_fry * survie_fry_vers_stade4)
      
      # On injecte ces individus directement dans le stade 4 au temps t+1
      sim_temp[4, t+1] <- sim_temp[4, t+1] + apport_stade4
      
      resultats_matures[t+1, i] <- sum(sim_temp[13:33, t+1])
    }
  }
  
  # --- Suite du code (Stats et Ggplot) identique ---
  df_stats <- data.frame(
    Annee = 0:n_annees,
    Mediane = apply(resultats_matures, 1, median),
    Inf_95 = apply(resultats_matures, 1, quantile, probs = 0.025),
    Sup_95 = apply(resultats_matures, 1, quantile, probs = 0.975)
  )
  
  df_stats[df_stats <= 0.1] <- 0.1
  
  ggplot(df_stats, aes(x = Annee)) +
    geom_ribbon(aes(ymin = Inf_95, ymax = Sup_95), fill = "#e67e22", alpha = 0.2) + # Changé en orange pour varier
    geom_line(aes(y = Mediane), color = "#d35400", linewidth = 1.2) +
    scale_y_log10(
      limits = c(0.1, 100000), # Augmenté car l'ensemencement risque de booster la pop
      oob = scales::squish
    ) +
    theme_minimal() +
    labs(
      title = "Évolution de la population avec ensemencement annuel",
      subtitle = paste("Apport :", n_larves, "larves et", n_fry, "fry / an"),
      x = "Années",
      y = "Abondance des matures (log)"
    )
}

fig2(matrice = null_pred,n_iterations = 500,n_annees = 100,n_ini = 2000)
