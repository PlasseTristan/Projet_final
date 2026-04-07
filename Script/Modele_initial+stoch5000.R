library(tidyverse)

### 1. Paramètres globaux ###
n_simulations <- 500
n_annees <- 100
n_stades <- nrow(null_pred)
b_params <- c(5.05, 20, 75, rep(0.75, 29)) # Tes paramètres de survie

# Matrice pour stocker uniquement les matures : 51 lignes (années 0-100) x 50 colonnes
resultats_matures <- matrix(0, nrow = n_annees + 1, ncol = n_simulations)

fig2<-function(matrice,n_iterations,n_annees, n_ini){
  
  n_stades <- nrow(matrice)
  ### 2. Boucle des 5000 simulations ###
  for (i in 1:n_iterations) {
  
  # Générer l'aléa propre à chaque simulation
  mat_fec_stoch <- fecondite_stochastique(matrice, n_annees)
  mat_surv_stoch <- survie_stochastique(matrice, n_annees)
  
  # Initialisation de la population
  sim_temp <- matrix(0, nrow = n_stades, ncol = n_annees + 1)
  pop_init <- calculer_structure_initiale(matrice, n_ini)
  pop_init[1:3] <- 0 
  sim_temp[, 1] <- pop_init
  
  # Stocker les matures à t=0
  resultats_matures[1, i] <- sum(sim_temp[13:33, 1])
  
  # Boucle de projection sur 100 ans
    for (t in 1:n_annees) {
      M_t <- matrix(0, nrow = n_stades, ncol = n_stades)
      diag(M_t[-1, -n_stades]) <- mat_surv_stoch[, t]
      
      # Combinaison des survies des 3 premiers stades
      s_an1_t <- mat_surv_stoch[1, t] * mat_surv_stoch[2, t] * mat_surv_stoch[3, t]
      # Réduction de la fécondité en fonction du taux de survie de la première année 1
      M_t[4, 13:33] <- mat_fec_stoch[13:33, t] * s_an1_t
    
      # Nettoyage des doublons
      M_t[2,1] <- 0; M_t[3,2] <- 0; M_t[4,3] <- 0
      
      # Projection
      sim_temp[, t+1] <- M_t %*% sim_temp[, t]
      
      # Stockage de la somme des matures (stades 13 à 33)
      resultats_matures[t+1, s] <- sum(sim_temp[13:33, t+1])
    }
  }

  ### 3. Préparation des statistiques pour le graphique ###
  df_stats <- data.frame(
    Annee = 0:100,
    Mediane = apply(resultats_matures, 1, median),
    Inf_95 = apply(resultats_matures, 1, quantile, probs = 0.025),
    Sup_95 = apply(resultats_matures, 1, quantile, probs = 0.975)
  )

  # Sécurité pour l'échelle log (remplacer les 0 par 0.1)
  df_stats[df_stats <= 0] <- 0.1

  ### 4. Graphique ggplot2 ###
  ggplot(df_stats, aes(x = Annee)) +
    # Intervalle de confiance (Ruban)
    geom_ribbon(aes(ymin = Inf_95, ymax = Sup_95), fill = "#3498db", alpha = 0.2) +
    
    # Ligne de la médiane
    geom_line(aes(y = Mediane), color = "#2980b9", linewidth = 1.2) +
    
    # Échelle logarithmique formatée
    scale_y_log10(
      limits = c(0.1, 10000),
      breaks = c(0.1, 1, 10, 100, 1000, 10000),
      labels = c("0", "1", "10", "100", "1 000", "10 000"),
      oob = scales::squish
    ) +
  
    theme_minimal() +
    labs(
      title = "Évolution de la population mature (n = 5000 simulations)",
      subtitle = "Médiane et intervalle de confiance à 95%",
      x = "Années",
      y = "Abondance des matures (échelle log)"
    ) +
    theme(
      panel.grid.minor = element_blank(),
      plot.title = element_text(face = "bold", size = 14)
    )
}

fig2(matrice = null_pred,n_iterations = 500,n_annees = 100,n_ini = 2000)
