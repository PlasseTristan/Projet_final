fig_3 <- function(matrice, n_iterations, n_annees, n_ini, stockage) {
  n_larves <- stockage[1]
  n_fry <- stockage[2]
  resultats_matures <- matrix(0, nrow = n_annees + 1, ncol = n_iterations)
  n_stades <- nrow(matrice)
  pop_initiale<-calculer_structure_initiale(matrice, n_ini)
  
  for (i in 1:n_iterations) {
    mat_fec_stoch <- fecondite_stochastique(matrice, n_annees)
    mat_surv_stoch <- survie_stochastique(matrice, n_annees)
    
    sim_temp <- matrix(0, nrow = n_stades, ncol = n_annees + 1)
    pop_init <- pop_initiale
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
  

  return(resultats_matures)
}

library(ggplot2)
library(dplyr)

# 1. Générer les matrices de résultats (Annes x Iterations)
# Note : ici on stocke directement la matrice
matrice_matures_null <- fig_3(null_pred, 1000, 100, 1000, stockage_nul)
matrice_matures_end  <- fig_3(endangered, 1000, 100, 1000, stockage_nul)
matrice_matures_meas <- fig_3(measured, 1000, 100, 1000, stockage_nul)

calculer_donnees_seuil <- function(matrice_resultats, seuils = seq(0, 1000, by = 1)) {
  # matrice_resultats est resultats_matures (années x itérations)
  
  # Pour chaque itération, on calcule la proportion de temps passé sous chaque seuil
  prob_par_iteration <- apply(matrice_resultats, 2, function(colonne_pop) {
    sapply(seuils, function(s) as.numeric(any(colonne_pop < s)))
  })
  
  # On calcule la médiane et l'intervalle de confiance (70% ici)
  res <- data.frame(
    Seuil = seuils,
    Mediane = apply(prob_par_iteration, 1, median),
    Inf_70 = apply(prob_par_iteration, 1, quantile, probs = 0.15),
    Sup_70 = apply(prob_par_iteration, 1, quantile, probs = 0.85)
  )
  return(res)
}

# 2. Transformer en données de "seuil" (Probabilité moyenne par itération)
# Cette fonction calcule pour chaque itération la proportion d'années < seuil
df_null <- calculer_donnees_seuil(matrice_matures_null) %>% mutate(Scenario = "Null")
df_end  <- calculer_donnees_seuil(matrice_matures_end)  %>% mutate(Scenario = "Endangered")
df_meas <- calculer_donnees_seuil(matrice_matures_meas) %>% mutate(Scenario = "Measured")

# 3. Combiner et ordonner pour la légende
df_final <- bind_rows(df_null, df_end, df_meas)
df_final$Scenario <- factor(df_final$Scenario, levels = c("Null", "Endangered", "Measured"))

# 4. Le Graphique Final
ggplot(df_final, aes(x = Seuil, y = Mediane, color = Scenario, fill = Scenario)) +
  # Enveloppes IC 70%
  geom_ribbon(aes(ymin = Inf_70, ymax = Sup_70), alpha = 0.15, color = NA) +
  # Lignes médianes
  geom_line(size = 1) +
  # Esthétique
  scale_color_manual(values = c("Null" = "black", "Endangered" = "red", "Measured" = "blue")) +
  scale_fill_manual(values = c("Null" = "black", "Endangered" = "red", "Measured" = "blue")) +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.1), expand = c(0,0)) +
  scale_x_continuous(limits = c(0, 1000), expand = c(0,0)) +
  labs(
    x = "Threshold value (Number of mature individuals)",
    y = "Average probability of being below\nthreshold value in the next 100 years"
  ) +
  theme_classic() + # theme_classic ressemble plus à ton image cible
  theme(
    legend.position = c(0.8, 0.2), 
    legend.title = element_blank(),
    panel.border = element_rect(colour = "black", fill=NA, linewidth =1) # Pour avoir le cadre noir
  )
