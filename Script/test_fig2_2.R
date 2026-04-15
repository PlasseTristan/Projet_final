fig_2_opti <- function(matrice, n_iterations, n_annees, n_ini, stockage, nom_scenario) {
  n_stades <- 4
  n_larves_supp <- stockage[1]
  n_fry_supp <- stockage[2]
  
  # 1. Structure initiale (1000 adultes répartis entre stades 3 et 4)
  pop_init <- calculer_structure_initiale_2(matrice, n_ini)
  
  # Matrice de résultats : [Années, Simulations]
  resultats_matures <- matrix(0, nrow = n_annees + 1, ncol = n_iterations)
  
  for (i in 1:n_iterations) {
    # Tirages stochastiques pour l'itération
    mat_fec  <- fecondite_stochastique_2(matrice, n_annees)
    mat_surv <- survie_stochastique_2(matrice, n_annees) # Passage G
    mat_stas <- stase_stochastique_2(matrice, n_annees)  # Stase P
    
    pop <- pop_init
    resultats_matures[1, i] <- sum(pop[3:4])
    
    for (t in 1:n_annees) {
      M_t <- matrix(0, nrow = n_stades, ncol = n_stades)
      
      # --- CONSTRUCTION DE LA MATRICE M_t ---
      # Fécondité (Ligne 1) : Les adultes (3 et 4) produisent des larves
      M_t[1, 3] <- mat_fec[3, t]
      M_t[1, 4] <- mat_fec[4, t]
      
      # Stase (Diagonale)
      diag(M_t) <- mat_stas[, t]
      
      # Passage (Sous-diagonale)
      M_t[2, 1] <- mat_surv[1, t] # Larve -> Juvénile
      M_t[3, 2] <- mat_surv[2, t] # Juvénile -> Adulte 1
      M_t[4, 3] <- mat_surv[3, t] # Adulte 1 -> Adulte 2
      
      # --- PROJECTION ET ENSEMENCEMENT ---
      # Projection naturelle
      pop_next <- round(M_t %*% pop)
      
      # Ajout du stockage (Ensemencement)
      # Les larves et fry ajoutés au temps t deviennent des juvéniles (stade 2) au temps t+1
      # On utilise la survie du stade 1 (mat_surv[1,t])
      survie_age0 <- mat_surv[2, t]
      
      # Recrutement supplémentaire au stade 2
      pop_next[2] <- pop_next[2] + round(n_larves_supp * survie_age0) + 
        round(n_fry_supp * (survie_age0 * 1.5))
      
      pop <- as.vector(pop_next)
      pop <- pmax(pop, 0) # Sécurité extinction
      
      # Stockage des adultes matures (3 + 4)
      resultats_matures[t + 1, i] <- sum(pop[3:4])
    }
  }
  
  # Calcul des statistiques
  return(data.frame(
    Annee = 0:n_annees,
    Mediane = apply(resultats_matures, 1, median),
    Inf_95 = apply(resultats_matures, 1, quantile, probs = 0.025),
    Sup_95 = apply(resultats_matures, 1, quantile, probs = 0.975),
    Scenario = nom_scenario
  ))
}

df_null <- bind_rows(
  fig_2_opti(null_pred3, 5000, 100, 1000, stockage_nul, "No stocking"), 
  fig_2_opti(null_pred3, 5000, 100, 1000, stockage_present, "Actual stocking"), 
  fig_2_opti(null_pred3, 5000, 100, 1000, stockage_propose, "Theoretical stocking"))
df_end <- bind_rows(
  fig_2_opti(endangered3, 5000, 100, 1000, stockage_nul, "No stocking"),
  fig_2_opti(endangered3, 5000, 100, 1000, stockage_present, "Actual stocking"),
  fig_2_opti(endangered3, 5000, 100, 1000, stockage_propose, "Theoretical stocking"))
df_meas <- bind_rows(
  fig_2_opti(measured3, 5000, 100, 1000, stockage_nul, "No stocking"),
  fig_2_opti(measured3, 5000, 100, 1000, stockage_present, "Actual stocking"),
  fig_2_opti(measured3, 5000, 100, 1000, stockage_propose, "Theoretical stocking"))

# Remplace les 0 par 0.1 pour permettre le calcul du log10 sans déformer le graphique
df_null <- df_null %>% mutate(across(c(Mediane, Inf_95, Sup_95), ~if_else(.x < 0.1, 0.0001, .x)))
df_end  <- df_end  %>% mutate(across(c(Mediane, Inf_95, Sup_95), ~if_else(.x < 0.1, 0.0001, .x)))
df_meas <- df_meas %>% mutate(across(c(Mediane, Inf_95, Sup_95), ~if_else(.x < 0.1, 0.0001, .x)))


# Applique ton graphique
graphique_fig2 <- graph_fig2(df_null, df_end, df_meas)
print(graphique_fig2)
