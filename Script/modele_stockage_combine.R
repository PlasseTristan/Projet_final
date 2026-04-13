fig_2_amelioree <- function(matrice, n_iterations, n_annees, n_ini, stockage, nom_scenario) {
  n_larves_supp <- stockage[1]
  n_fry_supp <- stockage[2]
  n_stades <- nrow(matrice)
  
  resultats_matures <- matrix(0, nrow = n_annees + 1, ncol = n_iterations)
  
  for (i in 1:n_iterations) {
    # 1. Génération de la stochasticité
    mat_fec_stoch <- fecondite_stochastique(matrice, n_annees)
    mat_surv_stoch <- survie_stochastique(matrice, n_annees)
    
    sim_temp <- matrix(0, nrow = n_stades, ncol = n_annees + 1)
    
    # 2. Initialisation (Structure stable)
    # On commence avec une population établie (stades 4 à 33)
    pop_init <- calculer_structure_initiale(matrice, n_ini)
    sim_temp[, 1] <- pop_init
    resultats_matures[1, i] <- sum(sim_temp[13:33, 1])
    
    for (t in 1:n_annees) {
      # --- ÉTAPE A : Survie des adultes (Age 1 à Age 30) ---
      # Les individus aux stades 4:32 passent aux stades 5:33
      for (s in 4:(n_stades - 1)) {
        sim_temp[s + 1, t + 1] <- sim_temp[s, t] * mat_surv_stoch[s, t]
      }
      
      # --- ÉTAPE B : Reproduction et stades intra-annuels ---
      # 1. Ponte (Production d'œufs au temps t)
      n_oeufs <- sum(sim_temp[13:33, t] * mat_fec_stoch[13:33, t]) 
      
      # 2. Traversée des stades 1-2-3 (Intra-année)
      # On calcule combien survivent jusqu'à devenir "Age 1" (stade 4) à t+1
      s1 <- mat_surv_stoch[1, t] # Egg -> Larva
      s2 <- mat_surv_stoch[2, t] # Larva -> Fry
      s3 <- mat_surv_stoch[3, t] # Fry -> Age 1
      # Juste après le calcul de n_oeufs, ajouter temporairement :
      if (t == 1 && i == 1) {
        cat("n_oeufs:", n_oeufs, "\n")
        cat("s1:", s1, "s2:", s2, "s3:", s3, "\n")
        cat("survie_totale_oeufs:", s1*s2*s3, "\n")
        cat("recrutement stade 4:", n_oeufs * s1*s2*s3, "\n")
      }
      # 3. Recrutement au stade 4 (Nés sur place + Apports externes)
      survie_totale_oeufs <- s1 * s2 * s3
      survie_totale_larves <- s2 * s3
      survie_totale_fry <- s3
      
      sim_temp[4, t + 1] <- (n_oeufs * survie_totale_oeufs) + 
        (n_larves_supp * survie_totale_larves) + 
        (n_fry_supp * survie_totale_fry)
      
      # Stockage des résultats
      resultats_matures[t+1, i] <- sum(sim_temp[13:33, t+1])
    }
  }
  
  # Formatage du data.frame de sortie
  return(data.frame(
    Annee = 0:n_annees,
    Mediane = apply(resultats_matures, 1, median),
    Inf_95 = apply(resultats_matures, 1, quantile, probs = 0.025),
    Sup_95 = apply(resultats_matures, 1, quantile, probs = 0.975),
    Scenario = nom_scenario
  ))
}


meas_nul     <- fig_2_amelioree(measured, 5000, 100, 2000, stockage_nul, "No stocking")
meas_present <- fig_2_amelioree(measured, 5000, 100, 2000, stockage_present, "Actual stocking")
meas_propose <- fig_2_amelioree(measured, 5000, 100, 2000, stockage_propose, "Theoretical stocking")

df_meas <- bind_rows(meas_nul, meas_present, meas_propose)
df_meas <- df_meas %>% mutate(across(c(Mediane, Inf_95, Sup_95), ~if_else(.x < 0.1, 0.0001, .x)))

ggplot(df_meas , aes(x = Annee, y = Mediane, color = Scenario, fill = Scenario)) +
  # 1. Intervalle de confiance (Ruban)
  geom_ribbon(aes(ymin = Inf_95, ymax = Sup_95), alpha = 0.2, color = NA) +
  
  # 2. Lignes médianes 
  geom_line(linewidth = 1) +
  
  # Annotation "(c)" en haut à gauche
  annotate("text", x = 0, y = 40000, label = "(c)", size = 6, hjust = 0) +
  
  # Configuration des axes et thèmes
  scale_y_log10(
    limits = c(1, 50000), # Commence à 1 pour voir le ralentissement
    labels = label_log(), 
    breaks = 10^(0:4),    # Force les paliers 1, 10, 100, 1000, 10000
    oob = scales::squish
  ) +
  scale_x_continuous(expand = c(0, 0)) +
  
  # Couleurs 
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
    legend.position = "none",
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12, color = "black"),
    axis.line = element_line(linewidth = 0.8)
  )
