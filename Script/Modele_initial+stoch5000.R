# 1. Paramètres globaux
n_iterations <- 5000
n_annees <- 100
n_stades <- nrow(null_pred)
n_mature_cible <- 2000 # Nombre de matures initial souhaité

# Stockage pour les résultats (une ligne par année, une colonne par simulation)
# On suit l'abondance des MATURES (stades 13 à 33)
resultats_matures <- matrix(0, nrow = n_annees + 1, ncol = n_iterations)

# 2. Début des simulations
set.seed(123) # Pour la reproductibilité

for (i in 1:n_iterations) {
  
  # Générer la stochasticité propre à cette simulation
  mat_fec_stoch <- fecondite_stochastique(null_pred, n_annees)
  mat_surv_stoch <- survie_stochastique(null_pred, n_annees)
  
  # Initialisation de la matrice de projection pour CETTE simulation
  sim_temp <- matrix(0, nrow = n_stades, ncol = n_annees + 1)
  
  # Structure initiale (on garde les jeunes pour éviter le "trou" démographique)
  pop_init <- calculer_structure_initiale(null_pred, n_mature_cible)
  sim_temp[, 1] <- pop_init
  
  # Stockage de l'année 0
  resultats_matures[1, i] <- sum(sim_temp[13:33, 1])
  
  # Boucle de 100 ans
  for (t in 1:n_annees) {
    M_t <- matrix(0, nrow = n_stades, ncol = n_stades)
    diag(M_t[-1, -n_stades]) <- mat_surv_stoch[, t]
    
    # Recrutement condensé
    s_an1_t <- mat_surv_stoch[1, t] * mat_surv_stoch[2, t] * mat_surv_stoch[3, t]
    M_t[4, 13:33] <- mat_fec_stoch[13:33, t] * s_an1_t
    
    # Nettoyage des liens directs pour les stades 1-3
    M_t[2,1] <- 0; M_t[3,2] <- 0; M_t[4,3] <- 0
    
    # Projection + Stockage (Exemple avec stockage présent)
    n_larves <- 231185
    n_fry <- 14868
    apport_ext <- (n_larves * mat_surv_stoch[2, t] * mat_surv_stoch[3, t]) + 
      (n_fry * mat_surv_stoch[3, t])
    
    # Projection
    sim_temp[, t+1] <- M_t %*% sim_temp[, t]
    sim_temp[4, t+1] <- sim_temp[4, t+1] + apport_ext
    
    # Sauvegarde des matures uniquement
    resultats_matures[t+1, i] <- sum(sim_temp[13:33, t+1])
  }
}

# 3. Calcul des statistiques (Médiane et Intervalles)
df_final <- data.frame(
  Annee = 0:n_annees,
  Mediane = apply(resultats_matures, 1, median),
  Inf_95 = apply(resultats_matures, 1, quantile, probs = 0.025),
  Sup_95 = apply(resultats_matures, 1, quantile, probs = 0.975)
)

# Sécurité log10
df_final <- df_final %>%
  mutate(across(c(Mediane, Inf_95, Sup_95), ~if_else(.x <= 0.1, 0.1, .x)))

# 4. Graphique final
ggplot(df_final, aes(x = Annee)) +
  geom_ribbon(aes(ymin = Inf_95, ymax = Sup_95), fill = "steelblue", alpha = 0.2) +
  geom_line(aes(y = Mediane), color = "steelblue", linewidth = 1) +
  scale_y_log10(labels = scales::label_comma()) +
  theme_minimal() +
  labs(title = "Projection du Chevalier Cuivré (5000 simulations)",
       subtitle = "Médiane et intervalle de confiance à 95%",
       x = "Années", y = "Abondance des matures (log10)")