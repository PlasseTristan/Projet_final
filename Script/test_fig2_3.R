fig_2_opti_3x3 <- function(matrice_3x3, n_iterations, n_annees, n_ini, stockage, nom_scenario) {
  
  # 1. Définition des taux de survie pour l'ensemencement (valeurs de votre étude)
  survie_larve_vers_fry <- 0.023
  survie_fry_vers_age1  <- 0.006
  
  # Calcul du recrutement effectif annuel issu du stockage
  # Ces poissons "apparaissent" directement comme des Age 1 (début du stade Pré-adulte)
  recrutement_larves <- round(stockage[1] * survie_larve_vers_fry * survie_fry_vers_age1)
  recrutement_fry    <- round(stockage[2] * survie_fry_vers_age1)
  apport_total_recrutement <- recrutement_larves + recrutement_fry
  
  # 2. Initialisation
  pop_init <- struct(matrice_3x3, n_ini)
  resultats_matures <- matrix(0, nrow = n_annees + 1, ncol = n_iterations)
  
  for (i in 1:n_iterations) {
    # Pré-générer la stochasticité
    fec_stoch   <- fecondite_stochastique_3(matrice_3x3, n_annees)
    surv_stoch  <- survie_stochastique_3(matrice_3x3, n_annees)
    stase_stoch <- stase_stochastique_3(matrice_3x3, n_annees)
    
    pop <- pop_init
    resultats_matures[1, i] <- pop[3]
    
    for (t in 1:n_annees) {
      B_t <- matrix(0, nrow = 3, ncol = 3)
      
      # Construction de la matrice stochastique annuelle
      B_t[1, ] <- fec_stoch[, t]
      diag(B_t) <- diag(B_t) + stase_stoch[, t]
      B_t[2, 1] <- surv_stoch[1, t]
      B_t[3, 2] <- surv_stoch[2, t]
      
      # 3. Projection de la population naturelle
      pop_next <- B_t %*% pop
      
      # 4. Ajout de l'ensemencement
      # L'ajout se fait au stade 2 (Pré-adultes) car ce sont des poissons 
      # qui ont survécu à leur première année (ils entrent dans l'année 2).
      pop_next[2] <- pop_next[2] + apport_total_recrutement
      
      pop <- round(pmax(pop_next, 0))
      resultats_matures[t + 1, i] <- pop[3]
    }
  }
  
  return(data.frame(
    Annee = 0:n_annees,
    Mediane = apply(resultats_matures, 1, median),
    Inf_95 = apply(resultats_matures, 1, quantile, probs = 0.025),
    Sup_95 = apply(resultats_matures, 1, quantile, probs = 0.975),
    Scenario = nom_scenario
  ))
}

# Calculs
df_null <- bind_rows(
  fig_2_opti_3x3(null_pred_3, 5000, 100, 2000, stockage = stockage_nul, "No stocking"), 
  fig_2_opti_3x3(null_pred_3, 5000, 100, 2000, stockage_present, "Actual stocking"), 
  fig_2_opti_3x3(null_pred_3, 5000, 100, 2000, stockage_propose, "Theoretical stocking"))
df_end <- bind_rows(
  fig_2_opti_3x3(endangered_3, 5000, 100, 2000, stockage_nul, "No stocking"),
  fig_2_opti_3x3(endangered_3, 5000, 100, 2000, stockage_present, "Actual stocking"),
  fig_2_opti_3x3(endangered_3, 5000, 100, 2000, stockage_propose, "Theoretical stocking"))
df_meas <- bind_rows(
  fig_2_opti_3x3(measured_3, 5000, 100, 2000, stockage_nul, "No stocking"),
  fig_2_opti_3x3(measured_3, 5000, 100, 2000, stockage_present, "Actual stocking"),
  fig_2_opti_3x3(measured_3, 5000, 100, 2000, stockage_propose, "Theoretical stocking"))

# Remplace les 0 par 0.1 pour permettre le calcul du log10 sans déformer le graphique
df_null <- df_null %>% mutate(across(c(Mediane, Inf_95, Sup_95), ~if_else(.x < 0.1, 0.0001, .x)))
df_end  <- df_end  %>% mutate(across(c(Mediane, Inf_95, Sup_95), ~if_else(.x < 0.1, 0.0001, .x)))
df_meas <- df_meas %>% mutate(across(c(Mediane, Inf_95, Sup_95), ~if_else(.x < 0.1, 0.0001, .x)))



graph_fig2 <- function(d1, d2, d3) {
  
  # On crée une fonction interne pour éviter de répéter 3 fois le même style
  style_fig2 <- function(data, label, show_x = FALSE, show_legend = FALSE) {
    plot <- ggplot(data, aes(x = Annee, y = Mediane, color = Scenario, fill = Scenario)) +
      
      # 1. Intervalle de confiance (Ruban)
      geom_ribbon(aes(ymin = Inf_95, ymax = Sup_95), alpha = 0.2, color = NA) +
      
      # 2. Lignes médianes
      geom_line(linewidth = 1) +
      
      # 3. Annotation (a),(b) ou (c) en haut à gauche
      annotate("text", x = 0, y = 40000, label = label, size = 6, hjust = 0) +
      
      # 4. Configuration des axes et thèmes
      scale_y_log10(
        limits = c(1, 50000),
        # Cette fonction affiche "1" pour 10^0, puis 10^1, 10^2...
        labels = function(x) ifelse(x == 1, "1", scales::label_log()(x)), 
        # Force les coupures à chaque puissance de 10
        breaks = scales::breaks_log(),
        oob = scales::squish
      ) +
      scale_x_continuous(expand = c(0, 0)) +
      scale_color_manual(values = c("No stocking" = "#b00014", 
                                    "Actual stocking" = "#29abe2", 
                                    "Theoretical stocking" = "#43a047")) +
      scale_fill_manual(values = c("No stocking" = "#b00014", 
                                   "Actual stocking" = "#29abe2", 
                                   "Theoretical stocking" = "#43a047")) +
      theme_minimal() +
      labs(x = if(show_x) "Time (yr)" else NULL, y = "Abundance", color = NULL, fill = NULL) + # Condition d’afficher le titre de l'axe des x seulement pour le graphique c)
      theme(
        axis.title = element_text(size = 14),
        axis.text.y = element_text(size = 12, color = "black"),
        axis.text.x = if(show_x) element_text(size = 12, color = "black") else element_blank(),# Condition d’afficher le titre de l'axe des x seulement pour le graphique c)
        axis.line = element_line(linewidth = 0.8),
        legend.position = if(show_legend) c(0.25, 0.25) else "none", # Condition d’afficher la légende seulement pour le graphique a)
        legend.background = element_blank(),
        panel.grid = element_blank()
      )
    return(plot)
  }
  
  # Construction des trois panneaux
  graph_a <- style_fig2(d1, "(a)", show_legend = TRUE)
  graph_b <- style_fig2(d2, "(b)")
  graph_c <- style_fig2(d3, "(c)", show_x = TRUE)
  
  # Assemblage avec patchwork
  return(graph_a / graph_b / graph_c)
}

# Pour l'afficher :
graphique_fig2 <- graph_fig2(df_null, df_end, df_meas)
print(graphique_fig2)
