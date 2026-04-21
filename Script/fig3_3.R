#___________________________________________________________________________
##Simulation de base pour la figure
simuler_trajectoires_adultes_3 <- function(matrice, n_iterations, n_annees = 100) {
  n_stades <- nrow(matrice) #4 comme les matrices modifiées
  historique <- matrix(0, nrow = n_iterations, ncol = n_annees + 1) #matrice pour stocker les résultats
  
  
  #Création des matrices stochastiques (survie et fécondité) pour chaque ittération
  for (i in 1:n_iterations) {
    mat_fec  <- fecondite_stochastique_3(matrice, n_annees)
    mat_surv <- survie_stochastique_3(matrice, n_annees)
    mat_stas <- stase_stochastique_3(matrice,n_annees)
    pop <- calculer_structure_initiale_3(matrice, 1000)
    
    
    historique[i, 1] <- pop[3]
    
    #Calcul de la population pour chaque années dans chaque ittération
    for (t in 1:n_annees) {
      M_t <- matrix(0, nrow = n_stades, ncol = n_stades)
      
      # Bug fix : 
      diag(M_t[-1, -n_stades]) <- mat_surv[1:(n_stades - 1), t]
      
      M_t[2,2] <- mat_stas[2,t]
      M_t[3,3] <- mat_stas[3,t]
      
      M_t[1,3] <- mat_fec[3,t]
     
      
      pop <- M_t %*% pop
      
      
      historique[i, t + 1] <- pop[3]
    }
  }
  return(historique)
}
#___________________________________________________________________________
# Calcul des statistiques pour les intervals de confiance à 70%

calculer_stats <- function(historique, nom_scenario) {
  seuils <- seq(0, 1000, by = 1)
  n_sims <- nrow(historique)
  
  temps_sous_seuil <- matrix(0, nrow = n_sims, ncol = length(seuils))
  for (j in seq_along(seuils)) {
    temps_sous_seuil[, j] <- rowMeans(historique < seuils[j])
  }
  
  data.frame(
    Threshold = seuils,
    Median    = apply(temps_sous_seuil, 2, median),
    Inf_70    = apply(temps_sous_seuil, 2, quantile, probs = 0.15),
    Sup_70    = apply(temps_sous_seuil, 2, quantile, probs = 0.85),
    Scenario  = factor(nom_scenario, levels = c("Null", "Endangered", "Measured"))
  )
}


#__________________________________________________________________________
##exécuter le code pour les 3 scénarios (5000 itérations)
hist_null <- simuler_trajectoires_adultes_3(null_pred_3,  5000)
hist_end  <- simuler_trajectoires_adultes_3(endangered_3, 5000)
hist_meas <- simuler_trajectoires_adultes_3(measured_3,   5000)

#Stocker les résultats dans un data frame unique
df_final <- bind_rows(
  calculer_stats(hist_null, "Null"),
  calculer_stats(hist_end,  "Endangered"),
  calculer_stats(hist_meas, "Measured")
)

#___________________________________________________________________________
##lissage des résultats
df_final$Scenario <- factor(df_final$Scenario, levels = c("Measured", "Endangered", "Null"))

df_smoothed <- df_final %>%
  group_by(Scenario) %>%
  mutate(
    Median = predict(loess(Median ~ Threshold, span = 0.15)),
    Inf_70 = predict(loess(Inf_70 ~ Threshold, span = 0.15)),
    Sup_70 = predict(loess(Sup_70 ~ Threshold, span = 0.15)),
    
    # Empêche les valeurs négatives dues au lissage
    Median = pmax(Median, 0),
    Inf_70 = pmax(Inf_70, 0),
    Sup_70 = pmax(Sup_70, 0)
  ) %>%
  ungroup()

#___________________________________________________________________________
## un graphique combinant les trois scénarios avec un interval de confiance de 70%%
fig3_3 <- ggplot(df_smoothed, aes(x = Threshold, group = Scenario)) +
  
  geom_ribbon(aes(ymin = Inf_70, ymax = Sup_70, fill = Scenario),
              alpha = 0.25, show.legend = FALSE) +
  
  geom_line(aes(y = Median, color = Scenario), linewidth = 1.1) +
  
  #Ajouter les couleurs voulues
  scale_color_manual(
    values = c("Null" = "black", "Endangered" = "red", "Measured" = "blue"),
    breaks = c("Null", "Endangered", "Measured")
  ) +
  #idem pour les intervalles
  scale_fill_manual(
    values = c("Null" = "black", "Endangered" = "red", "Measured" = "blue"),
    guide  = "none"
  ) +
  
  scale_y_continuous(
    limits = c(0, 1.01),
    breaks = seq(0, 1, 0.2),
    expand = c(0, 0),
    labels = function(x) sprintf("%.1f", x)
  ) +
  scale_x_continuous(
    limits = c(0, 1000),
    breaks = seq(0, 1000, 200),
    expand = c(0, 0)
  ) +
  
  theme_classic() +
  labs(
    x = "Valeurs seuils (Nombre d'individus matures)",
    y = "Probabilité médiane d'être sous un certain seuil d'ici les 100 prochaines années"
  ) +
  
  ##Les paramètres ici ont été proposé par l'ia pour ressembler le plus possible visuellement à la figure d'origine
  theme(
    text              = element_text(family = "serif"),
    panel.border      = element_rect(colour = "black", fill = NA, linewidth = 1),
    axis.line         = element_blank(),
    legend.position   = c(0.80, 0.25),
    legend.title      = element_blank(),
    legend.text       = element_text(size = 11),
    legend.key        = element_blank(),
    legend.background = element_rect(fill = "white", color = NA),
    axis.text         = element_text(color = "black", size = 11),
    axis.title        = element_text(size = 12),
    plot.margin       = margin(10, 20, 10, 10)
  )
#___________________________________________________________________________
#Afficher la figure 3
print(fig3_3)
#ggsave("figure3.png", plot = fig3, width = 6, height = 5, dpi = 300)