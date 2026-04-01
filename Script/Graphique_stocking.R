# --- PRÉ-REQUIS : CHARGEZ VOS VECTEURS ICI ---
f_rates <- c(0,0,0,0,0,0,0,0,0,0,0,0,18096, 21035, 24114, 27133, 29563, 31934, 34142, 35761, 
             37062, 38435, 39821, 40936, 41923, 42972, 44075, 45086, 
             46025, 46914, 47767, 48568,0)
length(f_rates)
# Fécondités spécifiques (0 pour index 1-12)
s_rates <- c(
  0.073, 0.023, 0.006, 0.609, 0.723, 0.778, 0.812, 0.836, 0.851, 0.863, 0.875, 0.887, 0.893, # Jusqu'à Age 10
  0.895, 0.898, 0.902, 0.903, 0.905, 0.908, 0.910, 0.910, 0.910, 0.911, 0.912, 0.911, 0.911, # Jusqu'à Age 22
  0.912, 0.912, 0.912, 0.913, 0.913, 0.913,0 # Fin
) # Survies spécifiques (Dernière valeur = 0)
# ensure lengths are correct
length(s_rates)
# Install/Load libraries
if(!require(ggplot2)) install.packages("ggplot2")
if(!require(tidyr)) install.packages("tidyr")
library(ggplot2)
library(tidyr)

# --- FONCTION DE SIMULATION MATRICIELLE DE HAUTE PRÉCISION ---
simulate_point <- function(stocking_val, stage_to_stock) {
  max_idx <- 33
  L <- matrix(0, max_idx, max_idx)
  L[1, ] <- f_rates  # Fécondité spécifique
  for (i in 1:32) L[i + 1, i] <- s_rates[i] # Survie spécifique
  
  pop <- rep(0, max_idx)
  
  idx <- switch(stage_to_stock, "Larva"=2, "Fry"=3, "Age 1"=4, "Age 10"=13)
  
  for (t in 1:100) { # Simulation 100 ans
    pop <- L %*% pop
    # Stocking avec délai de 10 ans pour les géniteurs
    if (stage_to_stock == "Age 10") {
      if (t > 10) pop[13] <- pop[13] + stocking_val
    } else {
      pop[idx] <- pop[idx] + stocking_val
    }
    # Sécurité technique
    if (any(is.na(pop)) || sum(pop) > 1e18) return(1e18)
  }
  # Résultat : Abondance des matures (Index 13 à 33)
  return(sum(pop[13:33], na.rm=TRUE))
}

# --- 1. CRÉATION DU GRADIENT DE STOCKING (ÉCHELLE LOG) ---
# Similaire à l'axe X de l'image : de 1 à 10,000,000
stocking_gradient <- c(1, 10, 100, 1000, 10000, 100000, 1000000, 10000000)

# Pour des courbes lisses, on utilise seq() sur l'échelle log
efforts <- seq(0, 7, length.out = 100) # De 10^0 à 10^7
grid_vals <- 10^efforts

# --- 2. CALCUL DES RÉSULTATS POUR CHAQUE STADE ---
stades <- c("Larva", "Fry", "Age 1", "Age 10")

# Initialisation du data frame de résultats
df_plot <- data.frame(Stocking = grid_vals)

for (stade in stades) {
  # Pour chaque valeur d'effort, on applique la simulation matricielle précise
  df_plot[[stade]] <- sapply(grid_vals, simulate_point, stage_to_stock = stade)
}

# --- 3. FORMATAGE DES DONNÉES (POUR GGPLOT2) ---
df_long <- pivot_longer(df_plot, cols = c("Larva", "Fry", "Age 1", "Age 10"),
                        names_to = "Stocking_Stage", values_to = "Abundance_Mature")

# --- 4. GÉNÉRATION DU GRAPHIQUE FINAL (GGPLOT2) ---
# Reproduit le style de image_0.png
ggplot(df_long, aes(x = Stocking, y = Abundance_Mature, group = Stocking_Stage)) +
  geom_line(aes(linetype = Stocking_Stage, color = Stocking_Stage), size = 1.1) +
  # Échelles et labels pour correspondre parfaitement à image_0.png
  scale_x_log10(breaks = stocking_gradient, 
                labels = scales::label_log()) +
  coord_cartesian(ylim = c(0, 5000)) +
  labs(x = "Stocking (N/yr)", 
       y = "Abundance of mature individuals (N)",
       linetype = "Stocking Scenario",
       color = "Stocking Scenario") +
  # Style et placement des labels sur le graphique
  scale_linetype_manual(values = c("Larva"="solid", "Fry"="solid", 
                                   "Age 1"="solid", "Age 10"="dashed")) +
  theme_bw() + # Fond blanc
  theme(panel.grid.minor = element_blank(),
        legend.position = "none", # Pas de légende, labels sur les courbes
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14, face = "bold")) +
  # Ajout manuel des annotations de texte sur les courbes
  annotate("text", x = 1e7/2, y = 5100, label = "Larva\nstocking", fontface="bold") +
  annotate("text", x = 1.5e5, y = 5100, label = "Fry\nstocking", fontface="bold") +
  annotate("text", x = 1e3*1.5, y = 5100, label = "Age 1\nstocking", fontface="bold") +
  annotate("text", x = 10^2.1, y = 5100, label = "Age 10\nstocking", fontface="bold")

