fig_4_3 <- function(matrice_choisie, n_iterations = 100) {
  
  # 1. Paramètres fixes (ensemencement actuel)
  n_larve_base <- stockage_present[1]
  n_fry_base   <- stockage_present[2]
  n_stades     <- nrow(matrice_choisie)
  
  # Calcul des survies cumulées vers le stade 4 (Age 1)
  s_larve_to_2 <- measured[3,2]*measured[4,3] # Survie Larve -> Fry -> Age 1
  s_fry_to_2   <- measured[4,3] # Survie Fry -> Age 1
  
  simulate_point <- function(stocking_val, stage_to_stock) {
    # Initialisation (SSD pour 1000 adultes)
    pop <- calculer_structure_initiale_3(matrice_choisie, 1000)
    
    
    for (t in 1:100) {
      # 1. Projection naturelle
      pop <- as.vector(matrice_choisie %*% pop)
      
      # 3. Ajout de l'EFFORT testé
      if (stage_to_stock == "Larva") {
        # Les larves deviennent des Age 1 (Stade 2) après avoir survécu à l'Age 0
        pop[2] <- pop[2] + (stocking_val * s_larve_to_2)
      } else if (stage_to_stock == "Fry") {
        # Les Fry sont plus vieux, on estime leur survie 1.5x meilleure que les larves
        pop[2] <- pop[2] + (stocking_val * (s_fry_to_2))
      } else if (stage_to_stock == "Age 1") {
        # Injection directe au stade Juvénile (Stade 2)
        pop[2] <- pop[2] + stocking_val
      } else if (stage_to_stock == "Age 10") {
        # Injection directe au stade Adulte (Stade 3 ou 4)
        # On utilise l'indice 3 pour les adultes entrant en maturité
        pop[3] <- pop[3] + stocking_val
      }
      
    }
    return(pop[3])
  }
  # 4. Création de la grille d'efforts
  # Création d'une séquence logarithmique (10^0 à 10^9) pour couvrir tous les ordres de grandeur
  efforts   <- seq(0, 9, length.out = 3000)
  grid_vals <- c(0,10^efforts)
  stades    <- c("Larva", "Fry", "Age 1", "Age 10")
  
  # 5. Calcul et transformation des données 
  # expand.grid crée toutes les combinaisons possibles entre efforts et stades
  df_clean <- expand.grid(Stocking = grid_vals, Stocking_Stage = stades) %>%
    mutate(
      Stocking_Stage = factor(Stocking_Stage, levels = stades),
      # Application de la simulation à chaque ligne
      Abundance_Mature = mapply(simulate_point, Stocking, as.character(Stocking_Stage))
    ) %>%
    # Filtrage final pour le graphique
    # On retire les valeurs nulles (log scale), les erreurs et on limite à l'échelle du graphique (5000)
    filter(Stocking > 0, 
           !is.na(Abundance_Mature), 
           Abundance_Mature <= 5000)
  
  return(df_clean)
}

df_fig4_3 <- fig_4_3(measured_3)
df_fig4_3

#_______________________________________________________________________
stades <- c("Larva", "Fry", "Age 1", "Age 10")
# Positionnement des labels (étiquettes sur les courbes)
label_data <- data.frame(
  Stocking_Stage = factor(stades, levels = stades),
  Stocking       = c(1e7, 5e5, 3e3, 3e2), 
  Abundance_Mature = c(5400, 5400, 5400, 5400),
  label          = c("Larva\nstocking", "Fry\nstocking", "Age 1\nstocking", "Age 10\nstocking")
)
#__________________________________________________________________________
#Créer la figure
graphique_fig4_3 <- ggplot(df_fig4_3,
                           aes(x        = Stocking,
                               y        = Abundance_Mature,
                               group    = Stocking_Stage, # Identifie les groupes pour tracer les lignes
                               linetype = Stocking_Stage)) +
  
  # Ajout des lignes de trajectoire
  geom_line(linewidth = 1, color = "black") +
  
  # Ajout d'étiquettes de texte
  geom_text(data = label_data,
            aes(label  = label),
            size       = 3.8,
            fontface   = "plain",
            color      = "black",
            show.legend = FALSE) +
  
  # Configuration de l'axe des X en échelle logarithmique
  scale_x_log10(
    limits = c(1, 1e8), 
    breaks = 10^(0:8),
    # Cette fonction affiche "1" pour 10^0, puis 10^1, 10^2...
    labels = function(x) ifelse(x == 1, "1", scales::label_log()(x)),
    # expand = c(0,0) force la ligne à toucher l'axe vertical
    expand = c(0, 0) 
  ) +
  
  # Configuration de l'axe des Y (Échelle linéaire)
  scale_y_continuous(
    limits = c(0, 6000),
    breaks = seq(0, 5000, 1000),
    expand = c(0, 0),
  ) +
  
  # Attribution manuelle des types de lignes (pointillés pour l'âge 10)
  scale_linetype_manual(values = c(
    "Larva"  = "solid",
    "Fry"    = "solid",
    "Age 1"  = "solid",
    "Age 10" = "dashed"
  )) +
  
  theme_classic() +
  labs(
    x = "Stocking (N/yr)",
    y = "Abundance of mature individuals (N)"
  ) +
  theme(
    panel.border    = element_rect(colour = "black", fill = NA, linewidth = 1),
    axis.line       = element_blank(),
    legend.position = "none",
    axis.text       = element_text(color = "black", size = 11),
    axis.title      = element_text(size = 12),
    plot.margin     = margin(40, 20, 10, 10)
  )
#ggsave("figure4.png", plot = fig4, width = 6, height = 5, dpi = 300)
#Afficher la figure 4 déja construite
print(graphique_fig4_3)
