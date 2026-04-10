s_rates <- c(
  0.073, 0.023, 0.006, 0.609, 0.723, 0.778, 0.812, 0.836, 0.851, 0.863, 0.875, 0.887, 0.893, # Jusqu'à Age 10
  0.895, 0.898, 0.902, 0.903, 0.905, 0.908, 0.910, 0.910, 0.910, 0.911, 0.912, 0.911, 0.911, # Jusqu'à Age 22
  0.912, 0.912, 0.912, 0.913, 0.913, 0.913,0)
  
n_larve <-  230000
n_fry <- 15000
pop<- calculer_structure_initiale(measured, 1000)
  
  
  simulate_point <- function(stocking_val, stage_to_stock, n_annees = 100) {
    
  
    pop[1:3] <- 0  # Stades intra-annuels jamais présents au recensement
    
    # Survie combinée des stades intra-annuels (1→2→3→4)
    s_larve <- s_rates[2] * s_rates[3] *s_rates[4]  # Larve → stade 4
    s_fry   <- s_rates[3] * s_rates[4]               # Fry   → stade 4
                            
    
    for (t in 1:n_annees) {
      pop <- measured %*% pop
      
      # Stades intra-annuels remis à 0 après chaque projection
      pop[1:3] <- 0
      
      # Ensemencement avec survie résiduelle jusqu'au stade 4
      if (stage_to_stock == "Larva") {
        pop[4] <- pop[4] + ((stocking_val+n_larve) * s_larve)
        
      } else if (stage_to_stock == "Fry") {
        pop[4] <- pop[4] + ((stocking_val+n_fry) * s_fry)
        
      } else if (stage_to_stock == "Age 1") {
        pop[4] <- pop[4] + stocking_val 
        
      } else if (stage_to_stock == "Age 10") {
        if (t > 10) pop[13] <- pop[13] + stocking_val
      }
      
      if (any(is.na(pop)) || sum(pop) > 1e18) return(NA)
    }
    
    return(sum(pop[13:33], na.rm = TRUE))
  }
  
  # ==========================================================
  # 2. GRILLE DE SIMULATION
  # ==========================================================
  efforts   <- seq(0, 8, length.out = 2000)
  grid_vals <- c(0, 10^efforts)
  stades    <- c("Larva", "Fry", "Age 1", "Age 10")
  
  df_plot <- data.frame(Stocking = grid_vals)
  for (stade in stades) {
    df_plot[[stade]] <- sapply(grid_vals, simulate_point, stage_to_stock = stade)
  }
  
  plot(df_plot)
  # ==========================================================
  # 3. MISE EN FORME
  # ==========================================================
  df_long <- pivot_longer(df_plot,
                          cols      = all_of(stades),
                          names_to  = "Stocking_Stage",
                          values_to = "Abundance_Mature") %>%
    mutate(Stocking_Stage = factor(Stocking_Stage,
                                   levels = c("Larva", "Fry", "Age 1", "Age 10")))
  #Labels positionnés à x fixe pour chaque stade
  label_data <- data.frame(
    Stocking_Stage = factor(c("Larva", "Fry", "Age 1", "Age 10"),
                            levels = c("Larva", "Fry", "Age 1", "Age 10")),
    Stocking       = c(1e8,   1e6,   1e4,   2e2),   # Position X de chaque label
    Abundance_Mature = c(5400, 5400, 5400, 5400),    # Tous au dessus des courbes
    label          = c("Larva\nstocking", "Fry\nstocking",
                       "Age 1\nstocking", "Age 10\nstocking")
  )
  
  # ==========================================================
  # 4. GRAPHIQUE
  # ==========================================================
  # Filtrer les NA avant le graphique
  df_long_clean <- df_long %>%
    filter(Stocking > 0, !is.na(Abundance_Mature)) %>%
    group_by(Stocking_Stage) %>%
    filter(Abundance_Mature <= 5000) %>%
    ungroup()
  
  p <- ggplot(df_long_clean,
              aes(x        = Stocking,
                  y        = Abundance_Mature,
                  group    = Stocking_Stage,
                  linetype = Stocking_Stage)) +
    
    geom_line(linewidth = 1, color = "black") +
    
    geom_text(data       = label_data,
              aes(label  = label),
              vjust      = 0.5,
              hjust      = 0.5,
              size       = 3.8,
              fontface   = "plain",
              color      = "black",
              show.legend = FALSE) +
    
    scale_x_log10(
      breaks = 10^(0:9),
      labels = scales::label_log(),
      limits = c(1, 1e9)
    ) +
    
    scale_y_continuous(
      limits = c(0, 6000),
      breaks = seq(0, 5000, 1000),
      expand = c(0, 0),
    ) +
    
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
  
  print(p)
  ggsave("figure4.png", plot = p, width = 6, height = 5, dpi = 300)