# 1. Préparation des vecteurs (longueur 33)
# Indices : 2=Larva, 3=Fry, 4=Age1 ... 33=Age30
s_rates <- taux_survie

length(taux_survie)
taux_survie
# Fecundité : Seuls les adultes (indices 13 à 33, correspondant aux âges 10-30) pondent
f_rates <- taux_fecondite
f_rates
length(taux_fecondite)

simulate_complex <- function(stocking_val, stage_to_stock, years =100) {
  max_idx <- 33
  pop <- rep(0, max_idx)
  
  # Mapping des noms pour le stocking
  stock_idx <- switch(stage_to_stock,
                      "Larva" = 2,
                      "Fry" = 3,
                      "Age1" = 4,
                      "Mature" = 13) # Index 13 = Individu de 10 ans
  
  for (t in 1:years) {
    new_pop <- rep(0, max_idx)
    
    # --- Reproduction (Les œufs produits vont à l'index 1) ---
    new_pop[1] <- sum(pop * f_rates)
    
    # --- Survie et Transition ---
    for (i in 1:(max_idx - 1)) {
      new_pop[i + 1] <- pop[i] * s_rates[i]
    }
    
    # --- Logique de Stockage ---
    if (stage_to_stock == "Mature") {
      if (t > 10) { # Délai de 10 ans pour produire des matures
        new_pop[13] <- new_pop[13] + stocking_val
      }
    } else {
      new_pop[stock_idx] <- new_pop[stock_idx] + stocking_val
    }
    
    pop <- new_pop
    
    # Sécurité anti-explosion numérique
    if (any(is.na(pop)) || sum(pop) > 1e15) return(1e15)
  }
  
  # Retourne le nombre d'individus matures (Index 13 à 33)
  return(sum(pop[13:33]))
}

# 2. Solver pour trouver l'effort de stocking nécessaire
find_stocking <- function(stage) {
  # Vérification si la population est déjà auto-suffisante
  if (simulate_complex(0, stage) >= 5000) return(0)
  
  # Recherche de la borne supérieure (upper)
  upper_val <- 100
  while(simulate_complex(upper_val, stage) < 5000 && upper_val < 1e12) {
    upper_val <- upper_val * 10
  }
  
  res <- uniroot(function(x) simulate_complex(x, stage) - 5000, 
                 interval = c(0, upper_val), tol = 1)
  return(res$root)
}

# 3. Résultats
noms_stades <- c("Larva", "Fry", "Age1", "Mature")
resultats <- data.frame(
  Stade_Stocke = noms_stades,
  Individus_Par_An = sapply(noms_stades, find_stocking)
)

print(resultats)
