options(scipen = 10)


########################
stockage_present<-c(0,231185,14868)
stockage_propose<-c(0,500000,15000)

# Scénario Mesuré
pop_ini_measured_500 <- calculer_structure_initiale(measured, 500)

# Scénario Endangered
# pop_endangered <- calculer_structure_initiale(endangered, 500)

# Scénario Null
# pop_null <- calculer_structure_initiale(null_prev, 500)

### Runner les modèles 100 ans ###

n_annees <- 100
# Créer une structure pour stocker les résultats (33 stades x 101 colonnes)
simulation <- matrix(0, nrow = n_stades, ncol = n_annees + 1)
rownames(simulation) <- rownames(measured)
colnames(simulation) <- paste0("An_", 0:n_annees)

# Année 0
simulation[, 1]<-calculer_structure_initiale(measured,500)
# Boucle de projection sur 100 ans
for (t in 1:n_annees) {
  # N(t+1) = Matrice %*% N(t)
  simulation[, t+1] <- measured %*% simulation[, t]
}

simulation[,101]


simulation

#Nombre d'individus matures (10 ans ou plus) après 100 ans (avec stock)
#pas trop clair mais gap de 10 ans avant stock (maturité à 10ans)
pop_final <- 5000

# Ajout de l'apport annuel

apport_annuel <- rep(0, n_stades) 
apport_annuel[1:3] <- stockage_present



n_annees <- 100
# Créer une structure pour stocker les résultats (33 stades x 101 colonnes)
simulation <- matrix(0, nrow = n_stades, ncol = n_annees + 1)
rownames(simulation) <- rownames(measured)
colnames(simulation) <- paste0("An_", 0:n_annees)

# Année 0
simulation[, 1]<-calculer_structure_initiale(measured,500)
# Boucle de projection sur 100 ans
for (t in 1:n_annees) {
  # Calcul du stock à t+1 : (Matrice * État actuel) + Apport
  simulation[, t+1] <- (measured %*% simulation[, t]) + apport_annuel
}

simulation[,101]

#


simule<-function(matrice, n_annees, stockage, n_age_1_ini) {
  n_stades<-nrow(matrice)
  
  # Ajout de l'apport annuel
  apport_annuel <- rep(0, n_stades) 
  apport_annuel[1:3] <- stockage
  
  # Matrice de stockage des résultats
  simulation <- matrix(0, nrow = n_stades, ncol = n_annees + 1)
  rownames(simulation) <- rownames(matrice)
  colnames(simulation) <- paste0("An_", 0:n_annees)
  
  # Année 0
  simulation[, 1]<-calculer_structure_initiale(matrice, n_age_1_ini)
  # Boucle de projection sur 100 ans
  for (t in 1:n_annees) {
    # Calcul du stock à t+1 : (Matrice * État actuel) + Apport
    simulation[, t+1] <- (matrice %*% simulation[, t]) + apport_annuel
  }
  
  return(simulation[,101])
}


# Test
(test<-simule(measured, 100, stockage_present, 500))



simule_stochastique <- function(matrice_base, n_annees, stockage, n_age_1_ini, b_params) {
  n_stades <- nrow(matrice_base)
  
  # Préparation de l'apport annuel
  apport_annuel <- rep(0, n_stades) 
  apport_annuel[1:3] <- stockage
  
  # Matrice de stockage des résultats
  simulation <- matrix(0, nrow = n_stades, ncol = n_annees + 1)
  simulation[, 1] <- calculer_structure_initiale(matrice_base, n_age_1_ini)
  
  # Extraire les probabilités de survie de base (sous-diagonale)
  p_mesure <- diag(matrice_base[-1, -n_stades])
  # Calculer les paramètres Alpha (fixes, basés sur la moyenne souhaitée)
  alpha_params <- (p_mesure * b_params) / (1 - p_mesure)
  
  # Boucle de projection
  for (t in 1:n_annees) {
    
    # 1. Créer une matrice temporaire pour cette année précise
    M_t <- matrix(0, nrow = n_stades, ncol = n_stades)
    
    # 2. Appliquer la stochasticité sur la fécondité (Ligne 1)
    # On varie autour de la première ligne de la matrice de base
    M_t[1, ] <- (1 + rnorm(n_stades, mean = 0, sd = 0.25)) * matrice_base[1, ]
    
    # 3. Appliquer la stochasticité sur la survie (Sous-diagonale)
    taux_survie_t <- rbeta(n = length(alpha_params), 
                           shape1 = alpha_params, 
                           shape2 = b_params)
    
    # Remplissage de la sous-diagonale
    for(i in 1:length(taux_survie_t)) {
      M_t[i+1, i] <- taux_survie_t[i]
    }
    
    # 4. Calcul de l'année t+1 avec la matrice de l'année et l'apport
    simulation[, t+1] <- (M_t %*% simulation[, t]) + apport_annuel
  }
  
  return(simulation) # Retourne toute la simulation pour analyse
}

# Test de la fonction
# On passe b_params (définis précédemment) en argument
resultat <- simule_stochastique(measured, 100, stockage_present, 500, b_params)

# Pour voir le résultat final (An 100) :
resultat[, 101]
