options(scipen = 10)



# Creation du nom de chaque stade
noms_stades <- c("Egg", "Larva", "Fry", paste0("Age", 1:30))
# Nombre de stades
n_stades <- length(noms_stades)

# 2. Créer une matrice vide (remplie de 0)
measured <- matrix(0, nrow = n_stades, ncol = n_stades, 
                     dimnames = list(noms_stades, noms_stades))

# 3. Remplir la PREMIÈRE LIGNE (Fécondité)
# Les indices correspondent aux colonnes où la reproduction commence
fecondite <- c(18096, 21035, 24114, 27133, 29563, 31934, 34142, 35761, 
               37062, 38435, 39821, 40936, 41923, 42972, 44075, 45086, 
               46025, 46914, 47767, 48568)

# On place ces valeurs de Age 11 à Age 30 (colonnes 14 à 33) 
# Avant = pas à maturité sexuelle
measured[1, 13:32] <- fecondite

# 4. Remplir la SOUS-DIAGONALE (Taux de survie)
# On liste les probabilités de passage d'un stade au suivant
survie <- c(
  0.073, 0.023, 0.006, 0.609, 0.723, 0.778, 0.812, 0.836, 0.851, 0.863, 0.875, 0.887, 0.893, # Jusqu'à Age 10
  0.895, 0.898, 0.902, 0.903, 0.905, 0.908, 0.910, 0.910, 0.910, 0.911, 0.912, 0.911, 0.911, # Jusqu'à Age 22
  0.912, 0.912, 0.912, 0.913, 0.913, 0.913 # Fin
)

# On injecte ces valeurs dans la sous-diagonale (ligne i+1, colonne i)
for(i in 1:length(survie)) {
  measured[i+1, i] <- survie[i]
}


max(Re(eigen(measured)$values)) # Donne la même valeur de lambda que l'étude



# 2. Créer une matrice vide (remplie de 0)
endangered <- matrix(0, nrow = n_stades, ncol = n_stades, 
                 dimnames = list(noms_stades, noms_stades))

# 3. Remplir la PREMIÈRE LIGNE (Fécondité)
# Les indices correspondent aux colonnes où la reproduction commence
fecondite <- c(18523, 21571, 24749, 27847, 30339, 32771, 35037, 36699, 
               38034, 39442, 40865, 42009, 43022, 44098, 45230, 46268, 
               47231, 48144, 49019, 49841)

# On place ces valeurs de Age 11 à Age 30 (colonnes 14 à 33)
endangered[1, 14:33] <- fecondite

# 4. Remplir la SOUS-DIAGONALE (Taux de survie)
# On liste les probabilités de passage d'un stade au suivant
survie <- c(
  0.075, 0.023, 0.007, 0.625, 0.741, 0.798, 0.833, 0.857, 0.873, 0.885, 0.898, 0.910, 0.916, # Jusqu'à Age 10
  0.918, 0.921, 0.926, 0.927, 0.929, 0.932, 0.933, 0.933, 0.934, 0.935, 0.935, 0.935, 0.935, # Suite
  0.936, 0.936, 0.936, 0.937, 0.937, 0.937 # Fin
)

# On injecte ces valeurs dans la sous-diagonale (ligne i+1, colonne i)
for(i in 1:length(survie)) {
  endangered[i+1, i] <- survie[i]
}

# --- Vérification ---
# Afficher les 5 premières lignes et colonnes
print(endangered[1:5, 1:5])

max(Re(eigen(endangered)$values)) # Même chose que dans l'étude



null_pred <- matrix(0, nrow = n_stades, ncol = n_stades, 
                    dimnames = list(noms_stades, noms_stades))

# 2. Remplissage de la Fécondité (Ligne 1)
# Valeurs extraites de ton texte (Age 10 à Age 29)
fecondite <- c(
  18822, 21920, 25150, 28298, 30830, 33301, 35604, 37292, 38649, 40080, 
  41526, 42689, 43718, 44811, 45962, 47017, 47996, 48923, 49813, 50648
)

# On les place de la colonne 13 (Age 10) à la colonne 32 (Age 29)
null_pred[1, 13:32] <- fecondite

# 3. Remplissage de la Survie (Sous-diagonale)
# Valeurs extraites de ton texte (32 transitions au total)
survie <- c(
  0.076, 0.024, 0.007, 0.635, 0.753, 0.811, 0.847, 0.871, 0.888, 0.899, 0.912, 0.924, 0.931, 
  0.933, 0.936, 0.941, 0.942, 0.944, 0.947, 0.949, 0.949, 0.949, 0.950, 0.951, 0.950, 0.950, 
  0.951, 0.951, 0.951, 0.952, 0.952, 0.952
)

for(i in 1:length(survie)) {
  null_pred[i+1, i] <- survie[i]
}

# 4. Calcul du Lambda (Taux de croissance)
max(Re(eigen(null_pred)$values)) # Même chose que dans l'étude

########################
stockage_present<-c(0,231185,14868)
stockage_propose<-c(0,500000,15000)

### Étape 2.3
### Creation de la boucle 


# Créer un vecteur vide (rempli de 0)
nombre_ind <- vector("numeric", length =  n_stades)
names(nombre_ind)<-noms_stades

calculer_structure_initiale <- function(matrice_choisie, n_age1) {
  
  # 1. Préparation du vecteur
  n_stades <- nrow(matrice_choisie)
  noms_stades <- rownames(matrice_choisie)
  pop <- vector("numeric", length = n_stades)
  names(pop) <- noms_stades
  
  # 2. Fixer la valeur de référence
  pop["Age1"] <- n_age1
  
  # 3. REMONTER le temps (Rétro-calcul pour Egg, Larva, Fry)
  # On divise par le taux de survie pour retrouver l'effectif précédent
  # Indice 4 = Age 1, 3 = Fry, 2 = Larva, 1 = Egg
  pop["Fry"]   <- pop["Age1"] / matrice_choisie[4, 3]
  pop["Larva"] <- pop["Fry"]   / matrice_choisie[3, 2]
  pop["Egg"]   <- pop["Larva"] / matrice_choisie[2, 1]
  
  # 4. AVANCER dans le temps (Pour tous les âges de 2 à 30)
  # On multiplie par le taux de survie de la sous-diagonale
  for (i in 4:(n_stades - 1)) {
    # i correspond à l'indice de la colonne (ex: Age 1 est indice 4)
    # i + 1 correspond à l'indice de la ligne suivante (ex: Age 2 est indice 5)
    pop[i + 1] <- pop[i] * matrice_choisie[i + 1, i]
  }
  
  return(pop)
}

# Scénario Mesuré
pop_ini_measured_500 <- calculer_structure_initiale(measured, 500)

# Scénario Endangered
# pop_endangered <- calculer_structure_initiale(endangered, 500)

# Scénario Null
# pop_null <- calculer_structure_initiale(null_prev, 500)


### Générateur de stochasité sur la fécondité
taux_fecondite <- (1 + rnorm(n_stades, mean = 0, sd = 0.25))*measured[1,]

# Vérification
print(taux_fecondite)

# 1. Définir tes paramètres Beta (le "poids" de la certitude)
# On crée un vecteur de 32 valeurs (pour les 32 transitions de survie)
b_params <- c(5.05, 20, 75, rep(0.75, 29)) 

# 2. Extraire les taux de survie réels de la matrice (sous-diagonale)
# On récupère les valeurs où row = col + 1
p_mesure <- diag(measured[-1, -ncol(measured)])

# 3. Calculer Alpha pour que la moyenne de la Beta soit égale à p_mesure
alpha_params <- (p_mesure * b_params) / (1 - p_mesure)

# 4. Tirer les taux de survie
taux_survie <- rbeta(n = length(alpha_params), 
                               shape1 = alpha_params, 
                               shape2 = b_params)
print(taux_survie)
print(p_mesure)
mean(taux_survie-p_mesure)




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


#Nombre d'individus matures après 100 ans (avec stock)
#pas trop clair mais gap de 10 ans avant stock (maturité à 10ans)

stock_max <- 5000

#