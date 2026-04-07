# Creation du nom de chaque stade
noms_stades <- c("Egg", "Larva", "Fry", paste0("Age", 1:30))
# Nombre de stades
n_stades <- length(noms_stades)

# 2. Créer une matrice vide (remplie de 0)
measured <- matrix(0, nrow = n_stades, ncol = n_stades, 
                   dimnames = list(noms_stades, noms_stades))
# 3. Remplir la PREMIÈRE LIGNE (Fécondité)
# Les indices correspondent aux colonnes où la reproduction commence
fecondite_m <- c(18096, 21035, 24114, 27133, 29563, 31934, 34142, 35761, 
               37062, 38435, 39821, 40936, 41923, 42972, 44075, 45086, 
               46025, 46914, 47767, 48568)

# On place ces valeurs de Age 11 à Age 30 (colonnes 14 à 33) 
# Avant = pas à maturité sexuelle
measured[1, 13:32] <- fecondite_m

# 4. Remplir la SOUS-DIAGONALE (Taux de survie)
# On liste les probabilités de passage d'un stade au suivant
survie_m <- c(
  0.073, 0.023, 0.006, 0.609, 0.723, 0.778, 0.812, 0.836, 0.851, 0.863, 0.875, 0.887, 0.893, # Jusqu'à Age 10
  0.895, 0.898, 0.902, 0.903, 0.905, 0.908, 0.910, 0.910, 0.910, 0.911, 0.912, 0.911, 0.911, # Jusqu'à Age 22
  0.912, 0.912, 0.912, 0.913, 0.913, 0.913 # Fin
)

# On injecte ces valeurs dans la sous-diagonale (ligne i+1, colonne i)
for(i in 1:length(survie_m)) {
  measured[i+1, i] <- survie_m[i]
}


max(Re(eigen(measured)$values)) # Donne la même valeur de lambda que l'étude



# 2. Créer une matrice vide (remplie de 0)
endangered <- matrix(0, nrow = n_stades, ncol = n_stades, 
                     dimnames = list(noms_stades, noms_stades))

# 3. Remplir la PREMIÈRE LIGNE (Fécondité)
# Les indices correspondent aux colonnes où la reproduction commence
fecondite_e <- c(18523, 21571, 24749, 27847, 30339, 32771, 35037, 36699, 
               38034, 39442, 40865, 42009, 43022, 44098, 45230, 46268, 
               47231, 48144, 49019, 49841)

# On place ces valeurs de Age 11 à Age 30 (colonnes 14 à 33)
endangered[1, 14:33] <- fecondite_e

# 4. Remplir la SOUS-DIAGONALE (Taux de survie)
# On liste les probabilités de passage d'un stade au suivant
survie_e <- c(
  0.075, 0.023, 0.007, 0.625, 0.741, 0.798, 0.833, 0.857, 0.873, 0.885, 0.898, 0.910, 0.916, # Jusqu'à Age 10
  0.918, 0.921, 0.926, 0.927, 0.929, 0.932, 0.933, 0.933, 0.934, 0.935, 0.935, 0.935, 0.935, # Suite
  0.936, 0.936, 0.936, 0.937, 0.937, 0.937 # Fin
)

# On injecte ces valeurs dans la sous-diagonale (ligne i+1, colonne i)
for(i in 1:length(survie_e)) {
  endangered[i+1, i] <- survie_e[i]
}

max(Re(eigen(endangered)$values)) # Même chose que dans l'étude



null_pred <- matrix(0, nrow = n_stades, ncol = n_stades, 
                    dimnames = list(noms_stades, noms_stades))

# 2. Remplissage de la Fécondité (Ligne 1)
# Valeurs extraites de ton texte (Age 10 à Age 29)
fecondite_n <- c(
  18822, 21920, 25150, 28298, 30830, 33301, 35604, 37292, 38649, 40080, 
  41526, 42689, 43718, 44811, 45962, 47017, 47996, 48923, 49813, 50648
)

# On les place de la colonne 13 (Age 10) à la colonne 32 (Age 29)
null_pred[1, 13:32] <- fecondite_n

# 3. Remplissage de la Survie (Sous-diagonale)
# Valeurs extraites de ton texte (32 transitions au total)
survie_n <- c(
  0.076, 0.024, 0.007, 0.635, 0.753, 0.811, 0.847, 0.871, 0.888, 0.899, 0.912, 0.924, 0.931, 
  0.933, 0.936, 0.941, 0.942, 0.944, 0.947, 0.949, 0.949, 0.949, 0.950, 0.951, 0.950, 0.950, 
  0.951, 0.951, 0.951, 0.952, 0.952, 0.952
)

for(i in 1:length(survie_n)) {
  null_pred[i+1, i] <- survie_n[i]
}

# 4. Calcul du Lambda (Taux de croissance)
lambda<-max(Re(eigen(null_pred)$values)) # Même chose que dans l'étude
