# 1. Initialisation d'une matrice vide de 33x33
A <- matrix(0, nrow = 33, ncol = 33)

# 2. Définition des noms des classes
age_names <- c("Egg", "Early_life", "Fry", paste0("Age_", 1:30))
rownames(A) <- colnames(A) <- age_names

# 3. Remplissage de la FÉCONDITÉ (Ligne 1)
# Indices [1, 1] à [1, 12] sont à 0 (déjà initialisés)
A[1, 13] <- 18096  # Age 10
A[1, 14] <- 21035  # Age 11
A[1, 15] <- 24114  # Age 12
A[1, 16] <- 27133  # Age 13
A[1, 17] <- 29563  # Age 14
A[1, 18] <- 31934  # Age 15
A[1, 19] <- 34142  # Age 16
A[1, 20] <- 35761  # Age 17
A[1, 21] <- 37062  # Age 18
A[1, 22] <- 38435  # Age 19
A[1, 23] <- 39821  # Age 20
A[1, 24] <- 40936  # Age 21
A[1, 25] <- 41923  # Age 22
A[1, 26] <- 42972  # Age 23
A[1, 27] <- 44075  # Age 24
A[1, 28] <- 45086  # Age 25
A[1, 29] <- 46025  # Age 26
A[1, 30] <- 46914  # Age 27
A[1, 31] <- 47767  # Age 28
A[1, 32] <- 48568  # Age 29
# A[1, 33] est noté "-" dans vos données, donc laissé à 0.

# 4. Remplissage de la SURVIE (Sous-diagonale [j+1, j])
A[2, 1] <- 0.073    # Egg -> Early life
A[3, 2] <- 0.023    # Early life -> Fry
A[4, 3] <- 0.006    # Fry -> Age 1
A[5, 4] <- 0.609    # Age 1 -> Age 2
A[6, 5] <- 0.723    # Age 2 -> Age 3
A[7, 6] <- 0.778    # Age 3 -> Age 4
A[8, 7] <- 0.812    # Age 4 -> Age 5
A[9, 8] <- 0.836    # Age 5 -> Age 6
A[10, 9] <- 0.851   # Age 6 -> Age 7
A[11, 10] <- 0.863  # Age 7 -> Age 8
A[12, 11] <- 0.875  # Age 8 -> Age 9
A[13, 12] <- 0.887  # Age 9 -> Age 10
A[14, 13] <- 0.893  # Age 10 -> Age 11
A[15, 14] <- 0.895  # Age 11 -> Age 12
A[16, 15] <- 0.898  # Age 12 -> Age 13
A[17, 16] <- 0.902  # Age 13 -> Age 14
A[18, 17] <- 0.903  # Age 14 -> Age 15
A[19, 18] <- 0.905  # Age 15 -> Age 16
A[20, 19] <- 0.908  # Age 16 -> Age 17
A[21, 20] <- 0.910  # Age 17 -> Age 18
A[22, 21] <- 0.910  # Age 18 -> Age 19
A[23, 22] <- 0.910  # Age 19 -> Age 20
A[24, 23] <- 0.911  # Age 20 -> Age 21
A[25, 24] <- 0.912  # Age 21 -> Age 22
A[26, 25] <- 0.911  # Age 22 -> Age 23
A[27, 26] <- 0.911  # Age 23 -> Age 24
A[28, 27] <- 0.912  # Age 24 -> Age 25
A[29, 28] <- 0.912  # Age 25 -> Age 26
A[30, 29] <- 0.912  # Age 26 -> Age 27
A[31, 30] <- 0.913  # Age 27 -> Age 28
A[32, 31] <- 0.913  # Age 28 -> Age 29
A[33, 32] <- 0.913  # Age 29 -> Age 30

# Afficher la matrice (les premières lignes et colonnes pour vérification)
print(A)

# Optionnel : Calculer le taux de croissance (lambda) si vous avez la librairie popbio
# install.packages("popbio")
# library(popbio)
# lambda(A)