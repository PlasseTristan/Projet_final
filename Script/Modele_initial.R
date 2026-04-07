# 1. Copie de la matrice
mat_annuelle <- null_pred

# 2. Calcul de la survie combinée de l'An 0 (Oeuf -> Age 1)
# On multiplie les survies des 3 premiers stades
s_an1 <- null_pred[2,1] * null_pred[3,2] * null_pred[4,3]

# 3. RECRUTEMENT DIRECT : Les adultes produisent des individus de Stade 4 (Age 1)
# On prend la fécondité (ligne 1) et on la multiplie par la survie de la 1ère année
mat_annuelle[4, 13:33] <- null_pred[1, 13:33] * s_an1

# 4. NETTOYAGE CRUCIAL : On supprime les anciennes transitions des stades 1, 2 et 3
# On met à zéro les colonnes et lignes 1, 2 et 3 pour que personne ne "stagne" dedans
mat_annuelle[1:3, ] <- 0
mat_annuelle[, 1:3] <- 0

# 5. Calcul du Lambda sur cette matrice "propre"
lambda_reel <- Re(eigen(mat_annuelle)$values[1])

### Runner les modèles 100 ans ###

n_annees <- 100
simulation <- matrix(0, nrow = n_stades, ncol = n_annees + 1)

# Année 0 : On génère la pop initiale
# (On vide les stades 1, 2, 3 de la pop initiale car ils sont maintenant "instantanés")
pop_init <- calculer_structure_initiale(null_pred, 500)
pop_init[1:3] <- 0 
simulation[, 1] <- pop_init

for (t in 1:n_annees) {
  # N(t+1) = Matrice %*% N(t)
  simulation[, t+1] <- mat_annuelle %*% simulation[, t]
}

# --- Vérifications ---
cat("N0 total :", sum(simulation[, 1]), "\n")
cat("Projection (N0 * L^100) :", sum(simulation[, 1]) * lambda_reel^100, "\n")
cat("Simulation An 100 :", sum(simulation[, 101]), "\n")