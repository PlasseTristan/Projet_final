# ==========================================================
# PARAMÈTRES DE BASE NULL_pred
# ==========================================================
f_rates <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
             18822, 21920, 25150, 28298, 30830, 33301, 35604, 37292, 38649, 40080, 
             41526, 42689, 43718, 44811, 45962, 47017, 47996, 48923, 49813, 50648,0)

s_rates <- c(
  0.076, 0.024, 0.007, 0.635, 0.753, 0.811, 0.847, 0.871, 0.888, 0.899, 0.912, 0.924, 0.931, 
  0.933, 0.936, 0.941, 0.942, 0.944, 0.947, 0.949, 0.949, 0.949, 0.950, 0.951, 0.950, 0.950, 
  0.951, 0.951, 0.951, 0.952, 0.952, 0.952, 0)

# ==========================================================
# FÉCONDITÉ PONDÉRÉE PAR lx DANS CHAQUE GROUPE
# ==========================================================
f1_eff <- mean(f_rates[1:3])
f2_eff <- mean(f_rates[4:12])
f3_eff <- mean(f_rates[13:32])
f4_eff <- f_rates[33]

# ==========================================================
# SURVIE ANNUELLE MOYENNE DANS CHAQUE GROUPE
# ==========================================================
s1 <- prod(s_rates[1:3])  #Produit des 3 survies
s2 <- prod(s_rates[4:12])   # Produit des 9 survies
s3 <- prod(s_rates[13:32])  # Produit des 11 survies
s4 <- s_rates[33]           # Stade seul


cat("\n\nSurvies moyennes par groupe :")
cat("\n  s1 :", round(s1, 4))
cat("\n  s2 :", round(s2, 4))
cat("\n  s3 :", round(s3, 4))
cat("\n  s4 :", round(s4, 4))


# ==========================================================
# DURÉE MOYENNE DANS CHAQUE GROUPE
# ==========================================================
d1 <- 1
d2 <- 9
d3 <- 20
d4 <- 1

# ==========================================================
# PROBABILITÉ RESTER / AVANCER
# ==========================================================
p_rester1  <- 0
p_avancer1 <- s1 * (1/d1)

p_rester2  <- mean(s_rates[4:12])
p_avancer2 <- s2 * (1/d2)

p_rester3  <- mean(s_rates[13:32])
p_avancer3 <- s3 * (1/d3)


cat("\n\nProbabilités rester / avancer :")
cat("\n  Groupe 1 : rester =", round(p_rester1, 4), "| avancer =", round(p_avancer1, 4))
cat("\n  Groupe 2 : rester =", round(p_rester2, 4), "| avancer =", round(p_avancer2, 4))
cat("\n  Groupe 3 : rester =", round(p_rester3, 4), "| avancer =", round(p_avancer3, 4))


# ==========================================================
# CONSTRUCTION DE LA MATRICE M5
# ==========================================================
M8 <- matrix(0, nrow = 4, ncol = 4)

# Fécondité (ligne 1)
M8[1, 1] <- f1_eff   # = 0
M8[1, 2] <- f2_eff
M8[1, 3] <- f3_eff
M8[1, 4] <- f4_eff


# Rétention (diagonale principale)
M8[1, 1] <- p_rester1
M8[2, 2] <- p_rester2
M8[3, 3] <- p_rester3
M8[4, 4] <- s4

# Avancement (sous-diagonale)
M8[2, 1] <- p_avancer1
M8[3, 2] <- p_avancer2
M8[4, 3] <- p_avancer3

# ==========================================================
# VÉRIFICATION
# ==========================================================


cat("\n\nMatrice réaliste 4x4 :\n")
print(round(M8, 6))

cat("\nLambda matrice originale (null_pred) :", round(Re(eigen(null_pred)$values[1]), 6))
cat("\nLambda matrice M8                   :", round(Re(eigen(M8)$values[1]), 6))

null_pred3 <- M8