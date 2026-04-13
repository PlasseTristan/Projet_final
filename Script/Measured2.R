
# ==========================================================
# PARAMÈTRES DE BASE
# ==========================================================
f_rates <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
             18096, 21035, 24114, 27133, 29563, 31934, 34142, 35761,
             37062, 38435, 39821, 40936, 41923, 42972, 44075, 45086,
             46025, 46914, 47767, 48568, 0)

s_rates <- c(
  0.073, 0.023, 0.006, 0.609, 0.723, 0.778, 0.812, 0.836, 0.851, 0.863, 0.875, 0.887, 0.893,
  0.895, 0.898, 0.902, 0.903, 0.905, 0.908, 0.910, 0.910, 0.910, 0.911, 0.912, 0.911, 0.911,
  0.912, 0.912, 0.912, 0.913, 0.913, 0.913, 0
)

# ==========================================================
# FÉCONDITÉ PONDÉRÉE PAR lx DANS CHAQUE GROUPE
# ==========================================================
f1_eff <- mean(f_rates[1:3])
f2_eff <- mean(f_rates[4:12])
f3_eff <- mean(f_rates[13:23])
f4_eff <- mean(f_rates[24:32])
f5_eff <- f_rates[33]

# ==========================================================
# SURVIE ANNUELLE MOYENNE DANS CHAQUE GROUPE
# ==========================================================
s1 <- prod(s_rates[1:3])  #Produit des 3 survies
s2 <- prod(s_rates[4:12])   # Produit des 9 survies
s3 <- prod(s_rates[13:23])  # Produit des 11 survies
s4 <- prod(s_rates[24:32])  # Produit des 9 survies
s5 <- s_rates[33]           # Stade seul


cat("\n\nSurvies moyennes par groupe :")
cat("\n  s1 :", round(s1, 4))
cat("\n  s2 :", round(s2, 4))
cat("\n  s3 :", round(s3, 4))
cat("\n  s4 :", round(s4, 4))
cat("\n  s5 :", round(s5, 4))

# ==========================================================
# DURÉE MOYENNE DANS CHAQUE GROUPE
# ==========================================================
d1 <- 1
d2 <- 9
d3 <- 11
d4 <- 9
d5 <- 1

# ==========================================================
# PROBABILITÉ RESTER / AVANCER
# ==========================================================
p_rester1  <- 0
p_avancer1 <- s1 * (1/d1)

p_rester2  <- mean(s_rates[4:12])
p_avancer2 <- s2 * (1/d2)

p_rester3  <- mean(s_rates[13:23])
p_avancer3 <- s3 * (1/d3)

p_rester4  <- mean(s_rates[24:32])
p_avancer4 <- s4 * (1/d4)

cat("\n\nProbabilités rester / avancer :")
cat("\n  Groupe 1 : rester =", round(p_rester1, 4), "| avancer =", round(p_avancer1, 4))
cat("\n  Groupe 2 : rester =", round(p_rester2, 4), "| avancer =", round(p_avancer2, 4))
cat("\n  Groupe 3 : rester =", round(p_rester3, 4), "| avancer =", round(p_avancer3, 4))
cat("\n  Groupe 4 : rester =", round(p_rester4, 4), "| avancer =", round(p_avancer4, 4))

# ==========================================================
# CONSTRUCTION DE LA MATRICE M5
# ==========================================================
M5 <- matrix(0, nrow = 5, ncol = 5)

# Fécondité (ligne 1)
M5[1, 1] <- f1_eff   # = 0
M5[1, 2] <- f2_eff
M5[1, 3] <- f3_eff
M5[1, 4] <- f4_eff
M5[1, 5] <- f5_eff

# Rétention (diagonale principale)
M5[1, 1] <- p_rester1
M5[2, 2] <- p_rester2
M5[3, 3] <- p_rester3
M5[4, 4] <- p_rester4
M5[5, 5] <- s5

# Avancement (sous-diagonale)
M5[2, 1] <- p_avancer1
M5[3, 2] <- p_avancer2
M5[4, 3] <- p_avancer3
M5[5, 4] <- p_avancer4

# ==========================================================
# VÉRIFICATION
# ==========================================================


cat("\n\nMatrice réaliste 5x5 :\n")
print(round(M5, 6))

cat("\nLambda matrice originale (measured) :", round(Re(eigen(measured)$values[1]), 6))
cat("\nLambda matrice M5                   :", round(Re(eigen(M5)$values[1]), 6))

measured2 <- M5