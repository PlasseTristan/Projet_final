# ==========================================================
# PARAMÈTRES DE BASE measured
# ==========================================================
f_rates <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
             18096, 21035, 24114, 27133, 29563, 31934, 34142, 35761,
             37062, 38435, 39821, 40936, 41923, 42972, 44075, 45086,
             46025, 46914, 47767, 48568, 0)

s_rates <- c(
  0.073, 0.023, 0.006, 0.609, 0.723, 0.778, 0.812, 0.836, 0.851, 0.863, 0.875, 0.887, 0.893,
  0.895, 0.898, 0.902, 0.903, 0.905, 0.908, 0.910, 0.910, 0.910, 0.911, 0.912, 0.911, 0.911,
  0.912, 0.912, 0.912, 0.913, 0.913, 0.913, 0)
# ==========================================================
# FÉCONDITÉ moyenne DANS CHAQUE GROUPE
# ==========================================================
f1_eff <- mean(f_rates[1:3])
f2_eff <- mean(f_rates[4:12])
f3_eff <- mean(f_rates[13:32])
f4_eff <- f_rates[33]

# ==========================================================
# SURVIE ANNUELLE cumulée par groupe
# ==========================================================
s1 <- prod(s_rates[1:3])  #Produit des 3 survies
s2 <- prod(s_rates[4:12])   # Produit des 9 survies
s3 <- prod(s_rates[13:32])  # Produit des 20 survies
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
M10 <- matrix(0, nrow = 4, ncol = 4)

# Fécondité (ligne 1)
M10[1, 1] <- f1_eff   # = 0
M10[1, 2] <- f2_eff
M10[1, 3] <- f3_eff
M10[1, 4] <- f4_eff


# Rétention (diagonale principale)
M10[1, 1] <- p_rester1
M10[2, 2] <- p_rester2
M10[3, 3] <- p_rester3
M10[4, 4] <- s4

# Avancement (sous-diagonale)
M10[2, 1] <- p_avancer1
M10[3, 2] <- p_avancer2
M10[4, 3] <- p_avancer3

# ==========================================================
# VÉRIFICATION
# ==========================================================


cat("\n\nMatrice réaliste 4x4 :\n")
print(round(M10, 6))

cat("\nLambda matrice originale (endangered) :", round(Re(eigen(measured)$values[1]), 6))
cat("\nLambda matrice M10                   :", round(Re(eigen(M10)$values[1]), 6))

measured3 <- M10
measured3
