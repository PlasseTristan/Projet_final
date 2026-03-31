---
  title: "Devoir 2"
author: "Les saumons maurons"
date: "12/03/2025"
output: html_document
---
  
  
  # À remettre sur Moodle au plus tard le 28 mars 2025 avant le début du cours. 
  
  ## Question 1 
  ### Mise en contexte
  
  Vous êtes impliqué dans un projet dont l'intérêt est d'étudier comment deux espèces bactériennes interagissent entre elles dans un environnement hautement contrôlé (c'est-à-dire une boîte de Petri). Au début de l'expérience, 50 individus de l'espèce *bogustonia proii* et 10 individus de l'espèce *aleastonia predatora* ont été inoculés dans la boîte de Petri. Pendant l'expérience, les individus de chaque espèce ont été comptés indépendamment toutes les heures pendant trois semaines (beaucoup d'efforts et de nuits blanches ont été nécessaires pour ce projet).  

Les données qui ont été recueillies pendant ces deux semaines sont disponibles dans le fichier `2Bacteries.csv`.

```{R}
library(deSolve)
# ouvrir le jeu de données
bact<-read.csv("2Bactéries.csv")

```
Un chercheur impliqué dans le projet pense que la relation entre les deux espèces bactériennes est une relation consommateur-ressource, mais il n'est pas certain de comment construire un modèle pour tester cette hypothèse. Comme il y a plusieurs façons de construire un modèle consommateur-ressource, proposer un modèle consommateur-ressource qui vous permettra de mieux caractériser la dynamique entre les deux espèces. 


### 2 points

a) Présenter le modèle que vous souhaitez utiliser. Assurez-vous de bien définir la notation pour le modèle.

Le modèle de Lokta-Voltera sera utilisé pour tester l'hypothèse du chercheur et tenter de modéliser la relation entre les deux espèces.

$$
  \frac{dR}{dt} = \alpha R(t) - \beta * R(t)*C(t)
$$

  $$
  \frac{dC}{dt} = \gamma R(t)*C(t) - \delta *C(t)
$$
  
  
  Définition des variables et des paramètres

- R(t)   : taille de la population de proies à l'instant t 
- C(t)   : taille de la population de prédateurs à l'instant t 
- $\alpha$ : taux de croissance naturel des proies en l'absence de prédateurs 
- $\beta$  : taux de prédation 
- $\gamma$ : taux de conversion des proies consommées en nouveaux prédateurs 
- $\delta$ : mortalité naturelle des prédateurs en absence de proies 


***

### 8 points

b) Implémentez le modèle que vous considérez le plus approprié dans R en utilisant les techniques apprises en cours et essayez une série de valeurs différentes pour trouver les meilleures paramètres. Utilisez la corrélation de Pearson pour trouver les paramètres qui correspondent le mieux aux données.

```{R}
#Visualiser les données
plot(bact$temps, bact$bogustonia_proii)
points(bact$temps,bact$aleastonia_predatora, col = "red")

#Fonction de proies prédateur lockta-voltera
test <- function(t, vars, parms=c( alpha = 0, beta=0, delta=0, gamma = 0)){
  with(as.list(c(parms, vars)), {
    
    # Modèle 
    dR <- alpha*R - (beta*R*C) # dR/dt
    dC <- (gamma*R*C) - (delta*C) #dN2/dt
    
    # Résultat
    res <- c(dR=dR, dC = dC)
    return(list(res))
  })
}

## fonction pour dessiner la solution de R (proies)
dessinSol_R <- function(ic=c(R=10, C=5), tmax=1, times=seq(0,tmax,by=tmax/500),func, parms, ... ) {
  soln <- ode(ic, times, func, parms)
  lines(soln[,1], soln[,"R"], col="blue", lwd=3, ...)
}

## fonction pour dessiner la solution de C (prédateur)
dessinSol_C <- function(ic=c(R=10, C=5), tmax=1, times=seq(0,tmax,by=tmax/500),func, parms, ... ) {
  soln <- ode(ic, times, func, parms)
  lines(soln[,1], soln[,"C"], col="blue", lwd=3, ...)
}

## Dessiner des solutions du modèle 
tmax <- max(bact$temps) # Fin de la série temporelle pour l'intégration numérique de système d'équation différentiel

par(mfrow = c(1, 2))
## Dessiner la base de la figure:
plot(0,0,xlim=c(0,tmax),ylim=c(0,max(bact$bogustonia_proii)), #On limite l'axe des y à notre plus haute incidence
type="n",xlab="Temps (semaines)",
ylab="Incidence (%)")

## Les Conditions initiales
R_0<-10
C_0<-5


## Dessin de la solution pour quelques valeurs du paramètre A:
alpha = c(0.06,0.2)
beta=0.00005
gamma=0.05
delta = 0.035


for (i in 1:length(alpha)) {
  dessinSol_R(ic=c(R=R_0,C=C_0), tmax=tmax,
              func=test,
              parms=c(alpha = alpha[i], beta=beta, delta=delta, gamma = gamma),
              lty=i # Utiliser différent style de ligne pour chaque solution
  )
}

# Ajout d'un affichage de nos données observées
lines(bact$temps, bact$bogustonia_proii, 
      col = "red",       # Couleur distincte pour les données
      lwd = 2,           # Épaisseur de ligne
      type = "b")



plot(0,0,xlim=c(0,tmax),ylim=c(0,max(bact$aleastonia_predatora)), #On limite l'axe des y à notre plus haute incidence
     type="n",xlab="Temps (semaines)",
     ylab="Incidence (%)")


for (i in 1:length(alpha)) {
  dessinSol_C(ic=c(R=R_0,C=C_0), tmax=tmax,
              func=test,
              parms=c(alpha = alpha[i], beta=beta, delta=delta, gamma = gamma),
              lty=i # Utiliser différent style de ligne pour chaque solution
  )
}
lines(bact$temps, bact$aleastonia_predatora, 
      col = "red",       # Couleur distincte pour les données
      lwd = 2,           # Épaisseur de ligne
      type = "b")

# Définition des plages de recherche
grille_parametres <- expand.grid(
  alpha = seq(0.03, 0.09, by = 0.01),
  beta  = seq(0.0075, 0.01, by = 0.0005),
  delta = seq(0.01, 0.05, by = 0.01),
  gamma = seq(0.0005, 0.002, by = 0.0005)
)

# Visualisation
print(paste("Nombre total de simulations à lancer :", nrow(grille_parametres)))


## Conditions initiales
tmax <- max(bact$temps)
R0 <-  bact$bogustonia_proii[1]
C0 <- bact$aleastonia_predatora[1]


grille_parametres$correlation <- NA


for (i in 1:nrow(grille_parametres)) {
  # 1. Préparation des paramètres
  current_parms <- c(alpha = grille_parametres$alpha[i], 
                     beta  = grille_parametres$beta[i], 
                     gamma = grille_parametres$gamma[i], 
                     delta = grille_parametres$delta[i])
  
  soln <- NULL # On réinitialise à chaque tour
  
  # 2. Simulation
  try({
    soln <- ode(y = c(R=R0, C=C0), 
                times = bact$temps, 
                func = test, 
                parms = current_parms,
                atol = 1e-4, # Moins strict sur la précision absolue
                rtol = 1e-4)
  }, silent = TRUE)
  
  # 3. On vérifie si soln existe ET s'il a le même nombre de lignes que nos données
  if (!is.null(soln) && nrow(soln) == length(bact$bogustonia_proii)) {
    
    i_simule <- soln[, "R"] 
    
    # On vérifie aussi que la simulation n'est pas une ligne plate (sd > 0)
    # sinon cor() renverra NA et un avertissement
    if (sd(i_simule) > 0) {
      # Corrélation pour les Proies (R)
      cor_R <- cor(i_simule, bact$bogustonia_proii)
      
      # Corrélation pour les Prédateurs (C)
      c_simule <- soln[, "C"]
      cor_C <- cor(c_simule, bact$aleastonia_predatora)
      
      # Le score final est la moyenne des deux
      grille_parametres$correlation[i] <- (cor_R + cor_C) / 2
    } else {
      grille_parametres$correlation[i] <- -1 # Mauvais fit (constant)
    }
    
  } else {
    # Si la simulation a planté ou est incomplète, on met un score très bas
    grille_parametres$correlation[i] <- -1
  }
}

# Extraction de la meilleure ligne
meilleur_param <- grille_parametres[which.max(grille_parametres$correlation), ]

print(meilleur_param)


# Récupération des meilleurs paramètres trouvés
p_opti <- c(alpha = meilleur_param$alpha, 
            beta = meilleur_param$beta, 
            gamma = meilleur_param$gamma, 
            delta = meilleur_param$delta)

# Simulation finale
soln_opti <- ode(y = c(R=R0, C=C0), times = bact$temps, func = test, parms = p_opti)


par(mfrow=c(1,2))
plot(soln_opti[,"time"], soln_opti[,"R"], col="red", type="b", main="Meilleur Fit - Proies", ylab = "Abondance des proies", xlab = "Temps")
lines(bact$temps, bact$bogustonia_proii, col="blue", lwd=3)
legend("topright", legend=c("Modèle", "Données"), col=c("red", "blue"), lty=1,cex = 0.5)

plot(bact$temps, bact$aleastonia_predatora, col="red", type="b", main="Meilleur Fit - Prédateurs", ylab = "Abondance des prédateurs", xlab = "Temps")
lines(soln_opti[,"time"], soln_opti[,"C"], col="blue", lwd=3)
legend("topright", legend=c("Données", "Modèle"), col=c("red", "blue"), lty=1,cex = 0.5)

par(mfrow=c(1,1))
plot(soln_opti[,"time"], soln_opti[,"R"], col="red", type="c", main="Meilleur Fit - Prédateur et proies",ylab = "Abondance des bactéries", xlab = "Temps")
lines(soln_opti[,"time"], soln_opti[,"R"], col="green", lwd=3)
lines(soln_opti[,"time"], soln_opti[,"C"], col="blue", lwd=3)
legend("topright", legend=c("R", "C"), col=c("green", "blue"), lty=1,cex = 0.5)
```



***
  
  ### 1 point
  
  c) Même si vous n'obtenez pas un modèle qui s'adapte très bien aux données, expliquez brièvement comment vous pensez que les différents modèles que vous obtenez peuvent aider à rejeter (ou non) l'hypothèse proposée par ce chercheur.

Bien que les valeurs que l'on obtient ne sont pas exactes, le modèle semble bien coller aux données avec une corrélation de Pearson d'environ 0,97 et un résultat visuel similaire. Les pics sont caractéristiques d'une relation proie-prédateur classique ou consomateur ressource. Les résultats obtenus tendent donc vers la validation de l'hypothèse principale du chercheur qui est une relation consomateur-ressource entre deux bactéries.

***

### 1 point

d) Après avoir étudié les données en utilisant le modèle consommateur-ressource à la question précédent, proposez une nouvelle expérience qui aiderait à donner des réponses plus précises sur les facteurs générant le comportement périodique trouvé pour les deux espèces bactériennes.

Assurez-vous que l'expérience puisse être réalisée dans un délai raisonnable (au maximum 3 semaines).