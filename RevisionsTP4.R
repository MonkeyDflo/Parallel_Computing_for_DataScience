#révisions TP4

dormir <- function(i) {
  Sys.sleep(i)
  return(paste("le fils", Sys.getpid(), "a dormi", i, "secondes"))
}

temps <- list(5, 30, 5, 10)
print(lapply(temps, FUN = dormir))

# Ex2 ####
#2 Ordonnancement statique des tâches avec durée connue
# 2.1. Calcul séquentiel.

# 2.2. Calcul en parallèle avec répartition de la charge
# (a) par lignes de la matrice: de la plus courte à la plus longue.
# (b) par lignes de la matrice: de la plus longue à la plus courte.
# (c) par bloques à l’aide de la fonction clusterSplit.
# (d) par bloques de manière aléatoire.
# Quels sont vos conclusions?

rep(NA, 3)
