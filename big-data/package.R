# On charge les librairies nécessaires
packages <- c(
  "glmulti",      # Pour la recherche de modèles
  "ggplot2",      # Pour la visualisation des données
  "caret",        # Pour l'entraînement de modèles de machine learning
  "dplyr",        # Pour la manipulation de données
  "corrplot",     # Pour la visualisation de corrélations
  "tidyverse",    # Pour un ensemble de packages de science des données
  "vcd",          # Pour la visualisation de données catégorielles
  "htmlwidgets",  # Pour créer des visualisations interactives
  "sf",           # Pour la manipulation des données spatiales
  "leaflet"       # Pour la visualisation cartographique
)

# Installer les packages qui ne sont pas déjà installés
new_packages <- packages[!(packages %in% installed.packages()[,"Package"])]
if (length(new_packages)) {
  install.packages(new_packages)
}

# Charger tous les packages nécessaires
lapply(packages, library, character.only = TRUE)

# Message de confirmation
cat("Tous les packages nécessaires ont été chargés avec succès.\n")
