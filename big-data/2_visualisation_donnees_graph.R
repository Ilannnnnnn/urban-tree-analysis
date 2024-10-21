#Charger ggplot2
library(ggplot2)

# Lit le fichier CSV en indiquant de ne pas convertir les chaînes de caractères en facteurs
file_path <- "big-data/echantillon_Patridata/echantillon_Patrimoine_Arboré_(RO).csv"
df <- read.csv(file_path, stringsAsFactors = FALSE)


#Créer des histogrammes

#Répartition des arbres par stade de développement


# Normaliser les valeurs dans la colonne fk_stadedev en les convertissant en minuscules
df$fk_stadedev <- tolower(df$fk_stadedev)

# Créer et afficher le graphique
ggplot(df, aes(x = fk_stadedev)) +  # Initialiser le graphique avec les données et les mappings esthétiques
  geom_bar() +                      # Ajouter un graphique à barres
  labs(                             # Ajouter des étiquettes et un titre
    title = "Répartition des arbres par stade de développement",  # Titre du graphique
    x = "Stade de développement",   # Étiquette de l'axe X
    y = "Nombre d'arbres"           # Étiquette de l'axe Y
  ) +
  theme_minimal()                   # Appliquer un thème minimaliste au graphique


#Répartition des arbres par état de santé
ggplot(df, aes(x = fk_arb_etat)) +
  geom_bar() +
  labs(title = "Répartition des arbres par état de santé",
       x = "État de santé",
       y = "Nombre d'arbres") +
  theme_minimal()


#Distribution des hauteurs des arbres
ggplot(df, aes(x = haut_tot)) +
  geom_histogram(binwidth = 1) +
  labs(title = "Distribution des hauteurs des arbres",
       x = "Hauteur totale (m)",
       y = "Nombre d'arbres") +
  theme_minimal()

#Répartition des arbres remarquables
ggplot(df, aes(x = remarquable)) +
  geom_bar() +
  labs(title = "Répartition des arbres remarquables",
       x = "Remarquable",
       y = "Nombre d'arbres") +
  theme_minimal()

#Répartition des arbres par quartier
ggplot(df, aes(x = clc_quartier)) +
  geom_bar() +
  labs(title = "Répartition des arbres par quartier",
       x = "Quartier",
       y = "Nombre d'arbres") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
  # Orientation du texte des axes X à 45 degrés et alignement à droite


  
  
# Création d'un graphique à barres pour la répartition des arbres par quartier en fonction du nombre de diagnostics
  ggplot(data = df, aes(x = clc_quartier, fill = fk_situation)) +
    geom_bar() +
    labs(title = "Répartition des arbres par quartier selon les diagnostics",
         x = "Quartier",
         y = "Nombre de diagnostics") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  
#Répartition des arbres par type de revêtement
ggplot(df, aes(x = fk_revetement)) +
  geom_bar() +
  labs(title = "Répartition des arbres par type de revêtement",
       x = "Type de revêtement",
       y = "Nombre d'arbres") +
  theme_minimal()

#Distribution des âges estimés des arbres
# Créer et afficher le graphique avec les axes ajustés
ggplot(df, aes(x = age_estim)) +
  geom_histogram(binwidth = 5) +
  labs(title = "Distribution des âges estimés des arbres",
       x = "Âge estimé (années)",
       y = "Nombre d'arbres") +
  theme_minimal() +
  xlim(0, 250)  # Limiter l'axe x de 0 à 250


#Répartition des arbres par situation
ggplot(df, aes(x = fk_situation)) +
  geom_bar() +
  labs(title = "Répartition des arbres par situation",
       x = "Situation",
       y = "Nombre d'arbres") +
  theme_minimal()

#Répartition des arbres par port
df$fk_port <- tolower(df$fk_port)

ggplot(df, aes(x = fk_port)) +
  geom_bar() +
  labs(title = "Répartition des arbres par port",
       x = "Port",
       y = "Nombre d'arbres") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Pivoter les étiquettes d'axe à 45 degrés



#Répartition des arbres par type de pied


df$fk_pied <- tolower(df$fk_pied)

ggplot(df, aes(x = fk_pied)) +
  geom_bar() +
  labs(title = "Répartition des arbres par type de pied",
       x = "Type de pied",
       y = "Nombre d'arbres") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  
  # Pivoter les étiquettes d'axe à 45 degrés



# Créer et afficher un nuage de points pour la répartition des arbres par stade de développement
ggplot(df, aes(x = fk_stadedev)) +
  geom_point(stat = "count") +  # Utiliser stat = "count" pour compter le nombre d'arbres dans chaque catégorie
  labs(title = "Répartition des arbres par stade de développement",
       x = "Stade de développement",
       y = "Nombre d'arbres") +
  theme_minimal()

# Créer et afficher un nuage de points pour la distribution des âges estimés des arbres
ggplot(df, aes(x = age_estim)) +
  stat_count(geom = "point") +
  labs(title = "Distribution des âges estimés des arbres",
       x = "Âge estimé (années)",
       y = "Nombre d'arbres") +
  theme_minimal()


# Créer le nuage de points 
ggplot(df, aes(x = fk_stadedev, y = age_estim)) +
  geom_point() +
  scale_y_continuous(limits = c(0, 100)) +  # Ajuster l'échelle de l'axe y
  labs(title = "Relation entre l'âge estimé et le stade de développement des arbres",
       x = "Stade de développement",
       y = "Âge estimé") +
  theme_minimal()


# Nuage de points pour la relation entre la hauteur totale et l'âge estimé des arbres
ggplot(df, aes(x = age_estim, y = haut_tot)) +
  geom_point() +
  labs(title = "Relation entre l'âge estimé et la hauteur totale des arbres",
       x = "Âge estimé (années)",
       y = "Hauteur totale (mètres)") +
  theme_minimal()

# Nuage de points pour la relation entre le type de pied et la hauteur totale des arbres
ggplot(df, aes(x = fk_pied, y = haut_tot)) +
  geom_point() +
  labs(title = "Relation entre le type de pied et la hauteur totale des arbres",
       x = "Type de pied",
       y = "Hauteur totale (mètres)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Nuage de points pour la relation entre le stade de développement et le diamètre du tronc des arbres
ggplot(df, aes(x = fk_stadedev, y = tronc_diam)) +
  geom_point() +
  labs(title = "Relation entre le stade de développement et le diamètre du tronc des arbres",
       x = "Stade de développement",
       y = "Diamètre du tronc (mètres)") +
  theme_minimal()



ggplot(df, aes(x = fk_pied, y = haut_tot)) +
  geom_point() +
  labs(title = "Taille des arbres adultes en fonction du type de sol",
       x = "Type de sol",
       y = "Taille des arbres adultes (mètres)") +
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


ggplot(df, aes(x = fk_revetement, y = haut_tot)) +
  geom_bar(stat = "identity") +
  labs(title = "Taille des arbres en fonction de la présence de revêtement",
       x = "Revêtement (Présence/Absence)",
       y = "Taille des arbres adultes (mètres)") +
  theme_minimal()

ggplot(df, aes(x = fk_port, y = haut_tot)) +
  geom_bar(stat = "identity") +
  labs(title = "Taille des arbres en fonction du port",
       x = "Port",
       y = "Taille des arbres adultes (mètres)") +
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Création du graphique avec les données df
ggplot(df, aes(x = fk_pied, y = haut_tot)) +
  geom_bar(stat = "identity") +
  labs(title = "Taille des arbres en fonction du type de pied",
       x = "Type de pied",
       y = "Taille des arbres (mètres)") +
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
  


ggplot(df, aes(x = clc_quartier, y = haut_tot)) +
  geom_bar(stat = "identity") +
  labs(title = "Taille des arbres en fonction des quartiers",
       x = "Quartier",
       y = "Taille des arbres (mètres)") +
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
