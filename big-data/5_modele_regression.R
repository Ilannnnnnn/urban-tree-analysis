# On charge les librairies nécessaires
library(glmulti)
library(ggplot2)
library(caret)
library(dplyr)


# On charge les données
# Pour ouvrir le fichier -> Session -> Set Working Directory -> To Source File Location
df <- read.csv("data/echantillon_Patrimoine_Arboré_(RO).csv") 

# On convertit les colonnes de dates
df$dte_plantation <- as.Date(df$dte_plantation, format="%Y/%m/%d")
df$dte_abattage <- as.Date(df$dte_abattage, format="%Y/%m/%d")


# On supprime les doublons en ne gardant que la première occurrence
df <- df[!duplicated(df$OBJECTID), ]


#On supprime des colonnes inutiles
df$Editor <- NULL
df$last_edited_user <- NULL
df$Creator <- NULL
df$created_user <- NULL
df$CreationDate <- NULL
df$EditDate <- NULL
df$created_date <- NULL
df$src_geo <-NULL
df$last_edited_date <-NULL


#On attribut un état (vivant ou mort)
# on crée une nouvelle colonne etat_arbre
df$etat_arbre <- "Non défini"
# on attribut les valeurs "Vivant" et "Mort" en fonction de fk_arb_etat
df$etat_arbre[df$fk_arb_etat %in% c("EN PLACE")] <- "Vivant"
df$etat_arbre[df$fk_arb_etat %in% c("SUPPRIMÉ", "ABATTU", "Essouché", "Non essouché", "REMPLACÉ")] <- "Mort"
# on compte le nombre d'arbres par état de vie
table(df$etat_arbre)
# On filtre les lignes où etat_arbre est "Vivant" ou "Mort" pour enlever les "Non défini" 
df <- df[df$etat_arbre %in% c("Vivant", "Mort"), ]


# Obtenir la date actuelle
current_date <- Sys.Date()


# Calculer l'âge réel de chaque arbre pour les arbres ayant une date de plantation et pas de date d'abattage
df$age_estim <- ifelse(!is.na(df$dte_plantation) & (is.na(df$dte_abattage)),
                      floor(as.numeric(difftime(current_date, df$dte_plantation, units = "days")) / 365),
                      df$age_estim)


#on calcule l'age des arbres ayant été abattus et dont la datede plantation est connue
df$age_estim <- ifelse(!is.na(df$dte_plantation) & (!is.na(df$dte_abattage)),
                       floor(as.numeric(difftime(df$dte_abattage, df$dte_plantation, units = "days")) / 365),
                       df$age_estim)


#On enlève les lignes où la date de plantation de l'arbre arrive après la date d'abatage
df <- df[!(df$age_estim < 0) , ]



#On supprime les aberrations d'age 
df <- df[!(df$age_estim > 1000 ) , ]


#On supprime les lignes dans lesquelles l'état de l'arbre est marqué "EN PLACE" alors qu'il a une date d'abattage
df <- df[!(df$fk_arb_etat == "EN PLACE" & !is.na(df$dte_abattage)), ]







#On enlève les arbres mort n'ayant pas de hauteur pour eviter de creer des erreurs par la suite 
df <- df[!(df$etat_arbre == "Mort" & df$haut_tot == 0), ]

#on calcule l'age des arbres ayant été abattus et dont la datede plantation est connue
df$fk_prec_estim <- ifelse(is.na(df$fk_prec_estim) & (!is.na(df$dte_plantation)),
                       0,
                       df$fk_prec_estim)


#On attribut le stade de développement aux case n'en ayant pas (Je n'ai fait que "jeune" car le + simple)
df$fk_stadedev <- ifelse((df$fk_stadedev == "") & (df$age_estim <=18),
                       "jeune",
                       df$fk_stadedev)


#On enlève les valeurs aberrantes lié au stade de développement
df <- df[ !(df$age_estim >25 & ((df$fk_stadedev == "jeune") | (df$fk_stadedev == "Jeune"))), ]
df <- df[ !(df$age_estim < 15 & ((df$fk_stadedev == "adulte") | (df$fk_stadedev == "Adulte"))), ]


#On enlève les occurences où la hauteur du tronc est supérieur à la hauteur totale
df <- df[ !(df$haut_tot < df$haut_tronc), ]


#On impute les valeurs manquantes de age_estim avec la moyenne par espèce
mean_age_by_species <- aggregate(age_estim ~ nomfrancais, data = df, FUN = mean, na.rm = TRUE)
df <- merge(df, mean_age_by_species, by = "nomfrancais", all.x = TRUE)
df$age_estim <- ifelse(is.na(df$age_estim.x), df$age_estim.y, df$age_estim.x)
df$age_estim.y <- NULL
df$age_estim.x <- NULL

#On retire toutes les lignes où il nous manque une info 
df <- df[!(df$nomfrancais == ""), ]
df <- df[!(df$clc_quartier == ""), ]
df <- df[!(df$fk_port == ""), ]
df <- df[!(df$fk_pied == ""), ]
df <- df[!(df$fk_revetement == ""), ]
df <- df[ !(df$fk_situation == ""),  ]
df <- df[!(df$commentaire_environnement == " "), ]
df <- df[!(df$fk_nomtech == ""), ]
df <- df[!(df$villeca == ""), ]
df <- df[!(df$feuillage == ""), ]
df <- df[!(df$remarquable == ""), ]
#On rassemble les différentes technique de coupe
df$coupe_categorie <- ifelse(df$fk_port %in% c("libre", "Libre", "relâché", "semi libre", "Semi libre"), "Libre",
                             ifelse(df$fk_port %in% c("réduit", "rideau", "réduit relâché"), "Réduit",
                                    ifelse(df$fk_port %in% c("Couronne", "couronné", "architecturé"), "Couronne",
                                           ifelse(df$fk_port %in% c("cépée", "étêté", "têtard", "tête de chat", "tête de chat relaché", "têtard relaché", "têtard relâché"), "Élagage spécifique",
                                                  NA))))

#On rassemble les lieux de plantation
# Regroupement des lieux de plantation
df$lieu_categorie <- ifelse(df$fk_pied %in% c("bande de terre", "Bande de terre", "gazon", "Terre"), "Sol naturel",
                            ifelse(df$fk_pied %in% c("fosse arbre", "Bac de "), "Plantation urbaine spécifique",
                                   ifelse(df$fk_pied %in% c("Revetement non permeable"), "Sol artificiel", "Autre")))

df$stadedvp <- ifelse(df$fk_stadedev %in% c("jeune", "Jeune"), "Jeune",
                      ifelse(df$fk_stadedev %in% c("adulte", "Adulte"), "Adulte",
                             ifelse(df$fk_stadedev %in% c("vieux", "Vieux"), "Vieux", 
                                    ifelse(df$fk_stadedev %in% c("senescent", "Senescent"), "Senescent",
                                           NA))))



#On enlève les lignes où l'OBJECT ID n'est pas renseigné
df <- df[ !is.na(df$OBJECTID), ]

# Statistiques descriptives
print("Statistiques descriptives par espèce :")
print(aggregate(cbind(haut_tot, haut_tronc, tronc_diam) ~ nomfrancais, data = df, FUN = summary))

# On affiche le nombre d'arbres par etat
etat_counts <- table(df$fk_arb_etat)
print("Nombre d'arbres par état :")
print(etat_counts)


# On affiche le nombre d'arbres par espèce (10 espèces les plus fréquentes)
species_counts <- sort(table(df$nomfrancais), decreasing = TRUE)
print("Nombre d'arbres par espèce (10 plus fréquentes) :")
print(head(species_counts, 10))





# On utilise une approche statistique pour savoir où planter les arbres
# Calculer le nombre d'arbres par quartier
arbres_par_quartier <- aggregate(id_arbre ~ clc_quartier, data = df, FUN = length)

# Calculer la densité moyenne d'arbres
densite_moyenne <- mean(arbres_par_quartier$id_arbre)

# Identifier les quartiers avec un nombre d'arbres inférieur à la moyenne
quartiers_prioritaires <- subset(arbres_par_quartier, id_arbre < densite_moyenne)

# Ajouter une colonne pour le nombre d'arbres à planter
quartiers_prioritaires$arbres_a_planter <- densite_moyenne - quartiers_prioritaires$id_arbre

# Afficher les quartiers prioritaires
print(quartiers_prioritaires)

# Visualiser la densité d'arbres par quartier
ggplot(arbres_par_quartier, aes(x = clc_quartier, y = id_arbre)) +
  geom_bar(stat = "identity") +
  geom_hline(yintercept = densite_moyenne, linetype = "dashed", color = "red") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title = "Nombre d'Arbres par Quartier", x = "Quartier", y = "Nombre d'Arbres")

# Sauvegarder le dataframe des quartiers prioritaires avec les arbres à planter
write.csv(quartiers_prioritaires, "quartiers_prioritaires_plan_de_plantation.csv", row.names = FALSE)






#Optimisation avec glmulti
# On selectionne les variables qualitatives pour l'apprentissage
colonnes_selectionnees <- c("age_estim", "haut_tronc", "haut_tot", "tronc_diam", "stadedvp", "lieu_categorie", "coupe_categorie", "etat_arbre")

# On divise des données en ensembles d'entraînement et de test
set.seed(123) #On choisit arbitrairement cette seed pour la reproductibilité
index_entrainement <- createDataPartition(df$age_estim, p = 0.8, list = FALSE)
donnees_entrainement_selection <- df[index_entrainement, ] %>% select(all_of(colonnes_selectionnees))
donnees_test <- df[-index_entrainement, ] %>% select(all_of(colonnes_selectionnees))

# Formule pour glmulti (avec toutes les variables explicatives possibles)
formule <- age_estim ~ haut_tronc + haut_tot + tronc_diam + stadedvp + lieu_categorie + coupe_categorie + etat_arbre

# On utilise le modèle de glmulti (recherche exhaustive)
modele_glmulti <- glmulti(formule, data = donnees_entrainement_selection, 
                          level = 1, #level 2 prends beaucoup de temps (pas de résultat après 24h de calcul) donc on reste sur level 1
                          method = "h", 
                          crit = "aicc", 
                          confsetsize = 100, 
                          plotty = FALSE, report = FALSE)

# On obtient le meilleur modèle
meilleur_modele <- modele_glmulti@objects[[1]]
print(meilleur_modele)
# Graphique de l'importance de chaque variable dans l'apprentissage du modèle
plot(modele_glmulti, type = "s") 

# On entraine le modèle avec le meilleur modèle glmulti
modele_lm <- lm(meilleur_modele, data = donnees_entrainement_selection)

# Résumé du modèle
summary(modele_lm)

# Prédictions sur l'ensemble de test
predictions_lm <- predict(modele_lm, newdata = donnees_test)

# Évaluation du modèle
MSE <- mean((donnees_test$age_estim - predictions_lm)^2, na.rm = TRUE)
RMSE <- sqrt(MSE)
R2 <- summary(modele_lm)$r.squared

cat("MSE:", MSE, "\n")
cat("RMSE:", RMSE, "\n")
cat("R²:", R2, "\n")




#Regression logistique arbres à abattre
#On crée la variable "a_abattre"
df <- df[!(is.na(df$tronc_diam)) , ]
df$a_abattre <- ifelse(df$fk_arb_etat %in% c("EN PLACE"), "NON", "OUI")

# On divise des données en ensembles d'entraînement et de test
set.seed(123)  #On choisit arbitrairement cette seed pour la reproductibilité
index_entrainement <- createDataPartition(df$a_abattre, p = 0.8, list = FALSE)
donnees_entrainement <- df[index_entrainement, ]
donnees_test <- df[-index_entrainement, ]
# On verifie les valeurs manquantes dans toutes les colonnes
colSums(is.na(donnees_entrainement))
# On selectionne les colonnes souhaitées pour l'entraînement
colonnes_selectionnees <- c("etat_arbre", "stadedvp", "haut_tot", "tronc_diam", "a_abattre", "lieu_categorie", "coupe_categorie")
donnees_entrainement_selection <- donnees_entrainement %>% select(all_of(colonnes_selectionnees))

# On entraine le modèle de régression logistique
# On utilise plus glmulti car le rapport temps/efficacité n'est pas bon
modele_logit <- train(as.factor(a_abattre) ~ stadedvp + haut_tot + tronc_diam + lieu_categorie + coupe_categorie,
                data = donnees_entrainement_selection,
                method = "glm",
                family = binomial,
                trControl = trainControl(method = "cv", number = 10, sampling="smote")) #Smote permet de gerer le déséquilibre de classes
# On affiche les résultats
print(modele_logit)

# Prédictions sur l'ensemble de test
predictions_logit <- predict(modele_logit, newdata = donnees_test, type = "prob")
print(predictions_logit)

predictions_classe <- ifelse(predictions_logit[, "NON"] > 0.5, "NON", "OUI")
predictions_classe <- factor(predictions_classe, levels = c("NON", "OUI"))

# On affiche la matrice de confusion
confusionMatrix(predictions_classe, as.factor(donnees_test$a_abattre))



