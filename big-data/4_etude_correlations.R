library(corrplot)
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


#On enlève les lignes où la datede plantation de l'arbre arrive après la date d'abatage
df <- df[!(df$age_estim < 0) , ]



#On supprime les aberrations d'age 
df <- df[!(df$age_estim > 1000 ) , ]


#On supprime les lignes dans lesquelles l'état de l'arbre est marqué "EN PLACE" alors qu'il a une date d'abattage
df <- df[!(df$fk_arb_etat == "EN PLACE" & !is.na(df$dte_abattage)), ]


#On supprime les lignes où la situation de l'arbre n'est pas donnée
df <- df[ !(df$fk_situation == ""),  ]





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
df <- df[!(df$commentaire_environnement == " "), ]
df <- df[!(df$fk_nomtech == ""), ]
df <- df[!(df$villeca == ""), ]
df <- df[!(df$feuillage == ""), ]
df <- df[!(df$remarquable == ""), ]
df <- df[ !is.na(df$tronc_diam), ]

#On rassemble les différentes technique de coupe
df$coupe_categorie <- ifelse(df$fk_port %in% c("libre", "Libre", "relâché", "semi libre", "Semi libre"), "Libre",
                             ifelse(df$fk_port %in% c("réduit", "rideau", "réduit relâché"), "Réduit",
                                    ifelse(df$fk_port %in% c("Couronne", "couronné", "architecturé"), "Couronne",
                                           ifelse(df$fk_port %in% c("cépée", "étêté", "têtard", "tête de chat", "tête de chat relaché", "têtard relaché", "têtard relâché"), "Élagage spécifique",
                                                  NA))))

# On rassemble les lieux de plantation
df$lieu_categorie <- ifelse(df$fk_pied %in% c("bande de terre", "Bande de terre", "gazon", "Terre"), "Sol naturel",
                            ifelse(df$fk_pied %in% c("fosse arbre", "Bac de plantation"), "Plantation urbaine spécifique",
                                   ifelse(df$fk_pied %in% c("Revetement non permeable"), "Sol artificiel", "Autre")))

df$stadedvp <- ifelse(df$fk_stadedev %in% c("jeune", "Jeune"), "Jeune",
                      ifelse(df$fk_stadedev %in% c("adulte", "Adulte"), "Adulte",
                             ifelse(df$fk_stadedev %in% c("vieux", "Vieux"), "Vieux", 
                                    ifelse(df$fk_stadedev %in% c("senescent", "Senescent"), "Senescent",
                                           NA))))



# On enlève les lignes où l'OBJECT ID n'est pas renseigné
df <- df[ !is.na(df$OBJECTID), ]


# Relations entre variables quantitatives

# On corrèle l'âge de l'arbre et ses caractéristiques
correlations <- cor(df[, c("age_estim", "haut_tot", "haut_tronc", "tronc_diam")], use = "pairwise.complete.obs")
print(correlations)

# On visualise
corrplot(correlations, method = "number")

# On utilise des modèles linéaires pour estimer l'âge
model1 <- lm(age_estim ~ haut_tot, df)
model2 <- lm(age_estim ~ haut_tronc, df)
model3 <- lm(age_estim ~ tronc_diam,df)
model4 <- lm(age_estim ~ haut_tot + haut_tronc + tronc_diam, df)

# On les compare
summary(model1)
summary(model2)
summary(model3)
summary(model4)


# Relations entre variables qualitatives
# On créer les tableaux croisés et on test le chi2
for (col1 in c("fk_situation", "fk_revetement", "villeca",  
               "feuillage", "remarquable","etat_arbre","coupe_categorie","lieu_categorie", "stadedvp")) {
  for (col2 in c("fk_situation", "fk_revetement", "villeca",  
                 "feuillage", "remarquable","etat_arbre","coupe_categorie","lieu_categorie", "stadedvp")) {
    if (col1 != col2) {
      cat("\nTable croisée entre", col1, "et", col2, ":\n")
      print(table(df[[col1]], df[[col2]]))
      
      # Test du chi2 
      chi2_result <- chisq.test(df[[col1]], df[[col2]], correct = TRUE)
      cat("Test du chi2 : p-value =", chi2_result$p.value, "\n")
      
      # Représentation graphique
      mosaicplot(table(df[[col1]], df[[col2]]), 
                 main = paste("Mosaic plot:", col1, "vs", col2), shade = TRUE)
    }
  }
}

