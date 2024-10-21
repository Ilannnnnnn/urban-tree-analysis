# On charge les librairies nécessaires
library(tidyverse) 
library(vcd) 
library(corrplot) 
library(htmlwidgets) 
library(sf) 
library(leaflet) 

#Nettoyage et ajustement des données :

# On charge les données
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


#On enlève les lignes où l'OBJECT ID n'est pas renseigné
df <- df[ !is.na(df$OBJECTID), ]


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
#View(df)

#On Supprime les cases vides 
df <- df[ !is.na(df$OBJECTID), ]



#Visualisation des données sur une carte :


#On fait la convertion des coordonnées de lambert93 à wgs84:
XYdonnee <- st_as_sf(df, coords = c("X", "Y"), crs = 3949)
latilon <- st_transform(XYdonnee, crs = 4326)
#On crée deux colonnes dans notre tableau pour la longitude et pour la latitude
df$longitude <- st_coordinates(latilon)[, 1]
df$latitude <- st_coordinates(latilon)[, 2]
#View(df)


#TOUTES LES CLASSIFICATIONS DES ARBRES PAR ETAT ET REMARQUABILITE :

#on filtre les informations qu'on veut garder et on les met dans un nouveau datafram
df_vivant <- df %>% filter(etat_arbre == "Vivant")
df_mort <- df %>% filter(etat_arbre == "Mort")
df_supprime <- df %>% filter(fk_arb_etat == "SUPPRIMÉ")
df_abattu <- df %>% filter(fk_arb_etat == "ABATTU")
df_essouche <- df %>% filter(fk_arb_etat == "Essouché")
df_nonessouche <- df %>% filter(fk_arb_etat == "Non essouché")
df_replacé <- df %>% filter(fk_arb_etat == "REMPLACÉ")
df_remarquable <- df %>% filter(remarquable == "Oui")

#On crée la carte et on precise la longitude et la latitude de saint quentin
carte8 <- leaflet() %>% addTiles() %>%  
  setView(lng = 3.28333, lat = 49.849998, zoom = 13) %>%
  
  #avec le addcirclemarkers on place nos points en mettant le df qui comporte l'information d'ou on recupere les longitudes et les latitudes de chaque points et on ajoute le groupe auquel on veut l'associer et on fait ca pour tous les états et la remarquabilité :
  addCircleMarkers(data = df_mort,lng = ~longitude,lat = ~latitude,radius = 1,color = "red",group="Mort")%>%
  addCircleMarkers(data = df_vivant,lng = ~longitude,lat = ~latitude,radius = 1,color = "green",group="Vivant")%>%
  addCircleMarkers(data = df_supprime,lng = ~longitude,lat = ~latitude,radius = 1,color = "blue",group="Supprimé")%>%
  addCircleMarkers(data = df_abattu,lng = ~longitude,lat = ~latitude,radius = 1,color = "brown",group="Abattu")%>%
  addCircleMarkers(data = df_essouche,lng = ~longitude,lat = ~latitude,radius = 1,color = "black",group="Essouché")%>%
  addCircleMarkers(data = df_nonessouche,lng = ~longitude,lat = ~latitude,radius = 1,color = "orange",group="Non essouché")%>%
  addCircleMarkers(data = df_replacé,lng = ~longitude,lat = ~latitude,radius = 1,color = "yellowgreen",group="Remplacé")%>%
  addCircleMarkers(data = df_remarquable,lng = ~longitude,lat = ~latitude,radius = 1,color = "darkviolet",group="Remarquable")%>%
  
  #La fonction addlayerscontrol nous permet d'ajouter la fonctionnalité de faire apparaitre ou disparraitre les points en indiquant le groupe en question :
  addLayersControl(
    overlayGroups = c("Vivant", "Mort", "Supprimé", "Abattu","Essouché","Non essouché","Remplacé","Remarquable"),
  )

#on enregiste la carte en html:
saveWidget(carte8, "carte8.html", selfcontained = TRUE)

#on lance la carte :
browseURL("carte8.html")

#TOUS LES ARBRES PAR QUARTIER

#print(unique(df$clc_quartier))

#on filtre les informations qu'on veut garder et on les met dans un nouveau datafram :
df_centreville <- df %>% filter(clc_quartier == "Quartier du Centre-Ville")
df_saintmartin <- df %>% filter(clc_quartier == "Quartier Saint-Martin - Oëstres")
df_remicourt <- df %>% filter(clc_quartier == "Quartier Remicourt")
df_saintjean <- df %>% filter(clc_quartier == "Quartier Saint-Jean")
df_vermandois <- df %>% filter(clc_quartier == "Quartier du Vermandois")
df_europe <- df %>% filter(clc_quartier == "Quartier de l'Europe")
df_neuville <- df %>% filter(clc_quartier == "Quartier de Neuville")
df_harly <- df %>% filter(clc_quartier == "HARLY")
df_isle <- df %>% filter(clc_quartier == "Quartier du faubourg d'Isle")
df_ommissy <- df %>% filter(clc_quartier == "OMISSY")
df_rouvroy <- df %>% filter(clc_quartier == "ROUVROY")

#On crée la carte et on precise la longitude et la latitude de saint quentin :
carte9 <- leaflet() %>% addTiles() %>% 
  setView(lng = 3.28333, lat = 49.849998, zoom = 13) %>%
  
  #Avec le addcirclemarkers on place nos points en mettant le df qui comporte l'information d'ou on recupere les longitudes et les latitudes de chaque points et on ajoute le groupe auquel on veut l'associer et on fait ca pour tous les quarties :
  addCircleMarkers(data = df_centreville,lng = ~longitude,lat = ~latitude,radius = 1,color = "red",group="Quartier du Centre-Ville")%>%
  addCircleMarkers(data = df_saintmartin,lng = ~longitude,lat = ~latitude,radius = 1,color = "green",group="Quartier Saint-Martin - Oëstres")%>%
  addCircleMarkers(data = df_remicourt,lng = ~longitude,lat = ~latitude,radius = 1,color = "blue",group="Quartier Remicourt")%>%
  addCircleMarkers(data = df_saintjean,lng = ~longitude,lat = ~latitude,radius = 1,color = "brown",group="Quartier Saint-Jean")%>%
  addCircleMarkers(data = df_vermandois,lng = ~longitude,lat = ~latitude,radius = 1,color = "black",group="Quartier du Vermandois")%>%
  addCircleMarkers(data = df_europe,lng = ~longitude,lat = ~latitude,radius = 1,color = "orange",group="Quartier de l'Europe")%>%
  addCircleMarkers(data = df_neuville,lng = ~longitude,lat = ~latitude,radius = 1,color = "yellowgreen",group="Quartier de Neuville")%>%
  addCircleMarkers(data = df_harly,lng = ~longitude,lat = ~latitude,radius = 1,color = "darkviolet",group="HARLY")%>%
  addCircleMarkers(data = df_isle,lng = ~longitude,lat = ~latitude,radius = 1,color = "gray",group="Quartier du faubourg d'Isle")%>%
  addCircleMarkers(data = df_ommissy,lng = ~longitude,lat = ~latitude,radius = 1,color = "sienna",group="OMISSY")%>%
  addCircleMarkers(data = df_rouvroy,lng = ~longitude,lat = ~latitude,radius = 1,color = "peru",group="ROUVROY")%>%
  
  #La fonction addlayerscontrol nous permet d'ajouter la fonctionnalité de faire apparaitre ou disparraitre les points en indiquant le groupe en question :
  addLayersControl(
    overlayGroups = c("Quartier du Centre-Ville", "Quartier Saint-Martin - Oëstres", "Quartier Remicourt", "Quartier Saint-Jean","Quartier du Vermandois","Quartier de l'Europe","Quartier de Neuville","HARLY","Quartier du faubourg d'Isle","OMISSY","ROUVROY"),
  )

#On enregiste la carte en html:
saveWidget(carte9, "carte9.html", selfcontained = TRUE)

#on lance la carte
browseURL("carte9.html")


