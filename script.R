knitr::opts_chunk$set(echo = TRUE)
library(tidyr)
library(dplyr)
library(sf) # pour gérer les fichiers shapefile
library(ggplot2) # pas besoin car c'est pour les cartes statiques
library(tmap) # pour créer une carte 
library(stringi) # pour le problème d'encodage du fichier shapefile
library(maptools)
library(mapview) # permet une carte dynamique
library(tmaptools) 
library(psych)
library(readxl)
library(spgwr)
library(grid)
library(gridExtra)
library(spdep)
library(rgdal)
library(rgeos)
library(sp)





#mets dans une liste nommée fichiers la liste des fichiers csv contenant les données par région et par année puis les ouvre un par un et les concatènes dans un seul dataframe, on précise code_postal_acheteur sinon erreur le premier zero part

fichiers = list.files(pattern="^bnvd.*\\.csv$")
bddsubstance = do.call(rbind, lapply(fichiers, function(x) read.csv(x, sep=";", dec=".", stringsAsFactors = FALSE,header=TRUE, colClasses=c("code_postal_acheteur"="character"),encoding = "UTF-8" )))

# Nb de lignes avant 7745991
# Nb de lignes après  avoir enlevé le code postal == 0000 : 7667880
# Soit 78111 lignes en moins car postal non renseigné ou égal à 0000


#certaines lignes n'ont pas de code postal, on les enlève car on ne peut pas les traiter

bddsubstance <- bddsubstance[-which(bddsubstance$code_postal_acheteur=="00000"),]


# certaines lignes contiennent des Valeurs non renseignée qui ne sont pas automatiquement transformées en NA, cette ligne permet de le faire


bddsubstance$quantite_substance[bddsubstance$quantite_substance == ""] <- NA


#enleve les lignes ou la quantité n'est pas renseignée, on se retrouve avec 7639094 lignes soit 28786 lignes en moins
bddsubstance<-bddsubstance[!is.na(bddsubstance$quantite_substance),]


# Permet de convertir bdd$quantite_substance en nombre à virgule, avant considéré comme une chaine de caractères (affiche une erreur mais semble normal)

bddsubstance$quantite_substance<-as.numeric(bddsubstance$quantite_substance)





# transforme bdd$classification en un facteur

bddsubstance$classification<-as.factor(bddsubstance$classification)



# on ne garde que l'année, le code postal, la quantité et la classification 
bddsubstance<-bddsubstance[c(1,2,6,7)]

#On garde toxique et nc dans toxique

levels(bddsubstance$classification)[c(4,5)]<-"Toxique"



#group by r permet de fusionner la quantité de substance par année, code postal et classification
# nb de lignes: 93080


bddsubstance = bddsubstance %>% group_by(code_postal_acheteur,annee, classification) %>%
  summarise(quantite_totale = sum(quantite_substance), .groups = 'drop')

#permet d'exporter la base de données créee en CSV

#write.table(bdd,"BDDSUBSTANCE.csv",sep=";",dec=".",row.names=FALSE)


#groupby qui permet d'avoir le code postal en identifiant 



bddlongsubstance<-bddsubstance %>%
  pivot_wider(
    id_cols = code_postal_acheteur,
    names_from = c(annee, classification),
    values_from = quantite_totale,
    names_glue = "{classification}_quantite-totale-{annee}"
  )


#write.table(bddlongsubstance,file="bddlongsubstance.csv",fileEncoding = "UTF-8", sep= ",", dec= ".",row.names=FALSE)





#création de la base de donnéess sur l'eau

fichiers = list.files(pattern="^moy.*\\.csv$")


pe<- do.call(rbind, lapply(fichiers, function(x) read.csv(x, sep=";", dec=",", stringsAsFactors = FALSE,header=TRUE,encoding = "UTF-8" )))
cp<-read.table(file="codepostal.csv",header=TRUE, sep=";", dec=',',colClasses=c("Code_postal"="character"),encoding = "UTF-8")
stat<-read.csv2(file="stations.csv")

stat<-stat[,c(1,2)]
cp<-cp[,c(1,3)]

fus<-merge(pe,stat, by="CD_STATION")

names(cp)<-c("NUM_COM","COD_POS")

bddeau<-merge(fus,cp,by="NUM_COM")
bddeau<-bddeau[,-c(1,2)]

#write.csv(x = bddeau, file = "BDDEAU.csv",row.names = FALSE)





bddlongeau = bddeau %>% group_by(COD_POS,ANNEE) %>%
  summarise(nbpreltot=sum(NBPREL),MOYPTOTAL=mean(MOYPTOT), MAXPTOTAL=max(MAXPTOT), MINMOLRECHTOTAL=min(MINMOLRECH), MINMOLQTOTAL=min(MINMOLQ),MAXMOLQTOTAL=max(MAQMOLQ), .groups = 'drop')


rm(list=c("cp","fus","pe","stat","fichiers"))

#write.csv(bddlongeau,file="bddlongeau.csv",row.names=FALSE)


#carte


bddlongsubstance$code_postal_acheteur= as.factor(bddlongsubstance$code_postal_acheteur)
names(bddlongsubstance)[1]= "ID"
codes_postaux= st_read(dsn = "codes_postaux_V5/codes_postaux_region.shp", 
                       layer = "codes_postaux_region",
                       quiet = TRUE) %>%
  select(ID, LIB, DEP)

codes_postaux$LIB= stri_encode(codes_postaux$LIB, from= "ISO-8859-1", to= "utf8")

fusion= codes_postaux %>% 
  left_join(bddlongsubstance[1:17], by= "ID") %>%
  st_transform(2154)



tmap_mode(mode= "view") # permet de zoomer
autre_2015= 
  tm_basemap("CartoDB.Voyager")+ tm_shape(shp= fusion)+
  tm_fill(col= "Autre_quantite-totale-2015", palette= "YlOrBr", id= "ID",
          textNA = "Valeur manquante", style = "quantile", n= 6, title= "Quantité totale : <br> Catégorie AUTRE",
          popup.vars = c("Ville"= "LIB", "Quantité"= "Autre_quantite-totale-2015"))+
  tm_borders("black", lwd= 0.3, alpha= 0.6)+
  tm_layout(title = "Quantité de substances phytopharmaceutiques achetées en 2015 (en kilogrammes)", title.position = c("center", "bottom"), legend.bg.color = "white", legend.bg.alpha = 0.4)+
  tm_scale_bar(position = c("left", "bottom"))+
  tm_view(view.legend.position = c("right", "bottom"))


autre_2015





rm(list=c("autre_2015"))


#hypothèse forte achat


bdd<-bddlongsubstance

bdd$tauxdecroissancetoxique2016<- ((bdd$`Toxique_quantite-totale-2016`-bdd$`Toxique_quantite-totale-2015`   )/bdd$`Toxique_quantite-totale-2015`)+1
bdd$tauxdecroissancetoxique2017<- ((bdd$`Toxique_quantite-totale-2017`-bdd$`Toxique_quantite-totale-2016`   )/bdd$`Toxique_quantite-totale-2016`)+1
bdd$tauxdecroissancetoxique2018<- ((bdd$`Toxique_quantite-totale-2018`-bdd$`Toxique_quantite-totale-2017`   )/bdd$`Toxique_quantite-totale-2017`)+1


bdd$tauxdecroissanceAutre2016<-   ((bdd$`Autre_quantite-totale-2016`-bdd$`Autre_quantite-totale-2015`)/bdd$`Autre_quantite-totale-2015`)+1
bdd$tauxdecroissanceAutre2017<-   ((bdd$`Autre_quantite-totale-2017`-bdd$`Autre_quantite-totale-2016`)/bdd$`Autre_quantite-totale-2016`)+1
bdd$tauxdecroissanceAutre2018<-   ((bdd$`Autre_quantite-totale-2018`-bdd$`Autre_quantite-totale-2017`)/bdd$`Autre_quantite-totale-2017`)+1

bdd$tauxdecroissanceNorganique2016 <- ((bdd$`N Organique_quantite-totale-2016`-bdd$`N Organique_quantite-totale-2015`)/bdd$`N Organique_quantite-totale-2015`)+1
bdd$tauxdecroissanceNorganique2017 <- ((bdd$`N Organique_quantite-totale-2017`-bdd$`N Organique_quantite-totale-2016`)/bdd$`N Organique_quantite-totale-2016`)+1
bdd$tauxdecroissanceNorganique2018 <- ((bdd$`N Organique_quantite-totale-2018`-bdd$`N Organique_quantite-totale-2017`)/bdd$`N Organique_quantite-totale-2017`)+1

bdd$tauxdecroissanceNmineral2016<-(( bdd$`N minéral_quantite-totale-2016`-bdd$`N minéral_quantite-totale-2015` )/bdd$`N minéral_quantite-totale-2015`)+1
bdd$tauxdecroissanceNmineral2017<-(( bdd$`N minéral_quantite-totale-2017`-bdd$`N minéral_quantite-totale-2016` )/bdd$`N minéral_quantite-totale-2016`)+1
bdd$tauxdecroissanceNmineral2018<-(( bdd$`N minéral_quantite-totale-2018`-bdd$`N minéral_quantite-totale-2017` )/bdd$`N minéral_quantite-totale-2017`)+1



bdd$TAUXtoxiqueMOYENannuel<- ((bdd$tauxdecroissancetoxique2016*bdd$tauxdecroissancetoxique2017*bdd$tauxdecroissancetoxique2018)^(1/rowSums(!is.na(bdd[c(20,21,22)])))-1 )*100   
bdd$TAUXautreMOYENannuel<-((bdd$tauxdecroissanceAutre2016*bdd$tauxdecroissanceAutre2017*bdd$tauxdecroissanceAutre2018)^(1/rowSums(!is.na(bdd[c(23,24,25)])))  -1)*100
bdd$TAUXnorganiqueMOYENannuel<-((bdd$tauxdecroissanceNorganique2016*bdd$tauxdecroissanceNorganique2017*bdd$tauxdecroissanceNorganique2018)^(1/rowSums(!is.na(bdd[c(26,27,28)])))-1)*100
bdd$TAUXNmineralMOYENannuel<-((bdd$tauxdecroissanceNmineral2016*bdd$tauxdecroissanceNmineral2017*bdd$tauxdecroissanceNmineral2016)^(1/rowSums(!is.na(bdd[c(29,30,31)])))-1 )*100   



bdd <- lapply(bdd[-1], function(x) {
  q1 <- quantile(x, .25,na.rm=TRUE)
  q2 <- quantile(x, .75,na.rm=TRUE)
  IQR <- q2 - q1
  replace(x, x < (q1 - 1.5*IQR) | x > (q2+1.5*IQR), NA)
})


bdd<-as.data.frame(bdd)

ggplot(stack(bdd[,seq(29,32)]), aes(x = ind, y = values )) + labs(
  title    = "Taux de croissance annuel moyen de la quantité de produits phytosanitaires achetés ",
  
  x        = "catégorie",
  y        = "taux de croissance annuel moyen entre 2015 et 2018"
) +
  geom_boxplot()


#hypothèse forte eau


longeau<-bddlongeau %>%
  pivot_wider(
    id_cols = COD_POS,
    names_from = c(ANNEE),
    values_from = MOYPTOTAL,
    names_glue = "moyptotal-{ANNEE}"
  )



#permet de remettre les années dans l'ordre croissant
longeau<-longeau[c(1,6,7,2,3,4,5)]

longeau$TC2008<-((longeau$`moyptotal-2008`-longeau$`moyptotal-2007`)/(longeau$`moyptotal-2007`))+1
longeau$TC2009<-((longeau$`moyptotal-2009`-longeau$`moyptotal-2008`)/(longeau$`moyptotal-2008`))+1
longeau$TC2010<-((longeau$`moyptotal-2010`-longeau$`moyptotal-2009`)/(longeau$`moyptotal-2009`))+1
longeau$TC2011<-((longeau$`moyptotal-2011`-longeau$`moyptotal-2010`)/(longeau$`moyptotal-2010`))+1
longeau$TC2012<-((longeau$`moyptotal-2012`-longeau$`moyptotal-2011`)/(longeau$`moyptotal-2011`))+1


longeau <- longeau[is.finite(rowSums(longeau[,-1])),]



longeau$TCglobal<-longeau$TC2008*longeau$TC2009*longeau$TC2010*longeau$TC2011*longeau$TC2012


longeau$TCAMmoyenpor<-((longeau$TCglobal^(1/5))   -1)*100



longeauAB <- lapply(longeau[seq(8,14)], function(x) {
  q1 <- quantile(x, .25,na.rm=TRUE)
  q2 <- quantile(x, .75,na.rm=TRUE)
  IQR <- q2 - q1
  replace(x, x < (q1 - 1.5*IQR) | x > (q2+1.5*IQR), NA)
})






ggplot(stack(longeauAB[7]), aes(x = ind, y = values )) + labs(
  title    = "Taux de croissance annuel moyen entre 2007 et 2012 en % de la quantité de produits phytosanitaires retrouvée dans l'eau",
  
  x=" taux de croissance moyen en %",
  y        = ""
) +
  geom_boxplot()

#lien variable

bddsub<-bddlongsubstance

eau2012<-longeau[c(1,7)]


bddsub$total2015<-bddsub$`Autre_quantite-totale-2015`+bddsub$`Autre_quantite-totale-2015`+bddsub$`N minéral_quantite-totale-2015`+bdd$N.Organique_quantite.totale.2015
sub2015<-bddsub[c(1,18)]


colnames(sub2015)[1]<-'COD_POS'
cor<-merge(sub2015,eau2012,'COD_POS')


cor$dep<-substr(cor$COD_POS,1,2)

cordep<-cor[c(2,3,4)]
cordep<-cordep[c(3,1,2)]

cordeplong = cordep %>% group_by(dep) %>%
  summarise(Total2015dep=sum(total2015,na.rm = TRUE),moyenne2012dep=mean(`moyptotal-2012`,na.rm = TRUE), .groups = 'drop')


cordeplong<-cordeplong[-3,]





#qtt retrouvée ds l'eau de manièrere générale


plot(cor$`moyptotal-2012`~cor$total2015,xlab='Total des quantités de produits phytosanitaires achetées en 2015',ylab="quantitée de produit retrouvée dans les eaux en 2012",xlim=c(0,15000),ylim=c(0,0.5)    )


#qtt retrouvée ds l'eau par département

plot(cordeplong$moyenne2012dep~cordeplong$Total2015dep ,xlab='Total des quantités de produits phytosanitaires achetées en 2015 par département',ylab="quantitée de produits retrouvés dans les eaux en 2012",xlim=c(0,300000),ylim=c(0,0.6) )




#agreste


agreste<-read_excel("agreste.xlsx")


#enleve la première colonne, pas important
agreste<-agreste[-1]



#enleve les valeurs pour la corse et les dom tom
agreste<-agreste[-c(seq(1,5),100,101),]





#transforme les valeurs en numérique

cols.num <- c("Céréales","Maïs grain et maïs semence","Oléagineux","Protéagineux","Tournesol","Fourrages et superficies toujours en herbe","Superficie toujours en herbe","Vignes","Jachères")
agreste[cols.num] <- sapply(agreste[cols.num],as.numeric)
agreste[is.na(agreste)]<-0


agreste$Département <-substr(agreste$Département,1,2)


names(agreste)[1]<-"dep"

df<-merge(agreste,cordeplong,"dep")



#obligé d'enlever les accents sinon erreur caractère ascii
names(df)<-c("dep","SAU","cereales","ble tendre","Mais","oleagineux","Proteagineux","Tournesol","Colza","Superficie Fourrage","Superficie toujours en herbe","Vignes","Jacheres","Total2015depAchat","Moyenne2012eau")


corPlot(df[-1],upper=FALSE,xlas=3,scale=FALSE,MAR = 10 ,cex=1,cex.axis=1,main="Matrice de corrélation")


#moran


fusion<-fusion[-c(1,2,3)]

fusion$total2015<-fusion$`Autre_quantite-totale-2015`+fusion$`Toxique_quantite-totale-2015`+fusion$`N minéral_quantite-totale-2015`+fusion$`N Organique_quantite-totale-2015`
fusion$total2016<-fusion$`Autre_quantite-totale-2016`+fusion$`Toxique_quantite-totale-2016`+fusion$`N minéral_quantite-totale-2016`+fusion$`N Organique_quantite-totale-2016`
fusion$total2017<-fusion$`Autre_quantite-totale-2017`+fusion$`Toxique_quantite-totale-2017`+fusion$`N minéral_quantite-totale-2017`+fusion$`N Organique_quantite-totale-2017`
fusion$total2018<-fusion$`Autre_quantite-totale-2018`+fusion$`Toxique_quantite-totale-2018`+fusion$`N minéral_quantite-totale-2018`+fusion$`N Organique_quantite-totale-2018`



fusion_sf<-st_as_sf(fusion)


neighbours_sf <- poly2nb(fusion_sf)

listw <- nb2listw(neighbours_sf,zero.policy = TRUE)


globalMoran <- moran.test(fusion$`Autre_quantite-totale-2015` , listw,na.action=na.omit,zero.policy=TRUE)
globalMoran


local <- localmoran(x = fusion$total2017, listw = nb2listw(neighbours_sf, style = "W",zero.policy = TRUE),na.action=na.exclude,zero.policy = TRUE)

moran.map <- cbind(fusion, local)

tm_shape(moran.map) +
  tm_fill(col = "Ii",
          style = "quantile",
          title = "local moran statistic") 
