#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(dplyr)
library(sf) 
library(ggplot2)
library(tmap)  
library(leaflet)
library(stringi) 
library(tmaptools)
library(mapview) 
library(shiny)
library(rsconnect)


#On importe les data frames nécessaires




base= read.csv("bddlongsubstance.csv", stringsAsFactors = FALSE, encoding = "UTF-8", sep= ",", dec= ".",colClasses=c("ID"="character"))
base= data.frame(base)
base$ID= as.factor(base$ID)

codes_postaux= st_read(dsn = "codes_postaux_V5/codes_postaux_region.shp", 
                       layer = "codes_postaux_region",
                       quiet = TRUE) %>%
  select(ID, LIB, DEP)



codes_postaux$LIB= stri_encode(codes_postaux$LIB, from= "ISO-8859-1", to= "utf8")




fusion= codes_postaux %>% 
  left_join(base, by= "ID") %>%
  st_transform(2154)





categorie<-c("Toxique","Autre","N.Organique","N.minéral")
annee<-c(2015,2016,2017,2018)




ui <- fluidPage(
  
  #permet d'avoir 2 inputs, un pour l'annee, l'autre pour la categorie
  tmapOutput("map",width=1000,height = 1000),
  
  selectInput("Annee","Choisir annee",annee),
  selectInput("Categorie","Choisir categorie",categorie)
)

server <- function(input, output, session) {
  output$map <- renderTmap({
    
    tm_basemap("CartoDB.Voyager")+ tm_shape(shp= fusion)+
      
      tm_borders("black", lwd= 0.3, alpha= 0.6)+
      tm_layout(title = "Quantitee de substances phytopharmaceutiques achetees par annee et par categorie (en kilogrammes)")
    
    
    
    
    
  })
  
  observe({
    
    #on récupère la colonne du dataset qui correspond a l entree, grepl nous donne son index, puis var contient le nom de cette colonne
    a<-input$Annee
    c<-input$Categorie
    d<-which(grepl(a, colnames(fusion)) & grepl(c, colnames(fusion))) 
    var<-colnames(fusion)[d]
    
    tmapProxy("map", session, {
      tm_remove_layer(401) +
        tm_shape(fusion) +
        tm_polygons(var, zindex = 401,breaks=c(0,250,500,1000,2000,4000,8000,16000,32000,80000,300000),textNA='valeur manquante',popup.vars = c("Ville :"= "LIB",'Quantite :'= var ))        
      
      
    })
  })
}	


app <- shinyApp(ui, server)