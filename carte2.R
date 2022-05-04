###BASE EAU (2010)
#packages
library(sf)
library(leaflet)
library(tidyr)
library(dplyr)
library(tmaptools)
library(tmap)
library(shiny)
library(rsconnect)


#csv
df <- read.csv(file="bddlongeau.csv",colClasses=c("COD_POS"="character"))

#shp
codes <- st_read(dsn="codes_postaux_V5/codes_postaux_region.shp",options="ENCODING=WINDOWS-1252")
codes <- codes[,1:3]
colnames(codes)[1] <- "COD_POS"

#pollution
pollution <- df %>%
  full_join(y=codes,by="COD_POS") %>%
  st_as_sf() %>%
  write_sf("pollution.shp")




ui <- fluidPage(
  tmapOutput("map",width=1000,height = 1000)
)





server <- function(input, output, session) {
  output$map <- renderTmap({
    tm_shape(shp=pollution)+
      tm_basemap("CartoDB.Voyager")+
      tm_bubbles(id="COD_POS",
                 palette="-RdYlBu",
                 breaks=c(-Inf,0.5,1,2,4,6,max(df$"MOYPTOTAL")),
                 labels=c("moins de 0,5 µg/L","de 0,5 à 1 µg/L","de 1 à 2 µg/L","de 2 à 4 µg/L","de 4 à 6 µg/L",
                          "de 6 à 9 µg/L"),
                 col="MOYPTOTAL",
                 title.col="Concentrations moyennes<br/>de pesticides dans les eaux<br/>souterraines",
                 border.col="black",
                 size="MOYPTOTAL",popup.vars =c("quantité :"= "MOYPTOTAL") )
      
      
    })
  
}	


app <- shinyApp(ui, server)




