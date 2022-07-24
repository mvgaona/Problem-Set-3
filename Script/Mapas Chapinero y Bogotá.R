# Valeria Gaona - 202214418
# Andrea Beleño - 200620739

#### PROBLEM SET 3 #####
#En el siguiente archivo de trabajo se encuentra el proceso de creación de los mapas para las localidades de Chapinero, Bogotá y El Poblado, Medellín. 
install.packages("pacman") #Instalar librería si no cuenta con esta 
library(pacman) #Llamar librería#Se cargan las librerías a usar en el presente Problem Set
p_load(caret, 
       Matrix,
       recipes,
       rio, #Instalar librerías que falten
       tidyverse,
       glmnet,
       dplyr,
       readr,
       gamlr,
       tidymodels,
       ggplot2,
       scales,
       ggpubr,
       skimr,
       rvest,
       caret,
       stringr,
       boot,
       caret,
       modeest,
       recipes,
       glmnet,
       stargazer,
       pROC, 
       sf,
       leaflet,
       tmaptools,
       class,
       nngeo,
       osmdata)
rm(list = ls()) #Limpia las variables que existan al momento de correr el código
##---Base de datos oficial--##
library(readr)
HOUSEM<- readRDS("../Elementos_Guardados/HOUSEM.rds")
class(HOUSEM)
#Se crearán dos bases de datos: Una para Bogotá y otra para Medellín.
HOUSEBOG <- HOUSEM[HOUSEM$l3=="Bogotá D.C",]
HOUSEMED<- HOUSEM[HOUSEM$l3=="Medellín",]
##Mapa Bogotá
Polchapinero <- getbb(place_name = "UPZ Chapinero, Bogota", 
                      featuretype = "boundary:administrative", 
                      format_out = "sf_polygon") %>% .$multipolygon
leaflet() %>% addTiles() %>% addPolygons(data= Polchapinero, col = "darkblue")%>% addCircleMarkers(data=HOUSEBOG, col= "red")
cajaChap <- opq(bbox = getbb("Chapinero Bogotá"))
mnzB<-readRDS("../Elementos_Guardados/Bogota.rds") #Datos de manzanas Antioquia
sf_use_s2(FALSE)
mnzBogota<-subset(mnzB, select=c("MANZ_CCNCT", "geometry"))
Bog_buf <- st_buffer(Polchapinero,dist=0.001)
mnz_bog <- mnzBogota[Bog_buf,]
## objeto osm para extracción de amenity Transporte público
osmbog = opq(bbox = getbb("Bogotá")) %>%
  add_osm_feature(key="amenity" , value="bus_station") 
class(osmbog)
osmbog_sf = osmbog %>% osmdata_sf()
View(osmbog_sf)
#Estaciones de buses
Transporte_publicoBog = osmbog_sf$osm_points %>% select(osm_id,amenity) 
class(Transporte_publicoBog)
View(Transporte_publicoBog)
leaflet() %>% addTiles() %>% addCircleMarkers(data=Transporte_publicoBog , col="green")%>% addPolygons(data = Poblado, color = "black")
#Bares
barbog = opq(bbox = st_bbox(mnzBogota)) %>%
  add_osm_feature(key = "amenity", value = "bar") %>%
  osmdata_sf() %>% .$osm_points %>% select(osm_id,name)
leaflet() %>% addTiles() %>% 
  addPolygons(data=mnz_pob) %>% # manzanas
  addCircles(data= Transporte_publicoB , col="green") %>%  # transportepub
  addCircles(data= HOSUEBOG , col="red", weight=2) %>% # apartamentos
  addCircles(data=barant , col="black" , weight=2)
#Parques
parkbog = opq(bbox = st_bbox(mnzBogota)) %>%
  add_osm_feature(key = "leisure", value = "park") %>%
  osmdata_sf() %>% .$osm_points %>% select(osm_id,name)
parkmed %>% head()

#---MAPA OFICIAL CHAPINERO---#
leaflet() %>% addTiles() %>% 
  addPolygons(data = Bog_buf, color = "darkblue")%>%  
  addPolygons(data=Polchapinero, color ="grey" ) %>% # Polígono Chapinero
  addCircles(data= Transporte_publicoBog , col="purple") %>%  #Transporte publico Bogotá
  addCircles(data= HOUSEBOG , col="red", weight=2) %>% # Apartamentos en Chapinero.
  addCircles(data=barbog , col="orange" , weight=2) %>% #Bares Chapinero  
  addCircles(data = parkbog, col= "green", weight = 2)#Parques Chapinero

####Mapa de Medellín
mnzAnt<-readRDS("../Elementos_Guardados/Antioquia.rds") #Datos de manzanas Antioquia
sf_use_s2(FALSE)
mnzMedellin<-subset(mnzAnt, select=c("MANZ_CCNCT", "geometry"))
Poblado_buf <- st_buffer(Poblado,dist=0.001)
mnz_pob <- mnzMedellin[Poblado_buf,]
## objeto osm para extracción de amenity Transporte público
osmmed = opq(bbox = getbb("Medellin")) %>%
  add_osm_feature(key="amenity" , value="bus_station") 
class(osmmed)
osmmed_sf = osmmed %>% osmdata_sf()
View(osmmed_sf)
Transporte_publicoMed = osmmed_sf$osm_points %>% select(osm_id,amenity) 
class(Transporte_publicoMed)
View(Transporte_publicoMed)
leaflet() %>% addTiles() %>% addCircleMarkers(data=Transporte_publicoMed , col="green")%>% addPolygons(data = Poblado, color = "black")
#Bares
barant = opq(bbox = st_bbox(mnz_pob)) %>%
  add_osm_feature(key = "amenity", value = "bar") %>%
  osmdata_sf() %>% .$osm_points %>% select(osm_id,name)
leaflet() %>% addTiles() %>% 
  addPolygons(data=mnz_pob) %>% # manzanas
  addCircles(data= Transporte_publicoMed , col="green") %>%  # transportepub
  addCircles(data= HOUSEMED , col="red", weight=2) %>% # apartamentos
  addCircles(data=barant , col="black" , weight=2)
#Parques
parkmed = opq(bbox = st_bbox(mnzAnt)) %>%
  add_osm_feature(key = "leisure", value = "park") %>%
  osmdata_sf() %>% .$osm_points %>% select(osm_id,name)
parkmed %>% head()

#---MAPA OFICIAL EL POBLADO---#
leaflet() %>% addTiles() %>% 
  addPolygons(data = Poblado_buf, color = "darkblue")%>%
  addPolygons(data= Poblado, color ="#pink" ) %>% #Poblado
  addCircles(data= Transporte_publicoMed , col="purple") %>%  # Transporte Público Poblado
  addCircles(data= HOUSEMED , col="red", weight=2) %>% # Apartamentos Poblado
  addCircles(data=barant , col="black" , weight=2) %>% #Bares Poblado
  addCircles(data = parkmed, col= "green", weight = 2) #Parques Poblado
