# Valeria Gaona - 202214418
# Andrea Beleño - 200620739

#### PROBLEM SET 3 #####
#En siguiente archivo de trabajo se evidencia el tratamiento de limpieza de datos y de creación de variables por medio de identificación de datos por descripción de la vivienda y variables adquiridas por ell análisis geoespacial.
#Además, se evidencia el procedimiento de análisis descriptivo de las variables del modelo de precio de vivienda.
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
       rgeos,
       nngeo,
       osmdata)
rm(list = ls()) #Limpia las variables que existan al momento de correr el código
###Base de datos Problem set 3
library(readr)
#Se debe poner el directorio de donde está el script:
#Session-> Set Working directory -> To source file location, para lo cual se debe descargar el repositorio DTEST<-data.frame(readRDS("../Elementos_Guardados/test.rds" #Guardar las bases de datos
DTEST<-data.frame(readRDS("../Elementos_Guardados/test.rds"))  #Guardar las bases de datos
DTRAIN <- data.frame(readRDS("../Elementos_Guardados/train.rds"))
View(DTRAIN)
####---Normalización de palabras y caracteres---####

#Se ponen en minúscula los caracteres de description y title en la base train
DTRAIN$description<-str_to_lower(string=DTRAIN$description)
DTRAIN$title<-str_to_lower(string=DTRAIN$title)

#Se ponen en minúscula los caracteres de description y title en la base test
DTEST$description<-str_to_lower(string=DTEST$description)
DTEST$title<-str_to_lower(string=DTEST$title)

# Se eliminan las tildes
DTRAIN$description <- iconv(DTRAIN$description, from = "UTF-8", to = "ASCII//TRANSLIT")
DTEST$description <- iconv(DTEST$description, from = "UTF-8", to = "ASCII//TRANSLIT")

# Se eliminan caracteres especiales
DTRAIN$description <- str_replace_all(DTRAIN$description, "[^[:alnum:]]", " ")
DTEST$description <- str_replace_all(DTEST$description, "[^[:alnum:]]", " ")

# Se eliminan espacios extras
DTRAIN$description <- gsub("\\s+", " ", str_trim(DTRAIN$description))
DTEST$description <- gsub("\\s+", " ", str_trim(DTEST$description))

#En esta sección se unen la base Train y test para realizar la limpieza de datos

train<- DTRAIN %>% mutate(base = "train") #Se crea columna para identificar datos de train
test <- DTEST %>% mutate(base="test") #Se crea columna para identificar datos de test
HOUSE<- bind_rows(train,test) %>% st_as_sf(coords=c("lon","lat"),crs=4326)
class(HOUSE)
#Dado que el modelo se implementará para las localidades de Chapinero y El Poblado, dentro solo se utilizarán los datos de aquellos apartamentos que estén en las zonas ya mencionadas. 
###---LIMPIEZA DE DATOS---###
leaflet() %>% addTiles() %>% addCircles(data = HOUSE)
str(HOUSE)
Polchapinero <- getbb(place_name = "UPZ Chapinero, Bogota", 
                      featuretype = "boundary:administrative", 
                      format_out = "sf_polygon") %>% .$multipolygon
leaflet() %>% addTiles() %>% addPolygons(data= Polchapinero, col = "red")
st_crs(HOUSE)
st_crs(Polchapinero)


####
Polchapinero <- st_transform(Polchapinero, st_crs(HOUSE))
House_Chapinero<- HOUSE[Polchapinero,]
leaflet() %>% addTiles() %>% addCircles(data = House_Chapinero, color = "red" ) %>% addPolygons(data= Polchapinero, col = "blue")
available_features()
available_tags("amenity")
####
mnzBog<-readRDS("../Elementos_Guardados/Bogota.rds") #Datos de manzanas Bogotá
sf_use_s2(FALSE)
mnzBogota<-subset(mnzBog, select=c("MANZ_CCNCT", "geometry"))
####
mnz_chap <- mnzBogota[Polchapinero,]

leaflet() %>% addTiles() %>% addCircles(data = House_Chapinero, color = "red" ) %>% addPolygons(data= mnz_chap, col = "blue")
house_chapinero_mnz <- st_join(x=House_Chapinero, y=mnz_chap)
colnames(house_chapinero_mnz)

table(is.na(house_chapinero_mnz$MANZ_CCNCT))
db_1 <- house_chapinero_mnz %>% subset(is.na(MANZ_CCNCT)==F)
db_2 <- house_chapinero_mnz %>% subset(is.na(MANZ_CCNCT)==T) %>% mutate(MANZ_CCNCT = NULL)
leaflet() %>% addTiles() %>% addPolygons(data=db_2[1,] %>% st_buffer(dist = 0.0005))
db_2 <- st_join(st_buffer(db_2, dist = 0.0005), mnzBogota)%>% subset(duplicated(property_id)==F)


filtro<-is.na(house_chapinero_mnz$MANZ_CCNCT)
house_chapinero_mnz$MANZ_CCNCT[filtro]<-db_2$MANZ_CCNCT
table(is.na(house_chapinero_mnz$MANZ_CCNCT))

house_chapinero_mnz <-  house_chapinero_mnz %>%
  group_by(MANZ_CCNCT) %>%
  mutate(new_surface_2=median(surface_total,na.rm=T))

table(is.na( house_chapinero_mnz$new_surface_2))

house_buf_Bog <- st_buffer(House_Chapinero,dist=0.005)

leaflet() %>% addTiles() %>% addPolygons(data=house_buf_Bog , color="red") %>% addCircles(data=house_chapinero_mnz)

house_buf_Bog<- st_join(house_buf_Bog,House_Chapinero[,"surface_total"])

st_geometry(house_buf_Bog) = NULL

house_buf_mean_Bog <-house_buf_Bog %>% group_by(property_id) %>% summarise(surface_new_3=mean(surface_total.y,na.rm=T))

house_chapinero_mnz<- left_join(house_chapinero_mnz,house_buf_mean_Bog,"property_id")

table(is.na( house_chapinero_mnz$new_surface_2))
table(is.na( house_chapinero_mnz$surface_new_3))

rm(house_buf_Bog, house_buf_mean_Bog)

#Para Medellín
PolPoblado <- getbb(place_name = "Comuna 14 - El Poblado, Medellín", 
                    featuretype = "boundary:administrative", 
                    format_out = "sf_polygon") 

leaflet() %>% addTiles() %>% addPolygons(data= PolPoblado, col = "red")

PolPoblado_buf <- st_buffer(PolPoblado,dist=0.001) #Debido a que se detectaron apartamentos fuera del polígono de El Poblado, se realiza el buffer para aumentar dicho Polígono y así incluirlos
leaflet() %>% addTiles() %>% addPolygons(data= PolPoblado, col = "red")%>% addPolygons(data= PolPoblado_buf, col = "blue")
House_Poblado<- HOUSE[PolPoblado_buf,]
table(House_Poblado$base)

available_features()
available_tags("amenity")
mnzAnt<-readRDS("../Elementos_Guardados/Antioquia.rds") #Datos de manzanas Antioquia
sf_use_s2(FALSE)
mnzMedellin<-subset(mnzAnt, select=c("MANZ_CCNCT", "geometry"))
mnz_pob <- mnzMedellin[PolPoblado_buf,]

leaflet() %>% addTiles() %>% addCircles(data = House_Poblado, color = "red" ) %>% addPolygons(data= mnz_pob, col = "blue")
house_pob_mnz <- st_join(x=House_Poblado, y=mnz_pob)
colnames(house_pob_mnz)
table(is.na(house_pob_mnz$MANZ_CCNCT))
db_1M <- house_pob_mnz %>% subset(is.na(MANZ_CCNCT)==F)
db_2M<- house_pob_mnz %>% subset(is.na(MANZ_CCNCT)==T) %>% mutate(MANZ_CCNCT = NULL)
leaflet() %>% addTiles() %>% addPolygons(data=db_2M[1,] %>% st_buffer(dist = 0.0005))
db_2M <- st_join(st_buffer(db_2M, dist = 0.001), mnzMedellin)%>% subset(duplicated(property_id)==F)
table(is.na(db_2M$MANZ_CCNCT))

filtro_Med<-is.na(house_pob_mnz$MANZ_CCNCT)
house_pob_mnz$MANZ_CCNCT[filtro_Med]<-db_2M$MANZ_CCNCT
table(is.na(house_pob_mnz$MANZ_CCNCT))

house_pob_mnz <-  house_pob_mnz %>%
  group_by(MANZ_CCNCT) %>%
  mutate(new_surface_2=median(surface_total,na.rm=T))

table(is.na( house_pob_mnz$new_surface_2))

house_buf_Med <- st_buffer(House_Poblado,dist=0.008)

house_buf_Med<- st_join(house_buf_Med,House_Poblado[,"surface_total"])

st_geometry(house_buf_Med) = NULL

house_buf_mean_Med <-house_buf_Med %>% group_by(property_id) %>% summarise(surface_new_3=mean(surface_total.y,na.rm=T))

house_pob_mnz<- left_join(house_pob_mnz,house_buf_mean_Med ,"property_id")

table(is.na( house_pob_mnz$new_surface_2))
table(is.na( house_pob_mnz$surface_new_3))


rm(house_buf_Med, house_buf_mean_Med )

##Se unen las dos bases de datos de área Chapinero y Área Bgototá
HOUSEOF<- rbind.data.frame(house_pob_mnz, house_chapinero_mnz)
view(HOUSEOF)
table(is.na( HOUSEOF$surface_new_3))
table(HOUSEOF$base)

#CREACIÓN VARIABLES POR MEDIO DE DESCRIPCIÓN PARA BASE DE DATOS
Descripc<-HOUSEOF$description
parqueaderoT_aux1<-str_detect( Descripc,"parqueadero") 
parqueaderoT_aux2<-str_detect( Descripc,"parqueaderos") 
parqueaderoT_aux3<-str_detect( Descripc,"parqeadero") 
parqueaderoT_aux4<-str_detect( Descripc,"parqeaderos") 
parqueaderoT_aux5<-str_detect( Descripc,"garaje") 
parqueaderoT_aux6<-str_detect( Descripc,"garajes") 
parqueaderoT_aux7<-str_detect( Descripc,"garage") 
parqueaderoT_aux8<-str_detect( Descripc,"garages") 
parqueaderoT_aux9<-str_detect( Descripc,"garjes") 
parqueaderoT_aux10<-str_detect( Descripc,"garje") 
parqueaderoT<-ifelse(parqueaderoT_aux1==TRUE|parqueaderoT_aux2==TRUE| parqueaderoT_aux3==TRUE|parqueaderoT_aux4==TRUE|parqueaderoT_aux5==TRUE|parqueaderoT_aux6==TRUE|parqueaderoT_aux7==TRUE|parqueaderoT_aux8==TRUE|parqueaderoT_aux9 == TRUE |parqueaderoT_aux10==TRUE , 1,0 )
parqueaderoT<-data.frame(parqueaderoT)
summary(parqueaderoT)
parqueaderoT[is.na(parqueaderoT)] = 0 #Se imputa cero a los datos NA, porque existen datos donde no había descripción
summary(parqueaderoT)
HOUSEOF <- cbind(HOUSEOF, parqueaderoT)

#Se creará la variable: Ascensor

ascensorT_aux1<-str_detect( Descripc,"ascensor") 
ascensorT_aux2<-str_detect( Descripc,"acensor") 
ascensorT_aux3<-str_detect( Descripc,"asensor") 
ascensorT_aux4<-str_detect( Descripc,"elevador") 
ascensorT_aux5<-str_detect( Descripc,"ascensores") 
ascensorT_aux6<-str_detect( Descripc,"acensores") 
ascensorT_aux7<-str_detect( Descripc,"asensores") 
ascensorT_aux8<-str_detect( Descripc,"elevadores") 
ascensorT<-ifelse(ascensorT_aux1==TRUE|ascensorT_aux2==TRUE| ascensorT_aux3==TRUE|ascensorT_aux4==TRUE|ascensorT_aux5==TRUE|ascensorT_aux6==TRUE|ascensorT_aux7==TRUE|ascensorT_aux8==TRUE, 1,0 )
ascensorT<-data.frame(ascensorT)
summary(ascensorT)
ascensorT[is.na(ascensorT)] = 0 #Se imputa cero a los datos NA, porque existen datos donde no había descripción
summary(ascensorT)
HOUSEOF <- cbind(HOUSEOF, ascensorT)


#Para obtener los datos de los baños

pat_1b <- "[:space:]+[:digit:]+[:space:]+baño" ## patrón para baños
pat_2b <- "[:space:]+[:digit:]+[:space:]+baños"
pat_3b<-"[:space:]+[:digit:]+baño"
pat_4b<-"[:space:]+[:digit:]+baños"
pat_5b <- "baño+[:space:]+[:digit:]"
pat_6b <- "baños+[:space:]+[:digit:]"
pat_7b <- "[:space:]+[:digit:]+[:space:]+banio"
pat_8b<-"[:space:]+[:digit:]+[:space:]+banios"
pat_9b <- "[:space:]+[:digit:]+banios"
pat_10b <- "banio+[:space:]+[:digit:]"
pat_11b <- "banios+[:space:]+[:digit:]"
pat_12b <- "[:space:]+[:digit:]+[:space:]+bano"
pat_13b <- "[:space:]+[:digit:]+[:space:]+banos"
pat_14b <- "[:space:]+[:digit:]+bano"
pat_15b <- "[:space:]+[:digit:]+banos"
pat_16b <- "bano+[:space:]+[:digit:]"
pat_17b <- "banos+[:space:]+[:digit:]"
pat_18b <- "[:space:]+[:digit:]+[:space:]+vano"
pat_19b <- "[:space:]+[:digit:]+[:space:]+vanos"
pat_20b <- "[:space:]+[:digit:]+vano"
pat_21b <- "[:space:]+[:digit:]+vanos"
pat_22b <- "vano+[:space:]+[:digit:]"
pat_23b <- "vanos+[:space:]+[:digit:]"
pat_24b <- "[:space:]+[:digit:]+[:space:]+vanio"
pat_25b <- "[:space:]+[:digit:]+[:space:]+vanios"
pat_26b <- "[:space:]+[:digit:]+vanio"
pat_27b<-"[:space:]+[:digit:]+vanios"
pat_28b <- "vanio+[:space:]+[:digit:]"
pat_29b <- "vanios+[:space:]+[:digit:]"
pat_30b <- "[:space:]+[:digit:]+[:space:]+vaño"
pat_31b <- "[:space:]+[:digit:]+[:space:]+vaños"
pat_32b <- "[:space:]+[:digit:]+vaño"
pat_33b<-"[:space:]+[:digit:]+vaños"
pat_34b <- "vaño+[:space:]+[:digit:]"
pat_35b <- "vaños+[:space:]+[:digit:]"
pat_36b <- "[:space:]+[:digit:]+[:space:]+bañio"
pat_37b <- "[:space:]+[:digit:]+[:space:]+bañios"
pat_38b <- "[:space:]+[:digit:]+bañio"
pat_39b<-"[:space:]+[:digit:]+bañios"
pat_40b <- "bañio+[:space:]+[:digit:]"
pat_41b <- "bañios+[:space:]+[:digit:]"
pat_42b <- "[:space:]+[:digit:]+[:space:]+bao"
pat_43b <- "[:space:]+[:digit:]+[:space:]+baos"
pat_44b <- "[:space:]+[:digit:]+bao"
pat_45b<-"[:space:]+[:digit:]+baos"
pat_46b <- "bao+[:space:]+[:digit:]"
pat_47b <- "baos+[:space:]+[:digit:]"

HOUSEOF<-readRDS("../Elementos_Guardados/HOUSEOF.rds")
HOUSEOF <- HOUSEOF  %>% 
  mutate(banio_tot= str_extract(string=HOUSEOF$description , pattern= paste0(pat_1b,"|",pat_2b,"|", pat_3b,"|", pat_4b,"|", pat_5b,"|", pat_6b,"|", pat_7b,"|", pat_8b,"|", pat_9b,"|", pat_10b,"|", pat_11b,"|", pat_12b,"|", pat_13b,"|", pat_14b,"|", pat_15b,"|", pat_16b,"|", pat_17b,"|", pat_18b,"|", pat_19b,"|", pat_20b,"|", pat_21b,"|", pat_22b,"|", pat_23b,"|", pat_24b,"|", pat_25b,"|", pat_26b,"|", pat_27b,"|", pat_28b,"|", pat_29b,"|", pat_30b,"|", pat_31b,"|", pat_32b,"|", pat_33b,"|", pat_34b,"|", pat_35b,"|", pat_36b,"|", pat_37b,"|", pat_38b,"|", pat_39b,"|", pat_40b,"|", pat_41b,"|", pat_42b,"|", pat_43b,"|", pat_44b,"|", pat_45b,"|", pat_46b,"|", pat_47b) ))

HOUSEOF$banio_tot<-str_replace_all(string = HOUSEOF$banio_tot, pattern = "," , replacement = ".")
HOUSEOF$banio_tot<-str_replace_all(string = HOUSEOF$banio_tot, pattern = "baño" , replacement = "")
HOUSEOF$banio_tot<-str_replace_all(string = HOUSEOF$banio_tot, pattern = "baños" , replacement = "")
HOUSEOF$banio_tot<-str_replace_all(string = HOUSEOF$banio_tot, pattern = "banio" , replacement = "")
HOUSEOF$banio_tot<-str_replace_all(string = HOUSEOF$banio_tot, pattern = "banios" , replacement = "")
HOUSEOF$banio_tot<-str_replace_all(string = HOUSEOF$banio_tot, pattern = "bano" , replacement = "")
HOUSEOF$banio_tot<-str_replace_all(string = HOUSEOF$banio_tot, pattern = "banos" , replacement = "")
HOUSEOF$banio_tot<-str_replace_all(string = HOUSEOF$banio_tot, pattern = "vano" , replacement = "")
HOUSEOF$banio_tot<-str_replace_all(string = HOUSEOF$banio_tot, pattern = "vanos" , replacement = "")
HOUSEOF$banio_tot<-str_replace_all(string = HOUSEOF$banio_tot, pattern = "vanio" , replacement = "")
HOUSEOF$banio_tot<-str_replace_all(string = HOUSEOF$banio_tot, pattern = "vanios" , replacement = "")
HOUSEOF$banio_tot<-str_replace_all(string = HOUSEOF$banio_tot, pattern = "vaño" , replacement = "")
HOUSEOF$banio_tot<-str_replace_all(string = HOUSEOF$banio_tot, pattern = "vaños" , replacement = "")
HOUSEOF$banio_tot<-str_replace_all(string = HOUSEOF$banio_tot, pattern = "bañio" , replacement = "")
HOUSEOF$banio_tot<-str_replace_all(string = HOUSEOF$banio_tot, pattern = "bañios" , replacement = "")
HOUSEOF$banio_tot<-str_replace_all(string = HOUSEOF$banio_tot, pattern = "bao" , replacement = "")
HOUSEOF$banio_tot<-str_replace_all(string = HOUSEOF$banio_tot, pattern = "baos" , replacement = "")
HOUSEOF$banio_tot<-str_replace_all(string = HOUSEOF$banio_tot, pattern = "\n" , replacement = "")
HOUSEOF$banio_tot<-as.numeric(HOUSEOF$banio_tot)
View(HOUSEOF)
HOUSEOF$banio_tot[is.na(HOUSEOF$banio_tot)] = 1 #Se imputa el número 1, teniendo en cuenta que por nivel de sanidad, debe existir al menos 1 baño en las viviendas
summary(HOUSEOF$banio_tot) 
filtro2<-is.na(HOUSEOF$bathrooms)
HOUSEOF$bathrooms[filtro2]<-HOUSEOF$banio_tot
table(is.na(HOUSEOF$bathrooms))
#Limpieza Habitaciones
HOUSEOF$rooms[is.na(HOUSEOF$rooms)] = 1
summary(HOUSEOF$rooms)
habitaciones_aux<-cbind(HOUSEOF$rooms, HOUSEOF$bedrooms)
View(habitaciones_aux)
habitaciones<-apply(habitaciones_aux, 1 , max)
habitaciones<- data.frame(habitaciones)
View(habitaciones)
summary(habitaciones)
View(HOUSEOF)
HOUSEOF<- cbind(HOUSEOF,habitaciones)
View(HOUSEOF)
rm(habitaciones)

##====================================================

####----Creación de variables geoespacial----###
Polchapinero <- getbb(place_name = "UPZ Chapinero, Bogota", 
                      featuretype = "boundary:administrative", 
                      format_out = "sf_polygon") %>% .$multipolygon
Poblado = getbb(place_name = "Comuna 14 - El Poblado Medellin", 
                featuretype = "amenity",
                format_out = "sf_polygon")
require("tmaptools")
geocode_OSM("El poblado, Medellin") 
PointElPoblado = geocode_OSM("Comuna 14 - El Poblado, Medellín", as.sf=T)  

PointChapinero = geocode_OSM("UPZ Chapinero, Bogotá", as.sf=T) 

leaflet() %>% addTiles() %>% addCircles(data=PointElPoblado)
leaflet() %>% addTiles() %>% addCircles(data=PointChapinero)
## la función addTiles adiciona la capa de OpenStreetMap
leaflet() %>% addTiles() %>% addCircles(data=PointElPoblado)

#Atributos
#Puede acceder a la lista de features disponibles en OSM aquí. En R puede obtener un vector con los nombres de los features usando la función available_features():
available_features() %>% head(20)
## obtener la caja de coordenada que contiene el polígono de Medellín
opq(bbox = getbb("El poblado Medellin"))
## objeto osm
## obtener la caja de coordenada que contiene el polígono de Chapinero y El poblado
cajaElpob <- opq(bbox = getbb("Comuna 14 - El Poblado Medellín"))
opq(bbox = getbb("Chapinero Bogotá"))
## objeto osm para extracción de amenity
osmmed = opq(bbox = getbb("Medellin")) %>%
  add_osm_feature(key="amenity" , value="bus_station") 
class(osmmed)
osmbog = opq(bbox = getbb(" Bogotá ")) %>%
  add_osm_feature(key="amenity" , value="bus_station") 
class(osmbog)
osmmed_sf = osmmed %>% osmdata_sf()
View(osmmed_sf)
available_tags("amenity")
osmbog_sf = osmbog %>% osmdata_sf()
View(osmbog_sf)
 Transporte_publicoMed = osmmed_sf$osm_points %>% select(osm_id,amenity) 
View(Transporte_publicoMed)
Transporte_publicoBog = osmbog_sf$osm_points %>% select(osm_id,amenity) 
View(Transporte_publicoBog)
## Pintar las transporte publico
leaflet() %>% addTiles() %>% addCircleMarkers(data=Transporte_publicoMed , col="red")
leaflet() %>% addTiles() %>% addCircleMarkers(data=Transporte_publicoBog, col="blue")%>% addPolygons(data= Polchapinero, col = "RED")
p_load(rgdal)

#Se obtendrá la distancia mínima a transporte público
#Con respecto a transporte público en Bogotá
dist_transp_bog<- st_distance(x=HOUSEOF , y=Transporte_publicoBog)
min_dist_transp_bog <- apply(dist_transp_bog , 1 , min)
min_dist_transp_bog<-data.frame(min_dist_transp_bog)

#Con respecto a transporte público en Medellín
dist_transp_med <- st_distance(x=HOUSEOF , y=Transporte_publicoMed)
min_dist_transp_med <- apply(dist_transp_med, 1 , min)
min_dist_transp_med<-data.frame(min_dist_transp_med)

#Se selecciona la menor distancia, esto porque se compararon distancias de apartamentos en Bogotá con ubicaciones en Antioquia y viceversa
min_dist_transp<-cbind(min_dist_transp_bog,min_dist_transp_med)
min_dist_transp_<- apply(min_dist_transp, 1 , min)
min_dist_transp_<-data.frame(min_dist_transp_)


#Se incorpora la mínima distancia al transporte a la base de datos
HOUSEOF<-cbind(HOUSEOF, min_dist_transp_)
table(HOUSEOF$base)
######-----Extracción datos de manzanas---####
#Apartamentos
CHAPINERO = getbb(place_name = "Chapinero Bogotá", 
                  featuretype = "amenity",
                  format_out = "sf_polygon")
Poblado = getbb(place_name = "Comuna 14 - El Poblado Medellin", 
                featuretype = "amenity",
                format_out = "sf_polygon")

#Bares Bogotá y Antioquia 
barbog = opq(bbox = st_bbox(mnzBogota)) %>%
  add_osm_feature(key = "amenity", value = "bar") %>%
  osmdata_sf() %>% .$osm_points %>% select(osm_id,name)
barbog %>% head()
barant = opq(bbox = st_bbox(mnzMedellin)) %>%
  add_osm_feature(key = "amenity", value = "bar") %>%
  osmdata_sf() %>% .$osm_points %>% select(osm_id,name)
barant %>% head()
#Visualizar info 
leaflet() %>% addTiles() %>% 
  addPolygons(data=mnzBogota) %>% # manzanas
  addPolygons(data= CHAPINERO , col="green") %>%  # transportepub
  addCircles(data= HOUSEOF , col="red", weight=2) %>% # apartamentos
  addCircles(data=barbog , col="black" , weight=2)
st_crs(mnzBogota) == st_crs(HOUSEOF)
st_crs(mnzMedellin) == st_crs(HOUSEOF)
#Uniondatbog = st_join(x=DTRAIN_sf , y=mnzBogota)

#Se calculará distancia a bares
#Con respecto a bares Bogotá
dist_bar_bog <- st_distance(x=HOUSEOF , y=barbog)
min_dist_bog <- apply(dist_bar_bog , 1 , min)
min_dist_bog<-data.frame(min_dist_bog)

#Con respecto a bares Medellín
dist_bar_med <- st_distance(x=HOUSEOF , y=barant)
min_dist_bar_med <- apply(dist_bar_med, 1 , min)
min_dist_bar_med<-data.frame(min_dist_bar_med)

#Se selecciona la menor distancia, esto porque se compararon distancias de apartamentos en Bogotá con ubicaciones en Antioquia y viceversa
min_dist_bar<-cbind(min_dist_bog,min_dist_bar_med)
min_dist_bar_<- apply(min_dist_bar, 1 , min)
min_dist_bar_<-data.frame(min_dist_bar_)

#Se incorpora la mínima distancia a bares a la base de datos 
HOUSEOF<-cbind(HOUSEOF, min_dist_bar_ )

# Obtendremos la distancia más cercana a parques

PolPoblado_buf <- st_buffer(PolPoblado,dist=0.001)
PolChap_buf <- st_buffer(CHAPINERO,dist=0.001)

mnz_chap_buf <- mnzBogota[Polchapinero,]

#Parques Bogotá y Medellín
Parkbog = opq(bbox = st_bbox(mnz_chap_buf)) %>%
  add_osm_feature(key = "leisure", value = "park") %>%
  osmdata_sf() %>% .$osm_points %>% select(osm_id,name)
Parkbog  %>% head()

Parkmed = opq(bbox = st_bbox(mnz_pob)) %>%
  add_osm_feature(key = "leisure", value = "park") %>%
  osmdata_sf() %>% .$osm_points %>% select(osm_id)
Parkmed  %>% head()

#Se calcula el centroide para los parques de Bogotá y Medellín
centroides_PARK_BOG <- gCentroid(as(Parkbog$geometry, "Spatial"), byid = T)
centroides_PARK_Med <- gCentroid(as(Parkmed$geometry, "Spatial"), byid = T)

#Se convierten los centroides a formato SF con coordenadas WGS84
centroides_park_bog_sf <- st_as_sf(centroides_PARK_BOG, coords = c("x", "y"),crs=4326)
centroides_park_med_sf <- st_as_sf(centroides_PARK_Med, coords = c("x", "y"),crs=4326)

#Se calcula la mínima distancia a parques de los apartamentos con respecto a parques de Bogotá
dist_park_bog <- st_distance(x = HOUSEOF, y = centroides_park_bog_sf)
min_dist_park_bog <- apply(dist_park_bog, 1 , min)
min_dist_park_bog<-data.frame(min_dist_park_bog)

#Se calcula la mínima distancia a parques de los apartamentos con respecto a parques de Medellín
centroides_park_med_sf <- st_as_sf(centroides_PARK_Med, coords = c("x", "y"),crs=4326)
dist_park_med <- st_distance(x = HOUSEOF, y = centroides_park_med_sf)
min_dist_park_med <- apply(dist_park_med, 1 , min)
min_dist_park_med<-data.frame(min_dist_park_med)

#Se selecciona la menor distancia, esto porque se compararon distancias de apartamentos en Bogotá con ubicaciones en Medellín y viceversa
min_dist_park_<-cbind(min_dist_park_bog,min_dist_park_med)
min_dist_park<- apply(min_dist_park_, 1 , min)
min_dist_park<-data.frame(min_dist_park)
#Se incorpora la mínima distancia a parques a la base de datos 
HOUSEOF<-cbind(HOUSEOF, min_dist_park )
cantidad_na <- sapply(HOUSEOF, function(x) sum(is.na(x)))
cantidad_na <- data.frame(cantidad_na)
porcentaje_na <- cantidad_na/nrow(HOUSEOF)
porcentaje_na <-porcentaje_na*100
porcentaje_na #Visualizo el porcentaje de los datos que tienen NA

#Para guardar la base de datos
saveRDS(HOUSEOF, "../Elementos_Guardados/HOUSEOF.rds" )

HOUSEM<-subset(HOUSEOF, select=c("property_id", "l3", "property_type", "price", "surface_new_3", "MANZ_CCNCT", "geometry", "parqueaderoT", "ascensorT", "bathrooms", "habitaciones", "base", "min_dist_bar_", "min_dist_transp_", "min_dist_park"))
#La base de datos oficial de trabajo es HOUSEM, en donde existen 24843 observaciones (Base de entrenamiento y base de testeo)
#Para guardar la base de datos
saveRDS(HOUSEM, "../Elementos_Guardados/HOUSEM.rds" )

DTRAINHOUSE<- HOUSEM[HOUSEM$base=="train",]
DTESTHOUSE<- HOUSEM[HOUSEM$base=="test",]
#La base de datos de entrenamiento cuenta con 13693 observaciones.
#La base de datos de testeo cuenta con 11150 observaciones

#---Para guardar la base de datos---#

saveRDS(DTRAINHOUSE, "../Elementos_Guardados/DTRAINHOUSE.rds" )
saveRDS(DTESTHOUSE, "../Elementos_Guardados/DTESTHOUSE.rds" )

#########----Descripción de variables----#####
#Si se desea realizar la descripción de variables sin necesidad de correr todo el código anterior, se carga la base de datos HOUSEM y así, se procede a realizarse el análisis.
HOUSEM <- readRDS("../Elementos_Guardados/HOUSEM.rds")
###Ubicación del inmueble
Ubicación <- HOUSEM$l3
class(Ubicación)
table(Ubicación)
skim(Ubicación)
rm(Ubicación)
#Tipo de vivienda
TipoViv <- HOUSEM$property_type
class(TipoViv)
table(TipoViv)
skim(TipoViv)
rm(TipoViv)
.#Habitaciones
habitaciones <- data.frame(HOUSEM$habitaciones) 
class(HOUSEM$habitaciones)
plot(hist(HOUSEM$habitaciones),col = "black", main="Histograma No. de habitaciones de la vivienda",
     xlab="Habitaciones",
     ylab="Frecuencia")
min(HOUSEM$habitaciones)
max(HOUSEM$habitaciones)
mean(HOUSEM$habitaciones)
modehabitaciones <- function(habitaciones){
  return(as.numeric(names(which.max(table(habitaciones)))))}
modehabitaciones(habitaciones)
summary(habitaciones)
rm(habitaciones)
#Descripción baños
baños <- as.numeric(HOUSEM$bathrooms) 
class(baños)
plot(hist(baños),col = "red", main="Histograma No. de baños de la vivienda",
     xlab="Habitaciones",
     ylab="Frecuencia")
min(baños)
max(baños)
mean(baños)
modebaños<- function(baños){
  return(as.numeric(names(which.max(table(baños)))))}
modebaños(baños)
summary(baños)
rm(baños)
#Ascensor
Ascensor <- as.factor(HOUSEM$ascensorT)
class(Ascensor)
skim(Ascensor)
Ascensor <- factor(Ascensor, labels = c("1", "0"))
summary(Ascensor)
rm(Ascensor)
#Parqueadero
Parqueadero <- as.factor(HOUSEM$parqueaderoT)
class(Parqueadero)
Parqueadero <- factor(Parqueadero, labels = c("1", "0"))
summary(Parqueadero)
rm(Parqueadero)
#Tipo inmueble
TipoVivienda <- as.factor(HOUSEM$property_type)
class(TipoVivienda)
summary(TipoVivienda)
rm(TipoVivienda)
#Área
área <-HOUSEM$surface_new_3
class(área)
plot(hist(área),col = "black", main="Histograma Área",
     xlab="Área",
     ylab="Frecuencia")
summary(área)
modeárea <- function(área){
  return(as.numeric(names(which.max(table(área)))))}
modeárea(área)
summary(área)
rm(área)
######
#DistanciaTransP
TP <- HOUSEM$min_dist_transp_
modeTP <- function(TP){
  return(as.numeric(names(which.max(table(TP)))))}
modeTP(TP)
summary(TP)
rm(TP)
#Distancia Bares
Bares <- HOUSEM$min_dist_bar_
modeBares <- function(Bares){
  return(as.numeric(names(which.max(table(Bares)))))}
modeBares(Bares)
summary(Bares)
rm(Bares)
#Distancia Parques
Parques <- HOUSEM$min_dist_park
summary(Parques)
modeParques <- function(Parques){
  return(as.numeric(names(which.max(table(Parques)))))}
modeParques(Parques)
rm(Parques)

