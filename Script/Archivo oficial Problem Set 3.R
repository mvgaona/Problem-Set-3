# Valeria Gaona - 202214418
# Andrea Beleño - 200620739

#### PROBLEM SET 3 #####

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
###Base de datos Problem set 2
library(readr)
#Se debe poner el directorio de donde está el script:
#Session-> Set Working directory -> To source file location, para lo cual se debe descargar el repositorio DTEST<-data.frame(readRDS("../Elementos_Guardados/test.rds" #Guardar las bases de datos
DTEST<-data.frame(readRDS("../Elementos_Guardados/test.rds"))  #Guardar las bases de datos
DTRAIN <- data.frame(readRDS("../Elementos_Guardados/train.rds"))
View(DTRAIN)

#Se ponen en minúscula los caracteres de description y title en la base train
DTRAIN$description<-str_to_lower(string=DTRAIN$description)
DTRAIN$title<-str_to_lower(string=DTRAIN$title)

#Se ponen en minúscula los caracteres de description y title en la base test
DTEST$description<-str_to_lower(string=DTEST$description)
DTEST$title<-str_to_lower(string=DTEST$title)

#En esta sección se unen la base Train y test para realizar la limpieza de datos
train<- DTRAIN %>% mutate(base = "train")
test <- DTEST %>% mutate(base="test")
HOUSE<- bind_rows(train,test) %>% st_as_sf(coords=c("lon","lat"),crs=4326)
leaflet() %>% addTiles() %>% addCircles(data = HOUSE)
str(HOUSE)
Polchapinero <- getbb(place_name = "UPZ Chapinero, Bogota", 
                      featuretype = "boundary:administrative", 
                      format_out = "sf_polygon") %>% .$multipolygon
leaflet() %>% addTiles() %>% addPolygons(data= Polchapinero, col = "red")
st_crs(HOUSE)
st_crs(Polchapinero)
Polchapinero <- st_transform(Polchapinero, st_crs(HOUSE))
House_Chapinero<- HOUSE[Polchapinero,]

leaflet() %>% addTiles() %>% addCircles(data = House_Chapinero, color = "red" ) %>% addPolygons(data= Polchapinero, col = "blue")
available_features()
available_tags("amenity")
mnzBog<-readRDS("../Elementos_Guardados/Bogota.rds") #Datos de manzanas Bogotá
sf_use_s2(FALSE)
mnzBogota<-subset(mnzBog, select=c("MANZ_CCNCT", "geometry"))

mnz_chap <- mnzBogota[Polchapinero,]
leaflet() %>% addTiles() %>% addCircles(data = House_Chapinero, color = "red" ) %>% addPolygons(data= mnz_chap, col = "blue")
house_chapinero_mnz <- st_join(House_Chapinero, mnz_chap)
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

house_buf_Bog <- st_buffer(house_chapinero_mnz,dist=0.005)

leaflet() %>% addTiles() %>% addPolygons(data=house_buf_Bog , color="red") %>% addCircles(data=house_chapinero_mnz)

house_buf_Bog<- st_join(house_buf_Bog,house_chapinero_mnz[,"surface_total"])

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
PolPoblado <- st_transform(PolPoblado, st_crs(HOUSE))
House_Poblado<- HOUSE[PolPoblado,]
available_features()
available_tags("amenity")
mnzAnt<-readRDS("../Elementos_Guardados/Antioquia.rds") #Datos de manzanas Antioquia
sf_use_s2(FALSE)
mnzMedellin<-subset(mnzAnt, select=c("MANZ_CCNCT", "geometry"))
mnz_pob <- mnzMedellin[PolPoblado,]

leaflet() %>% addTiles() %>% addCircles(data = House_Poblado, color = "red" ) %>% addPolygons(data= mnz_pob, col = "blue")
house_pob_mnz <- st_join(House_Poblado, mnz_pob)
colnames(house_pob_mnz)
table(is.na(house_pob_mnz$MANZ_CCNCT))
db_1M <- house_pob_mnz %>% subset(is.na(MANZ_CCNCT)==F)
db_2M<- house_pob_mnz %>% subset(is.na(MANZ_CCNCT)==T) %>% mutate(MANZ_CCNCT = NULL)
leaflet() %>% addTiles() %>% addPolygons(data=db_2M[1,] %>% st_buffer(dist = 0.0005))
db_2M <- st_join(st_buffer(db_2M, dist = 0.0009), mnzMedellin)%>% subset(duplicated(property_id)==F)
table(is.na(db_2M$MANZ_CCNCT))

filtro_Med<-is.na(house_pob_mnz$MANZ_CCNCT)
house_pob_mnz$MANZ_CCNCT[filtro_Med]<-db_2M$MANZ_CCNCT
table(is.na(house_pob_mnz$MANZ_CCNCT))

house_pob_mnz <-  house_pob_mnz %>%
  group_by(MANZ_CCNCT) %>%
  mutate(new_surface_2=median(surface_total,na.rm=T))

table(is.na( house_pob_mnz$new_surface_2))

house_buf_Med <- st_buffer(house_pob_mnz,dist=0.01)

leaflet() %>% addTiles() %>% addPolygons(data=house_buf_Med , color="red") %>% addCircles(data=house_pob_mnz)

house_buf_Med<- st_join(house_buf_Med,house_pob_mnz[,"surface_total"])

st_geometry(house_buf_Med) = NULL

house_buf_mean_Med <-house_buf_Med %>% group_by(property_id) %>% summarise(surface_new_3=mean(surface_total.y,na.rm=T))

house_pob_mnz<- left_join(house_pob_mnz,house_buf_mean_Med ,"property_id")

table(is.na( house_pob_mnz$new_surface_2))
table(is.na( house_pob_mnz$surface_new_3))




#Creación variable Parqueadero para train y test
Descripc_test<-DTEST$description
parqueaderoT_aux1<-str_detect( Descripc_test,"parqueadero") 
parqueaderoT_aux2<-str_detect( Descripc_test,"parqueaderos") 
parqueaderoT_aux3<-str_detect( Descripc_test,"parqeadero") 
parqueaderoT_aux4<-str_detect( Descripc_test,"parqeaderos") 
parqueaderoT_aux5<-str_detect( Descripc_test,"garaje") 
parqueaderoT_aux6<-str_detect( Descripc_test,"garajes") 
parqueaderoT<-ifelse(parqueaderoT_aux1==TRUE|parqueaderoT_aux2==TRUE| parqueaderoT_aux3==TRUE|parqueaderoT_aux4==TRUE|parqueaderoT_aux5==TRUE|parqueaderoT_aux6==TRUE,1,0 )
parqueaderoT<-data.frame(parqueaderoT)
summary(parqueaderoT)
parqueaderoT[is.na(parqueaderoT)] = 0
summary(parqueaderoT)
DTEST<- cbind(DTEST, parqueaderoT)
rm(parqueaderoT)
Descripc_train<-DTRAIN$description
parqueadero_aux1<-str_detect( Descripc_train,"parqueadero") 
parqueadero_aux2<-str_detect( Descripc_train,"parqueaderos") 
parqueadero_aux3<-str_detect( Descripc_train,"parqeadero") 
parqueadero_aux4<-str_detect( Descripc_train,"parqeaderos") 
parqueadero_aux5<-str_detect( Descripc_train,"garaje") 
parqueadero_aux6<-str_detect( Descripc_train,"garajes") 

parqueaderoT<-ifelse(parqueadero_aux1==TRUE|parqueadero_aux2==TRUE| parqueadero_aux3==TRUE|parqueadero_aux4==TRUE|parqueadero_aux5==TRUE|parqueadero_aux6==TRUE,1,0 )
parqueaderoT<-data.frame(parqueaderoT)
summary(parqueaderoT)
parqueaderoT[is.na(parqueaderoT)] = 0
View(parqueaderoT)
summary(parqueaderoT)
DTRAIN <- cbind(DTRAIN, parqueaderoT)
rm(parqueaderoT)
#Creación variable  para train y test
ascensorT_aux1<-str_detect( Descripc_test,"ascensor") 
ascensorT_aux2<-str_detect( Descripc_test,"acensor") 
ascensorT_aux3<-str_detect( Descripc_test,"asensor") 
ascensorT_aux4<-str_detect( Descripc_test,"elevador") 
ascensorT_aux5<-str_detect( Descripc_test,"ascensores") 
ascensorT_aux6<-str_detect( Descripc_test,"acensores") 
ascensorT_aux7<-str_detect( Descripc_test,"asensores") 
ascensorT_aux8<-str_detect( Descripc_test,"elevadores") 
ascensorT<-ifelse(ascensorT_aux1==TRUE|ascensorT_aux2==TRUE| ascensorT_aux3==TRUE|ascensorT_aux4==TRUE|ascensorT_aux5==TRUE|ascensorT_aux6==TRUE|ascensorT_aux7==TRUE|ascensorT_aux8==TRUE, 1,0 )
ascensorT<-data.frame(ascensorT)
summary(ascensorT)
ascensorT[is.na(ascensorT)] = 0
summary(ascensorT)
DTEST <- cbind(DTEST, ascensorT)
view(DTEST)
rm(ascensorT)
ascensor_aux1<-str_detect( Descripc_train,"ascensor") 
ascensor_aux2<-str_detect( Descripc_train,"acensor") 
ascensor_aux3<-str_detect( Descripc_train,"asensor") 
ascensor_aux4<-str_detect( Descripc_train,"elevador")
ascensor_aux5<-str_detect( Descripc_train,"ascensores") 
ascensor_aux6<-str_detect( Descripc_train,"acensores") 
ascensor_aux7<-str_detect( Descripc_train,"asensores") 
ascensor_aux8<-str_detect( Descripc_train,"elevadores")
ascensorT<-ifelse(ascensor_aux1==TRUE|ascensor_aux2==TRUE| ascensor_aux3==TRUE|ascensor_aux4==TRUE|ascensor_aux5==TRUE|ascensor_aux6==TRUE|ascensor_aux7==TRUE|ascensor_aux8==TRUE, 1,0 )
ascensorT<-data.frame(ascensorT)
summary(ascensorT)
ascensorT[is.na(ascensorT)] = 0
summary(ascensorT)
DTRAIN <- cbind(DTRAIN, ascensorT)
view(DTRAIN)
rm(ascensorT)

table(DTRAIN$l3)
table(DTEST$l3)
table(is.na(DTRAIN$surface_total))
table(is.na(DTRAIN$surface_covered))

#Vamos a obtener los datos de baños y área 

#Para área
pat_1 <- "[:space:]+[:digit:]+[:punct:]+[:digit:]+[:space:]+m2" ## patrón para área
pat_2 <- "[:space:]+[:digit:]+[:punct:]+[:digit:]+m2"
pat_3<-"[:space:]+[:digit:]+[:space:]+metros"
pat_4<-"[:space:]+[:digit:]+metros"
pat_5 <- "[:space:]+[:digit:]+[:punct:]+[:digit:]+[:space:]+mts"
pat_6 <- "[:space:]+[:digit:]+mts"
pat_7 <- "[:space:]+[:digit:]+[:space:]+mts"
pat_8<-"[:space:]+[:digit:]+m^2"
pat_9 <- "[:space:]+[:digit:]+[:punct:]+[:digit:]+[:space:]+m^2"
pat_10 <- "[:space:]+[:digit:]+[:punct:]+[:digit:]+[:space:]+mtrs"
pat_11 <- "[:space:]+[:digit:]+mtrs"
pat_12 <- "[:space:]+[:digit:]+[:space:]+mtrs"
pat_13 <- "[:space:]+[:digit:]+[:space:]+[:punct:]+[:digit:]+[:space:]+mts"
pat_14 <- "[:space:]+[:digit:]+[:punct:]+[:digit:]+[:space:]+mt"
pat_15 <- "[:space:]+[:digit:]+mt"
pat_16 <- "[:space:]+[:digit:]+[:space:]+mt"
pat_17 <- "[:space:]+[:digit:]+[:space:]+[:punct:]+[:digit:]+[:space:]+mts2"
pat_18 <- "[:space:]+[:digit:]+[:punct:]+[:digit:]+[:space:]+mts2"
pat_19 <- "[:space:]+[:digit:]+mts2"
pat_20 <- "[:space:]+[:digit:]+[:space:]+mts2"
pat_21 <- "[:space:]+[:digit:]+[:space:]+[:punct:]+[:digit:]+mts2"
pat_22 <- "[:space:]+[:digit:]+[:space:]+[:punct:]+[:digit:]+[:space:]+metros cuadrados"
pat_23 <- "[:space:]+[:digit:]+[:punct:]+[:digit:]+[:space:]+metros cuadrados"
pat_24 <- "[:space:]+[:digit:]+metros cuadrados"
pat_25 <- "[:space:]+[:digit:]+[:space:]+metros cuadrados"
pat_26 <- "[:space:]+[:digit:]+[:space:]+[:punct:]+[:digit:]+[:space:]+metros cuadrados"


DTRAIN <- DTRAIN  %>% 
    mutate(area_total= str_extract(string=DTRAIN$description , pattern= paste0(pat_1,"|",pat_2,"|", pat_3,"|", pat_4,"|", pat_5,"|", pat_6,"|", pat_7,"|", pat_8,"|", pat_9,"|", pat_10,"|", pat_11,"|", pat_12,"|", pat_13,"|", pat_14,"|", pat_15,"|", pat_16,"|", pat_17,"|", pat_18,"|", pat_19,"|", pat_20,"|", pat_21,"|", pat_22,"|", pat_23,"|", pat_24,"|", pat_25,"|", pat_26) ))

DTRAIN$area_total<-str_replace_all(string = DTRAIN$area_total, pattern = "," , replacement = ".")
DTRAIN$area_total<-str_replace_all(string = DTRAIN$area_total, pattern = "m2" , replacement = "")
DTRAIN$area_total<-str_replace_all(string = DTRAIN$area_total, pattern = "mts" , replacement = "")
DTRAIN$area_total<-str_replace_all(string = DTRAIN$area_total, pattern = "mtrs" , replacement = "")
DTRAIN$area_total<-str_replace_all(string = DTRAIN$area_total, pattern = "metros" , replacement = "")
DTRAIN$area_total<-str_replace_all(string = DTRAIN$area_total, pattern = "m^2" , replacement = "")
DTRAIN$area_total<-str_replace_all(string = DTRAIN$area_total, pattern = "mt" , replacement = "")
DTRAIN$area_total<-str_replace_all(string = DTRAIN$area_total, pattern = "metros cuadrados" , replacement = "")
DTRAIN$area_total<-str_replace_all(string = DTRAIN$area_total, pattern = "cuadrados" , replacement = "")
DTRAIN$area_total<-str_replace_all(string = DTRAIN$area_total, pattern = "\n" , replacement = "")

DTRAIN$area_total<-as.numeric(DTRAIN$area_total)
mnzBog<-readRDS("../Elementos_Guardados/Bogota.rds") #Datos de manzanas Bogotá
mnzAnt<-readRDS("../Elementos_Guardados/Antioquia.rds") #Datos de manzanas Antioquia
mnzBogota<-subset(mnzBog, select=c("MANZ_CCNCT", "geometry"))
saveRDS(mnzBogota, file = "mnzBogotá.rds")
mnzAntioquia<-subset(mnzAnt, select=c("MANZ_CCNCT", "geometry"))
saveRDS(mnzAntioquia, file = "mnzAntioquia.rds")
DTRAIN_sf <- DTRAIN %>% st_as_sf(coords = c("lon", "lat"), crs = 4326)
DTRAIN_Bog<- DTRAIN_sf[DTRAIN_sf$l3=="Bogotá D.C",]
DTRAIN_Med<- DTRAIN_sf[DTRAIN_sf$l3=="Medellín",]

#####
new_area_aux<-cbind(DTRAIN$surface_total, DTRAIN$surface_covered)
new_area_aux_2<-apply(new_area_aux, 1 , max)
new_area_aux_2<-data.frame(new_area_aux_2)
####
DTRAIN_Bog<- DTRAIN_sf[DTRAIN_sf$l3=="Bogotá D.C",]
View(DTRAIN_Bog)
summary(DTRAIN_Bog)
DTRAIN_Med<- DTRAIN_sf[DTRAIN_sf$l3=="Medellín",]
require("tidyverse")
require("sf")
mnzBog$MANZ_CCNCT<-NULL
mnzBog<-st_join(mnzBog,mnzBogota,join = st_intersects)
sum(is.na(mnzBog$MANZ_CCNCT))
sf::sf_use_s2(FALSE)

mnzBogota<-st_transform(mnzBogota, 4326)

DTRAIN_Bog<-DTRAIN_Bog %>% st_as_sf(coords = c("lon", "lat"), crs = 4326)
class(DTRAIN_Bog)
st_crs(mnzBogota) == st_crs(DTRAIN_Bog)
leaflet() %>% addTiles() %>% addPolygons(data=mnzBog , color="red") %>% addCircles(data=DTRAIN_Bog)
Casa_mnz = st_join(x = DTRAIN_Bog,y = mnzBog)
DTRAIN_Bog_NA<-Casa_mnz[is.na(Casa_mnz$MANZ_CCNCT),]

#Se crea vector para tener los datos de las filas de las manzanas más cercanas a los NA
Distancia_cercana<-c(1:nrow(DTRAIN_Bog_NA))

#Distancia_cercana<-st_nn(DTRAIN_Bog_NA, mnzBogota, k = 1, maxdist = 50, progress=TRUE)

#Se realiza un for para que se haga la función st_nn para cada dato de apartamentos con NA y se compara con las manzanas Bogotá
#Se demora mucho este proceso
for (i in 1:nrow(DTRAIN_Bog_NA)){ #Realizar la distancia mínima para los NAs
  Distancia_cercana[i]<-st_nn(DTRAIN_Bog_NA[i,], mnzBogota, k = 1, maxdist = 50, progress=TRUE)
}
saveRDS(Distancia_cercana, file = "../Elementos_Guardados/Manzanas_Bogota_NA.rds")
#Para Medellín#Para Medellin correr
DTRAIN_Med<-DTRAIN_Med %>% st_as_sf(coords = c("lon", "lat"), crs = 4326)
st_crs(mnzAntioquia) == st_crs(DTRAIN_Med)
Casa_mnz_Med = st_join(x = DTRAIN_Med,y = mnzAntioquia)
DTRAIN_Med_NA<-Casa_mnz[is.na(Casa_mnz_Med$MANZ_CCNCT),]

Distancia_cercana_Med<-c(1:nrow(DTRAIN_Med_NA))
for (i in 1:nrow(DTRAIN_Med_NA)){ #Realizar la distancia mínima para los NAs
  Distancia_cercana_Med[i]<-st_nn(DTRAIN_Med_NA[i,], mnzAntioquia, k = 1, maxdist = 50, progress=FALSE)
}

Distancia_cercana_Med<-st_nn(DTRAIN_Med_NA, mnzAntioquia, k = 1, maxdist = 50, progress=FALSE) #Para hacerlo sin el For, a ver si demora menos


saveRDS(Distancia_cercana_Med, file = "../Elementos_Guardados/Manzanas_Med_NA.rds")
#Para Test

colnames(Casa_mnz)
Casa_mnz = Casa_mnz %>%
  group_by(MANZ_CCNCT) %>%
  mutate(new_surface_2=median(surface_total,na.rm=T))
table(is.na(Casa_mnz$new_surface_2))
table(is.na(Casa_mnz$surface_total),
      is.na(Casa_mnz$new_surface_2))
Casa_buf = st_buffer(DTRAIN_Bog,dist=20)
leaflet() %>% addTiles() %>% addPolygons(data=Casa_buf , color="red") %>% addCircles(data=DTRAIN_Bog)
Casa_buf = st_join(Casa_buf,DTRAIN_Bog[,"surface_total"])

st_geometry(Casa_buf) = NULL
Casa_buf_mean = Casa_buf %>% group_by(property_id) %>% summarise(surface_new_3=mean(surface_total.y,na.rm=T))

Casa_mnz = left_join(Casa_mnz,Casa_buf_mean,"property_id")
######
mnzBog<-readRDS("../Elementos_Guardados/Bogota.rds") #Datos de manzanas Bogotá
mnzAnt<-readRDS("../Elementos_Guardados/Antioquia.rds") #Datos de manzanas Antioquia

mnzBogota<-subset(mnzBog, select=c("MANZ_CCNCT", "geometry"))
mnzAntioquia<-subset(mnzAnt, select=c("MANZ_CCNCT", "geometry"))

DTRAIN_Bog<- DTRAIN_sf[DTRAIN_sf$l3=="Bogotá D.C",]
DTRAIN_Med<- DTRAIN_sf[DTRAIN_sf$l3=="Medellín",]

sf::sf_use_s2(FALSE)

mnzBogota<-st_transform(mnzBogota, 4326)

DTRAIN_Bog<-DTRAIN_Bog %>% st_as_sf(coords = c("lon", "lat"), crs = 4326)

st_crs(mnzBogota) == st_crs(DTRAIN_Bog)

Casa_mnz_aux<- st_join(DTRAIN_Bog,mnzBogota,join = nngeo::st_nn, maxdist = 50, k = 1, progress = TRUE)


Casa_mnz_aux2 = st_join(DTRAIN_Med,mnzAntioquia, join = nngeo::st_nn, maxdist = 50, k = 1, progress = FALSE)

leaflet() %>% addTiles() %>% addPolygons(data=mnzBogota , color="red") %>% addCircles(data=Casa_mnz_aux)


## Promedio de la manzana
Casa_mnz_aux = Casa_mnz_aux %>%
  group_by(MANZ_CCNCT) %>%
  mutate(nueva_area=median(surface_total,na.rm=T))

table(is.na(Casa_mnz_aux$surface_total))

table(is.na(Casa_mnz_aux$surface_total),
      is.na(Casa_mnz_aux$nueva_area)) # a

## k-vecinos espaciales
test = is.na(Casa_mnz_aux$surface_total)
no_test = is.na(Casa_mnz_aux$surface_total)==F


k1 = knn(train=Casa_mnz_aux[no_test,c("geometry","surface_total")], ## base de entrenamiento
         test=Casa_mnz_aux[test,c("geometry","surface_total")],   ## base de testeo
         cl=Casa_mnz_aux$surface_total[test], ## outcome
         k=1)  


table(is.na(Casa_mnz_aux$MANZ_CCNCT))

DTRAIN[12810,]

area_tot<- apply(new_area_aux, 1 , max)


area_tot<-data.frame(area_tot)
summary(area_tot)

DTRAIN<-cbind(DTRAIN, area_tot )


table(house$new_surface)

DTRAIN[106000,]


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


DTRAIN <- DTRAIN  %>% 
  mutate(banio_tot= str_extract(string=DTRAIN$description , pattern= paste0(pat_1b,"|",pat_2b,"|", pat_3b,"|", pat_4b,"|", pat_5b,"|", pat_6b,"|", pat_7b,"|", pat_8b,"|", pat_9b,"|", pat_10b,"|", pat_11b,"|", pat_12b,"|", pat_13b,"|", pat_14b,"|", pat_15b,"|", pat_16b,"|", pat_17b,"|", pat_18b,"|", pat_19b,"|", pat_20b,"|", pat_21b,"|", pat_22b,"|", pat_23b,"|", pat_24b,"|", pat_25b,"|", pat_26b,"|", pat_27b,"|", pat_28b,"|", pat_29b,"|", pat_30b,"|", pat_31b,"|", pat_32b,"|", pat_33b,"|", pat_34b,"|", pat_35b,"|", pat_36b,"|", pat_37b,"|", pat_38b,"|", pat_39b,"|", pat_40b,"|", pat_41b,"|", pat_42b,"|", pat_43b,"|", pat_44b,"|", pat_45b,"|", pat_46b,"|", pat_47b) ))

DTRAIN$banio_tot<-str_replace_all(string = DTRAIN$banio_tot, pattern = "," , replacement = ".")
DTRAIN$banio_tot<-str_replace_all(string = DTRAIN$banio_tot, pattern = "baño" , replacement = "")
DTRAIN$banio_tot<-str_replace_all(string = DTRAIN$banio_tot, pattern = "baños" , replacement = "")
DTRAIN$banio_tot<-str_replace_all(string = DTRAIN$banio_tot, pattern = "banio" , replacement = "")
DTRAIN$banio_tot<-str_replace_all(string = DTRAIN$banio_tot, pattern = "banios" , replacement = "")
DTRAIN$banio_tot<-str_replace_all(string = DTRAIN$banio_tot, pattern = "bano" , replacement = "")
DTRAIN$banio_tot<-str_replace_all(string = DTRAIN$banio_tot, pattern = "banos" , replacement = "")
DTRAIN$banio_tot<-str_replace_all(string = DTRAIN$banio_tot, pattern = "vano" , replacement = "")
DTRAIN$banio_tot<-str_replace_all(string = DTRAIN$banio_tot, pattern = "vanos" , replacement = "")
DTRAIN$banio_tot<-str_replace_all(string = DTRAIN$banio_tot, pattern = "vanio" , replacement = "")
DTRAIN$banio_tot<-str_replace_all(string = DTRAIN$banio_tot, pattern = "vanios" , replacement = "")
DTRAIN$banio_tot<-str_replace_all(string = DTRAIN$banio_tot, pattern = "vaño" , replacement = "")
DTRAIN$banio_tot<-str_replace_all(string = DTRAIN$banio_tot, pattern = "vaños" , replacement = "")
DTRAIN$banio_tot<-str_replace_all(string = DTRAIN$banio_tot, pattern = "bañio" , replacement = "")
DTRAIN$banio_tot<-str_replace_all(string = DTRAIN$banio_tot, pattern = "bañios" , replacement = "")
DTRAIN$banio_tot<-str_replace_all(string = DTRAIN$banio_tot, pattern = "bao" , replacement = "")
DTRAIN$banio_tot<-str_replace_all(string = DTRAIN$banio_tot, pattern = "baos" , replacement = "")
DTRAINbanio_tot<-str_replace_all(string = DTRAIN$banio_tot, pattern = "\n" , replacement = "")
DTRAIN$banio_tot<-as.numeric(DTRAIN$banio_tot)
View(DTRAIN)
DTRAIN$banio_tot[is.na(DTRAIN$banio_tot)] = 1
summary(DTRAIN$banio_tot) 
DTRAIN$bathrooms[is.na(DTRAIN$bathrooms)] = 1
new_banio_aux<-cbind(DTRAIN$bathrooms, DTRAIN$banio_tot)
View(new_banio_aux)
baniostot<-apply(new_banio_aux, 1 , max)
baniostot<-data.frame(baniostot)
View(baniostot)
summary(baniostot)
View(DTRAIN)
DTRAIN<- cbind(DTRAIN, baniostot)
View(DTRAIN)
View(DTRAIN)
rm(baniostot)
#TEST BAÑOS
DTEST <- DTEST  %>% 
  mutate(banio_tot= str_extract(string=DTEST$description , pattern= paste0(pat_1b,"|",pat_2b,"|", pat_3b,"|", pat_4b,"|", pat_5b,"|", pat_6b,"|", pat_7b,"|", pat_8b,"|", pat_9b,"|", pat_10b,"|", pat_11b,"|", pat_12b,"|", pat_13b,"|", pat_14b,"|", pat_15b,"|", pat_16b,"|", pat_17b,"|", pat_18b,"|", pat_19b,"|", pat_20b,"|", pat_21b,"|", pat_22b,"|", pat_23b,"|", pat_24b,"|", pat_25b,"|", pat_26b,"|", pat_27b,"|", pat_28b,"|", pat_29b,"|", pat_30b,"|", pat_31b,"|", pat_32b,"|", pat_33b,"|", pat_34b,"|", pat_35b,"|", pat_36b,"|", pat_37b,"|", pat_38b,"|", pat_39b,"|", pat_40b,"|", pat_41b,"|", pat_42b,"|", pat_43b,"|", pat_44b,"|", pat_45b,"|", pat_46b,"|", pat_47b) ))

DTEST$banio_tot<-str_replace_all(string = DTEST$banio_tot, pattern = "," , replacement = ".")
DTEST$banio_tot<-str_replace_all(string = DTEST$banio_tot, pattern = "baño" , replacement = "")
DTEST$banio_tot<-str_replace_all(string = DTEST$banio_tot, pattern = "baños" , replacement = "")
DTEST$banio_tot<-str_replace_all(string = DTEST$banio_tot, pattern = "banio" , replacement = "")
DTEST$banio_tot<-str_replace_all(string = DTEST$banio_tot, pattern = "banios" , replacement = "")
DTEST$banio_tot<-str_replace_all(string = DTEST$banio_tot, pattern = "bano" , replacement = "")
DTEST$banio_tot<-str_replace_all(string = DTEST$banio_tot, pattern = "banos" , replacement = "")
DTEST$banio_tot<-str_replace_all(string = DTEST$banio_tot, pattern = "vano" , replacement = "")
DTEST$banio_tot<-str_replace_all(string = DTEST$banio_tot, pattern = "vanos" , replacement = "")
DTEST$banio_tot<-str_replace_all(string = DTEST$banio_tot, pattern = "vanio" , replacement = "")
DTEST$banio_tot<-str_replace_all(string = DTEST$banio_tot, pattern = "vanios" , replacement = "")
DTEST$banio_tot<-str_replace_all(string = DTEST$banio_tot, pattern = "vaño" , replacement = "")
DTEST$banio_tot<-str_replace_all(string = DTEST$banio_tot, pattern = "vaños" , replacement = "")
DTEST$banio_tot<-str_replace_all(string = DTEST$banio_tot, pattern = "bañio" , replacement = "")
DTEST$banio_tot<-str_replace_all(string = DTEST$banio_tot, pattern = "bañios" , replacement = "")
DTEST$banio_tot<-str_replace_all(string = DTEST$banio_tot, pattern = "bao" , replacement = "")
DTEST$banio_tot<-str_replace_all(string = DTEST$banio_tot, pattern = "baos" , replacement = "")
DTEST$banio_tot<-str_replace_all(string = DTEST$banio_tot, pattern = "\n" , replacement = "")
DTEST$banio_tot<-as.numeric(DTEST$banio_tot)
View(DTEST)
DTEST$banio_tot[is.na(DTEST$banio_tot)] = 1
summary(DTEST$banio_tot) 
DTEST$bathrooms[is.na(DTEST$bathrooms)] = 1
new_banio_auxT<-cbind(DTEST$bathrooms, DTEST$banio_tot)
View(new_banio_auxT)
baniostot<-apply(new_banio_auxT, 1 , max)
baniostot<-data.frame(baniostot)
View(baniostot)
summary(baniostot)
View(DTRAIN)
DTEST<- cbind(DTEST, baniostot)
View(DTEST)
View(DTRAIN)
rm(baniostot)
#Limpieza Rooms
#TRAIN
DTRAIN$rooms[is.na(DTRAIN$rooms)] = 1
summary(DTRAIN$rooms)
habitaciones_aux<-cbind(DTRAIN$rooms, DTRAIN$bedrooms)
View(habitaciones_aux)
habitaciones<-apply(habitaciones_aux, 1 , max)
habitaciones<- data.frame(habitaciones)
View(habitaciones)
summary(habitaciones)
View(DTRAIN)
DTRAIN<- cbind(DTRAIN,habitaciones)
View(DTRAIN)
rm(habitaciones)
#test
summary(DTEST$rooms)
DTEST$rooms[is.na(DTEST$rooms)] = 1
summary(DTEST$rooms)
habitaciones_auxT<-cbind(DTEST$rooms, DTEST$bedrooms)
View(habitaciones_auxT)
habitaciones<-apply(habitaciones_auxT, 1 , max)
habitaciones<- data.frame(habitaciones)
View(habitaciones)
summary(habitaciones)
View(DTEST)
DTEST<- cbind(DTEST,habitaciones)
View(DTEST)
rm(habitaciones)
####Descripción de variables 
#Habitaciones
habitaciones <- data.frame(DTRAIN$habitaciones) 
class(DTRAIN$habitaciones)
plot(hist(DTRAIN$habitaciones),col = "black", main="Histograma No. de habitaciones de la vivienda",
     xlab="Habitaciones",
     ylab="Frecuencia")
min(DTRAIN$habitaciones)
max(DTRAIN$habitaciones)
mean(DTRAIN$habitaciones)
modehabitaciones <- function(habitaciones){
  return(as.numeric(names(which.max(table(habitaciones)))))}
modehabitaciones(habitaciones)
summary(habitaciones)
rm(habitaciones)
#Descripción baños
baños <- as.numeric(DTRAIN$baniostot) 
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
Ascensor <- as.factor(DTRAIN$ascensorT)
class(Ascensor)
skim(Ascensor)
Ascensor <- factor(Ascensor, labels = c("1", "0"))
summary(Ascensor)
rm(Ascensor)
#Parqueadero
Parqueadero <- as.factor(DTRAIN$parqueaderoT)
class(Parqueadero)
Parqueadero <- factor(Parqueadero, labels = c("1", "0"))
summary(Parqueadero)
rm(Parqueadero)
#Tipo inmueble
TipoVivienda <- as.factor(DTRAIN$property_type)
class(TipoVivienda)
summary(TipoVivienda)
rm(TipoVivienda)




##Para obtener datos de Open Street Map
#===================================================================================

DTRAIN[46,]
DTRAIN_sf <- DTRAIN %>% st_as_sf(coords = c("lon", "lat"), crs = 4326)
class(DTRAIN_sf) 
DTRAIN_sf_mapa <-DTRAIN_mapa %>% st_as_sf(coords = c("lon", "lat"), crs = 4326)
class(DTRAIN_sf_mapa) 
DTEST_sf <- DTEST %>% st_as_sf(coords = c("lon", "lat"), crs = 4326)
#Para visualizar todas las observaciones en Bogotá y Medellín
leaflet() %>% addTiles() %>% addCircleMarkers(data=DTRAIN_sf_mapa)
#Solo observaremos 1000
leaflet() %>% addTiles() %>% addCircleMarkers(data=DTRAIN_sf[1:1000,])
#Buscar el poblado
require("tmaptools")
geocode_OSM("El poblado, Medellin") 
PointElPoblado = geocode_OSM("Comuna 14 - El Poblado, Medellín", as.sf=T)  
PointElPoblado
PointChapinero = geocode_OSM("UPZ Chapinero, Bogotá", as.sf=T) 
PointChapinero
leaflet() %>% addTiles() %>% addCircles(data=PointElPoblado)
leaflet() %>% addTiles() %>% addCircles(data=PointChapinero)
## la función addTiles adiciona la capa de OpenStreetMap
leaflet() %>% addTiles() %>% addCircles(data=PointElPoblado)
#Poligono chapinero
Polchapinero <- getbb(place_name = "UPZ Chapinero, Bogota", 
                   featuretype = "boundary:administrative", 
                   format_out = "sf_polygon") %>% .$multipolygon
leaflet() %>% addTiles() %>% addPolygons(data= Polchapinero, col = "blue")%>% 
           addCircleMarkers(data=DTRAIN_sf_mapa, col= "red")
#Poligono poblado
PolPoblado <- getbb(place_name = "Comuna 14 - El Poblado, Medellín", 
                      featuretype = "boundary:administrative", 
                      format_out = "sf_polygon") %>% .$multipolygon
leaflet() %>% addTiles() %>% addPolygons(data= Poblado, col = "blue")%>% 
  addCircleMarkers(data=DTRAIN_sf, col= "red")
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

##Extracción datos de manzanas


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
barant = opq(bbox = st_bbox(mnzAntioquia)) %>%
  add_osm_feature(key = "amenity", value = "bar") %>%
  osmdata_sf() %>% .$osm_points %>% select(osm_id,name)
barant %>% head()
#Visualizar info 
leaflet() %>% addTiles() %>% 
  addPolygons(data=mnzBogota) %>% # manzanas
  addPolygons(data= CHAPINERO , col="green") %>%  # transportepub
  addCircles(data= DTRAIN_sf , col="red", weight=2) %>% # apartamentos
  addCircles(data=barbog , col="black" , weight=2)
st_crs(mnzBogota) == st_crs(DTRAIN_sf)
st_crs(mnzAntioquia) == st_crs(DTRAIN_sf)
#Uniondatbog = st_join(x=DTRAIN_sf , y=mnzBogota)

#Se calculará distancia a bares
#Con respecto a bares Bogotá
dist_bar_bog <- st_distance(x=DTRAIN_sf , y=barbog)
min_dist_bog <- apply(dist_bar_bog , 1 , min)
min_dist_bog<-data.frame(min_dist_bog)

#Con respecto a bares Antioquia
dist_bar_med <- st_distance(x=DTRAIN_sf , y=barant)
min_dist_bar_med <- apply(dist_bar_med, 1 , min)
min_dist_bar_med<-data.frame(min_dist_bar_med)

#Se selecciona la menor distancia, esto porque se compararon distancias de apartamentos en Bogotá con ubicaciones en Antioquia y viceversa
min_dist_bar<-cbind(min_dist_bog,min_dist_bar_med)
min_dist_bar_<- apply(min_dist_bar, 1 , min)
min_dist_bar_<-data.frame(min_dist_bar_)

#Se incorpora la mínima distancia a bares al DTRAIN
DTRAIN<-cbind(DTRAIN, min_dist_bar_ )
DTRAIN_sf<-cbind(DTRAIN_sf, min_dist_bar_ )


#Para Test:
rm(min_dist_bar_)

dist_bar_test_bog <- st_distance(x=DTEST_sf , y=barbog)
min_dist_test_bog <- apply(dist_bar_test_bog , 1 , min)
min_dist_test_bog<-data.frame(min_dist_test_bog)

#Con respecto a bares Antioquia
dist_bar_test_med <- st_distance(x=DTEST_sf , y=barant)
min_dist_bar_test_med <- apply(dist_bar_test_med, 1 , min)
min_dist_bar_test_med<-data.frame(min_dist_bar_test_med)

#Se selecciona la menor distancia, esto porque se compararon distancias de apartamentos en Bogotá con ubicaciones en Antioquia y viceversa
min_dist_test_bar<-cbind(min_dist_test_bog,min_dist_bar_test_med)
min_dist_bar_<- apply(min_dist_test_bar, 1 , min)
min_dist_bar_<-data.frame(min_dist_bar_)

#Se incorpora la mínima distancia a bares al DTRAIN
DTEST<-cbind(DTEST, min_dist_bar_ )
DTEST_sf<-cbind(DTEST_sf, min_dist_bar_ )



#Se obtendrá la distancia mínima a transporte público

#Con respecto a transporte público en Bogotá
dist_transp_bog<- st_distance(x=DTRAIN_sf , y=Transporte_publicoBog)
min_dist_transp_bog <- apply(dist_transp_bog , 1 , min)
min_dist_transp_bog<-data.frame(min_dist_transp_bog)

#Con respecto a transporte público en Medellín
dist_transp_med <- st_distance(x=DTRAIN_sf , y=Transporte_publicoMed)
min_dist_transp_med <- apply(dist_transp_med, 1 , min)
min_dist_transp_med<-data.frame(min_dist_transp_med)

#Se selecciona la menor distancia, esto porque se compararon distancias de apartamentos en Bogotá con ubicaciones en Antioquia y viceversa
min_dist_transp<-cbind(min_dist_transp_bog,min_dist_transp_med)
min_dist_transp_<- apply(min_dist_transp, 1 , min)
min_dist_transp_<-data.frame(min_dist_transp_)

#Se incorpora la mínima distancia a bares al DTRAIN
DTRAIN<-cbind(DTRAIN, min_dist_transp_)
DTRAIN_sf<-cbind(DTRAIN_sf, min_dist_transp_ )

#Para Test
rm(min_dist_transp_)
#Con respecto a transporte público en Bogotá
dist_transp_test_bog<- st_distance(x=DTEST_sf , y=Transporte_publicoBog)
min_dist_transp_test_bog <- apply(dist_transp_test_bog , 1 , min)
min_dist_transp_test_bog<-data.frame(min_dist_transp_test_bog)

#Con respecto a transporte público en Medellín
dist_transp_test_med <- st_distance(x=DTEST_sf , y=Transporte_publicoMed)
min_dist_transp_test_med <- apply(dist_transp_test_med, 1 , min)
min_dist_transp_test_med<-data.frame(min_dist_transp_test_med)

#Se selecciona la menor distancia, esto porque se compararon distancias de apartamentos en Bogotá con ubicaciones en Antioquia y viceversa
min_dist_test_transp<-cbind(min_dist_transp_test_bog,min_dist_transp_test_med)
min_dist_transp_<- apply(min_dist_test_transp, 1 , min)
min_dist_transp_<-data.frame(min_dist_transp_)

#Se incorpora la mínima distancia a bares al DTRAIN
DTEST<-cbind(DTEST, min_dist_transp_)
DTEST_sf<-cbind(DTEST_sf, min_dist_transp_)






######
