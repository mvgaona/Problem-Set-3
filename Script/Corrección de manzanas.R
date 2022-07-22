require(pacman)
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
       nngeo,
       osmdata)
p_load(tydiverse,
       rio,
       leaftlet,
       tmaptools,
       omsdata,
       nngeo)
DTEST<-data.frame(readRDS("C:/Users/valer/Desktop/Andes/Intersemestral/Big Data/ArchivoPS3/test.rds"))  #Guardar las bases de datos
DTRAIN <- data.frame(readRDS("C:/Users/valer/Desktop/Andes/Intersemestral/Big Data/ArchivoPS3/train.rds"))
train<- DTRAIN %>% mutate(base = "train")
test <- DTEST %>% mutate(base="test")
HOUSE<- bind_rows(DTRAIN,DTEST) %>% st_as_sf(coords=c("lon","lat"),crs=4326)
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
mnzBog<-readRDS("C:/Users/valer/Desktop/Andes/Intersemestral/Big Data/ArchivoPS3/Bogota.rds") #Datos de manzanas Bogotá
sf_use_s2(FALSE)
mnzBogota<-subset(mnzBog, select=c("MANZ_CCNCT", "geometry"))
sf_use_s2(FALSE)
mnz_chap <- mnzBogota[Polchapinero,]
leaflet() %>% addTiles() %>% addCircles(data = House_Chapinero, color = "red" ) %>% addPolygons(data= mnz_chap, col = "blue")
house_chapinero_mnz <- st_join(House_Chapinero, mnz_chap)
colnames(house_chapinero_mnz)
table(is.na(house_chapinero_mnz$MANZ_CCNCT))
db_1 <- house_chapinero_mnz %>% subset(is.na(MANZ_CCNCT)==F)
db_2 <- house_chapinero_mnz %>% subset(is.na(MANZ_CCNCT)==T) %>% mutate(MANZ_CCNCT)
leaflet() %>% addTiles() %>% addPolygons(data=db_2[1,] %>% st_buffer(dist = 0.0005))
db_2 <- st_join(st_buffer(db_2, dist = 0.0005), mnz_chap)%>% subset(duplicated(property_id)==F)
#MANNZ
table(is.na(db_2$MANZ_CCNCT.y))
