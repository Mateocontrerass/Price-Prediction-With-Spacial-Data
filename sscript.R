## Problem Set 3 : Predicting price with Spacial data
# Big data and Machine Learning

# Mateo Contreras
# Carlos Ayala
# Federico Meneses


#------------------------------------------------------------------------------
# Limpiar el espacio de trabajo

#Enviroment
rm(list=ls())
#Console
cat("\f")

#------------------------------------------------------------------------------
# Cargar paquetes
require(pacman)
p_load(tidyverse,
       rio,
       sf,
       leaflet,
       tmaptools,
       ggsn,
       osmdata,
       dplyr,
       skimr,
       tidyr,
       janitor, 
       mixgb, #XGBoost
       rstudioapi,
       tictoc, ##Saber cuanto demora corriendo el script
       install = TRUE)

set.seed(666)



#------------------------------------------------------------------------------
# Directorio

path_code <- dirname(getActiveDocumentContext()$path)

setwd(path_code)


#------------------------------------------------------------------------------
#Importar los datos

p1_train <- import("data/train.rds")
p1_test <- import("data/test.rds")

## dataframe to sf
train <- st_as_sf(x = p1_train, ## datos
                   coords=c("lon","lat"), ## coordenadas
                   crs=4326) ## CRS


test <- st_as_sf(x = p1_test, ## datos
                   coords=c("lon","lat"), ## coordenadas
                   crs=4326) ## CRS


#------------------------------------------------------------------------------
#Carreteras principales ( falta subrayar la autonorte, la NQS y la 80)


 # 1. Trunk

## objeto osm

osm = opq(bbox = getbb("Bogota Colombia")) %>%
  add_osm_feature(key="highway" , value="trunk") 

## extraer Simple Features Collection
osm_sf = osm %>% osmdata_sf()
osm_sf

## Obtener un objeto sf
carreteras = osm_sf$osm_lines %>% select(osm_id,name) 
carreteras


# Pintar las carreteras -> faltan muchas

leaflet() %>% addTiles() %>% addPolylines(data=carreteras , col="red")



  # 2. Primary


osm2 = opq(bbox = getbb("Bogota Colombia")) %>%
  add_osm_feature(key="highway" , value="primary") 

## extraer Simple Features Collection
osm_sf2 = osm2 %>% osmdata_sf()
osm_sf2

## Obtener un objeto sf
carreteras2 = osm_sf2$osm_lines %>% select(osm_id,name) 
carreteras2

# Pegar las vias trunk + primary
vias_principales<-rbind(carreteras,carreteras2)

# Dibujar para verificar
leaflet() %>% addTiles() %>% addPolylines(data=vias_principales , col="red")

# Distancia de los inmuebles a alguna via principal

prueba_bog<- subset(train, city=="Bogotá D.C") # voy a hacer la prueba en bog

# verificar crs
st_crs(prueba_bog)==st_crs(vias_principales)



#lbreria tictoc para ver los tiempos
tic()
#calcular la matriz de distancias entre las casas y las vias principales
matrix_distancias_vias<-st_distance(x=prueba_bog,y=vias_principales)
toc()


#Sacar la distancia a la via más cercana
mat_dis_min_vias <- apply(matrix_distancias_vias , 1 , min)



# Se agrega el vector de distancia a via más cercana al dataframe 
prueba_bog$dist_via <- mat_dis_min_vias



######### Nota: Voy a hacer todo el proceso para bogotá sola y despuess hacemos
# un loop para que itere sobre las tres ciudades, pero mejor dejar una sola bien 
# hecha


#------------------------------------------------------------------------------

#     Leisure

# Bares

osm_b = opq(bbox = getbb("Bogota Colombia")) %>%
  add_osm_feature(key="amenity" , value="bar") 

osm_sf_b = osm_b %>% osmdata_sf()
bares = osm_sf_b$osm_points %>% select(osm_id,name) 


leaflet() %>% addTiles() %>% addCircles(data=bares , col="purple")

matriz_distancia<-st_distance(x=prueba_bog,y=bares)
min_matriz <- apply(matriz_distancia , 1 , min)
prueba_bog$bar <- min_matriz


#------------------------------------------------------------------------------
 
# Yo no voy a hacer esta vga 25 veces voy a hacer un loop para iterar 1. sobre los features y despues sobre los key.





                                          