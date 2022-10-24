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
       rstudioapi, install = TRUE)

set.seed(666)



#------------------------------------------------------------------------------
# Directorio

path_code <- dirname(getActiveDocumentContext()$path)

setwd(path_code)


#------------------------------------------------------------------------------
#Importar los datos

train <- import("data/train.rds")
test <- import("data/test.rds")


#------------------------------------------------------------------------------
#Carreteras principales ( falta subrayar la autonorte, la NQS y la 80)

#Sumar Trunk + Primary


 # 1. Trunk

## objeto osm

osm = opq(bbox = getbb("Bogota Colombia")) %>%
  add_osm_feature(key="highway" , value="trunk") 
class(osm)

## extraer Simple Features Collection
osm_sf = osm %>% osmdata_sf()
osm_sf

## Obtener un objeto sf
carreteras = osm_sf$osm_lines %>% select(osm_id,name) 
carreteras

## Pintar las estaciones de autobus
leaflet() %>% addTiles() %>% addPolylines(data=carreteras , col="red")



  # 2. Primary


osm2 = opq(bbox = getbb("Bogota Colombia")) %>%
  add_osm_feature(key="highway" , value="primary") 
class(osm)

## extraer Simple Features Collection
osm_sf2 = osm %>% osmdata_sf()
osm_sf2

## Obtener un objeto sf
carreteras2 = osm_sf2$osm_lines %>% select(osm_id,name) 
carreteras2


prueba<-st_union(x=carreteras,y=carreteras2)

dibujo <- prueba %>% select(osm_id,name,geometry)
leaflet() %>% addTiles() %>% addPolylines(data=dibujo , col="red")

