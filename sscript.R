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

tic()
osm_b = opq(bbox = getbb("Bogota Colombia")) %>%
  add_osm_feature(key="amenity" , value="bar") 

osm_sf_b = osm_b %>% osmdata_sf()
bares = osm_sf_b$osm_points %>% select(osm_id) 
st_crs(bares)


matriz_distancia<-st_distance(x=prueba_bog,y=bares)
min_matriz <- apply(matriz_distancia , 1 , min)
prueba_bog$bar2 <- min_matriz
toc()

#------------------------------------------------------------------------------
 
### Este si

amenity<- c("cafe","pub","restaurant","college","library","school","university","fuel","atm","bank",
              "clinic","hospital","pharmacy","cinema","gambling","nightclub","courthouse","police","townhall","bus_station")

building<-c("cathedral","church","stadium")

landuse<-c("commercial","education","industrial","residential")

leisure<- c("fitness_centre","park","playground")

shop <- c("alcohol","bakery","coffee","mall","supermarket","jewelry","cosmetics","car")


tourism <- c("hotel")

features <- c("amenity","building","landuse","leisure","shop","tourism")

for (i in features){

    if (i =="amenity"){
      for (j in amenity){
        
        osm = opq(bbox = getbb("Bogota Colombia")) %>%
          add_osm_feature(key=i , value=j) 
        
        osm_sf = osm %>% osmdata_sf()
        generico_j = osm_sf$osm_points %>% select(osm_id) 
        
        
        matriz_distancia<-st_distance(x=prueba_bog,y=generico_j)
        dist_ <- apply(matriz_distancia , 1 , min)
        prueba_bog[,ncol(prueba_bog) + 1]<-dist_
        colnames(prueba_bog)[ncol(prueba_bog)]<-paste0("dist_",z)
        
        
      }
      
      
      }
    else if (i == "building"){
      print(building)
    }
    else if ( i== "landuse"){
      print(suelo)
      
    }
    else if (i=="leisure"){
      print(leisure)
      
    }
    else if (i=="shop"){
      print(shops)
      
    }
    else if (i=="nature"){
      print(nature)
      
    }
    else if (i=="tourism"){
      print(tourism)
      
    }
}



#--------------------


############## Este nooo


amenity <- c("cafe","pub")
#nature <- c("tree")

keys <- c("amenity","nature")

for (i in keys){
  
  if (i == "amenity"){
    for (z in amenity){
      
      osm = opq(bbox = getbb("Bogota Colombia")) %>%
        add_osm_feature(key=i , value=z) 
      
      osm_sf = osm %>% osmdata_sf()
      generico_j = osm_sf$osm_points %>% select(osm_id) 
      
      
      matriz_distancia<-st_distance(x=prueba_bog,y=generico_j)
      dist_ <- apply(matriz_distancia , 1 , min)
      prueba_bog[,ncol(prueba_bog) + 1]<-dist_
      colnames(prueba_bog)[ncol(prueba_bog)]<-paste0("dist_",z)
      
    }
  }
  else if (i =="nature"){
    for (z in nature){
      
      osm = opq(bbox = getbb("Bogota Colombia")) %>%
        add_osm_feature(key=i , value=z) 
      
      osm_sf = osm %>% osmdata_sf()
      generico_j = osm_sf$osm_points %>% select(osm_id) 
      
      
      matriz_distancia<-st_distance(x=prueba_bog,y=generico_j)
      dist_ <- apply(matriz_distancia , 1 , min)
      prueba_bog[,ncol(prueba_bog) + 1]<-dist_
      colnames(prueba_bog)[ncol(prueba_bog)]<-paste0("dist_",z)
    
  }
  
}
}
                         


    