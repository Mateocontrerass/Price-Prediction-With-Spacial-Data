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
       rlang)


       

set.seed(666)



#------------------------------------------------------------------------------
# Directorio

path_code <- dirname(getActiveDocumentContext()$path)

setwd(path_code)


#------------------------------------------------------------------------------
#Importar los datos

p1_train <- readRDS("data/train.rds")
p1_test <- readRDS("data/test.rds")

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
carreteras <- osm_sf$osm_lines %>% select(osm_id,name) 
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

landuse<-c("commercial","education","industrial")

leisure<- c("fitness_centre","park","playground")

shop <- c("alcohol","bakery","coffee","mall","supermarket","jewelry","cosmetics","car")


tourism <- c("hotel")

features <- c("amenity","building","landuse","leisure","shop","tourism")

#features <- c("building","landuse","leisure","shop","tourism")


bogota<- subset(train, city=="Bogotá D.C") 


#------------------------------------------------------------------------------

#Bogota


tic()


  for (i in features){
  
      if (i =="amenity"){
        for (j in amenity){
          
          osm = opq(bbox = getbb("Bogota Colombia")) %>%
            add_osm_feature(key=i , value=j) 
          
          osm_sf = osm %>% osmdata_sf()
          generico_j = osm_sf$osm_points %>% dplyr::select(osm_id) 
          
          
          matriz_distancia<-st_distance(x=bogota,y=generico_j)
          dist_ <- apply(matriz_distancia , 1 , min)
          bogota[,ncol(bogota) + 1]<-dist_
          colnames(bogota)[ncol(bogota)]<-paste0("dist_",j)
          
          avance <- paste0("Terminada feature ", j, ".")
          print(avance)
          
        }
        
        
        }
      else if (i == "building"){
        for (j in building){
          
          osm = opq(bbox = getbb("Bogota Colombia")) %>%
            add_osm_feature(key=i , value=j) 
          
          osm_sf = osm %>% osmdata_sf()
          generico_j = osm_sf$osm_points %>% dplyr::select(osm_id) 
          
          
          matriz_distancia<-st_distance(x=bogota,y=generico_j)
          dist_ <- apply(matriz_distancia , 1 , min)
          bogota[,ncol(bogota) + 1]<-dist_
          colnames(bogota)[ncol(bogota)]<-paste0("dist_",j)
          
          avance <- paste0("Terminada feature ", j, ".")
          print(avance)
          
        }
        
        
            }
      else if ( i== "landuse"){
        for (j in landuse){
          
          osm = opq(bbox = getbb("Bogota Colombia")) %>%
            add_osm_feature(key=i , value=j) 
          
          osm_sf = osm %>% osmdata_sf()
          generico_j = osm_sf$osm_points %>% dplyr::select(osm_id) 
          
          
          matriz_distancia<-st_distance(x=bogota,y=generico_j)
          dist_ <- apply(matriz_distancia , 1 , min)
          bogota[,ncol(bogota) + 1]<-dist_
          colnames(bogota)[ncol(bogota)]<-paste0("dist_",j)
          
          avance <- paste0("Terminada feature ", j, ".")
          print(avance)
          
        }      
      }
      else if (i=="leisure"){
  
        for (j in leisure){
          
          osm = opq(bbox = getbb("Bogota Colombia")) %>%
            add_osm_feature(key=i , value=j) 
          
          osm_sf = osm %>% osmdata_sf()
          generico_j = osm_sf$osm_points %>% dplyr::select(osm_id) 
          
          
          matriz_distancia<-st_distance(x=bogota,y=generico_j)
          dist_ <- apply(matriz_distancia , 1 , min)
          bogota[,ncol(bogota) + 1]<-dist_
          colnames(bogota)[ncol(bogota)]<-paste0("dist_",j)
          
          avance <- paste0("Terminada feature ", j, ".")
          print(avance)
          
        }
        
        
      }
      else if (i=="shop"){
  
        for (j in shop){
          
          osm = opq(bbox = getbb("Bogota Colombia")) %>%
            add_osm_feature(key=i , value=j) 
          
          osm_sf = osm %>% osmdata_sf()
          generico_j = osm_sf$osm_points %>% dplyr::select(osm_id) 
          
          
          matriz_distancia<-st_distance(x=bogota,y=generico_j)
          dist_ <- apply(matriz_distancia , 1 , min)
          bogota[,ncol(bogota) + 1]<-dist_
          colnames(bogota)[ncol(bogota)]<-paste0("dist_",j)
          
          avance <- paste0("Terminada feature ", j, ".")
          print(avance)
          
        }
        
        
      }
      else if (i=="nature"){
  
        for (j in nature){
          
          osm = opq(bbox = getbb("Bogota Colombia")) %>%
            add_osm_feature(key=i , value=j) 
          
          osm_sf = osm %>% osmdata_sf()
          generico_j = osm_sf$osm_points %>% dplyr::select(osm_id) 
          
          
          matriz_distancia<-st_distance(x=bogota,y=generico_j)
          dist_ <- apply(matriz_distancia , 1 , min)
          bogota[,ncol(bogota) + 1]<-dist_
          colnames(bogota)[ncol(bogota)]<-paste0("dist_",j)
          
          avance <- paste0("Terminada feature ", j, ".")
          print(avance)
          
        }
        
        
      }
      else if (i=="tourism"){
  
        for (j in tourism){
          
          osm = opq(bbox = getbb("Bogota Colombia")) %>%
            add_osm_feature(key=i , value=j) 
          
          osm_sf = osm %>% osmdata_sf()
          generico_j = osm_sf$osm_points %>% dplyr::select(osm_id) 
          
          
          matriz_distancia<-st_distance(x=bogota,y=generico_j)
          dist_ <- apply(matriz_distancia , 1 , min)
          bogota[,ncol(bogota) + 1]<-dist_
          colnames(bogota)[ncol(bogota)]<-paste0("dist_",j)
          
          avance <- paste0("Terminada feature ", j, ".")
          print(avance)
          
        }
        
        
      }
  }


toc()


write.csv(x = bogota, file = "bogota.csv", sep = ",",
          row.names = T, col.names = TRUE)

df <- read.csv("bogota.csv", header = TRUE, sep = ",")



#------------------------------------------------------------------------------
#Medellin

#Base medellin
medellin<- subset(train, city=="Medellín") 


tic()


for (i in features){
  
  if (i =="amenity"){
    for (j in amenity){
      
      osm = opq(bbox = getbb("medellin Colombia")) %>%
        add_osm_feature(key=i , value=j) 
      
      osm_sf = osm %>% osmdata_sf()
      generico_j = osm_sf$osm_points %>% select(osm_id) 
      
      
      matriz_distancia<-st_distance(x=medellin,y=generico_j)
      dist_ <- apply(matriz_distancia , 1 , min)
      medellin[,ncol(medellin) + 1]<-dist_
      colnames(medellin)[ncol(medellin)]<-paste0("dist_",j)
      
      avance <- paste0("Terminada feature ", j, ".")
      print(avance)
      
    }
    
    
  }
  else if (i == "building"){
    for (j in building){
      
      osm = opq(bbox = getbb("medellin Colombia")) %>%
        add_osm_feature(key=i , value=j) 
      
      osm_sf = osm %>% osmdata_sf()
      generico_j = osm_sf$osm_points %>% select(osm_id) 
      
      
      matriz_distancia<-st_distance(x=medellin,y=generico_j)
      dist_ <- apply(matriz_distancia , 1 , min)
      medellin[,ncol(medellin) + 1]<-dist_
      colnames(medellin)[ncol(medellin)]<-paste0("dist_",j)
      
      avance <- paste0("Terminada feature ", j, ".")
      print(avance)
      
    }
    
    
  }
  else if ( i== "landuse"){
    for (j in landuse){
      
      osm = opq(bbox = getbb("medellin Colombia")) %>%
        add_osm_feature(key=i , value=j) 
      
      osm_sf = osm %>% osmdata_sf()
      generico_j = osm_sf$osm_points %>% select(osm_id) 
      
      
      matriz_distancia<-st_distance(x=medellin,y=generico_j)
      dist_ <- apply(matriz_distancia , 1 , min)
      medellin[,ncol(medellin) + 1]<-dist_
      colnames(medellin)[ncol(medellin)]<-paste0("dist_",j)
      
      avance <- paste0("Terminada feature ", j, ".")
      print(avance)
      
    }      
  }
  else if (i=="leisure"){
    
    for (j in leisure){
      
      osm = opq(bbox = getbb("medellin Colombia")) %>%
        add_osm_feature(key=i , value=j) 
      
      osm_sf = osm %>% osmdata_sf()
      generico_j = osm_sf$osm_points %>% select(osm_id) 
      
      
      matriz_distancia<-st_distance(x=medellin,y=generico_j)
      dist_ <- apply(matriz_distancia , 1 , min)
      medellin[,ncol(medellin) + 1]<-dist_
      colnames(medellin)[ncol(medellin)]<-paste0("dist_",j)
      
      avance <- paste0("Terminada feature ", j, ".")
      print(avance)
      
    }
    
    
  }
  else if (i=="shop"){
    
    for (j in shop){
      
      osm = opq(bbox = getbb("medellin Colombia")) %>%
        add_osm_feature(key=i , value=j) 
      
      osm_sf = osm %>% osmdata_sf()
      generico_j = osm_sf$osm_points %>% select(osm_id) 
      
      
      matriz_distancia<-st_distance(x=medellin,y=generico_j)
      dist_ <- apply(matriz_distancia , 1 , min)
      medellin[,ncol(medellin) + 1]<-dist_
      colnames(medellin)[ncol(medellin)]<-paste0("dist_",j)
      
      avance <- paste0("Terminada feature ", j, ".")
      print(avance)
      
    }
    
    
  }
  else if (i=="nature"){
    
    for (j in nature){
      
      osm = opq(bbox = getbb("medellin Colombia")) %>%
        add_osm_feature(key=i , value=j) 
      
      osm_sf = osm %>% osmdata_sf()
      generico_j = osm_sf$osm_points %>% select(osm_id) 
      
      
      matriz_distancia<-st_distance(x=medellin,y=generico_j)
      dist_ <- apply(matriz_distancia , 1 , min)
      medellin[,ncol(medellin) + 1]<-dist_
      colnames(medellin)[ncol(medellin)]<-paste0("dist_",j)
      
      avance <- paste0("Terminada feature ", j, ".")
      print(avance)
      
    }
    
    
  }
  else if (i=="tourism"){
    
    for (j in tourism){
      
      osm = opq(bbox = getbb("medellin Colombia")) %>%
        add_osm_feature(key=i , value=j) 
      
      osm_sf = osm %>% osmdata_sf()
      generico_j = osm_sf$osm_points %>% select(osm_id) 
      
      
      matriz_distancia<-st_distance(x=medellin,y=generico_j)
      dist_ <- apply(matriz_distancia , 1 , min)
      medellin[,ncol(medellin) + 1]<-dist_
      colnames(medellin)[ncol(medellin)]<-paste0("dist_",j)
      
      avance <- paste0("Terminada feature ", j, ".")
      print(avance)
      
    }
    
    
  }
}


toc()


write.csv(x = medellin, file = "medellin.csv", sep = ",",
          row.names = T, col.names = TRUE)

df <- read.csv("medellin.csv", header = TRUE, sep = ",")






#------------------------------------------------------------------------------
#Pa cali (Bien)

for (i in features){
  
  if (i =="amenity"){
    for (j in amenity){
      
      osm = opq(bbox = getbb("cali colombia")) %>%
        add_osm_feature(key=i , value=j) 
      
      osm_sf = osm %>% osmdata_sf()
      generico_j = osm_sf$osm_points %>% select(osm_id) 
      
      
      matriz_distancia<-st_distance(x=test,y=generico_j)
      dist_ <- apply(matriz_distancia , 1 , min)
      test[,ncol(test) + 1]<-dist_
      colnames(test)[ncol(test)]<-paste0("dist_",j)
      
      avance <- paste0("Terminada feature ", j, ".")
      print(avance)
      
    }
    
    
  }
  else if (i == "building"){
    for (j in building){
      
      osm = opq(bbox = getbb("cali colombia")) %>%
        add_osm_feature(key=i , value=j) 
      
      osm_sf = osm %>% osmdata_sf()
      generico_j = osm_sf$osm_points %>% select(osm_id) 
      
      
      matriz_distancia<-st_distance(x=test,y=generico_j)
      dist_ <- apply(matriz_distancia , 1 , min)
      test[,ncol(test) + 1]<-dist_
      colnames(test)[ncol(test)]<-paste0("dist_",j)
      
      avance <- paste0("Terminada feature ", j, ".")
      print(avance)
      
    }
    
    
  }
  else if ( i== "landuse"){
    for (j in landuse){
      
      osm = opq(bbox = getbb("cali colombia")) %>%
        add_osm_feature(key=i , value=j) 
      
      osm_sf = osm %>% osmdata_sf()
      generico_j = osm_sf$osm_points %>% select(osm_id) 
      
      
      matriz_distancia<-st_distance(x=test,y=generico_j)
      dist_ <- apply(matriz_distancia , 1 , min)
      test[,ncol(test) + 1]<-dist_
      colnames(test)[ncol(test)]<-paste0("dist_",j)
      
      avance <- paste0("Terminada feature ", j, ".")
      print(avance)
      
    }      
  }
  else if (i=="leisure"){
    
    for (j in leisure){
      
      osm = opq(bbox = getbb("cali colombia")) %>%
        add_osm_feature(key=i , value=j) 
      
      osm_sf = osm %>% osmdata_sf()
      generico_j = osm_sf$osm_points %>% select(osm_id) 
      
      
      matriz_distancia<-st_distance(x=test,y=generico_j)
      dist_ <- apply(matriz_distancia , 1 , min)
      test[,ncol(test) + 1]<-dist_
      colnames(test)[ncol(test)]<-paste0("dist_",j)
      
      avance <- paste0("Terminada feature ", j, ".")
      print(avance)
      
    }
    
    
  }
  else if (i=="shop"){
    
    for (j in shop){
      
      osm = opq(bbox = getbb("cali colombia")) %>%
        add_osm_feature(key=i , value=j) 
      
      osm_sf = osm %>% osmdata_sf()
      generico_j = osm_sf$osm_points %>% select(osm_id) 
      
      
      matriz_distancia<-st_distance(x=test,y=generico_j)
      dist_ <- apply(matriz_distancia , 1 , min)
      test[,ncol(test) + 1]<-dist_
      colnames(test)[ncol(test)]<-paste0("dist_",j)
      
      avance <- paste0("Terminada feature ", j, ".")
      print(avance)
      
    }
    
    
  }
  else if (i=="nature"){
    
    for (j in nature){
      
      osm = opq(bbox = getbb("cali colombia")) %>%
        add_osm_feature(key=i , value=j) 
      
      osm_sf = osm %>% osmdata_sf()
      generico_j = osm_sf$osm_points %>% select(osm_id) 
      
      
      matriz_distancia<-st_distance(x=test,y=generico_j)
      dist_ <- apply(matriz_distancia , 1 , min)
      test[,ncol(test) + 1]<-dist_
      colnames(test)[ncol(test)]<-paste0("dist_",j)
      
      avance <- paste0("Terminada feature ", j, ".")
      print(avance)
      
    }
    
    
  }
  else if (i=="tourism"){
    
    for (j in tourism){
      
      osm = opq(bbox = getbb("cali colombia")) %>%
        add_osm_feature(key=i , value=j) 
      
      osm_sf = osm %>% osmdata_sf()
      generico_j = osm_sf$osm_points %>% select(osm_id) 
      
      
      matriz_distancia<-st_distance(x=test,y=generico_j)
      dist_ <- apply(matriz_distancia , 1 , min)
      test[,ncol(test) + 1]<-dist_
      colnames(test)[ncol(test)]<-paste0("dist_",j)
      
      avance <- paste0("Terminada feature ", j, ".")
      print(avance)
      
    }
    
    
  }
}

skim(test)


write.csv(x = test, file = "test1.csv", sep = ",",
          row.names = FALSE, col.names = TRUE)

df1 <- read.csv("test1.csv", header = TRUE, sep = ",")


#------------------------------------------------------------------------------
#Pegar las bases

train<- rbind(bogota,medellin)



#------------------------------------------------------------------------------
#Regex


p1_test <- test
p1_train <- train

skim(train)
head(train$surface_total, n = 16L)

# Reducir a minuscula
train$description<- str_to_lower(string=train$description)

# Quitar caracteres inncesarios

train$description <- gsub("\n", " ", train$description)

### Remodelada

train$description <- gsub("nuevo", "remodelada", train$description)
train$description <- gsub("nueva", "remodelada", train$description)
train$description <- gsub("remodelado", "remodelada", train$description)
train$description <- gsub("estrenar", "remodelada", train$description)

train$remodelada <- str_count(string=train$description , pattern = "remodelada[:blank:]" )  

skim(train$remodelada)

train$remodelada



### Regex baño

#Pasar todo a minuscula

test$description<- str_to_lower(string=test$description)
train$description<- str_to_lower(string=train$description)


#Dropear columna operation_type y operation_id (no dice nada)

train <- subset(train, select=-c(operation_type,property_id,rooms))
test <- subset(test, select=-c(operation_type,property_id,rooms))


#Reemplazo la puntuación con espacios

train$description <- gsub(","," ", train$description)


#Mala escriura de baño (bao / bano)

train$description <- gsub("bao","baño", train$description)
train$description <- gsub("bano","baño", train$description)



  #Baño

#Este me cuenta cada vez que haya una palabra de baño solita
train$nuevos_baño <- str_count(string=train$description , pattern = "baño[:blank:]" )  


#Cuando la palabra es bañoS, este me agarra la palabra que estaba antes

x <- "[:alnum:]+[:blank:]+baños"

train$nuevos_baños <- str_extract(string=train$description, pattern = x) 


train$nuevos_baños <- gsub("baños","",train$nuevos_baños)
    


train$nuevos_baños <- gsub("tres","3",train$nuevos_baños)
train$nuevos_baños <- gsub("cinco","5",train$nuevos_baños)
train$nuevos_baños <- gsub("dos","2",train$nuevos_baños)
train$nuevos_baños <- gsub("doss","2",train$nuevos_baños)
train$nuevos_baños <- gsub("cuatro","4",train$nuevos_baños)
train$nuevos_baños <- gsub("cuastro","4",train$nuevos_baños)
train$nuevos_baños <- gsub("seis","6",train$nuevos_baños)
train$nuevos_baños <- gsub("doas","2",train$nuevos_baños)
train$nuevos_baños <- gsub("balcn4","4",train$nuevos_baños)
train$nuevos_baños <- gsub("comerdor2","2",train$nuevos_baños)
train$nuevos_baños <- gsub("habitaciones4","4",train$nuevos_baños)
train$nuevos_baños <- gsub("federman2","2",train$nuevos_baños)
train$nuevos_baños <- gsub("integral3","3",train$nuevos_baños)
train$nuevos_baños <- gsub("servicio3","3",train$nuevos_baños)
train$nuevos_baños <- gsub("alcobas3","3",train$nuevos_baños)


train$nuevos_baños<-str_extract(train$nuevos_baños,pattern="[:digit:]")
train$nuevos_baños[is.na(train$nuevos_baños)] = 0
train$nuevos_baños <- str_trim(train$nuevos_baños,side = c("both"))

train$nuevos_baños <- as.numeric(train$nuevos_baños)

# Este bloque sirve para imputar cuando bathrooms está vacio con nuevos baños
# La segunda linea pone NA si la imputacion pone un 0
train %>% mutate(bathrooms=coalesce(bathrooms,nuevos_baños))
train %>% mutate(bathrooms,ifelse(bathrooms==0,NA,bathrooms))

# Esto hace lo mismo que arriba, pero si nuevos_Baños no sirve usa nuevos_baño
train %>% mutate(bathrooms=coalesce(bathrooms,nuevos_baño))
train %>% mutate(bathrooms,ifelse(bathrooms==0,NA,bathrooms))
#------------------------------------------------------------------------------

# Baños pa test


#Reemplazo la puntuación con espacios

test$description <- gsub(","," ", test$description)


#Mala escriura de baño (bao / bano)

test$description <- gsub("bao","baño", test$description)
test$description <- gsub("bano","baño", test$description)



#Baño

#Este me cuenta cada vez que haya una palabra de baño solita
test$nuevos_baño <- str_count(string=test$description , pattern = "baño[:blank:]" )  


#Cuando la palabra es bañoS, este me agarra la palabra que estaba antes

x <- "[:alnum:]+[:blank:]+baños"

test$nuevos_baños <- str_extract(string=test$description, pattern = x) 


test$nuevos_baños <- gsub("baños","",test$nuevos_baños)



test$nuevos_baños <- gsub("tres","3",test$nuevos_baños)
test$nuevos_baños <- gsub("cinco","5",test$nuevos_baños)
test$nuevos_baños <- gsub("dos","2",test$nuevos_baños)
test$nuevos_baños <- gsub("doss","2",test$nuevos_baños)
test$nuevos_baños <- gsub("cuatro","4",test$nuevos_baños)
test$nuevos_baños <- gsub("cuastro","4",test$nuevos_baños)
test$nuevos_baños <- gsub("seis","6",test$nuevos_baños)
test$nuevos_baños <- gsub("doas","2",test$nuevos_baños)
test$nuevos_baños <- gsub("balcn4","4",test$nuevos_baños)
test$nuevos_baños <- gsub("comerdor2","2",test$nuevos_baños)
test$nuevos_baños <- gsub("habitaciones4","4",test$nuevos_baños)
test$nuevos_baños <- gsub("federman2","2",test$nuevos_baños)
test$nuevos_baños <- gsub("integral3","3",test$nuevos_baños)
test$nuevos_baños <- gsub("servicio3","3",test$nuevos_baños)
test$nuevos_baños <- gsub("alcobas3","3",test$nuevos_baños)


test$nuevos_baños<-str_extract(test$nuevos_baños,pattern="[:digit:]")
test$nuevos_baños[is.na(test$nuevos_baños)] = 0
test$nuevos_baños <- str_trim(test$nuevos_baños,side = c("both"))

test$nuevos_baños <- as.numeric(test$nuevos_baños)

# Este bloque sirve para imputar cuando bathrooms está vacio con nuevos baños
# La segunda linea pone NA si la imputacion pone un 0
test %>% mutate(bathrooms=coalesce(bathrooms,nuevos_baños))
test %>% mutate(bathrooms,ifelse(bathrooms==0,NA,bathrooms))

# Esto hace lo mismo que arriba, pero si nuevos_Baños no sirve usa nuevos_baño
test %>% mutate(bathrooms=coalesce(bathrooms,nuevos_baño))
test %>% mutate(bathrooms,ifelse(bathrooms==0,NA,bathrooms))

#----------------------------------------------------------------------------
#Área de las viviendas 

x1 <- "[:space:]+[:digit:]+[:space:]+"
x2 <- "[:space:]+[:digit:]+[:punct:]+[:digit:]+[:space:]+"
x3 <- "[:digit:]+[:space:]+"
x4 <- "[:digit:]+"
train$area <- NA


## replace values
x5 <- "terraza + [:space:] + de + [:space:] + [:digit:] + [:space:]+"
x6 <- "terraza + [:space:] +[:alpha:]+ [:space:]+ de + [:space:] + [:digit:] + [:space:]+"

for (i in c("mts","m2","mt2","mts2","metros","cuadrad","mtro","mtr2")){
  train <- train %>% 
    mutate(area = ifelse(is.na(area)==T,str_extract(string=description , pattern=paste0(x1,i)),area),
           area = ifelse(is.na(area)==T,str_extract(string=description , pattern=paste0(x2,i)),area),
           area = ifelse(is.na(area)==T,str_extract(string=description , pattern=paste0(x3,i)),area),
           area = ifelse(is.na(area)==T,str_extract(string=description , pattern=paste0(x4,i)),area),
           )
}
table(is.na(train$area))

for (i in c("mts","m2","mt2","mts2","metros","cuadrad","mtro","mtr2")){
  train <- train %>% 
    mutate( area = ifelse(is.na(str_extract(string=description,pattern=paste0(x5,i)))==T  ,area,NA),
            area = ifelse(is.na(str_extract(string=description,pattern=paste0(x6,i)))==T,area, NA))
} 
table(is.na(train$area)) ## dado que deja muchos NA utilizaremos una metodologÃ­a de vecinos

#------------------------------------------------------------------------------

# Preparación de la base para el modelo


train <- subset(train,select=-c(nuevos_baños,nuevos_baño,geometry,description,title))
test <- subset(test,select=-c(nuevos_baños,nuevos_baño,geometry,description,title))

train$property_type <- as.factor(train$property_type)
train$city          <- as.factor(train$city) 
test$property_type  <- as.factor(test$property_type)


# % de cada ciudad
prop.table(table(train$city))










#------------------------------------------------------------------------------

# XGBoost


#Haré la prueba con una base mucho más pequeña


















