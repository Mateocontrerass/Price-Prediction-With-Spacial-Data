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

install.packages("mltools")
library(mltools)
library(xgboost)
p_load(glue,
       hexbin, # genera un grafico de exagonos
       patchwork,vip, ## plot: 
       ggrepel, ## plot: geom_text_repel
       stringi,tidytext,stopwords, ## text-data
       tidymodels,finetune) 

p_load(mltools, xgboost,
       mixgb, vctrs,
       mlr,
       install = T)

install.packages("mixgb")
install.packages("vctrs")
install.packages("mlr")
#install.packages("mlr3")

#Set seed
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

rm(p1_test,p1_train)



#------------------------------------------------------------------------------
#Carreteras principales 


bogota<- subset(train, city=="Bogotá D.C") 
medellin<- subset(train, city=="Medellín") 

#Bogota

# 1. Trunk

osm = opq(bbox = getbb("Bogota Colombia")) %>%
  add_osm_feature(key="highway" , value="trunk") 
osm_sf = osm %>% osmdata_sf()
carreteras <- osm_sf$osm_lines %>% select(osm_id,name) 

# 2. Primary


osm2 = opq(bbox = getbb("Bogota Colombia")) %>%
  add_osm_feature(key="highway" , value="primary") 

osm_sf2 = osm2 %>% osmdata_sf()
carreteras2 = osm_sf2$osm_lines %>% select(osm_id,name) 

# Pegar las vias trunk + primary
vias_principales<-rbind(carreteras,carreteras2)
matrix_distancias_vias<-st_distance(x=bogota,y=vias_principales)
mat_dis_min_vias <- apply(matrix_distancias_vias , 1 , min)
bogota$dist_via <- mat_dis_min_vias

#Medellin

# 1. Trunk

osm = opq(bbox = getbb("Bogota Colombia")) %>%
  add_osm_feature(key="highway" , value="trunk") 
osm_sf = osm %>% osmdata_sf()
carreteras <- osm_sf$osm_lines %>% select(osm_id,name) 

# 2. Primary


osm2 = opq(bbox = getbb("Bogota Colombia")) %>%
  add_osm_feature(key="highway" , value="primary") 

osm_sf2 = osm2 %>% osmdata_sf()
carreteras2 = osm_sf2$osm_lines %>% select(osm_id,name) 

# Pegar las vias trunk + primary
vias_principales<-rbind(carreteras,carreteras2)
matrix_distancias_vias<-st_distance(x=medellin,y=vias_principales)
mat_dis_min_vias <- apply(matrix_distancias_vias , 1 , min)
medellin$dist_via <- mat_dis_min_vias


# Cali

# 1. Trunk

osm = opq(bbox = getbb("Bogota Colombia")) %>%
  add_osm_feature(key="highway" , value="trunk") 
osm_sf = osm %>% osmdata_sf()
carreteras <- osm_sf$osm_lines %>% select(osm_id,name) 

# 2. Primary


osm2 = opq(bbox = getbb("Bogota Colombia")) %>%
  add_osm_feature(key="highway" , value="primary") 

osm_sf2 = osm2 %>% osmdata_sf()
carreteras2 = osm_sf2$osm_lines %>% select(osm_id,name) 

# Pegar las vias trunk + primary
vias_principales<-rbind(carreteras,carreteras2)
matrix_distancias_vias<-st_distance(x=test,y=vias_principales)
mat_dis_min_vias <- apply(matrix_distancias_vias , 1 , min)
test$dist_via <- mat_dis_min_vias




#------------------------------------------------------------------------------


amenity<- c("cafe","pub","restaurant","college","library","school","university","fuel","atm","bank",
            "clinic","hospital","pharmacy","cinema","nightclub","police","bus_station")

landuse<-c("commercial","education","industrial")

leisure<- c("fitness_centre","playground")

shop <- c("alcohol","coffee","mall","supermarket","jewelry","cosmetics")



features <- c("amenity","landuse","leisure","shop")




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
      write.csv2(x=bogota, file="bogota_f.csv")
      
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
      write.csv2(x=bogota, file="bogota_f.csv")
      
      
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
      write.csv2(x=bogota, file="bogota_f.csv")
      
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
      write.csv2(x=bogota, file="bogota_f.csv")
      
    }
    
    
  }
  
}


toc()


#write.csv(x = bogota, file = "bogota_1.0.csv", sep = ",",
#row.names = T, col.names = TRUE)

#df <- read.csv("bogota_1.0.csv", header = T, sep = ",")



#prueba<-read.csv("bogota_1.0.csv")


#------------------------------------------------------------------------------
#Medellin



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
      write.csv2(x=medellin, file="medellin_f.csv")
      
      
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
      write.csv2(x=medellin, file="medellin_f.csv")
      
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
      write.csv2(x=medellin, file="medellin_f.csv")
      
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
      write.csv2(x=medellin, file="medellin_f.csv")
      
    }
    
    
  }
  
  
}


toc()


write.csv2(x=medellin, file="data/medellin_final.csv")
write.csv2(x=bogota, file="data/bogota_final.csv")


#hola<-read_csv2(file="data/medellin_final.csv")

#------------------------------------------------------------------------------
#Pa cali (Bien)

tic()

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
      write.csv2(x=test, file="cali_f.csv")
      
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
      write.csv2(x=test, file="cali_f.csv")
      
      
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
      write.csv2(x=test, file="cali_f.csv")
      
      
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
      write.csv2(x=test, file="data/cali_final.csv")
      
      
    }
    
    
  }


#------------------------------------------------------------------------------
#Pegar las bases

  
  prueba <- read_csv2(file="data/train_final.csv")
  
  #------------------------------------------------------------------------------
  #Pegar las bases
  
  
  nombres <- names(prueba2)
  colnames(prueba1)<-nombres
  
  colnames(prueba1)
  
  
  train<- rbind(prueba1,prueba2)
  
  rm(bogota,medellin)
  
  write_csv2(x=train,file="data/train_final.csv")
  
  
  train<- read_csv2(file="data/train_final.csv")
  test <- read_csv2(file="data/cali_final.csv")

#------------------------------------------------------------------------------

#Juntando barrios para bogotá

mnz <- st_read("sector_shp/SECTOR.shp")

mnz <- st_transform(mnz, crs=4326)

df_bogota <- st_transform(bogota, crs=4326)




## unir dos conjuntos de datos basados en la geometría

house <- st_join(x=df_bogota , y=mnz)


write.csv2(x = bogota, file = "bogota_loop.csv", sep = ";",
          row.names = F, col.names = TRUE)

x<-read.csv2("bogota_loop.csv", header = T, sep = ";")


## Veamos la intuición primero
new_house <- house[st_buffer(house[100,],200),]
new_mnz <- mnz[new_house,]

leaflet() %>% addTiles() %>%
  addPolygons(data=new_mnz,col="red") %>%
  addCircles(data=new_house)

####
prueba <- subset(train,train$city == 'Bogota')
> prueba <- subset(train,train$city == "Bogotá D.C")
> View(prueba)
> View(p1_train)
> bogota<-select(bogota, -X)
> prueba <- subset(p1_train,train$city == "Bogotá D.C")
> prueba <- select(prueba, c(property_id, lat, lon) )
> View(prueba)
> bogota_j <- merge(x=bogota,y=prueba,by="property_id")
> bogota_sf <- st_as_sf(x = bogota_j, ## datos
                        +                  coords=c("lon","lat"), ## coordenadas
                        +                  crs=4326) ## CRS
sf_use_s2(F)
> house <- st_join(x=df_bogota , y=mnz)
house_bogota <- st_join(x=bogota_sf , y=mnz)
nb_house = poly2nb(pl=new_house_sp , queen=T)
install.packages(spdep)
library(spdep)
nb_house = poly2nb(pl=new_house_sp , queen=T) # opcion reina


## Medellin

df_medellin <- read.csv("data/catastro_repositorio2_gdb.csv", header = T,
                        sep = ",")


#------------------------------------------------------------------------------
#Regex

df<-read_csv2("data/train_final.csv") %>%
  select(-1) %>% mutate(description=str_to_lower(description),
                        description=stri_trans_general(str = description, id = "Latin-ASCII"))


df$description <- gsub("\n", " ", df$description)
df$description <- gsub("<", " ", df$description)
df$description <- gsub(">", " ", df$description)
df$description <- gsub("br", " ", df$description)
df$description <- gsub("&", " ", df$description)
df$description <- gsub("tilde", " ", df$description)
df$description <- gsub("/", " ", df$description)
df$description <- gsub(" n ", " ", df$description)
df$description <- gsub(" av ", " ", df$description)
df$description <- gsub(";", " ", df$description)

df$description <- gsub("mas", " ", df$description)
df$description <- gsub("cuenta", " ", df$description)
df$description <- gsub(" con ", " ", df$description)
df$description <- gsub("amplia", " ", df$description)
df$description <- gsub("excelente", " ", df$description)
df$description <- gsub("exceñemtes", " ", df$description)
df$description <- gsub("principal", " ", df$description)
df$description <- gsub("venta", " ", df$description)
df$description <- gsub("ubicado", " ", df$description)
df$description <- gsub("hermoso", " ", df$description)
df$description <- gsub("calle", " ", df$description)
df$description <- gsub("carrera", " ", df$description)
df$description <- gsub("amplias", " ", df$description)
df$description <- gsub("norte", " ", df$description)
df$description <- gsub("barrio", " ", df$description)
df$description <- gsub("espectacular", " ", df$description)
df$description <- gsub("gran", " ", df$description)
df$description <- gsub("ciudad", " ", df$description)
df$description <- gsub("bogota", " ", df$description)
df$description <- gsub("vendo", " ", df$description)
df$description <- gsub("buena", " ", df$description)
df$description <- gsub("informaion", " ", df$description)
df$description <- gsub("cerca", " ", df$description)

df$description <- gsub("nuevo", "remodelada", df$description)
df$description <- gsub("nueva", "remodelada", df$description)
df$description <- gsub("remodelado", "remodelada", df$description)
df$description <- gsub("estrenar", "remodelada", df$description)
df$description <- gsub("construida", "remodelada", df$description)
df$description <- gsub("re modelada", "remodelada", df$description)

df$description <- gsub("bao","bano", df$description)
df$description <- gsub("baño","bano", df$description)
df$description <- gsub("banos","bano", df$description)
df$description <- gsub("baos","bano", df$description)
df$description <- gsub("anos","bano", df$description)

df$description <- gsub("apto","apartamento", df$description)

df$description <- gsub("balcn","balcon", df$description)
df$description <- gsub("balcones", "balcon", df$description)

df$description <- gsub("independientes", "independiente", df$description)

df$description <- gsub("24", "vigilancia", df$description)
df$description <- gsub("conjunto", "vigilancia", df$description)

df$description <- gsub("alcobas","habitaciones", df$description)
df$description <- gsub("habitacin", "habitaciones", df$description)

df$description <- gsub("parqueaderos","parqueadero", df$description)
df$description <- gsub("garajes","parqueadero", df$description)
df$description <- gsub("garaje","parqueadero", df$description)
df$description <- gsub("parqueo", "parqueadero", df$description)
df$description <- gsub("2parqueadero", "parqueadero", df$description)
df$description <- gsub("y2parqueadero", "parqueadero", df$description)


df$description <- gsub("terrazas", "terraza", df$description)
df$description <- gsub("balcon", "terraza", df$description)

df$description <- gsub("amplio", "amplia", df$description)

df$description <- gsub("mts2","m2", df$description)
df$description <- gsub("cuadrados","m2", df$description)
df$description <- gsub("mt2", "m2", df$description)
df$description <- gsub("metros","m2", df$description)
df$description <- gsub("mts","m2", df$description)

df$description <- gsub("ascensores", "ascensor", df$description)

df$description <- gsub("ropas","lavanderia", df$description)
df$description <- gsub("lavandera","lavanderia", df$description)
df$description <- gsub("lavado", "lavanderia", df$description)

df$description <- gsub("iluminado", "iluminacion", df$description)
df$description <- gsub("luz", "iluminacion", df$description)

df$description <- gsub("saln", "salon", df$description)

df$description <- gsub("tres","3",df$description)
df$description <- gsub("cinco","5",df$description)
df$description <- gsub(" dos ","2",df$description)
df$description <- gsub("doss","2",df$description)
df$description <- gsub("cuatro","4",df$description)
df$description <- gsub("cuastro","4",df$description)
df$description <- gsub("seis","6",df$description)
df$description <- gsub("doas","2",df$description)

df$description <- gsub(" s ", " ", df$description)
df$description <- gsub(" n ", " ", df$description)
df$description <- gsub(" b ", " ", df$description)

bg <- df %>%
  unnest_tokens(output = word, input = description) %>% 
  anti_join(get_stopwords("es"), "word")


bg %>% count(word, sort = TRUE) %>% print(n=100)

top_words <- bg %>%
  count(word, sort = TRUE) %>%
  filter(!word %in% as.character(0:10)) %>%
  slice_max(n, n = 100) %>%
  pull(word)

top_words  

subset(df,property_id=="c2ebfdda5d4614bb8379b3cc")$description

x<- bg[bg$word=="s",]

  words <- c("apartamento", "parqueadero", "lavanderia", "social", "terraza",
             "remodelada", "ascensor", "vigilancia", "iluminacion")


#area de lote de doscientos un m2 ########## revisar esto cuando sacar area
subset(df,property_id=="c2ebfdda5d4614bb8379b3cc")$description

x<- bg[bg$word=="s",]


### Remodelada

train$remodelada <- str_count(string=train$description , pattern = "remodelada[:blank:]" )


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

train<-subset(train,select=-c(nuevos_baño,nuevos_baños))
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

test<-subset(test,select=-c(nuevos_baño,nuevos_baños))


#------------------------------------------------------------------------------

# Preparación de la base para el modelo


train <- subset(train,select=-c(geometry,description,title))
test <- subset(test,select=-c(geometry,description,title))

train <- select(train,-geometry)
test  <- select(test,-geometry)


#Dejar geometria
train <- st_drop_geometry(train)
test <- st_drop_geometry(test)

train$property_type <- as.factor(train$property_type)
train$city          <- as.factor(train$city) 
test$property_type  <- as.factor(test$property_type)
test$city           <- as.factor(test$city) 



# % de cada ciudad
prop.table(table(train$city))

library(rsample)

split<-rsample::initial_split(train,prop=0.2,strata=city)

training_set<-rsample::training(split)
testing_set<-rsample::testing(split)

prop.table(table(training_set$city))

install.packages("data.table")
library(data.table)

setDT(training_set)
setDT(testing_set)


#NA

table(is.na(training_set))
sapply(training_set, function(x) sum(is.na(x))/length(x))*100

table(is.na(testing_set))
sapply(testing_set, function(x) sum(is.na(x))/length(x))*100


#codificacion



new_tr<-one_hot(as.data.table(training_set))
new_ts<-one_hot(as.data.table(testing_set))

new_tr<-as.matrix(new_tr)
new_ts<-as.matrix(new_ts)


output<-training_set$price
output_test<-testing_set$price


dtrain <- xgb.DMatrix(data=new_tr, label=output)
dtest <- xgb.DMatrix(data=new_ts, label=output_test)




#------------------------------------------------------------------------------

# XGBoost


#Haré la prueba con una base mucho más pequeña

#Default parametros

set.seed(999)
params <- list(booster = "gbtree",
               eta=0.3, gamma=0, max_depth=6, min_child_weight=1,
               subsample=1, colsample_bytree=1)

#Pa saber # arboles
xgbcv <- xgb.cv(params = params, data = dtrain , nrounds = 500, nfold = 5,
                showsd = T, stratified = T, print_every_n = 10, early_stop_round = 20,
                maximize = F)


elog <- as.data.frame(xgbcv$evaluation_log)

#arboles recomendados

(nrounds<-which.min(elog$test_rmse_mean))

model <- xgboost(data=dtrain,label=output,
                 nrounds=nrounds,
                 params = params)

pred <- predict(model,dtest)


library(MLmetrics)

RMSE(pred,testing_set$price)

#------------------------------------------------------------------------------

#Ahora tunear parametros

lrn <- makeLearner("regr.xgboost")


df_train<-as.data.frame(new_tr)

colnames(df_train)[1]<-"Bogota"
colnames(df_train)[2]<-"Medellin"


traintask<-makeRegrTask(data=df_train,target="price")


lrn$par.vals <- list(objective="reg:linear",nrounds=100L, eta=0.1 )

params <- makeParamSet()


colnames(df_train) <- make.names(colnames(df_train),unique = T)








