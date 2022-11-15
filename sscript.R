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


p_load(glue,
       stringi,tidytext,stopwords, ## text-data
       tidymodels,finetune) 

p_load(mltools, xgboost,
       mixgb, vctrs,
       mlr, spdep,
       install = T)


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

mnz <- st_read("data/manz_shp_bogota/MANZ.shp")

mnz <- st_transform(mnz, crs=4326)

####
train_f <- read_csv2("data/train_final.csv")
bogota_f <- subset(train_f,train$city == "Bogotá D.C")
prueba <- subset(p1_train,train$city == "Bogotá D.C")
prueba <- select(prueba, c(property_id, lat, lon) )
bogota_f <- merge(x=bogota_f,y=prueba,by="property_id")
bogota_sf <- st_as_sf(x = bogota_f, ## datos,
                      coords=c("lon","lat"), ## coordenadas
                      crs=4326) ## CRS
sf_use_s2(F)
house_bogota <- st_join(x=bogota_sf , y=mnz)

## Veamos la intuición primero
new_house <- house_bogota[st_buffer(house_bogota[100,],200),]
new_mnz <- mnz[new_house,]

leaflet() %>% addTiles() %>%
  addPolygons(data=new_mnz,col="red") %>%
  addCircles(data=new_house)

new_house_sp <- new_house %>% st_buffer(20) %>% as_Spatial()
nb_house = poly2nb(pl=new_house_sp , queen=T) # opcion reina
nb_house[[32]]


#------------------------------------------------------------------------------
####Regex train

train<-read_csv2("data/train_final.csv") %>%
  select(-1, -dist_cafe...16,-dist_cafe...17, -dist_education) %>% mutate(description=str_to_lower(description),
                        description=stri_trans_general(str = description, id = "Latin-ASCII"))


vacio <- c("\n", "<", ">", "br", "&", "tilde", "/", " n ", ",",
           " av ", ";", "\r", "mas", "cuenta", " con ", "amplia",
           "excelente", "excelentes", "principal", "venta", "ubicado",
           "hermoso", "calle", "carrera", "amplias", "norte", "barrio",
           "espectacular", "gran", "ciudad", "bogota", "vendo", "buena",
           "informacion", "cerca", "cerrada", "consta", "social", "amplio",
           "amplia")

remodelada <- c("nuevo", "nueva", "remodelado", "estrenar", "construida",
                "re modelada", "remodeladas")

bano <- c("bao", "banos", "baos", "ano", "anos", "bbano", " ba ")

parqueadero <- c("parqueaderos", "garajes", "garaje", "parqueo", "2parqueadero",
                 "y2parqueadero")

terraza <- c("balcn", "balcones", "terrazas", "balcon")

vigilancia <- c("24", "conjunto", "seguridad")

m2 <- c("mts2", "cuadrados", "mt2", "metros", "mts")

lavanderia <- c("ropas", "lavandera", "lavado")

habitaciones <- c("alcobas", "habitacin", "habitacion", "habitacioneses")

iluminacion <- c("iluminado", "luz")

palabras <- c("vacio", "remodelada", "bano", "parqueadero", "terraza", "vigilancia", "m2",
              "lavanderia", "habitaciones", "iluminacion")


for (i in palabras){
  if (i =="vacio"){
    for (j in vacio){
      
      train$description <- gsub(j, " ", train$description)
      term<-paste0("Terminado ", j)
      print(term)
      
    }
  }
  
  else if (i=="remodelada"){
    for (j in remodelada){
      
      train$description <- gsub(j, "remodelada", train$description)
      term<-paste0("Terminado ", j)
      print(term)
      
    }
  }
  
  else if (i=="bano"){
    for (j in bano){
      
      train$description <- gsub(j, "bano", train$description)
      term<-paste0("Terminado ", j)
      print(term)
      
    }
  }
  
  else if (i=="parqueadero"){
    for (j in parqueadero){
      
      train$description <- gsub(j, "parqueadero", train$description)
      term<-paste0("Terminado ", j)
      print(term)
      
    }
  }
  
  else if (i=="terraza"){
    for (j in terraza){
      
      train$description <- gsub(j, "terraza", train$description)
      term<-paste0("Terminado ", j)
      print(term)
      
    }
  }
  
  else if (i=="vigilancia"){
    for (j in vigilancia){
      
      train$description <- gsub(j, "vigilancia", train$description)
      term<-paste0("Terminado ", j)
      print(term)
      
    }
  }
  
  else if (i=="m2"){
    for (j in m2){
      
      train$description <- gsub(j, "m2", train$description)
      term<-paste0("Terminado ", j)
      print(term)
      
    }
  }
  
  else if (i=="lavanderia"){
    for (j in lavanderia){
      
      train$description <- gsub(j, "lavanderia", train$description)
      term<-paste0("Terminado ", j)
      print(term)
      
    }
  }
  
  else if (i=="habitaciones"){
    for (j in habitaciones){
      
      train$description <- gsub(j, "habitaciones", train$description)
      term<-paste0("Terminado ", j)
      print(term)
      
    }
  }
  
  else if (i=="iluminacion"){
    for (j in iluminacion){
      
      train$description <- gsub(j, "iluminacion", train$description)
      term<-paste0("Terminado ", j)
      print(term)
      
    }
  }
}

train$description <- gsub("apto","apartamento", train$description)
train$description <- gsub("independientes", "independiente", train$description)
train$description <- gsub("ascensores", "ascensor", train$description)
train$description <- gsub("saln", "salon", train$description)
train$description <- gsub("tres","3",train$description)
train$description <- gsub("cinco","5",train$description)
train$description <- gsub(" dos","2",train$description)
train$description <- gsub(" un ", "1",train$description)
train$description <- gsub("doss","2",train$description)
train$description <- gsub("cuatro","4",train$description)
train$description <- gsub("cuastro","4",train$description)
train$description <- gsub("seis","6",train$description)
train$description <- gsub("doas","2",train$description)
train$description <- gsub(" s ", " ", train$description)
train$description <- gsub(" n ", " ", train$description)
train$description <- gsub(" b ", " ", train$description)

  # surface_total (toma valores de terraza tambien)

x1 <- "[:space:]+[:digit:]+[:space:]+"

train <- train %>% 
  mutate(surface_total = ifelse(is.na(surface_total)==T,str_extract(string=description , pattern=paste0(x1,m2)),surface_total))

train$surface_covered <- as.numeric(train$surface_covered)

for (i in 1:51437){
  if (is.na(train$surface_covered[i])==F){
    train$surface_total[i]=train$surface_covered[i]
  }
}

table(is.na(train$surface_total))

  #Baño

#Este me cuenta cada vez que haya una palabra de baño solita
train$bano <- str_count(string=train$description , pattern = "[:blank:]+bano+[:blank:]" )  

x <- "[:alnum:]+bano"

train$nuevos_banos <- str_extract(string=train$description, pattern = x) 
train$nuevos_banos <- gsub("bano","",train$nuevos_banos)

train$hab <- gsub("habitaciones","bano",train$nuevos_banos)

train$nuevos_banos <- as.numeric(train$nuevos_banos)

for (i in 1:51437){
  if (is.na(train$nuevos_banos[i])==F){
    train$bano[i]=train$nuevos_banos[i]
  }
}

for (i in 1:51437){
  if (is.na(train$hab[i]=="bano")==T){
    train$bano[i]=train$rooms[i]
  }
}

for (i in 1:51437){
  if (is.na(train$bathrooms[i])==T){
    train$bathrooms[i]=train$bano[i]
  }
}

for (i in 1:51437){
  if (is.na(train$bathrooms[i])==T){
    train$bathrooms[i]=0
  }
}

table(is.na(train$bathrooms))

train <- subset(train, select = -c(surface_covered, bedrooms, nuevos_banos, hab, bano))

train$surface_total <- as.numeric(train$surface_total)

train$geometry<-0

train <- subset(train, select = -c(geometry))

class(train$surface_total)

### Palabras con más incidencia

bg_train <- train %>%
  unnest_tokens(output = word, input = description) %>% 
  anti_join(get_stopwords("es"), "word")


bg_train %>% count(word, sort = TRUE) %>% print(n=100)

top_words_train <- bg_train %>%
  count(word, sort = TRUE) %>%
  filter(!word %in% as.character(0:10)) %>%
  slice_max(n, n = 100) %>%
  pull(word)

top_words_train

#subset(df,property_id=="d4c69c227b4cc8a3069e7dd3")$description
#x<- bg[bg$word=="ba",]

words <- c("apartamento", "parqueadero", "lavanderia", "terraza", "integral",
           "remodelada", "ascensor", "vigilancia", "iluminacion", "piscina")

words <- glue_collapse(words, sep = "|")


# Partir base train (train y evaluate)

prop.table(table(train$city))

split<-rsample::initial_split(train,prop=0.8,strata=city)

training_set<-rsample::training(split)
evaluating_set<-rsample::testing(split)

prop.table(table(training_set$city))


#Dejar geometria training
training_set <- st_drop_geometry(training_set)

evaluating_set <- st_drop_geometry(evaluating_set)


## recipe para trainning

train_recipe <- recipe(formula=price ~ . , data=training_set) %>% ## En recip se detallan los pasos que se aplicarán a un conjunto de datos para prepararlo para el análisis de datos.
  update_role(property_id, new_role = "property_id") %>% 
  step_regex(description, pattern = words, result="dummy") %>% ## generar dummy
  step_rm(description, operation_type, title) %>%
  step_dummy(city, property_type) %>%
  step_nzv(all_predictors())
train_recipe

rm(bg_train)

#-------------------------------------------------------------------------------

####Regex test

test<-read_csv2("data/cali_final.csv") %>%
  select(-1, -dist_cafe, -dist_education) %>% mutate(description=str_to_lower(description),
                        description=stri_trans_general(str = description, id = "Latin-ASCII"))

for (i in palabras){
  if (i =="vacio"){
    for (j in vacio){
      
      test$description <- gsub(j, " ", test$description)
      term<-paste0("Terminado ", j)
      print(term)
      
    }
  }
  
  else if (i=="remodelada"){
    for (j in remodelada){
      
      test$description <- gsub(j, "remodelada", test$description)
      term<-paste0("Terminado ", j)
      print(term)
      
    }
  }
  
  else if (i=="bano"){
    for (j in bano){
      
      test$description <- gsub(j, "bano", test$description)
      term<-paste0("Terminado ", j)
      print(term)
      
    }
  }
  
  else if (i=="parqueadero"){
    for (j in parqueadero){
      
      test$description <- gsub(j, "parqueadero", test$description)
      term<-paste0("Terminado ", j)
      print(term)
      
    }
  }
  
  else if (i=="terraza"){
    for (j in terraza){
      
      test$description <- gsub(j, "terraza", test$description)
      term<-paste0("Terminado ", j)
      print(term)
      
    }
  }
  
  else if (i=="vigilancia"){
    for (j in vigilancia){
      
      test$description <- gsub(j, "vigilancia", test$description)
      term<-paste0("Terminado ", j)
      print(term)
      
    }
  }
  
  else if (i=="m2"){
    for (j in m2){
      
      test$description <- gsub(j, "m2", test$description)
      term<-paste0("Terminado ", j)
      print(term)
      
    }
  }
  
  else if (i=="lavanderia"){
    for (j in lavanderia){
      
      test$description <- gsub(j, "lavanderia", test$description)
      term<-paste0("Terminado ", j)
      print(term)
      
    }
  }
  
  else if (i=="habitaciones"){
    for (j in habitaciones){
      
      test$description <- gsub(j, "habitaciones", test$description)
      term<-paste0("Terminado ", j)
      print(term)
      
    }
  }
  
  else if (i=="iluminacion"){
    for (j in iluminacion){
      
      test$description <- gsub(j, "iluminacion", test$description)
      term<-paste0("Terminado ", j)
      print(term)
      
    }
  }
}

test$description <- gsub("apto","apartamento", test$description)
test$description <- gsub("independientes", "independiente", test$description)
test$description <- gsub("ascensores", "ascensor", test$description)
test$description <- gsub("saln", "salon", test$description)
test$description <- gsub("tres","3",test$description)
test$description <- gsub("cinco","5",test$description)
test$description <- gsub(" dos","2",test$description)
test$description <- gsub(" un ", "1",test$description)
test$description <- gsub("doss","2",test$description)
test$description <- gsub("cuatro","4",test$description)
test$description <- gsub("cuastro","4",test$description)
test$description <- gsub("seis","6",test$description)
test$description <- gsub("doas","2",test$description)
test$description <- gsub(" s ", " ", test$description)
test$description <- gsub(" n ", " ", test$description)
test$description <- gsub(" b ", " ", test$description)

# surface_total (toma valores de terraza tambien)

test <- test %>% 
  mutate(surface_total = ifelse(is.na(surface_total)==T,str_extract(string=description , pattern=paste0(x1,m2)),surface_total))

test$surface_covered <- as.numeric(test$surface_covered)

for (i in 1:5000){
  if (is.na(test$surface_covered[i])==F){
    test$surface_total[i]=test$surface_covered[i]
  }
}

table(is.na(test$surface_total))

#Baño

#Este me cuenta cada vez que haya una palabra de baño solita

test$bano <- str_count(string=test$description , pattern = "[:blank:]+bano+[:blank:]" )  

x <- "[:alnum:]+bano"

test$nuevos_banos <- str_extract(string=test$description, pattern = x) 
test$nuevos_banos <- gsub("bano","",test$nuevos_banos)

test$hab <- gsub("habitaciones","bano",test$nuevos_banos)

test$nuevos_banos <- as.numeric(test$nuevos_banos)

for (i in 1:5000){
  if (is.na(test$nuevos_banos[i])==F){
    test$bano[i]=test$nuevos_banos[i]
  }
}

for (i in 1:5000){
  if (is.na(test$hab[i]=="bano")==T){
    test$bano[i]=test$rooms[i]
  }
}

for (i in 1:5000){
  if (is.na(test$bathrooms[i])==T){
    test$bathrooms[i]=test$bano[i]
  }
}

for (i in 1:5000){
  if (is.na(test$bathrooms[i])==T){
    test$bathrooms[i]=0
  }
}

table(is.na(test$bathrooms))

test <- subset(test, select = -c(surface_covered, bedrooms, nuevos_banos, hab, bano))

#Dejar geometria
test <- st_drop_geometry(test)

test$geometry<-0

test <- subset(test, select = -c(geometry))

test$surface_total<-(as.numeric(test$surface_total))
class(test$surface_total)


test_recipe <- recipe(formula=price ~ . , data=test) %>% ## En recip se detallan los pasos que se aplicarán a un conjunto de datos para prepararlo para el análisis de datos.
  update_role(property_id, new_role = "property_id") %>% 
  step_regex(description, pattern = words, result="dummy") %>% ## generar dummy
  step_rm(description, operation_type, title) %>%
  step_dummy(city, property_type) %>%
  step_nzv(all_predictors())
test_recipe


#-------------------------------------------------------------------------------

train <- readRDS("train_1.rds")
train_recipe <- readRDS("train_recip.rds")
training_set <- readRDS("training_set.rds")
evaluating_set <- readRDS("evaluating_set.rds")
test <- readRDS("test_1.rds")
xgb_word_rs <- readRDS("data/xgb_word_rs.rsd")


# XGBoost

## set n-folds
set.seed(234)
db_folds <- vfold_cv(data=training_set, v=5 , strata=price)
db_folds

## set metrics
db_metrics <- metric_set(yardstick::rmse, yardstick::rsq, ccc) ## para categoricas, la última es la función de perdida. RMSE para regresion

## Boosted Tree Model Specification
xgb_spec <- boost_tree(trees = 1000,
                       tree_depth = tune(),
                       min_n = tune(),
                       mtry = tune(),
                       sample_size = tune(),
                       learn_rate = tune()) %>%
  set_engine("xgboost") %>%
  set_mode("regression") ## para regresión cambiar classification por regress
xgb_spec

## workflow
xgb_word_wf <- workflow(train_recipe, xgb_spec)

## tunne hiperparametros
xgb_grid <- grid_max_entropy(tree_depth(c(5L, 10L)),
                             min_n(c(10L, 40L)),
                             mtry(c(5L, 10L)), 
                             sample_prop(c(0.5, 1.0)), 
                             learn_rate(c(-2, -1)),
                             size = 20)
xgb_grid

## estimate model
tic()
xgb_word_rs <- tune_race_anova(object = xgb_word_wf,
                               resamples = db_folds,
                               grid = xgb_grid,
                               metrics = db_metrics,
                               control = control_race(verbose_elim = T))
toc()

saveRDS(xgb_word_rs, file = "data/xgb_word_rs.rds")

##=== **3. Desempeño del modelo** ===##

## plot model
plot_race(xgb_word_rs)

## best model
show_best(xgb_word_rs)

## xgboost model
xgb_last <- xgb_word_wf %>%
  finalize_workflow(select_best(xgb_word_rs, "rmse")) %>%
  last_fit(split)
xgb_last

## min log loss
collect_predictions(xgb_last) 

%>%
  mn_log_loss(price, `.pred_17.8`:`.pred_3000`)


## predecir :)

xgb_word_wf_test <- workflow(test_recipe, xgb_spec)

xgb_last_test <- xgb_word_wf_test %>%
  finalize_workflow(select_best(xgb_word_rs, "rmse")) %>%
  last_fit(split)
xgb_last

predicciones <- predict(xgb_last, test)


test_prediction <- xgb_last %>%
  predict(new_data = test)


## predictons vs truht value
predictions <- collect_predictions(xgb_last) %>%
  conf_mat(price, .pred_class)
predictions

predictions %>%
  autoplot() + theme_light()

## ROC curve
collect_predictions(xgb_last) %>%
  roc_curve(price, `.pred_17.8`:`.pred_3000`) %>%
  ggplot(aes(1 - specificity, sensitivity, color = .level)) +
  geom_abline(lty = 2, color = "gray80", size = 1.5) +
  geom_path(alpha = 0.8, size = 1.2) +
  coord_equal() + theme_light()

## Var importance
extract_workflow(xgb_last) %>%
  extract_fit_parsnip() %>%
  vip(geom = "point", num_features = 15) + theme_light()



test_prediction <- xgb_last %>%
  # fit the model on all the training data
  fit(
    formula = price ~ ., 
    data    = train_processed
  ) %>%
  # use the training model fit to predict the test data
  predict(new_data = test_processed) %>%
  bind_cols(testing(ames_split))










