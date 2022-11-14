# Price-Prediction-With-Spacial-Data
## Motivación
Este proyecto se desarrolla como respuesta al problem set 3 de la clase Big data y Machine Learning. La motivación del trabajo investigativo surge en la necesidad de determinar si es posible desarrollar modelos de predicción para precio del mercado innmobiliario con el reto de evitar sobreajuste en este tipo de datos y evitar que se repitan errores como el ocurrido a la empresa Zillow, errores que resultaron ser muy costosos para la empresa. Por otra parte, predecir acertadamente los precios de las viviendas en el mercado inmobiliario puede ser útil en política pública para prevenir posibles burbujas inmobiliarias y actuar antes de que sea demasiado tarde.

##Estructura del repositorio

El repositorio cuenta con 4 carpetas principales: _Data, Scripts, Views y Document_. En la carpeta _Data_ se encuentran las bases de datos utilizadas para el desarrollo del proyecto. En _Scripts_ están los cuadernos en los cuales se escribió todo el codigo utilizado. En _Views_ se guardaron todas las imagenes y resultados obtenidos. Por ultimo, en la carpeta _Document_ se guardó el archivo final requerido por el problem set.

## ¿Cómo se hizo?

### Tecnología utilizada
El software estadístico utilizado para el pre-procesamiento de las bases de datos y la implementación de los modelos es R versión 4.2.1 

### Datos
Los datos que se utilizaron fueron provistos por el ejercicio. Las bases de datos están separadas por base train y base test. La base train contiene las viviendas por ID, localicación geográfica con latitud y longitud, si la vivienda pertenece a Bogotá o a Medellín y algunos caracterísiticas de la vivienda junto a una respectiva descripción ed la misma. La Base Test es igual a Train en variables, pero únicamente contiene datos de Cali. También en el proceso se saca provecho de ddatos spaciales y se obtienen distancias a diferentes ammenities para cada una de las viviendas en las dos bases de datos. Estas distancias se obtienen por medio de https://www.openstreetmap.org/#map=6/4.632/-74.299 . 


### Metodología
El reto de construir los modelos predictivos comienza desde la carga de las bases de datos, dado que contienen variables _NAN_ que son especialmente conflictivas al momento de hacer las predicciones. Como resultado de esto se plantea la siguiente metodología:

1. Pre-procesamiento

  i) Cargar las bases de datos y tomar la base Train y obtener dos bases sub grupos de estas, una con los datos de Bogotá y otra con los datos de Medellín
  
  ii) Agregar los datos de distacias espaciales de los ammenities y demás lugares que consideramos importante para el precio por medio de un loop para cada base.
  
  iii) Por medio de expresiones regulares, capturar datos en variables que tienen datos incompletos para cada una de las bases. Entre estas variables capturamos número de baños, número de habitaciones, superficie total de la vivienvienda, si la vivienda está remodelada, si es apartamento, si tiene parqueadero, si tiene lavanderia, si tiene terraza, si tiene cocina integral, si tiene ascensor, si tiene vigilancia, si tiene iluminacion, o si tiene piscina.

  iv) Finalmente vovler a unir las bases de train (unir nuevamente los datos de Bogotá y de Medellín).

2. Modelaje
  
  En esta sección se busca probar 5 modelos de clasificación y 5 modelos de predicción del ingreso, que al contrastarlo con la linea de pobreza (_Lp_ en la base de       datos) nos indique si la persona es pobre, y consecuentemente el hogar.

3. Elección del modelo con mejores metricas
  
  Al final, se elegirá el modelo con mejores resultados predictivos sobre la base de evaluación teniendo en cuenta que las clasificaciones como FN tienen un mayor peso   que las clasificaciones de FP. Es por esto que la metrica principal a optimizar es la sensitividad.
  
### Pre-procesamiento
Algunos de las modificaciones más importantes hechas a las bases de datos son: 

i) Se agregaron las bases de hogar y personas para tener dos bases de datos: una para entrenamiento y la otra para prueba.

ii) Se imputaron las observaciones que tenian valores vacios para facilitar la implementación de los modelos predictivos. El algoritmo utilizado fue obtenido de la libreria `mixgb`que facilita el comando con el mismo nombre que ejecuta una estructura de imputación multiple basada en _XGBoost
, boostraping y predictive mean matching_. Para más información sobre la libreria está este [repositorio.](https://github.com/agnesdeng/mixgb/blob/master/README.md)

iii) Por ultimo, se estableció que la aproximación más sencilla frente a las variables categoricas era crear una variable indicadora para los n-1 niveles de cada una. 

## ¿Cómo replicar los resultados?
Para replicar los resultados se sugiere clonar el repositorio entero a una carpeta de su preferencia y correr el __Script__ provisto. No hay necesidad de establecer el directorio dado que se hace automaticamente. Sin embargo, puede que se necesiten instalar algunas librerias a mano.



