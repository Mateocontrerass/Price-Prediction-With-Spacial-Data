# Price-Prediction-With-Spacial-Data
## Motivación
Este proyecto se desarrolla como respuesta al problem set 3 de la clase Big data y Machine Learning. La motivación del trabajo investigativo surge en la necesidad de determinar si es posible desarrollar modelos de predicción para precio del mercado innmobiliario con el reto de evitar sobreajuste en este tipo de datos y evitar que se repitan errores como el ocurrido a la empresa Zillow, errores que resultaron ser muy costosos para la empresa. Por otra parte, predecir acertadamente los precios de las viviendas en el mercado inmobiliario puede ser útil en política pública para prevenir posibles burbujas inmobiliarias y actuar antes de que sea demasiado tarde.

##Estructura del repositorio

El repositorio cuenta con 4 carpetas principales: _Data, Scripts, Views y Document_. En la carpeta _Data_ se encuentran las bases de datos utilizadas para el desarrollo del proyecto. En _Scripts_ están los cuadernos en los cuales se escribió todo el codigo utilizado. En _Views_ se guardaron todas las imagenes y resultados obtenidos. Por ultimo, en la carpeta _Document_ se guardó el archivo final requerido por el problem set.

## ¿Cómo se hizo?

### Tecnología utilizada
El software estadístico utilizado para el pre-procesamiento de las bases de datos y la implementación de los modelos es R versión 4.2.1 

### Datos
Los datos que se utilizaron fueron provistos por el ejercicio. Las bases de datos están separadas por base train y base test. La base train contiene las viviendas por ID, localicación geográfica con latitud y longitud, si la vivienda pertenece a Bogotá o a Medellín y algunos caracterísiticas de la vivienda junto a una respectiva descripción ed la misma. La Base Test es igual a Train en variables, pero únicamente contiene datos de Cali.
