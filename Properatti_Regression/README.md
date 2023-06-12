# Modelos de Regresión con datos inmobiliarios (XGBoost, Random Forest y CatBoost)

La inmobiliaria [Properati](https://www.properati.com.ar/data) publica periódicamente información sobre ofertas de propiedades para venta y alquiler. Ud. deberá asesorar a la inmobiliaria a desarrollar un modelo de regresión que permita predecir el precio por metro cuadrado de una propiedad. El dataset corresponde a 2017.

## Objetivos y Tecnica:

- El objetico principal es predecir el precio en dolares de un departamento localizado en CABA
- Efectuar una limpieza del dataset provisto. Particularmente, deberá diseñar estrategias para lidiar con los datos perdidos en ciertas variables.
- Agregar informacion sobre la localizacion de iglesias, estadios, boliches y estaciones de subtes de CABA para determinar si la cercania a dichas localizaciones influye en el precio de la propiedad.
     - **Iglesias**: https://data.buenosaires.gob.ar/dataset/iglesias
     - **Boliches**: https://data.buenosaires.gob.ar/dataset/locales-bailables
     - **Estadios**: https://data.buenosaires.gob.ar/dataset/estadios
     - **Estaciones de subtes**: https://data.buenosaires.gob.ar/dataset/subte-estaciones 
- Localizar en un mapa interactivo dichas ubicaciones.
- Realizar un análisis descriptivo de las principales variables y su relacion con el precio en dolares
- Utilizar 3 modelos (XGBoost, Random Forest Regression y CatBoost)
- Mediante el Framework [Optuna](https://optuna.org/) realizar la busqueda de los mejores hiperparametros
- Analizar mediante Feature Importance cuales son las variables mas importantes y realizar un modelos con ellas.

## Informacion de columnas

El dataset contiene información sobre todas las propiedades georeferenciadas de la base de datos de la empresa. La información de cada propiedad que incluye es la siguiente:

- **Unnamed: 0:** Valores numericos continuos en orden ascendente que numeran cada sample
- **operation:** Operacion. Solo hay ventas (sell)
- **property_type:** El tipo de propiedad house (casa), apartment (Apartamento),  store (negocio) o PH
- **place_name:** Ciudad en la que se cuentra la propiedad. Hay de todo el pais. 
- **place_with_parent_names:** Nombre de la Ciudad/Provincia/Pais
- **country_name:** Nombre del pais
- **state_name:** Nombre de la region donde se encuentra
- **geonames_id:** ID de [GeoNames](https://www.geonames.org/)
- **lat-lon,lat y lon:** Tres columnas con latitud - longitud y mismos valores por separado
- **price:** Precio original de la publicación (No especifica moneda)
- **currency:** Tipo de moneda del aviso. Hay 'USD', 'ARS', 'PEN', 'UYU'
- **price_aprox_local_currency:** Precio del aviso en moneda local (ARS)
- **price_aprox_usd:** Precio aproximado en USD
- **surface_total_in_m2:** Superficie total en m²
- **surface_covered_in_m2:** Superficie cubierta en m²
- **price_usd_per_m2:** Precio en USD/m²
- **price_per_m2:** Precio por m²
- **floor:** Numero de pisos (Si los tiene)
- **rooms:** Numero de cuartos
- **expenses:** Valor de expensas mensuales
- **properati_url:** URL de publicación
- **description:** Descripcion de la publicación
- **title:** Titulo de la publicación
- **image_thumbnail:** URL de una miniatura de la primer foto de la publicación
