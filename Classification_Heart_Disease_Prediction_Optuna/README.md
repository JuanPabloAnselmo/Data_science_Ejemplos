# Predictive Classifier for Heart Disease (Optuna / Logistic Regression, Random Forest y XGboost)

## Problema

 Las enfermedades cardiovasculares (ECV) son la primera causa de muerte en el mundo, y se calcula que cada año se cobran 17,9 millones de vidas, lo que representa el 31% de todas las muertes en el mundo. La insuficiencia cardíaca es una de las causas más frecuentes de ECV. Las personas con enfermedades cardiovasculares o que presentan un alto riesgo cardiovascular (debido a la presencia de uno o más factores de riesgo como hipertensión, diabetes, hiperlipidemia o enfermedad ya establecida) necesitan una detección y gestión tempranas en las que un modelo de aprendizaje automático puede ser de gran ayuda. De este modo, tratamos de resolver de forma automatizada otro problema que se presenta en la naturaleza con vistas a contrarrestarlo y centrarnos en el siguiente problema con la ayuda de las técnicas de IA.

## Descripcion de variables

1. *Age*: Edad del paciente [años]
2. *Sex*: sexo del paciente [M: Male, F: Female]
3. *ChestPainType*: Tipo de dolor torácico [AT: Angina típica, ATA: Angina atípica, PAN: Dolor no Anginoso, ASY: Asintomático].
4. *RestingBP*: tensión arterial en reposo [mm Hg]
5. *Cholesterol*: Colesterol Serico[mm/dl]
6. *FastingBS*: glucemia en ayunas [1: si FastingBS > 120 mg/dl, 0: en caso contrario].
7. *RestingECG*: resultados del electrocardiograma en reposo [Normal: Normal, ST: con anomalía de la onda ST-T (inversiones de la onda T y/o elevación o depresión del ST de > 0,05 mV), HVI: que muestra hipertrofia ventricular izquierda probable o definida según los criterios de Estes].
8. *MaxHR*: Frecuencia cardíaca máxima alcanzada [Valor numérico entre 60 y 202].
9. *ExerciseAngina*: Angina inducida por el ejercicio [S: Sí, N: No].
10. *Oldpeak*: oldpeak = ST [Valor numérico medido en depresión].. Se refiere a la diferencia en altura entre el pico de ejercicio y el reposo en un electrocardiograma durante un estudio de esfuerzo. Valores mayores de oldpeak suelen indicar una mayor gravedad de la enfermedad cardíaca isquémica. Clasificacion: Normal: menor de 0.5, Leve: entre 0.5 y 1.0, Moderado: entre 1.0 y 2.0 ,Severo: mayor de 2.0
11. *ST_Slope*: la pendiente del segmento ST de ejercicio máximo [Up: pendiente ascendente, Flat: plano, Down: pendiente descendente].
12. *HeartDisease*: output class [1: heart disease, 0: Normal]

## Fuente 

Este conjunto de datos se creó combinando diferentes conjuntos de datos que ya estaban disponibles de forma independiente pero que no se habían combinado antes. En este conjunto de datos se combinan 5 conjuntos de datos sobre el corazón con 11 características comunes, lo que lo convierte en el mayor conjunto de datos sobre enfermedades cardiacas disponible hasta la fecha para fines de investigación. Los cinco conjuntos de datos utilizados son:

- Cleveland: 303 observaciones
- Hungarian: 294 observaciones
- Switzerland: 123 observaciones
- Long Beach VA: 200 observaciones
- Stalog (Heart) Data Set: 270 observaciones
 
- Total: 1190 observaciones
- Duplicated: 272 observaciones

Final dataset: 918 observaciones

Todos los conjuntos de datos utilizados se pueden encontrar en el Índice de conjuntos de datos de enfermedades cardíacas del Repositorio de Aprendizaje Automático de la UCI en el siguiente enlace: https://archive.ics.uci.edu/ml/machine-learning-databases/heart-disease/

fedesoriano. (September 2021). Heart Failure Prediction Dataset. Retrieved [Date Retrieved] from https://www.kaggle.com/fedesoriano/heart-failure-prediction.


## Objetivos

- Realizar un EDA general para comprender el comportamiento de las diferentes variables en relacion al Target
- Realizar 3 modelos (Logistic Regression, Random Forest y XGboost) primero con sus hiperparametros Default, luego encontrando sus mejores hiperparametros mediante la utilizacion del framework Optuna y por ultimo filtrando segun sus principales variables.

## Optuna [https://optuna.org/]
Optuna es una biblioteca de Python para la optimización de hiperparámetros de algoritmos de aprendizaje automático. Utiliza un enfoque Bayesiano para buscar los mejores hiperparámetros, lo que significa que utiliza información previa sobre la distribución de los hiperparámetros para guiar la búsqueda. Esto se contrasta con otros enfoques, como la búsqueda exhaustiva o la búsqueda aleatoria, que no utilizan información previa.
Bayesian optimization es un proceso de optimización en el cual se modela la distribución de los puntos de optimización utilizando una probabilidad a priori y se actualiza con los nuevos puntos de optimización obtenidos.
