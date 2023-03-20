# Análisis de Sentimientos de reseñas de productos con RoBERTa en Español

## RoBERTa

El transformador RoBERTa es un modelo de lenguaje natural pre-entrenado basado en la arquitectura de los transformers. Fue desarrollado por Facebook AI Research (FAIR) y es una versión optimizada del modelo BERT de Google, con una arquitectura más grande y mejoras en el proceso de entrenamiento.

RoBERTa utiliza un enfoque de pre-entrenamiento no supervisado para aprender representaciones de alta calidad del lenguaje natural. Esto se logra a través de la tarea de "llenado de huecos" en el corpus de entrenamiento, donde el modelo debe predecir la palabra faltante en una oración dada.

Una vez que el modelo ha sido pre-entrenado, se puede utilizar para una variedad de tareas de procesamiento de lenguaje natural, como la clasificación de sentimientos, la generación de texto y la traducción de idiomas. RoBERTa ha demostrado un rendimiento sobresaliente en muchas de estas tareas y es uno de los modelos de lenguaje natural pre-entrenados más utilizados en la actualidad.

[Paper](https://arxiv.org/abs/1907.11692)

## Objetivo

- El objetivo de este proyecto es clasificar una opinión de un producto en POSITIVA o NEGATIVA mediante el uso de técnicas de procesamiento de lenguaje natural y Deep Learning.

## Técnica y Métodos

- Usar notebook en Google Colab para la utilización de su GPU.

- Para el procesamiento del texto y el entrenamiento del modelo, se utilizó la librería transformers de [Hugging Face](https://huggingface.co/), eligiendo el modelo [xlm-roberta-base](https://huggingface.co/xlm-roberta-base) para realizar el procesamiento del texto y agregando dos capas neuronales para mejorar el rendimiento. El modelo fue entrenado con lenguaje español utilizando el dataset de [Amazon](https://huggingface.co/datasets/amazon_reviews_multi), que contiene varias reviews de productos.

- La métrica utilizada para evaluar el modelo fue Accuracy, ya que la distribución de los ejemplos era equilibrada. A pesar de haber sido entrenado con el dataset de Amazon, se probó con opiniones de otras empresas como Mercado Libre y Ebay con grandes rendimientos.

# Interfaz grafica 
Se ha creado una interfaz gráfica del modelo que se puede encontrar en el siguiente enlace --> https://huggingface.co/spaces/JuanPabloAnselmo/Analisis_Sentimientos_RoBERTa

# Archivos

- **Análisis_de_sentimientos_Amazon_RoBERTa_Español.ipynb:** Notebook del entrenamiento del modelo con todas sus clases y procesamientos del dataset.
- **Modelo_Amazon_review.pt:** Modelo ya entrenado.
- **app.py:** Archivo donde se encuentra la implementación de la interfaz gráfica con la librería Gradio.
- **requirements.txt:** Librerías necesarias para que funcione la interfaz gráfica del modelo.

