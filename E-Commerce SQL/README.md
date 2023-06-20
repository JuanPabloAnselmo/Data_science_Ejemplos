# Análisis de datos de comercio electrónico de Brasil

En este proyecto voy a explorar y analizar un conjunto de datos de comercio electrónico de Brasil, obtenido de https://www.kaggle.com/olistbr/brazilian-ecommerce. Este conjunto de datos contiene información sobre más de 100 mil pedidos realizados entre 2016 y 2018 en la plataforma Olist, una empresa que conecta a pequeños negocios con clientes en todo el país.

El objetivo de este análisis es responder a algunas preguntas de interés sobre el comportamiento y las preferencias de los clientes, el desempeño y la satisfacción de los vendedores, y las tendencias y oportunidades del mercado. Para ello, voy a utilizar Python y SQL como herramientas para manipular, consultar y visualizar los datos.

Las preguntas que vamos a responder son las siguientes:

- ¿Cuál es el número de pedidos por año y mes?
- ¿Cuáles son las 10 categorías de producto más vendidas por cantidad y por precio total?
- ¿Cuál es el número de clientes por estado?
- ¿Cuál es el tiempo promedio de entrega (en días) por estado?
- ¿Cómo se relaciona el tiempo promedio de entrega del producto con la fecha del pedido?
- ¿Cuáles son las 5 categorías de producto con mayor y menor tiempo promedio de entrega (en días)?
- ¿Cuáles son los 10 estados con mayor puntuación promedio y el porcentaje de comentarios positivos por parte de los clientes?
- ¿Cuáles son las 10 categorías de producto con mayor puntuación promedio y el porcentaje de comentarios positivos por parte de los clientes?
- ¿Qué tipo de pago es el más utilizado por estado?
- ¿Cuáles son el promedio, el máximo y el mínimo del precio del producto y el promedio del precio de envío ordenados por las 5 categorías con mayor y menor precio de producto?

Para realizar este análisis, vamos a utilizar los siguientes archivos csv que contienen los datos:

- **olist_customers_dataset.csv:** contiene información sobre los clientes, como su identificador único, su ubicación (ciudad y estado) y su código postal.
- **olist_orders_dataset.csv:** contiene información sobre los pedidos, como su identificador único, el identificador del cliente que lo realizó, la fecha y hora en que se realizó, se aprobó, se envió, se entregó y se estimó la entrega, y el estado del pedido (entregado, cancelado, etc.).
- **olist_order_items_dataset.csv:** contiene información sobre los ítems que componen cada pedido, como su identificador único, el identificador del pedido al que pertenecen, el identificador del producto que compraron, el identificador del vendedor que lo vendió, el precio del producto y el precio del flete.
- **olist_order_payments_dataset.csv:** contiene información sobre los pagos realizados por cada pedido, como su identificador único, el identificador del pedido al que pertenecen, el tipo de pago (tarjeta de crédito, boleto, etc.), el número de cuotas y el valor del pago.
- **olist_order_reviews_dataset.csv:** contiene información sobre las reseñas realizadas por los clientes sobre cada pedido, como su identificador único, el identificador del pedido al que pertenecen, la puntuación otorgada por el cliente (de 1 a 5), el comentario escrito por el cliente (si lo hubo), la fecha en que se escribió la reseña y la fecha en que se respondió la reseña (si se respondió).
- **olist_products_dataset.csv:** contiene información sobre los productos vendidos en la plataforma, como su identificador único, su categoría, su peso en gramos, su longitud, altura y ancho en centímetros.
- **olist_sellers_dataset.csv:** contiene información sobre los vendedores que venden en la plataforma, como su identificador único, su ubicación (ciudad y estado) y su código postal.
- **product_category_name_translation.csv:** contiene la traducción al inglés de los nombres de las categorías de producto.

Utilicé Python, SQL, sqlalchemy, sqlite, plotly, seaborn y pandas para realizar el análisis de datos de comercio electrónico de Brasil. Utilicé sqlalchemy y sqlite para crear una conexión con la base de datos y ejecutar las consultas SQL, que me permitieron obtener los datos necesarios para responder a cada preguntas. Luego, utilicé pandas para almacenar los resultados de las consultas en dataframes, que facilitaron el manejo y la manipulación de los datos. Finalmente, utilicé plotly y seaborn para crear gráficos interactivos y atractivos que me permitieron visualizar y comprender mejor los datos.

## Aclaración sobre los gráficos
He creado algunos gráficos con Plotly para visualizar los datos de E-Commerce SQL. Sin embargo, GitHub no renderiza los gráficos interactivos, por lo que solo se ven como imágenes en blanco. Para ver los gráficos correctamente, puedes usar este enlace de nbviewer, que muestra el cuaderno de Jupyter como una página web estática con las características interactivas de Plotly: https://nbviewer.org/github/JuanPabloAnselmo/Data_science_Ejemplos/blob/main/E-Commerce%20SQL/E_Commerce_SQL.ipynb
