# Reelección Municipal y Corrupción

Proyecto basado en la propuesta de investigación para el CIES 2022: Reelección municipal y corrupción.
Investigadores: Paula Muñoz, José Incio, Wendy Adrianzen, Kilder Urrutia

## Descripción

En base a la idea de código abierto y ciencia reproducible, hacemos público el código usado para el análisis. El presente repositorio sigue buenas prácticas de reproducibilidad. Estas prácticas incluyen:

- Entorno privado para el proyecto completamente reproducible usando renv y snapshot
- Tratamiento de datos modular usando una estructura extraccion de datos de fuentes -> limpieza y transformacion -> análisis de datos
- Programación literata y reproducible con RMarkdown
- Pruebas de pipelines usando testthat

## Cómo empezar

### Instalando el proyecto

### Escogiendo el tipo de trabajo

El proyecto está compuesto por dos tipos de trabajos:
- Ingeniería de datos, que se encarga de ingestar y transformar las bases de datos en bruto en tablas que serán usadas en análisis
- Análisis de datos, que se encarga de la exploración y el modelamiento de los datos limpios generados por ingeniería de datos

Cada trabajo está compuesto de módulos, y cada módulo de subanálisis

#### Ingenieria de datos
En ingeniería de datos cada módulo corresponde a un paso en el ciclo de vida de generar una base de datos para su análisis. Estos son:
- 01_data_generation: Contiene todos los scripts que implican obetener las bases de datos originales en bruto. Pueden ser llamados a APIs, descarga de links, conexión a bases de datos, etc.
- 02_data_preparation: Contiene todos los sctipts que hilan las bases en bruto a un formato tabular coherente y normalizan los tipos de datos en cada columna.
- 03_feature_engineering: Contienen todos los scripts que crean variables a partir de las tablas coherentes del paso anterior, juntan bases de datos, etc., para ser usados en los análisis exploratorios.
- 04_model_training: Si existe algún modelo estadístico que se use en el análisis, este módulo debe compilar los scripts con las rutinas de entrenamiento "canónincas" que resulten del proceso de exploración de datos. Estos se tomarán como la base verdadera al momento de hacer inferencias y permiten explorar problemas en los modelos y predicciones de forma ordenada.
- 05_inference: Este módulo está separado del módulo de entrenamiento de los modelos. En estos scripts se recoge la forma canónica de utilizar los modelos para generar inferencias y predicciones. Estas rutinas están separadas porque en muchos casos las inferencias de los modelos son productos complejos hechos a partir de las predicciones en bruto de los modelos
- 06_response_construction: Este módulo se reserva solo para los scripts que impliquen publicar los resultados de cualquier módulo anterior en algún servicio externo, como publicar bases de datos en algún servicio en nube, o simplemente subirlos programáticamente a Google Drive, etc. Este módulo en particular se encarga de traducir los resultados de los módulos anteriores a un formato compatible, como pasar de data.frame a JSON para publicar, etc.
- 07_response_uploading: Este módulo se reserva para propiamente subir los resultados del módulo de response_construction a los servicios relevantes.

#### Análisis de datos


#### Escogiendo el subanálisis

Al momento tenemos 4 lineas de ataque:

- *Linea 1:* Cuantifiación de reelecciones y cambios de partidos políticos, bases de datos sugeridas: INFOGOB
- *Linea 2:* Cuantificacion y sintetizacion de indices de corrupcion a partir de datos de inversion. bases de datos sugeridas: SEACE, OSCE, MEF
- *Linea 3:* Cuantificacion y sintetizacion de indices de corrupcion a partir de datos policiales y judiciales. bases de datos sugeridas: Microdatos INEI, Base de datos abierta MINJUS
- *Linea 4:* Sintetizacion de indicadores sociales, variables de control y lineas de base. bases de datos sugeridos: Microdatos INEI, GEODATA, Censos Anuales.

## Descripción

Existe un debate teórico sobre los efectos que la reelección tiene sobre la decisión de los políticos sobre si se involucran o no en actos de corrupción durante su gestión. Algunos autores postulan que la reelección de autoridades incrementa la corrupción (Persson and Tabellini 2000; Alesina and Tabellini 2004; Ferraz y Finan 2005), mientras que otros  indican que cuando las autoridades subnacionales pueden ser reelegidas se encuentra menor corrupción (Melo Pereira and Figueiredo 2009; Ferraz y Finan 2011). 

Se propone evaluar el efecto de la prohibición de la reelección inmediata de autoridades subnacionales (regional y provincial) sobre los incentivos a cometer actos de corrupción administrativa vinculados a las compras públicas. Esto considerando que si bien las municipalidades son las entidades públicas que más denuncias de corrupción reúnen en los últimos años (Procuraduría Anticorrupción 2017), el número de autoridades regionales investigadas y/o condenadas por actos de corrupción en los últimos años es muy alto. 
Cabe destacar el aporte que constituirá la realización de un estudio abordado desde una metodología cuantitativa, pero que se ve como primera etapa para, en una segunda etapa (futura investigación), seleccionar casos a ser estudiados en profundidad luego de conocer el comportamiento general del universo de interés (municipalidades provinciales y gobiernos regionales). Adicionalmente, los resultados de esta investigación no sólo aportarán conocimiento académico, sino que también permitirán retomar el debate sobre la reforma política a nivel subnacional e informar la toma de decisiones de entidades de control/supervisión y rectoras (o con funciones) en materia de prevención de la corrupción subnacional.

