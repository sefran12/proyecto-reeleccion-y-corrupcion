# Reelección Municipal y Corrupción

Proyecto basado en la propuesta de investigación para el CIES 2022: Reelección municipal y corrupción.
Investigadores: Paula Muñoz, Wendy Adrianzen, Kilder Urrutia

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

Cada trabajo está compuesto de módulos:

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

## Descripción

Existe un debate teórico sobre los efectos que la reelección tiene sobre la decisión de los políticos sobre si se involucran o no en actos de corrupción durante su gestión. Algunos autores postulan que la reelección de autoridades incrementa la corrupción (Persson and Tabellini 2000; Alesina and Tabellini 2004; Ferraz y Finan 2005), mientras que otros  indican que cuando las autoridades subnacionales pueden ser reelegidas se encuentra menor corrupción (Melo Pereira and Figueiredo 2009; Ferraz y Finan 2011). 

Proponemos evaluar estas tesis utilizando una metodología mixta que aproveche una particularidad del caso peruano. Luego de una serie de escándalos de corrupción a nivel subnacional, en marzo del 2015 el Congreso prohibió la reelección inmediata de alcaldes y gobernadores regionales como una medida para frenar la corrupción. Proponemos, primero, evaluar si se produjo un cambio estructural antes (2010-marzo 2015) y después de la prohibición de la reelección (abril 2015-diciembre 2020), en un panel de datos de indicadores múltiples de riesgos de corrupción construido con base en los datos de contrataciones provinciales, controlando por la variabilidad pre-tratamiento en la evolución de estos indicadores a través de una técnica de matching. Segundo, utilizaremos los resultados de este análisis cuantitativo para seleccionar dos casos representativos de las provincias con mayores y menores cambios, para realizar trabajo de campo cualitativo (entrevistas a profundidad con informantes claves) que profundice sobre los mecanismos que conectan los incentivos electores y la disposición de involucrarse (o no) en actos de corrupción, así como para informar recomendaciones de política.
