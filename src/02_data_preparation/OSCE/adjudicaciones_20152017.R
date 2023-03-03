
reporte_20152017 <- read_excel("data/01_raw/reporte-adjudicaciones/2. Reporte Adjudicaciones 2015-2017.xlsx")

# Normalize column names
colnames(reporte_20152017) <- tolower(colnames(reporte_20152017))
colnames(reporte_20152017) <- gsub(" ", "_", colnames(reporte_20152017))
colnames(reporte_20152017) <- gsub("/", "_", colnames(reporte_20152017))
colnames(reporte_20152017) <- gsub("á", "a", colnames(reporte_20152017))
colnames(reporte_20152017) <- gsub("é", "e", colnames(reporte_20152017))
colnames(reporte_20152017) <- gsub("í", "i", colnames(reporte_20152017))
colnames(reporte_20152017) <- gsub("ó", "o", colnames(reporte_20152017))
colnames(reporte_20152017) <- gsub("ú", "u", colnames(reporte_20152017))
colnames(reporte_20152017) <- gsub("ñ", "n", colnames(reporte_20152017))
colnames(reporte_20152017) <- gsub("[^[:alnum:]_]", "", colnames(reporte_20152017))

# Normalize categorical variables
reporte_20152017$objeto <- as.factor(reporte_20152017$objeto)
reporte_20152017$moneda_proceso <- as.factor(reporte_20152017$moneda_proceso)
reporte_20152017$consorcio <- as.factor(reporte_20152017$consorcio)
reporte_20152017$estado <- as.factor(reporte_20152017$estado)

# Normalize numeric variables
reporte_20152017$valor_referencial <- as.numeric(reporte_20152017$valor_referencial)
reporte_20152017$valor_referencial_item <- as.numeric(reporte_20152017$valor_referencial_item)
reporte_20152017$valor_adjudicado_item <- as.numeric(reporte_20152017$valor_adjudicado_item)
reporte_20152017$porcentaje_participacion <- as.numeric(reporte_20152017$porcentaje_participacion)
reporte_20152017$numconvoca <- as.integer(reporte_20152017$numconvoca)
reporte_20152017$numitem <- as.integer(reporte_20152017$numitem)
reporte_20152017$cantidad_convocado_item <- as.integer(reporte_20152017$cantidad_convocado_item)
reporte_20152017$cantidad_adjudicado_item <- as.integer(reporte_20152017$cantidad_adjudicado_item)

# Normalize date variables
reporte_20152017$fechapublicacion <- as.POSIXct(reporte_20152017$fechapublicacion, format = "%Y-%m-%d %H:%M:%S")
reporte_20152017$fecha_buena_pro <- as.POSIXct(reporte_20152017$fecha_buena_pro, format = "%Y-%m-%d %H:%M:%S")

# Normalize values of categorical variables

# Categorize NAs in categorical variables
reporte_20152017$estado[is.na(reporte_20152017$estado)] <- "NA"
reporte_20152017$estado <- as.factor(reporte_20152017$estado)
reporte_20152017$numconvoca[is.na(reporte_20152017$numconvoca)] <- "NA"
reporte_20152017$numconvoca <- as.factor(reporte_20152017$numconvoca)
reporte_20152017$codigo_cubso[is.na(reporte_20152017$codigo_cubso)] <- "NA"
reporte_20152017$codigo_cubso <- as.factor(reporte_20152017$codigo_cubso)
reporte_20152017$ruc_miembro_consorcio[is.na(reporte_20152017$ruc_miembro_consorcio)] <- "NA"
reporte_20152017$nombre_miembro_consorcio[is.na(reporte_20152017$nombre_miembro_consorcio)] <- "NA"
reporte_20152017$n_id_consorcio[is.na(reporte_20152017$n_id_consorcio)] <- "NA"

# Check final data types
str(reporte_20152017)
