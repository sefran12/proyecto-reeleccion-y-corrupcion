library(foreign)
library(tidyverse)
library(readxl)
library(skimr)

reporte_20102014 <- read_excel("data/01_raw/reporte-adjudicaciones/1. Reporte Adjudicaciones 2010-2014.xlsx")

# Normalize column names
colnames(reporte_20102014) <- tolower(colnames(reporte_20102014))
colnames(reporte_20102014) <- gsub(" ", "_", colnames(reporte_20102014))
colnames(reporte_20102014) <- gsub("/", "_", colnames(reporte_20102014))
colnames(reporte_20102014) <- gsub("á", "a", colnames(reporte_20102014))
colnames(reporte_20102014) <- gsub("é", "e", colnames(reporte_20102014))
colnames(reporte_20102014) <- gsub("í", "i", colnames(reporte_20102014))
colnames(reporte_20102014) <- gsub("ó", "o", colnames(reporte_20102014))
colnames(reporte_20102014) <- gsub("ú", "u", colnames(reporte_20102014))
colnames(reporte_20102014) <- gsub("ñ", "n", colnames(reporte_20102014))

# Give columns a sensible data type
reporte_20102014$ano <- as.integer(reporte_20102014$ano)
reporte_20102014$num_convocada <- as.integer(reporte_20102014$num_convocada)
reporte_20102014$num_item <- as.integer(reporte_20102014$num_item)
reporte_20102014$cantidad_convocada <- as.integer(reporte_20102014$cantidad_convocada)
reporte_20102014$cant_adjudicada <- as.integer(reporte_20102014$cant_adjudicada)
reporte_20102014$fecha_publicacion <- as.POSIXct(reporte_20102014$fecha_publicacion, format = "%Y-%m-%d %H:%M:%S")
reporte_20102014$fecha_buenapro <- as.POSIXct(reporte_20102014$fecha_buenapro, format = "%Y-%m-%d %H:%M:%S", na.rm = TRUE)
reporte_20102014$porcentaje_participacion <- as.numeric(reporte_20102014$porcentaje_participacion)
reporte_20102014$valor_referencial <- as.numeric(reporte_20102014$valor_referencial)
reporte_20102014$valor_referencial_item <- as.numeric(reporte_20102014$valor_referencial_item)
reporte_20102014$valor_adjudicado_item <- as.numeric(reporte_20102014$valor_adjudicado_item)

# Categorize NAs in categorical variables
reporte_20102014$descripcion_item[is.na(reporte_20102014$descripcion_item)] <- "NA"
reporte_20102014$ganador[is.na(reporte_20102014$ganador)] <- "NA"
reporte_20102014$ruc_miembro_consorcio[is.na(reporte_20102014$ruc_miembro_consorcio)] <- "NA"
reporte_20102014$nombre_miembro_cosorcio[is.na(reporte_20102014$nombre_miembro_cosorcio)] <- "NA"
reporte_20102014$estado_item[is.na(reporte_20102014$estado_item)] <- "NA"
