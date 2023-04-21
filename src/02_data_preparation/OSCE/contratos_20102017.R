library(foreign)
library(tidyverse)
library(readxl)
library(skimr)
library(clipr)

contratos_20102017 <-  read_xlsx("data/01_raw/reporte-adjudicaciones/3. Reporte Contratos 201-2017.xlsx")

# Normalize column names
contratos_20102017 <- rename_all(contratos_20102017, function(col_name) {
    col_name <- gsub(" ", "_", col_name)
    col_name <- gsub("Ñ", "N", col_name)
    col_name <- gsub("º", "", col_name)
    tolower(col_name)
})

# Give columns sensible data types
contratos_20102017 <- contratos_20102017 %>%
    mutate(ano = as.integer(ano),
           ruc_entidad = as.character(ruc_entidad),
           entidad_convocante = as.character(entidad_convocante),
           entidad_contratante = as.character(entidad_contratante),
           modalidad = as.character(modalidad),
           tipo_proceso = as.character(tipo_proceso),
           proceso = as.character(proceso),
           objeto = as.character(objeto),
           descripcion_proceso = as.character(descripcion_proceso),
           moneda = as.character(moneda),
           descripcion_item = as.character(descripcion_item),
           ruc_codigo_ganador = as.character(ruc_codigo_ganador),
           es_consorcio = as.character(es_consorcio),
           ganador = as.character(ganador),
           ruc_destino_pago = as.character(ruc_destino_pago),
           nombre_destino_pago = as.character(nombre_destino_pago),
           ruc_miembro = as.character(ruc_miembro),
           nombre_miembro = as.character(nombre_miembro),
           codigo_contrato = as.character(codigo_contrato),
           tipo = as.character(tipo),
           n_contrato = as.character(n_contrato),
           valor_referencial = as.numeric(valor_referencial),
           valor_referencial_item = as.numeric(valor_referencial_item),
           mon_cont_item = as.numeric(mon_cont_item),
           mon_cont_total = as.numeric(mon_cont_total),
           fecha_suscripcion_contrato = as.Date(fecha_suscripcion_contrato, format = "%Y-%m-%d"),
           fecha_publicacion_contrato = as.Date(fecha_publicacion_contrato, format = "%Y-%m-%d"),
           fecha_vigencia_inicial = as.Date(fecha_vigencia_inicial, format = "%Y-%m-%d")
    )

