library(tidyverse)
library(readxl)
library(skimr)
library(arrow)

# 2010-2014
df1 <- read_xlsx("data/01_raw/reporte-adjudicaciones/1. Reporte Adjudicaciones 2010-2014.xlsx")

df1 <- df1 %>%
    rename(
        Year = AÑO,
        Entity = ENTIDAD,
        Entity_RUC = RUC,
        Num_Convoca = `NUM CONVOCA`,
        Process = PROCESO,
        Publication_Date = `FECHA PUBLICACION`,
        Quantity_Adjudicated = CANT_ADJUDICADA,
        Reference_Value = `VALOR REFERENCIAL`,
        Currency = MONEDA,
        Buenapro_Date = `FECHA BUENAPRO`,
        Process_Description = `DESCRIPCION PROCESO`,
        Item_Num = `NUM ITEM`,
        Item_Description = `DESCRIPCION ITEM`,
        Quantity_Convocated = CANTIDAD_CONVOCADA,
        Reference_Value_Item = `VALOR REFERENCIAL ITEM`,
        Adjudicated_Value_Item = `VALOR ADJUDICADO ITEM`,
        Winner_RUC = `RUC/CODIGO GANADOR`,
        Is_Consortium = `ES CONSORCIO`,
        Winner = GANADOR,
        Consortium_Member_RUC = `RUC MIEMBRO CONSORCIO`,
        Consortium_Member_Name = `NOMBRE MIEMBRO COSORCIO`,
        Participation_Percentage = `PORCENTAJE PARTICIPACION`,
        Item_State = `ESTADO ITEM`
    )

# 2015-2017
df2 <- read_xlsx("data/01_raw/reporte-adjudicaciones/2. Reporte Adjudicaciones 2015-2017.xlsx")

df2 <- df2 %>%
    rename(
        Year = AÑO,
        Entity = ENTIDAD,
        Entity_RUC = RUC,
        Num_Convoca = NUMCONVOCA,
        Process = PROCESO,
        Publication_Date = FECHAPUBLICACION,
        Quantity_Adjudicated = `CANTIDAD ADJUDICADO ITEM`,
        Reference_Value = `VALOR REFERENCIAL`,
        Currency = `MONEDA PROCESO`,
        Buenapro_Date = `FECHA BUENA PRO`,
        Process_Description = `DESCRIPCION PROCESO`,
        Item_Num = NUMITEM,
        Item_Description = `DESCRIPCION ITEM`,
        Quantity_Convocated = `CANTIDAD CONVOCADO ITEM`,
        Reference_Value_Item = `VALOR REFERENCIAL ITEM`,
        Adjudicated_Value_Item = `VALOR ADJUDICADO ITEM`,
        Winner_RUC = `RUC/Código GANADOR`,
        Is_Consortium = CONSORCIO,
        Winner = GANADOR,
        Consortium_Member_RUC = `RUC MIEMBRO CONSORCIO`,
        Consortium_Member_Name = `NOMBRE MIEMBRO CONSORCIO`,
        Participation_Percentage = `PORCENTAJE PARTICIPACION`,
        Item_State = ESTADO
    )

# 2018-2022
df3 <- read_xlsx("data/01_raw/CONOSCE/CONOSCE_ADJUDICACIONES2018_0.xlsx") %>%
    rename(
        Entity = CODIGOENTIDAD,
        Entity_RUC = ENTIDAD_RUC,
        Num_Convoca = CODIGOCONVOCATORIA,
        Process = PROCESO,
        Item_Num = N_ITEM,
        Item_Description = DESCRIPCION_ITEM,
        Item_State = ESTADO_ITEM,
        Quantity_Adjudicated = CANTIDAD_ADJUDICADO_ITEM,
        Reference_Value_Item = MONTO_REFERENCIAL_ITEM,
        Adjudicated_Value_Item = MONTO_ADJUDICADO_ITEM,
        Currency = MONEDA,
        Winner_RUC = RUC_PROVEEDOR,
        Winner = PROVEEDOR,
        Is_Consortium = TIPO_PROVEEDOR,
        Publication_Date = FECHA_CONVOCATORIA,
        Buenapro_Date = FECHA_BUENAPRO
    )

df4 <- read_xlsx("data/01_raw/CONOSCE/CONOSCE_ADJUDICACIONES2019_0.xlsx") %>%
    rename(
        Entity = CODIGOENTIDAD,
        Entity_RUC = ENTIDAD_RUC,
        Num_Convoca = CODIGOCONVOCATORIA,
        Process = PROCESO,
        Item_Num = N_ITEM,
        Item_Description = DESCRIPCION_ITEM,
        Item_State = ESTADO_ITEM,
        Quantity_Adjudicated = CANTIDAD_ADJUDICADO_ITEM,
        Reference_Value_Item = MONTO_REFERENCIAL_ITEM,
        Adjudicated_Value_Item = MONTO_ADJUDICADO_ITEM,
        Currency = MONEDA,
        Winner_RUC = RUC_PROVEEDOR,
        Winner = PROVEEDOR,
        Is_Consortium = TIPO_PROVEEDOR,
        Publication_Date = FECHA_CONVOCATORIA,
        Buenapro_Date = FECHA_BUENAPRO
    )

df5 <- read_xlsx("data/01_raw/CONOSCE/CONOSCE_ADJUDICACIONES2020_0.xlsx") %>%
    rename(
        Entity = CODIGOENTIDAD,
        Entity_RUC = ENTIDAD_RUC,
        Num_Convoca = CODIGOCONVOCATORIA,
        Process = PROCESO,
        Item_Num = N_ITEM,
        Item_Description = DESCRIPCION_ITEM,
        Item_State = ESTADO_ITEM,
        Quantity_Adjudicated = CANTIDAD_ADJUDICADO_ITEM,
        Reference_Value_Item = MONTO_REFERENCIAL_ITEM,
        Adjudicated_Value_Item = MONTO_ADJUDICADO_ITEM,
        Currency = MONEDA,
        Winner_RUC = RUC_PROVEEDOR,
        Winner = PROVEEDOR,
        Is_Consortium = TIPO_PROVEEDOR,
        Publication_Date = FECHA_CONVOCATORIA,
        Buenapro_Date = FECHA_BUENAPRO
    )

df6 <- read_xlsx("data/01_raw/CONOSCE/CONOSCE_ADJUDICACIONES2021_0.xlsx") %>%
    rename(
        Entity = CODIGOENTIDAD,
        Entity_RUC = ENTIDAD_RUC,
        Num_Convoca = CODIGOCONVOCATORIA,
        Process = PROCESO,
        Item_Num = N_ITEM,
        Item_Description = DESCRIPCION_ITEM,
        Item_State = ESTADO_ITEM,
        Quantity_Adjudicated = CANTIDAD_ADJUDICADO_ITEM,
        Reference_Value_Item = MONTO_REFERENCIAL_ITEM,
        Adjudicated_Value_Item = MONTO_ADJUDICADO_ITEM,
        Currency = MONEDA,
        Winner_RUC = RUC_PROVEEDOR,
        Winner = PROVEEDOR,
        Is_Consortium = TIPO_PROVEEDOR,
        Publication_Date = FECHA_CONVOCATORIA,
        Buenapro_Date = FECHA_BUENAPRO
    )

df7 <- read_xlsx("data/01_raw/CONOSCE/CONOSCE_ADJUDICACIONES2022_0.xlsx") %>%
    rename(
        Entity = CODIGOENTIDAD,
        Entity_RUC = ENTIDAD_RUC,
        Num_Convoca = CODIGOCONVOCATORIA,
        Process = PROCESO,
        Item_Num = N_ITEM,
        Item_Description = DESCRIPCION_ITEM,
        Item_State = ESTADO_ITEM,
        Quantity_Adjudicated = CANTIDAD_ADJUDICADO_ITEM,
        Reference_Value_Item = MONTO_REFERENCIAL_ITEM,
        Adjudicated_Value_Item = MONTO_ADJUDICADO_ITEM,
        Currency = MONEDA,
        Winner_RUC = RUC_PROVEEDOR,
        Winner = PROVEEDOR,
        Is_Consortium = TIPO_PROVEEDOR,
        Publication_Date = FECHA_CONVOCATORIA,
        Buenapro_Date = FECHA_BUENAPRO
    )


# Helper function to convert columns
convert_columns <- function(df, conversions) {
    for (col_name in names(conversions)) {
        if (col_name %in% colnames(df)) {
            df[[col_name]] <- conversions[[col_name]](df[[col_name]])
        }
    }
    df
}

# Column conversions
conversions <- list(
    Num_Convoca = as.character,
    Item_Num = as.character,
    Quantity_Adjudicated = as.numeric,
    Reference_Value = as.numeric,
    Quantity_Convocated = as.numeric,
    Reference_Value_Item = as.numeric,
    Adjudicated_Value_Item = as.numeric,
    Winner_RUC = as.character,
    Consortium_Member_RUC = as.character,
    Participation_Percentage = as.numeric
)

# Apply conversions to each dataset
df1_converted <- convert_columns(df1, conversions)
df2_converted <- convert_columns(df2, conversions)
df3_converted <- convert_columns(df3, conversions)
df4_converted <- convert_columns(df4, conversions)
df5_converted <- convert_columns(df5, conversions)
df6_converted <- convert_columns(df6, conversions)
df7_converted <- convert_columns(df7, conversions)


# Combine datasets
combined_df_adjudicaciones <- bind_rows(df1_converted, df2_converted)#, df3_converted,
                         # df4_converted, df5_converted, df6_converted,
                         # df7_converted) %>%
    # select(
    #     Year, Entity, Entity_RUC, Num_Convoca, Process, Publication_Date,
    #     Quantity_Adjudicated, Reference_Value, Currency, Buenapro_Date,
    #     Process_Description, Item_Num, Item_Description, Quantity_Convocated,
    #     Reference_Value_Item, Adjudicated_Value_Item, Winner_RUC, Is_Consortium,
    #     Winner, Consortium_Member_RUC, Consortium_Member_Name, Participation_Percentage,
    #     Item_State
    # )

# Normalize values:
combined_df_adjudicaciones$Is_Consortium <- ifelse(combined_df_adjudicaciones$Is_Consortium %in% c("S", "N"), 
                                                   combined_df_adjudicaciones$Is_Consortium, 
                                                   ifelse(combined_df_adjudicaciones$Is_Consortium %in% c("Consorcio"), 
                                                          "S", "N"))
combined_df_adjudicaciones$Is_Consortium <- replace_na(combined_df_adjudicaciones$Is_Consortium, "N")


# check
skim_without_charts(combined_df_adjudicaciones)

# Write the combined dataset to a PARQUET file
write_parquet(combined_df_adjudicaciones, "data/02_intermediate/OSCE/merged_adjudicaciones_data.parquet")
