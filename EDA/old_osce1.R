## ADJUDICACIONES DIRECTAS:
ADJUDICACIONES_REGEX <- "(?i)(Adjudicación|ADJUDICACION|Adquisición|ADQUISICION|Contratación|CONTRATACION).*?(Directa|DIRECTA)"

# Perform the first join and add a column to indicate the source of the match
result_1 <- combined_df_contratos %>%
    inner_join(combined_df_adjudicaciones %>% 
                   distinct(ENTIDAD, DESCRIPCION_PROCESO = `DESCRIPCION PROCESO`, OBJETO)) %>%
    mutate(match_source = "DESCRIPCION_PROCESO")

result_1b <- combined_df_contratos %>%
    inner_join(combined_df_invitados %>% 
                   distinct(ENTIDAD = entidad, DESCRIPCION_PROCESO = descripcion_proceso, OBJETO)) %>%
    mutate(match_source = "DESCRIPCION_PROCESO")


# Perform the second join and add a column to indicate the source of the match
result_2 <- combined_df_contratos %>%
    inner_join(combined_df_adjudicaciones %>% 
                   distinct(ENTIDAD, CODIGOCONVOCATORIA = `nombre_proceso`, OBJETO)) %>%
    mutate(match_source = "CODIGOCONVOCATORIA")

# Bind the rows from both results
combined_results <- bind_rows(result_1, result_1b, result_2)

combined_df_adjudicaciones %>% 
    inner_join(combined_df_contratos %>% distinct(nombre_proceso = CODIGOCONVOCATORIA, TIPO_PROCESO))

# Remove duplicates and prefer matches from "CODIGOCONVOCATORIA"
final_result_contratos <- combined_results %>%
    group_by(CODIGO_CONTRATO) %>%
    arrange(match_source) %>%
    slice(1) %>%
    ungroup()


final_result_contratos <- final_result_contratos %>% 
    mutate(
        gobierno = str_extract(ENTIDAD, regex_pattern),
        gobierno = str_remove(gobierno, "SEDE CENTRAL|Sede Central|sede central"),
        mesanho_publicacion = floor_date(FECHA_PUBLICACION_CONTRATO, "month"),
        semester = floor_date(FECHA_PUBLICACION_CONTRATO, unit = "6 month"),
        adjudicacion_directa = str_detect(TIPO_PROCESO, ADJUDICACIONES_REGEX)
    )

porcentaje_adjudicaciones_directas <- final_result_contratos %>% 
    group_by(gobierno, OBJETO, semester) %>% 
    summarise(
        perc_adjudicaciones_directas = mean(replace_na(adjudicacion_directa, 0)),
    )
