## JOINING SERIES AND CONTROLS

monthly_indices_df <- read_parquet("data/02_intermediate/OSCE/monthly_indices.parquet")
controles_oci_monthly <- read_parquet("data/02_intermediate/controles_OCI_mensual.parquet")
controles_infogob_monthly <- read_parquet("data/02_intermediate/controles_infogob_mensual.parquet")

semestral_indices_df <- read_parquet("data/02_intermediate/OSCE/semestral_indices.parquet")
controles_oci_semestral <- read_parquet("data/02_intermediate/controles_OCI_semestral.parquet")
controles_infogob_semestral <- read_parquet("data/02_intermediate/controles_infogob_semestral.parquet")

osce_infogob_matching <- read_parquet("data/02_intermediate/osce_infogob_entity_name_matching.parquet")
osce_oci_matching <- read_parquet("data/02_intermediate/osce_oci_entity_name_matching.parquet")


colnames(monthly_indices_df)
colnames(controles_oci_monthly)
colnames(controles_infogob_monthly)
colnames(osce_infogob_matching)
colnames(osce_oci_matching)

