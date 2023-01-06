
## CIERRE DE INVERSIONES ##
library(tidyverse)
library(lubridate)
library(skimr)
library(clipr)

cierre_inversiones <- data.table::fread("data/01_raw/CIERRE_INVERSIONES.csv", stringsAsFactors = FALSE)

skim(cierre_inversiones)
