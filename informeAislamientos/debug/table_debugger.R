pacman::p_load(readxl,rmarkdown,tidyverse,lubridate,knitr,RODBC,kableExtra)
# CONFIGURAR RUTAS

ruta_mdb1 <- "//woody/asan/Servicios/EnfermeriaMedPreventiva/APLICACIONES/COMPARTIDO/IRAS/IRAS_ERRONEA.mdb"
ruta_mdb2 <- "//woody/asan/Servicios/EnfermeriaMedPreventiva/APLICACIONES/COMPARTIDO/IRAS/IRAS.mdb"
ruta_programa <- "//woody/asan/Servicios/EnfermeriaMedPreventiva/APLICACIONES/AUTOMATIZACION/informeAislamientos"

con <- odbcConnectAccess(ruta_mdb1)
# OBTENER NOMBRE DE TABLAS DE VIGILANCIA
table_names <- sqlTables(con) %>% 
  select(TABLE_NAME)
vigilancia_tables <- table_names %>% 
  filter(grepl("Vigilancia",TABLE_NAME)) %>% 
  unlist()
# CARGAR TABLAS VIGILANCIA EN TABLA UNICA
# vigilancia <- vigilancia_tables %>% 
#   set_names() %>% 
#   map(.f = ~ sqlFetch(channel = con,
#                       sqtable = .x)) %>%
#   reduce(left_join, by="GlobalRecordId")

Vigilancia <- sqlFetch(channel = con,
                       sqtable = "Vigilancia")
Vigilancia1 <- sqlFetch(channel = con,
                       sqtable = "Vigilancia1")
Vigilancia12 <- sqlFetch(channel = con,
                        sqtable = "Vigilancia12")
Vigilancia13 <- sqlFetch(channel = con,
                        sqtable = "Vigilancia13")

status_1 <- Vigilancia %>% 
  filter(RECSTATUS==1)

limpios <- Vigilancia1 %>% 
  filter(GlobalRecordId %in% status_1$GlobalRecordId) %>% 
  filter(!is.na(nombre))

Vigilancia <- Vigilancia %>% 
  filter(GlobalRecordId %in% limpios$GlobalRecordId)
Vigilancia1 <- Vigilancia1 %>% 
  filter(GlobalRecordId %in% limpios$GlobalRecordId)
Vigilancia12 <- Vigilancia12 %>% 
  filter(GlobalRecordId %in% limpios$GlobalRecordId)
Vigilancia13 <- Vigilancia13 %>% 
  filter(GlobalRecordId %in% limpios$GlobalRecordId)

odbcClose(con)
con <- odbcConnectAccess(ruta_mdb2)

# sheets <- list("Vigilancia"=Vigilancia,
#             "Vigilancia1"=Vigilancia1,
#             "Vigilancia12"=Vigilancia12,
#             "Vigilancia13"=Vigilancia13)

# sqlSave(channel = con,
#         dat=Vigilancia,
#         tablename = "Vigilancia",
#         safer=F,
#         fast=F,
#         rownames = F,
#         append = F)
# 
# sqlSave(channel = con,
#         dat=Vigilancia1,
#         tablename = "Vigilancia1",
#         safer=F,
#         fast=F,
#         rownames = F,
#         append = F)
# sqlSave(channel = con,
#         dat=Vigilancia12,
#         tablename = "Vigilancia12",
#         safer=F,
#         fast=F,
#         rownames = F,
#         append = F)
# sqlSave(channel = con,
#         dat=Vigilancia13,
#         tablename = "Vigilancia13",
#         safer=F,
#         fast=F,
#         rownames = F,
#         append = F)


writexl::write_xlsx(x = sheets, path= "C:/Users/Y7205459X/Desktop/datos_base.xlsx")
odbcClose(con)
