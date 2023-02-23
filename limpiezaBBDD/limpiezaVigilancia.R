# CARGAR PAQUETES NECESARIOS
pacman::p_load(writexl,tcltk,tidyverse,lubridate,knitr,RODBC,kableExtra,labelled,scales,purrr, tidyquant)

ruta_mdb <- "//woody/asan/Servicios/EnfermeriaMedPreventiva/APLICACIONES/COMPARTIDO/IRAS/IRAS.mdb"

#####
##### CUIDADO!!!!!!

# ANTES DE EJECUTAR HACER BACKUP
# DESCOMENTAR EL CODIGO QUE CONTIENE LOS SQL SAVES

# CONEXION CON ARCHIVO ACCESS
con <- odbcConnectAccess(ruta_mdb)

table_names <- sqlTables(con) %>% 
  select(TABLE_NAME)
vigilancia_tables <- table_names %>% 
  filter(grepl("Vigilancia",TABLE_NAME)) %>% 
  unlist()
# CARGAR TABLAS VIGILANCIA EN TABLA UNICA
vigilancia <- vigilancia_tables %>% 
  set_names() %>% 
  map(.f = ~ sqlFetch(channel = con,
                      sqtable = .x)) %>%
  reduce(left_join, by="GlobalRecordId")

inactivos <- vigilancia %>% 
  filter(estadoActual=="INACTIVO" | is.na(aNHC)) 

inactivos$LastSaveTime %>% max()


#  A BORRAR ANTES DEL MES DESIGNADO
a_borrar <- inactivos %>% 
  filter(LastSaveTime<as_date("2023-01-01"))

rm(vigilancia,inactivos,table_names)

Vigilancia <- sqlFetch(channel = con,
                       sqtable = "Vigilancia") %>% 
  filter(!GlobalRecordId%in%a_borrar$GlobalRecordId)

Vigilancia1 <- sqlFetch(channel = con,
                       sqtable = "Vigilancia1")%>% 
  filter(!GlobalRecordId%in%a_borrar$GlobalRecordId) %>% 
  filter(GlobalRecordId%in%Vigilancia$GlobalRecordId)

Vigilancia12 <- sqlFetch(channel = con,
                       sqtable = "Vigilancia12")%>% 
  filter(!GlobalRecordId%in%a_borrar$GlobalRecordId) %>% 
  filter(GlobalRecordId%in%Vigilancia$GlobalRecordId)

Vigilancia13 <- sqlFetch(channel = con,
                       sqtable = "Vigilancia13") %>% 
  filter(!GlobalRecordId%in%a_borrar$GlobalRecordId) %>% 
  filter(GlobalRecordId%in%Vigilancia$GlobalRecordId)

# GRABAR EN BASE DE DATOS !!! CUIDADO ANTES DE EJECUTAR
# 
# sqlSave(channel = con,
#         dat=Vigilancia,
#         tablename = "Vigilancia",
#         safer=F,
#         fast=F,
#         rownames = F,
#         append = F)
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
odbcClose(con)
