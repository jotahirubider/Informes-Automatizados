pacman::p_load(RODBC, tidyverse, lubridate)

ruta_mdb1 <- "//woody/asan/Servicios/EnfermeriaMedPreventiva/APLICACIONES/BACKUPS-NO-TOCAR/IRAS/IRAS-2022-09-14.mdb"
ruta_mdb2 <- "//woody/asan/Servicios/EnfermeriaMedPreventiva/APLICACIONES/BACKUPS-NO-TOCAR/IRAS/IRAS-2023-01-17.mdb"
ruta_mdb_last <- "//woody/asan/Servicios/EnfermeriaMedPreventiva/APLICACIONES/COMPARTIDO/IRAS/IRAS.mdb"

get_data <- function(ruta_mdb) {
  # CONEXION CON ACCESS FILE
  con <- odbcConnectAccess(ruta_mdb)
  # OBTENER NOMBRE DE TABLAS DE VIGILANCIA
  table_names <- sqlTables(con) %>% 
    select(TABLE_NAME)
  vigilancia_tables <- table_names %>% 
    filter(grepl("Vigilancia",TABLE_NAME)) %>% 
    unlist()
  # CARGAR TABLAS VIGILANCIA EN TABLA UNICA
  vigilancia <- vigilancia_tables %>% 
    # set_names() %>% 
    map(.f = ~ sqlFetch(channel = con,
                        sqtable = .x)) %>%
    reduce(left_join, by="GlobalRecordId")
  # CERRAR CONEXION CON ACCESS
  odbcClose(con); rm(con, table_names)
  return(vigilancia)
}

data1 <- get_data(ruta_mdb1) %>% 
  mutate(plantaInfeccion1=NA, Edad_calculada=NA)
data2 <- get_data(ruta_mdb2)
data_last <- get_data(ruta_mdb_last)

data_total <- bind_rows(
  data_last,
  data2,
  data1
) %>% 
  distinct(GlobalRecordId, .keep_all = T) %>% 
  filter(RECSTATUS==1)


validos <- data_total %>% filter(RECSTATUS==1)
# activos <- validos %>% filter(estadoActual=="ACTIVO") %>% 
#   # PROTECCION CONTRA NOMBRES VACIOS
#   filter(!is.na(nombre))
# rm(vigilancia,validos)
# rm(vigilancia,validos)


# TABLAS MICROBIOLOGIA ----------------------------------------------------

# PIVOT TABLE DE MICROBIOLOGIA
micro <- validos %>% 
  select(GlobalRecordId,
         starts_with("microorganismo"),
         starts_with("fechaMuestra"),
         starts_with("mecanismoMR")) %>% 
  mutate(across(.cols = contains("fecha"),.fns = as.character.POSIXt)) %>% 
  pivot_longer(cols=-c(GlobalRecordId)) %>% # Pivot table by GLobalREcordId
  arrange(GlobalRecordId,str_sub(name,-1)) %>% # Ordenar por orden de microorganismo
  mutate(orderId=str_sub(name,-1),
         name=case_when(
           grepl("Estado",name)                  ~ "estadoAislamiento",
           grepl("MROtro",name)                  ~ "otroMecanismo",
           grepl("mecanismoMR",name)             ~ "mecanismoMR",
           grepl("fechaMuestra",name)            ~ "fechaMuestra",
           grepl("microorganismo",name)          ~ "microorganismo"),
         # # Si value es activo y value de la fila siguiente es NA, poner NA
         # value=if_else(value=="ACTIVO" & is.na(lead(value)),
         #               NA_character_,
         #               value),
         # # Si value de la fila anterior es INACTIVO 
         # # y el orderId de la fila anterior es igual a la actual
         # # y el nombre no es estadoAislamiento
         # # Poner NA sino el mismo valor
         # value=ifelse(lag(value)=="INACTIVO" & lag(orderId)==orderId & name!="estadoAislamiento",
         #              NA_character_,
         #              value),
         # # Idem anterior con 2 filas anteriores
         # value=ifelse(lag(value)=="INACTIVO" & lag(orderId,2)==orderId & name!="estadoAislamiento",
         #              NA_character_,
         #              value),
         # # Idem con 3 filas anteriores
         # value=ifelse(lag(value)=="INACTIVO" & lag(orderId,3)==orderId & name!="estadoAislamiento",
         #              NA_character_,
         #              value),
         # # Si es inactivo, poner NA
         # value=ifelse(value=="INACTIVO",NA_character_,value)
  ) %>%
  # Quitar todos los NA
  filter(!is.na(value))

# PIVOT TABLE DE MICROORGANISMOS ACTIVOS
micro2 <- micro %>% pivot_wider(names_from = name,values_from = value) %>% 
  select(-estadoAislamiento) %>% 
  filter(!is.na(microorganismo)) %>% 
  distinct(GlobalRecordId,microorganismo,mecanismoMR,.keep_all = T) %>% 
  mutate(
    mecanismoMR=case_when(
      mecanismoMR=="R = 3 grupos"              ~ "MR",
      mecanismoMR=="Carbapenemasa"             ~ "p. de carbapenemasa",
      grepl("BLEE",mecanismoMR)                ~ "BLEE",
      startsWith(mecanismoMR, "R meticilina")  ~ "R meticilina",
      mecanismoMR=="AmpC"                      ~ "AmpC"),
    # Correci?n de nombre de Clostridioides difficile
    microorganismo=if_else(microorganismo=="Clostridium difficile",
                           "Clostridioides difficile",
                           microorganismo),
    # Reformateo de nombre de microorganismo
    microorganismo=if_else(
      is.na(mecanismoMR),
      microorganismo,
      paste(microorganismo,mecanismoMR)),
    fechaMuestra=parse_date(fechaMuestra,format = "%Y-%m-%d")) %>% 
  # Remover columnas no necesarios
  select(-orderId)


# TABLAS INFORME ----------------------------------------------------------

# GENERAR TABLAS PARA INFORME POR PLANTA
tablas_informe <- validos %>% 
  select(GlobalRecordId,plantaActual,aNHC,
         camaActual,motivoAislamiento,
         tipoAislamiento,origenInfeccion,
         cvc,cvp,
         cirugia,diabetes,
         inmunodef,ventilacionMecanica,
         neo, sondaUrinaria,
         sexo, fechaNacimiento,
         fechaIngreso1,
         fechaAislamiento, fechaRetirada,
         ) %>% 
  rename(NHC=aNHC) %>% 
  left_join(micro2,by="GlobalRecordId") %>%
  # select(-GlobalRecordId) %>% 
  mutate(tipoAislamiento=str_to_title(tipoAislamiento),
         origenInfeccion=str_to_sentence(origenInfeccion),
         motivoAislamiento=str_to_title(motivoAislamiento),
         edad_al_ingreso=round(as.numeric(
           difftime(fechaIngreso1, fechaNacimiento, units="days")) / 365.25, 
           2),
         dias_aislado=floor(
           as.numeric(difftime(fechaRetirada, fechaAislamiento, units = "days"))
         )) %>% 
  select(-fechaNacimiento)




# MOVIMIENTO DE CAMAS -----------------------------------------------------

camas_mov <- data_total %>%
  select(aSIP, aNHC, GlobalRecordId,
         contains("fechaIngreso"),
         matches("cama\\d"), fechaAlta) %>%
  pivot_longer(cols=contains("fecha"),names_to = "fecha_name",values_to = "fecha_value") %>% 
  pivot_longer(cols=matches("cama\\d"),names_to = "cama_name",values_to = "cama_value") %>% 
  mutate(
    tipo=case_when(
      fecha_name=="fechaIngreso1" ~ "ingreso",
      fecha_name=="fechaAlta" ~ "alta",
      TRUE ~  "movimiento"
      ),
    cama=if_else(str_extract(fecha_name, "\\d")==str_extract(cama_name, "\\d"),
            cama_value,
            NA_character_),
    fecha=if_else(str_extract(fecha_name, "\\d")==str_extract(cama_name, "\\d"),
            fecha_value,
            NA_POSIXct_),
    fecha=if_else(tipo=="alta", fecha_value, fecha),
    cama=if_else(tipo=="alta",NA_character_,cama)
         ) %>% 
  distinct(GlobalRecordId, tipo, fecha, cama, .keep_all = T) %>% 
  select(GlobalRecordId, aSIP, aNHC, tipo, cama, fecha) %>% 
  filter(!is.na(fecha)) %>% 
  fill(cama)


# ESCRIBIR TABLAS ---------------------------------------------------------

setwd("//woody/asan/Servicios/EnfermeriaMedPreventiva/APLICACIONES/COMPARTIDO/RESUMEN_DATOS")
writexl::write_xlsx(x = data_total, path = "Total_IRAS.xlsx")
writexl::write_xlsx(x= tablas_informe, path = "Informe_IRAS.xlsx")
writexl::write_xlsx(x=camas_mov, path="Movimientos.xlsx")


