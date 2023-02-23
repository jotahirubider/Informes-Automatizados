# SETUP -------------------------------------------------------------------

# CARGAR PAQUETES NECESARIOS
pacman::p_load(tidyverse,lubridate,knitr,RODBC,kableExtra,labelled,scales,purrr)

# CARGAR CONSTANTES 
source("utils/constants.R")

# CARGAR FUNCIONES UTILES
source("utils/funs.R")



# CARGA DE BBDD ----------------------------------------------------------

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
  set_names() %>% 
  map(.f = ~ sqlFetch(channel = con,
                      sqtable = .x)) %>%
  reduce(left_join, by="GlobalRecordId")
# CARGAR VARIABLES Y RESULTADOS DE INDICADORES Y NUMERO DE CAMAS
if ("indicadores_resultados" %in% unlist(table_names)) {
  indicadores_resultados <- sqlFetch(con, "indicadores_resultados") %>% 
    select(everything())
} else (print("No existe la tabla indicadores_resultados en el archivo .mdb"))
if ("indicadores_variables" %in% unlist(table_names)) {
  indicadores_variables <- sqlFetch(con, "indicadores_variables") %>%
    mutate(calculo=gsub("/ ","/\n",calculo),
           orderId=1:length(calculo))
} else (print("No existe la tabla indicadores_variables en el archivo .mdb"))
# CARGAR TABLAS HOSPITAL
hospital_tables <- table_names %>% 
  filter(grepl("Hospital",TABLE_NAME)) %>% 
  unlist()
n_camas <- hospital_tables %>% 
  set_names() %>% 
  map(.f = ~ sqlFetch(channel = con,
                      sqtable = .x)) %>%
  detect(.f = ~ "nCamas" %in% colnames(.x))

# CERRAR CONEXION CON ACCESS
odbcClose(con); rm(con, table_names)
# LIMPIAR CASOS BORRADOS E INACTIVOS
validos <- vigilancia %>% filter(RECSTATUS==1) %>% 
  # PROTECCION CONTRA NOMBRES VACIONES
  filter(!is.na(nombre))
activos <- validos %>% filter(estadoActual=="ACTIVO")
rm(vigilancia,validos)


# TABLAS MICROBIOLOGIA ----------------------------------------------------

# PIVOT TABLE DE MICROBIOLOGIA
micro <- activos %>% 
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
         # Si value es activo y value de la fila siguiente es NA, poner NA
         value=if_else(value=="ACTIVO" & is.na(lead(value)),
                       NA_character_,
                       value),
         # Si value de la fila anterior es INACTIVO 
         # y el orderId de la fila anterior es igual a la actual
         # y el nombre no es estadoAislamiento
         # Poner NA sino el mismo valor
         value=ifelse(lag(value)=="INACTIVO" & lag(orderId)==orderId & name!="estadoAislamiento",
                      NA_character_,
                      value),
         # Idem anterior con 2 filas anteriores
         value=ifelse(lag(value)=="INACTIVO" & lag(orderId,2)==orderId & name!="estadoAislamiento",
                      NA_character_,
                      value),
         # Idem con 3 filas anteriores
         value=ifelse(lag(value)=="INACTIVO" & lag(orderId,3)==orderId & name!="estadoAislamiento",
                      NA_character_,
                      value),
         # Si es inactivo, poner NA
         value=ifelse(value=="INACTIVO",NA_character_,value)
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
      startsWith(mecanismoMR, "R meticilina")  ~ "R meticilina"),
    # Correción de nombre de Clostridioides difficile
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
  select(-orderId,-mecanismoMR)


# TABLAS INFORME ----------------------------------------------------------

# GENERAR TABLAS PARA INFORME POR PLANTA
tablas_informe <- activos %>% 
  select(GlobalRecordId,plantaActual,aNHC,
         camaActual,motivoAislamiento,
         tipoAislamiento,origenInfeccion) %>% 
  rename(NHC=aNHC) %>% 
  left_join(micro2,by="GlobalRecordId") %>%
  select(-GlobalRecordId) %>% 
  mutate(tipoAislamiento=str_to_title(tipoAislamiento),
         origenInfeccion=str_to_sentence(origenInfeccion),
         motivoAislamiento=str_to_title(motivoAislamiento)) %>% 
  mutate(
    origenInfeccion=case_when(
      origenInfeccion=="Comunitaria" ~ "C",
      origenInfeccion=="Nosocomial"  ~ "N"
    ),
    motivoAislamiento=case_when(
      motivoAislamiento=="Infeccion"     ~  "I",
      motivoAislamiento=="Colonizacion"  ~  "C"
    ))

try(expr = rm(activos,micro,micro2),silent=TRUE)
