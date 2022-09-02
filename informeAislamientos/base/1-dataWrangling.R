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
validos <- vigilancia %>% filter(RECSTATUS==1)
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
         motivoAislamiento=str_to_title(motivoAislamiento))


# GENERAR MAPA DE CAMAS ---------------------------------------------------

tabla_mapa_camas <- activos %>% 
  select(GlobalRecordId,plantaActual,camaActual,motivoAislamiento,
         aNHC) %>%
  rename(NHC=aNHC) %>% 
  left_join(micro2,by="GlobalRecordId") %>%
  select(-GlobalRecordId) %>% 
  mutate(motivoAislamiento=str_to_title(motivoAislamiento)) %>%
  filter(str_detect(microorganismo,"Virus|virus", negate = TRUE)) %>% 
  filter(
    str_detect(microorganismo,
               pattern = "BLEE|MR|carbapenemasa|vancomicina|difficile") |
      str_ends(microorganismo,
               pattern = "R meticilina") |
      str_detect(microorganismo,
                 pattern = "Candida auris") |
      str_detect(microorganismo,
                 pattern = "Serratia marcescens"))

total_pacientes <- tabla_mapa_camas %>% 
  distinct(NHC, .keep_all = T) %>% 
  group_by(plantaActual) %>% 
  summarise(n=n()) %>% 
  arrange(plantaActual) %>% 
  select(n) %>% 
  rename(`Nº ptes. aislados`=n)

total_por_unidad <- tabla_mapa_camas %>% 
  select(plantaActual,microorganismo) %>% 
  mutate(
    microorganismo=case_when(
      microorganismo=="Candida auris"           ~ "C. auris",
      str_ends(microorganismo,"MR")             ~ "MMR",
      str_ends(microorganismo, "carbapenemasa") ~ "EPC",
      str_ends(microorganismo, "BLEE")          ~ "BLEE",
      str_ends(microorganismo, "R meticilina")  ~ "R meticilina",
      str_ends(microorganismo, "difficile")     ~ "C. difficile",
      str_detect(microorganismo, "Serratia marcescens") ~ "Otros"
    )
  ) %>% 
  # CALCULAR TOTALES A LA DERECHA
  group_by(plantaActual,microorganismo) %>% 
  summarise(n=n(), .groups = "keep") %>% 
  pivot_wider(names_from = microorganismo, values_from = n) %>% 
  rowwise() %>% 
  mutate(`Total por Unidad`= sum(across(c(1:7)), na.rm = T)) %>% 
  ungroup() %>% 
  arrange(plantaActual) %>% 
  bind_cols(total_pacientes)

total_por_mmr <- total_por_unidad %>% summarise(
  across(.cols = -c(plantaActual),.fns = ~sum(... = .,na.rm = T))) %>% 
  mutate(plantaActual="Total por MMR") %>% 
  relocate(plantaActual) %>% 
  arrange(plantaActual)

tabla_cruzada <- bind_rows(total_por_unidad,total_por_mmr) %>% 
  rename(Planta=plantaActual) %>% 
  relocate(`Nº ptes. aislados`)

try(expr = rm(activos,micro,micro2,total_pacientes,
              total_por_mmr,total_por_unidad),silent=TRUE)
