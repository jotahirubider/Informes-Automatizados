# SETUP -------------------------------------------------------------------

# CARGAR CONSTANTES
source("utils/constants.R")

# CARGAR PAQUETES NECESARIOS
pacman::p_load(tidyverse,lubridate,knitr,RODBC,kableExtra,labelled,scales,purrr, tidyquant)

# ruta_mdb <- "//woody/asan/Servicios/EnfermeriaMedPreventiva/APLICACIONES/COMPARTIDO/IRAS/IRAS.mdb"

# CONEXION CON ARCHIVO ACCESS
con <- odbcConnectAccess(ruta_mdb)

# REGISTRO DE HORA DE INFORME EN BASE DE DATOS
datetime_user_reg <- sqlFetch(channel = con,
                              sqtable = "covid_report_reg")

fecha_hora_ultimo_informe <- datetime_user_reg %>% 
  arrange(desc(fecha_hora)) %>% 
  filter(as_date(fecha_hora)!=today()) %>% 
  slice_max(order_by = fecha_hora) %>% 
  pull(fecha_hora)

current_datetime_user <- tibble(.rows = 1) %>% 
  mutate(fecha_hora=now(),usuario=Sys.getenv("USERNAME"))

new_current_datetime_user <- datetime_user_reg %>% 
  # SI YA EXISTE UNA FILA CON LA MISMA FECHA SE QUITA
  filter(as_date(fecha_hora)!=today()) %>%
  bind_rows(current_datetime_user) %>% 
  arrange(desc(fecha_hora))

sqlSave(channel = con,
        dat=new_current_datetime_user,
        tablename =  "covid_report_reg",
        safer=F,
        fast=F,
        rownames = F,
        append = F,
        varTypes = c(
          fecha_hora="datetime"
        )
)

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

# CARGAR TABLAS HOSPITAL
hospital_tables <- table_names %>% 
  filter(grepl("Hospital",TABLE_NAME)) %>% 
  unlist()
n_camas <- hospital_tables %>% 
  set_names() %>% 
  map(.f = ~ sqlFetch(channel = con,
                      sqtable = .x)) %>%
  detect(.f = ~ "nCamas" %in% colnames(.x))

# OBTENER HISTORICO DE COVID
covid_historico <- sqlFetch(channel = con,
                            sqtable = "covid_historico")

rm(table_names)
# LIMPIAR CASOS BORRADOS E INACTIVOS
validos <- vigilancia %>% filter(RECSTATUS==1)
# rm(vigilancia,validos)

# TABLAS MICROBIOLOGIA ----------------------------------------------------

# PIVOT TABLE DE MICROBIOLOGIA
micro <- validos %>% 
  select(GlobalRecordId,
         starts_with("microorganismo"),
         starts_with("fechaMuestra"),
         starts_with("mecanismoMR"),
         starts_with("fechaActivacion"),
         starts_with("fechaInactivacion")
  ) %>% 
  mutate(across(.cols = contains("fechaMuestra"),.fns = as.character.POSIXt),
         across(.cols = matches("fechaActivacion|fechaInactivacion"), 
                .fns = as.character)) %>% 
  pivot_longer(cols=-c(GlobalRecordId)) %>% 
  # Pivot table by GLobalREcordId
  arrange(GlobalRecordId,str_sub(name,-1)) %>% # Ordenar por orden de microorganismo
  mutate(orderId=str_sub(name,-1),
         name=case_when(
           grepl("Estado",name)                  ~ "estadoAislamiento",
           grepl("MROtro",name)                  ~ "otroMecanismo",
           grepl("mecanismoMR",name)             ~ "mecanismoMR",
           grepl("fechaMuestra",name)            ~ "fechaMuestra",
           grepl("fechaActivacion",name)         ~ "fechaActivacion",
           grepl("fechaInactivacion",name)       ~ "fechaInactivacion",
           grepl("microorganismo",name)          ~ "microorganismo")
  ) %>% 
  # Quitar todos los NA
  filter(!is.na(value))

# PIVOT TABLE DE MICROORGANISMOS ACTIVOS
micro2 <- micro %>% pivot_wider(names_from = name,values_from = value) %>% 
  # select(-estadoAislamiento) %>% 
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
tablas_informe <- validos %>% 
  select(aSIP,aNHC,INGRESO,GlobalRecordId,contains("Time"),estadoActual,plantaActual,camaActual,tipoAislamiento,origenInfeccion,
         fechaAislamiento, fechaRetirada, motivoRetirada, fechaAlta, enfermeraActual, ingresoPorCovid) %>% 
  left_join(micro2,by="GlobalRecordId") %>%
  # select(-GlobalRecordId) %>% 
  mutate(tipoAislamiento=str_to_title(tipoAislamiento),
         origenInfeccion=str_to_sentence(origenInfeccion)) %>% 
  filter(microorganismo=="Virus SARS-CoV-2 (COVID-19)") %>% 
  arrange(desc(LastSaveTime)) %>% 
  mutate(
    fechaActivacion=ymd_hms(fechaActivacion),
    fechaInactivacion=ymd_hms(fechaInactivacion),
  )


total_ingresados <- tablas_informe %>% filter(estadoActual=="ACTIVO" & estadoAislamiento=="ACTIVO")


total_criticos <- tablas_informe %>% filter(estadoActual=="ACTIVO"& estadoAislamiento=="ACTIVO",
                                            plantaActual=="UMI" | plantaActual=="REA" |
                                              plantaActual=="UCP" | plantaActual=="UCN" |
                                              plantaActual=="REQ")
###### PONER VALIDACION DE FECHA

casos_nuevos <- tablas_informe %>% 
  filter(fechaActivacion>fecha_hora_ultimo_informe,
         # EN PRUEBA PARA CASOS NUEVOS QUE SON ALTA EL MISMO DIA
         
         # estadoAislamiento=="ACTIVO" & estadoActual=="ACTIVO"
         
         #######################################################
  ) %>% 
  filter(INGRESO=="Primer ingreso") # QUITA LOS REINGRESOS

altas <- tablas_informe %>% 
  filter(fechaInactivacion>fecha_hora_ultimo_informe,
         estadoAislamiento=="INACTIVO",
         motivoRetirada!="EXITUS"
  )

exitus <- tablas_informe %>% 
  filter(fechaInactivacion>fecha_hora_ultimo_informe,
         estadoAislamiento=="INACTIVO",
         motivoRetirada=="EXITUS",
         ingresoPorCovid=="SI"
  )

detalle_exitus <- exitus %>% 
  transmute(SIP=aSIP,
            `Fecha exitus`=fechaInactivacion %>% 
              format(., format="%d-%m-%Y"))

# CREAR TABLA ESTADO DEL DIA DEL INFORME

estado_hoy <- tibble(.rows = 1) %>% 
  mutate(fecha=today(),
         total_ingresados=total_ingresados %>% nrow(),
         total_criticos=total_criticos %>% nrow(),
         casos_nuevos=casos_nuevos %>% nrow(),
         altas=altas %>% nrow(),
         exitus=exitus %>% nrow()) 


covid_historico_new <- covid_historico %>% 
  # SI YA EXISTE UNA FILA CON LA MISMA FECHA SE QUITA
  filter(fecha!=today()) %>% 
  bind_rows(estado_hoy) %>% 
  arrange(fecha)


# COPIA DE SEGURIDAD
sqlSave(channel = con,
        dat=covid_historico,
        tablename = "covid_historico_copia",
        safer=F,
        fast=F,
        rownames = F,
        append = F,
        varTypes = c(fecha="date"))
# GRABAR EN BASE DE DATOS
sqlSave(channel = con,
        dat=covid_historico_new,
        tablename =  "covid_historico",
        safer=F,
        fast=F,
        rownames = F,
        append = F,
        varTypes = c(fecha="date")
)
# BORRAR COPIA DE SEGURIDAD
sqlDrop(con,"covid_historico_copia")
odbcClose(con)


tabla_hoy <- estado_hoy %>% 
  rename(Fecha=fecha,
         `Total ingresados`=total_ingresados,
         `Total críticos`=total_criticos,
         `Casos nuevos`=casos_nuevos,
         Altas=altas,
         Exitus=exitus)

tabla_por_plantas <- total_ingresados %>% 
  group_by(plantaActual) %>%  
  summarise(n=n()) %>% 
  rename(Planta=plantaActual,
         `Nº de ingresados`=n)

# FECHAS DINAMICAS  -------------------------------------------------------------

fecha_max_curva_total <- (covid_historico_new$fecha %>% 
                            max() + days(28)) %>% 
  as.character.Date()

fecha_max_curva_parcial <- (covid_historico_new$fecha %>% 
                              max() + days(7)) %>% 
  as.character.Date()

fecha_min_curva_parcial <- fecha_min_curva_parcial %>% 
  dmy() %>% as.character.Date()
