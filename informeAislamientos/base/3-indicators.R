# CARGAR CONSTANTES 
source("utils/constants.R")

dia_semana <- today() %>% wday()
if (dia_semana==dia_informe_completo) {
  
  # CALCULO DE INDICADORES --------------------------------------------------
  
  # CALCULO DE INDICADOR DEL CORTE ACTUAL
  indicadores_resultado_actual <- tibble(.rows = 1) %>% 
    mutate(fecha=today(),
           # n_aisl_totales
           n_aisl_totales=tablas_informe %>% 
             distinct(camaActual) %>% 
             nrow(),
           # n_aisl_hosp
           n_aisl_hosp=tablas_informe %>% 
             left_join(n_camas %>% select(-nCamas), 
                       by="plantaActual") %>% 
             filter(areaHospital=="HOSP") %>% 
             distinct(camaActual) %>% 
             nrow(),
           # n_aisl_criticos
           n_aisl_crit=tablas_informe %>% 
             left_join(n_camas %>% select(-nCamas), 
                       by="plantaActual") %>% 
             filter(areaHospital=="CRITICOS") %>% 
             distinct(camaActual) %>% 
             nrow(),
           # n_aisl_nosocomial
           n_aisl_nosocomial=tablas_informe %>% 
             group_by(camaActual) %>% 
             summarise(origenInfeccion,.groups = "keep") %>% ungroup() %>% 
             filter(origenInfeccion=="Nosocomial") %>% 
             nrow(),
           # total_camas
           n_camas %>% 
             summarise(total_camas=sum(nCamas, na.rm = T)),
           # total_camas_hosp
           n_camas %>% 
             filter(areaHospital=="HOSP") %>% 
             summarise(total_camas_hosp=sum(nCamas, na.rm = T)),
           # total_camas_crit
           n_camas %>% 
             filter(areaHospital=="CRITICOS") %>% 
             summarise(total_camas_crit=sum(nCamas, na.rm = T)),
           # n_virus_resp_total
           n_virus_resp_total=tablas_informe %>% 
             filter(grepl("Gotas",tipoAislamiento) & grepl("Virus",microorganismo)) %>% 
             nrow(),
           # n_virus_resp_total
           n_virus_resp=tablas_informe %>% 
             filter(grepl("Gotas",tipoAislamiento) & grepl("Virus",microorganismo)) %>%
             filter(!grepl("SARS-CoV-2",microorganismo)) %>% 
             nrow(),
           # n_covid
           n_covid=tablas_informe %>% 
             filter(grepl("SARS-CoV-2",microorganismo)) %>% 
             nrow(),
           # n_carba=
           n_carba=tablas_informe %>% 
             filter(grepl("carbapenemasa",tolower(microorganismo))) %>% 
             nrow(),
           # n_cauris
           n_cauris=tablas_informe %>% 
             filter(grepl("Candida auris",microorganismo)) %>% 
             nrow(),
           # n_cdif
           n_cdif=tablas_informe %>% 
             filter(grepl("difficile",microorganismo)) %>% 
             nrow(),
           # n_acineto
           n_acineto=tablas_informe %>% 
             filter(grepl("Acineto",microorganismo)) %>% 
             nrow(),
           # pa_global
           pa_global=n_aisl_totales / total_camas,
           # pa_crit
           pa_crit=n_aisl_crit / total_camas_crit,
           # pa_hosp
           pa_hosp=n_aisl_hosp / total_camas_hosp,
           # pa_acineto
           pa_acineto= n_acineto / total_camas,
           # pa_aisl_nosocomial
           pa_aisl_nosocomial=n_aisl_nosocomial / total_camas,
           # pa_carba
           pa_carba=n_carba / total_camas,
           # pa_cauris
           pa_cauris=n_cauris/total_camas,
           # pa_cdif
           pa_cdif=n_cdif/total_camas,
           # pa_covid
           pa_covid=n_covid / total_camas,
           # pa_virus_resp
           pa_virus_resp=n_virus_resp / total_camas,
           # razon_covid_resp
           razon_covid_resp=n_covid / n_virus_resp,
           # ta_aisl_nosocomial
           ta_aisl_nosocomial=n_aisl_nosocomial / n_aisl_totales,
           # ta_virus_resp
           ta_virus_resp=n_virus_resp / n_aisl_totales,
           # MULTIPLICAR POR 100 VALORES QUE SON PORCENTAJES
           across(.cols = c(starts_with("ta"),starts_with("pa")),.fns = function(x) x*100)
           ) %>% 
    select(everything())
  
  # AGREGAR FILA ACTUAL A BASE DE DATOS
  indicadores_resultados_towrite <- bind_rows(
    # SI YA EXISTE UNA FILA CON LA MISMA FECHA SE QUITA
    indicadores_resultados %>% 
      filter(fecha!=today()),
    indicadores_resultado_actual
  ) %>% 
    arrange(desc(fecha))
  
  


  # CREAR CONEXION CON ACCESS
  
  
  con <- odbcConnectAccess(ruta_mdb)
  # COPIA DE SEGURIDAD
  sqlSave(channel = con,
          dat=indicadores_resultados,
          tablename = "indicadores_resultados_copia",
          safer=F,
          fast=F,
          rownames = F,
          append = F,
          varTypes = c(fecha="date"))
  # GRABAR EN BASE DE DATOS
  sqlSave(channel = con,
          dat=indicadores_resultados_towrite,
          tablename =  "indicadores_resultados",
          safer=F,
          fast=F,
          rownames = F,
          append = F,
          varTypes = c(fecha="date")
  )
  # BORRAR COPIA DE SEGURIDAD
  sqlDrop(con,"indicadores_resultados_copia")
  odbcClose(con)
  # ACTUALIZAR DATAFRAME
  indicadores_resultados <- indicadores_resultados_towrite
  rm(indicadores_resultados_towrite)
  
  # -------------------------------------------------------------------------
  
  # CREAR TABLA DE INDICADORES
  indicadores_tabla <- indicadores_resultado_actual %>% 
    select(-fecha) %>% 
    pivot_longer(cols=everything(),names_to = "id_indicador",values_to = "resultado") %>% 
    inner_join(indicadores_variables,by="id_indicador") %>% 
    arrange(orderId) %>% 
    mutate(
      resultado=case_when(
        grepl("cauris|carba|cdif|acineto",id_indicador) ~ format_number(resultado,"percent",3),
        str_starts(id_indicador,"n_") ~ format_number(resultado),
        str_starts(id_indicador,"pa|ta") ~ format_number(resultado,"percent",2),
        str_starts(id_indicador,"razon") ~ format_number(resultado,"double",2)),
    ) %>% 
    select(indicador,calculo,resultado) %>% 
    # PASA TABLA A FORMATO KABLE
    kable(#format="latex",
      col.names = c("Indicador","Cálculo","Resultado"),
      linesep="\n\n\n\n") %>% 
    row_spec(0,bold=TRUE) %>% 
    kable_styling(latex_options = c("scale_down","hold_position"))
  rm(indicadores_resultado_actual)
}


