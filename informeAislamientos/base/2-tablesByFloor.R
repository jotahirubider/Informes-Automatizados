# TABLAS POR PLANTA #


# LISTADOS POR PLANTAS ACTIVAS --------------------------------------------
plantas <- tablas_informe %>% 
  group_by(plantaActual) %>% 
  summarise(.groups = "keep") %>%
  ungroup()
plantas_criticos <- n_camas %>% 
  filter(areaHospital=="CRITICOS",
         plantaActual%in%plantas$plantaActual) %>%
  select(plantaActual) %>% 
  arrange(plantaActual) %>%
  unlist()
plantas_hosp <- n_camas %>% 
  filter(areaHospital=="HOSP",
         plantaActual%in%plantas$plantaActual) %>%
  select(plantaActual) %>% 
  arrange(plantaActual) %>% 
  unlist()
rm(plantas)

plantas_print <- function(listado_plantas,print_tb=TRUE) {
  options(knitr.kable.NA = "")
  # TIDY DATA EN LISTADOS DE PLANTAS
  plantas_data <- listado_plantas %>% 
    set_names() %>% 
    map(.f = ~ 
          tablas_informe %>% 
          filter(plantaActual==.x) %>% 
          
          arrange(camaActual) %>% 
          mutate(fechaMuestra=strftime(
            fechaMuestra, format="%d-%m-%Y")
          ))
  # GENERAR TABLAS KABLE DE CADA PLANTA PARA IMPRIMIR
  plantas_toprint <- plantas_data %>% 
    map2(.x = .,
         .y= names(.),
         .f = ~ .x %>% 
           select(camaActual, NHC, motivoAislamiento,
                  tipoAislamiento, origenInfeccion, 
                  microorganismo, fechaMuestra
                  ) %>% 
           kable(x = .,
                 col.names = c(
                   "Cama","NHC","Clínica",
                   "Tipo aisl.","Origen",
                   "Microorganismo","Fecha 1º(+)"),
                 caption = sprintf("\\textbf{%s}",.y)) %>% 
           row_spec(0,bold=TRUE) %>% 
           kable_styling(latex_options = c("hold_position")) %>%
           collapse_rows(columns = c(1,2,4,5)))
  # CALCULO NUMERO DE AISLADOS POR PLANTA
  n_aislados <- plantas_data %>% 
    map(.f = ~ .x %>% 
          distinct(camaActual) %>% nrow()
    )
  # CAMAS DISPONIBLES POR PLANTA
  n_camas_planta <- plantas_data %>% 
    map(.x=names(.),
        .f = ~ 
          tibble("plantaActual"=.x) %>% 
          left_join(n_camas,by="plantaActual") %>% 
          select(nCamas) %>% 
          unlist())
  # CALCULO PORCENTAJE CAMAS AISLADAS POR PLANTA
  p_camas_aisladas <- plantas_data %>% 
    map2(
      .x = n_aislados,
      .y = n_camas_planta,
      .f = ~ paste0(sprintf("%2.2f",.x/.y*100),"%"))
  rm(n_aislados)
  # CALCULO PORCENTAJE NOSOCOMIALES
  p_nosocomial <- plantas_data %>% 
    map2(.x=.,
         .y=names(.),
         .f = ~ .x %>% 
           filter(plantaActual==.y) %>% 
           select(-plantaActual) %>% 
           arrange(camaActual) %>% 
           mutate(
             #
             tipoAislamiento=if_else(
               lag(camaActual)==camaActual & row_number(camaActual)!=1 &
                 lag(tipoAislamiento)==tipoAislamiento & row_number(tipoAislamiento) != 1,
               NA_character_,
               tipoAislamiento),
             #
             origenInfeccion=if_else(
               lag(camaActual)==camaActual & row_number(camaActual)!=1 &
                 lag(origenInfeccion)==origenInfeccion & row_number(origenInfeccion) != 1,
               NA_character_,
               origenInfeccion),
             #
             camaActual=if_else(
               lag(camaActual)==camaActual & row_number(camaActual)!=1,
               NA_character_,
               camaActual)
           )) %>% 
    map2(.x=.,
         .y=.,
         .f = ~ paste0(
           sprintf("%2.2f",
                   .x %>% 
                     filter(origenInfeccion=="Nosocomial") %>% nrow() /
                     .y %>% filter(!is.na(camaActual)) %>% nrow() *100),
           "%"))
  # TABLAS PORCENTAJES
  tablas_porcentaje <- pmap(.l = list(n_camas_planta,
                                      p_camas_aisladas,
                                      p_nosocomial),
                            .f = ~   tibble(
                              "Nº Camas"=..1,
                              "% Camas aisladas"=..2,
                              "% Nosocomiales"=..3)) %>% 
    map(
      .f = ~ .x %>%  
        kable() %>%
        kable_styling(latex_options = "hold_position"))

  # IMPRIMIR TABLAS
  if (print_tb) {  
    tables <- map2(
      .x = plantas_toprint,
      .y = tablas_porcentaje,
      .f = ~ list(.x,.y)
      # .f = ~ list(.x[1] %>% as.character(),.y[1] %>% class() )
    )
    tables %>% unlist() %>% paste0(collapse = "\n") %>% cat()
  }
}

