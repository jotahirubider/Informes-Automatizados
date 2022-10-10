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
                  origenInfeccion, 
                  microorganismo, fechaMuestra
                  ) %>% 
           kable(x = .,
                 col.names = c(
                   "Cama","NHC","Clín.",
                   "Orig.",
                   "Microorganismo","Fecha 1º(+)"),
                 caption = sprintf("\\textbf{%s}",.y)) %>% 
           row_spec(0,bold=TRUE) %>%
           column_spec(column = c(1,2,5), width_min = "10cm") %>% 
           column_spec(column = c(1:5),border_right=T) %>% 
           collapse_rows(columns = c(1,2,4,5),
                         valign = "middle") %>% 
           kable_styling(latex_options = c("hold_position","striped"),
                         full_width=T))

 
  # IMPRIMIR TABLAS
  if (print_tb) {  
    tables <- map(
      .x = plantas_toprint,
      .f = ~ .x
    )
    tables %>% unlist() %>% paste0(collapse = "\n") %>% cat()
  }
}

