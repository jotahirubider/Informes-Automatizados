# CARGAR CONSTANTES 
source("utils/constants.R")

dia_semana <- today() %>% wday()
if (dia_semana==dia_informe_completo) {
  
  # CURVA NUMERO AISLAMIENTOS TOTALES Y COVID -------------------------------
  
  plt_aisl_totales <- indicadores_resultados%>% 
    arrange(fecha) %>% 
    mutate(epiweek=epiweek(fecha),
           year=year(fecha) %>% as.factor()) %>% 
    ggplot(aes(fecha)) +
    geom_line(aes(y=n_aisl_totales,
                  # color=year,
                  color="red"
    ),
    size=1) +
    geom_point(aes(y=n_aisl_totales)) +
    geom_line(aes(y=ifelse(
      is.na(n_covid),
      pa_covid*9.73,
      n_covid),
      color="steelblue"),
      size=1) +
    geom_point(aes(y=ifelse(
      is.na(n_covid),
      pa_covid*9.73,
      n_covid))) + 
    # facet_grid(. ~ year) +
    theme_bw() +
    # scale_x_continuous(breaks = c(1:52)) +
    xlab("Fecha") +
    ylab("Nº aislamientos") +
    labs(color="") +
    scale_color_discrete(labels=c("Totales","SARS-CoV-2"))+
    scale_x_datetime(date_breaks = "1 month",date_labels = "%b-%y")
  
  
  # CURVA PA GLOBAL, CRITICOS, HOSPITALIZADOS Y COVID -----------------------
  
  plt_pa_global <- indicadores_resultados%>% 
    pivot_longer(cols = c("pa_global","pa_crit","pa_hosp","pa_covid"),
                 names_to = "pa_name",
                 values_to = "pa_value") %>% 
    ggplot(aes(fecha)) +
    geom_line(aes(y=pa_value,
                  color=pa_name),
              size=1) +
    geom_point(aes(y=pa_value,
                   color=pa_name)) +
    theme_bw() +
    ylab("Presión de aislamientos(%)") +
    xlab("Fecha") +
    scale_color_discrete(labels=c("SARS-CoV-2","Críticos","Global","Hospitalización"))+
    labs(color="") +
    scale_x_datetime(date_breaks = "1 month",date_labels = "%b-%y")
  
  
  
  # CURVA PRESION DE AISLAMIENTOS NOSOCOMIALES ------------------------------
  
  plt_pa_nosocomial <- indicadores_resultados %>%
    ggplot(aes(x=fecha)) +
    geom_line(aes(y=pa_aisl_nosocomial),
              color="chartreuse4",
              size=1) +
    geom_point(aes(y=pa_aisl_nosocomial),
               color="black") +
    geom_smooth(aes(y=pa_aisl_nosocomial),
                color="chartreuse2",
                fill="chartreuse2",
                formula = y ~ x,
                method = "loess",
                alpha=0.2,
                size=1,
                se=T) +
    theme_bw() +
    # scale_x_continuous(breaks = c(1:52)) +
    ylab("Presión de aislamientos(%)") +
    xlab("Fecha") +
    scale_x_datetime(date_breaks = "1 month",date_labels = "%b-%y")
  
  
  # CURVA MICROORGANISMO MULTIRRESISTENTES ----------------------------------
  
  
  # Pivot table
  mr_indicadores <- indicadores_resultados %>% 
    arrange(fecha) %>% 
    pivot_longer(cols = c(pa_cauris,pa_carba,pa_acineto,pa_cdif),
                 names_to = "pa_mr_name",
                 values_to = "pa_mr_value")
  mr_names <- c("Acinetobacter baumanii",
                "Prod. de carbapenemasa",
                "Candida auris",
                "Clostridioides difficile")
  names(mr_names) <- mr_indicadores %>% group_by(pa_mr_name) %>% summarise()  %>% 
    unlist() %>% as.vector()
  plt_mmr <- mr_indicadores %>% 
    ggplot(aes(fecha)) +
    geom_smooth(aes(y=pa_mr_value,
                    color=pa_mr_name,
                    fill=pa_mr_name),
                # color="black",
                formula = y ~ x,
                method = "loess",
                size=0.5,
                alpha=0.2,
                se=T) +
    geom_line(aes(y=pa_mr_value,
                  color=pa_mr_name),
              size=1
    ) +
    geom_point(aes(y=pa_mr_value,
                   color=pa_mr_name)) +
    facet_grid(pa_mr_name ~ .,
               labeller = labeller(pa_mr_name=mr_names)  ) +
    theme_bw() +
    xlab("Fecha") +
    ylab("Presión de aislamientos(%)") +
    labs(color="Microorganismo") +
    scale_x_datetime(date_breaks = "1 month",date_labels = "%b-%y") +
    theme(axis.text.x = element_text(angle=90),
          legend.position = "none")

}
  
  
  
