# CURVA TOTAL -------------------------------------------------------------

curva_ingresados_total <- covid_historico_new %>% 
  pivot_longer(cols = c(total_ingresados,total_criticos),
               names_to = "name",
               values_to = "value") %>% 
  ggplot(aes(x=fecha,
             y=value,
             color=name)) +
  geom_line(size=1) +
  theme_bw() +
  xlab("Fecha")+
  ylab("Nº") +
  labs(color="") +
  scale_color_discrete(name="",labels=c("Total críticos","Total ingresados")) +
  scale_x_datetime(breaks="30 days", # BUG al poner 1 month
                   labels=date_format(format = "%m-%y"),
                   expand = c(0,0)) +
  theme(axis.text.x = element_text(angle=90),
        legend.position = "bottom",
        plot.title = element_text(hjust = 0.5)) +
  labs(title = "Período completo")

curva_total <- covid_historico_new %>%
  select(fecha,casos_nuevos,exitus) %>%
  pivot_longer(cols = c(casos_nuevos,exitus),
               names_to = "name",
               values_to = "value") %>%
  mutate(fecha=as.Date.POSIXct(fecha)) %>%
  ggplot(aes(x=fecha,
             y=value,
             color=name)) +
  geom_ma(ma_fun = SMA, n=7,size=1,linetype="solid")+
  theme_bw() +
  xlab("Fecha")+
  ylab("Nº") +
  labs(color="") +
  scale_x_date(breaks="30 days",
               labels=date_format(format = "%m-%y"),
               expand = c(-0.030,0)) +
  scale_color_manual(values = c("#23993f","#f54242"),
                     labels=c("Casos nuevos","Exitus")) +
  theme(axis.text.x = element_text(angle=90),
        legend.position = "bottom",
        plot.title = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 0.5)) +
  labs(title = "Período completo") +
  labs(caption = "Media móvil 7 días") +
  coord_x_date(xlim=c(fecha_inicio_pandemia,fecha_max_curva_total))

# CURVA PARCIAL -----------------------------------------------------------

curva_ingresados_parcial <- covid_historico_new %>%
  filter(fecha>=as_date(fecha_min_curva_parcial)) %>%
  pivot_longer(cols = c(total_ingresados,total_criticos),
               names_to = "name",
               values_to = "value") %>%
  ggplot(aes(x=fecha,
             y=value,
             color=name)) +
  geom_line(size=1) +
  theme_bw() +
  xlab("Fecha")+
  ylab("Nº") +
  labs(color="") +
  scale_color_discrete(name="",labels=c("Total críticos","Total ingresados")) +
  scale_x_datetime(breaks="30 days",
                   labels=date_format(format = "%m-%y"),
                   expand = c(0,0)) +
  labs(title = "Período Marzo-Julio 2022")+
  theme(axis.text.x = element_text(angle=90),
        legend.position = "bottom",
        plot.title = element_text(hjust = 0.5))

# casos nuevos y exitus

curva_parcial <- covid_historico_new %>%
  filter(fecha>=as_date(fecha_min_curva_parcial)) %>%
  select(fecha,casos_nuevos,exitus) %>%
  pivot_longer(cols = c(casos_nuevos,exitus),
               names_to = "name",
               values_to = "value") %>%
  mutate(fecha=as.Date.POSIXct(fecha)) %>%
  ggplot(aes(x=fecha,
             y=value,
             color=name)) +
  geom_ma(ma_fun = SMA, n=7,size=1,linetype="solid")+
  theme_bw() +
  xlab("Fecha")+
  ylab("Nº") +
  labs(color="") +
  scale_x_date(breaks="30 days",
               labels=date_format(format = "%m-%y"),
               expand = c(-0.045,0)) +
  scale_color_manual(values = c("#23993f","#f54242"),
                     labels=c("Casos nuevos","Exitus")) +
  theme(axis.text.x = element_text(angle=90),
        legend.position = "bottom",
        plot.title = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 0.5)) +
  labs(title = "Período Marzo-Julio 2022",
       caption = "Media móvil 7 días") +
  coord_x_date(xlim=c(fecha_min_curva_parcial,fecha_max_curva_parcial))