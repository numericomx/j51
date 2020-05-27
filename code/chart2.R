# CHART 2
library(data.table)
library(lubridate)
library(patchwork)

df <- fread("data/defun.csv")
df$fecha_defun <- dmy(df$fecha_defun)
df$month <- month(df$fecha_defun)
df$year <- year(df$fecha_defun)

deaths_by_month <- df[,.N, by=.(year, month)]
promedio_3_meses <- deaths_by_month[year==2020 & month %in% c(1,2,3), mean(N)]


p1 <- ggplot(deaths_by_month[year==2020,])+
      aes(x=factor(month, labels=c("enero", "febrero", "marzo", "abril", "mayo")), y=N, label=scales::comma(N))+
      geom_bar(stat="identity", fill="#66c2a5")+
      geom_hline(yintercept=promedio_3_meses, color= "#555555", linetype="dashed", size=0.3)+
      geom_text(color="black", vjust=1.2, size=3)+
      scale_y_continuous(labels = scales::comma)+
      labs(title = "Existe un excedente total de 3,786 actas de defunción en abril y mayo ",
           subtitle= "Número de actas de defunción por mes, Juzgado 51, Ciudad de México, 2020 ",
           x= "",
           y= "")+
      theme_light()
    
    
p2 <- 
  ggplot(deaths_by_month[year==2020,])+
  aes(x=factor(month, labels=c("enero", "febrero", "marzo", "abril", "mayo")), N-promedio_3_meses, label=scales::comma(N-promedio_3_meses))+
  geom_bar(stat="identity", fill="#66c2a5")+
  geom_text(color="black", vjust=1.2, size=3)+
  scale_y_continuous(labels = scales::comma)+
  labs(title = "",
       subtitle= "Desviación respecto a la media del primer trimestre, Juzgado 51, Ciudad de México, 2020",
       caption = "Fuente: Elaboración propia con datos de la Dirección General del Registro Civil de la Ciudad de México.\n
       *La línea rayada en la primera gráfica representa la media del primer trimestre del 2020.",
       x= "",
       y= "")+
  theme_light()

p <- p1/p2

plot(p)
