library(data.table)
library(lubridate)
library(patchwork)

df <- fread("data/defun.csv")
df$fecha_defun <- dmy(df$fecha_defun)
df$month <- month(df$fecha_defun)
df$year <- year(df$fecha_defun)

# Seleccionar aquellos que mencionan COVID

deaths_by_month_ <- df[,.N, by=.(year, month, Covid)]
deaths_by_month_$covid <- factor(deaths_by_month_$Covid, levels= c(1, 0), labels=c("Mencionan Covid*", "No mencionan Covid"))

p1 <- ggplot(deaths_by_month_[year==2020,])+
  aes(x=factor(month, labels=c("enero", "febrero", "marzo", "abril", "mayo")), N, fill=covid, label=scales::comma(N))+
  geom_bar(stat="identity")+
  scale_y_continuous(labels = scales::comma)+
  scale_fill_manual(values=c("#66c2a5", "#fc8d62", "#8da0cb", "#e78ac3", "#a6d854"))+
  geom_text(color="black",  size=2,   position = position_stack(vjust = .8))+
  labs(title = "2,058 actas de defunción en el Juzgado 51 mencionan COVID19",
       subtitle= "Número actas de defunción que mencionan COVID, Juzgado 51, 2020 ",
       x= "",
       y= "")+
  guides(fill=guide_legend(title=""))+
  theme_light()

df$covneum <- df$Covid | df$Neumonia_atipica
deaths_by_month <- df[,.N, by=.(year, month, covneum)]
deaths_by_month$covid <- factor(deaths_by_month$covneum, levels= c(TRUE, FALSE), labels=c("Covid+Neum atipica**", "No Covid, no neum"))

p2 <- ggplot(deaths_by_month[year==2020,])+
  aes(x=factor(month, labels=c("enero", "febrero", "marzo", "abril", "mayo")), N, fill=covid, label=scales::comma(N))+
  geom_bar(stat="identity")+
  scale_y_continuous(labels = scales::comma)+
  scale_fill_manual(values=c("#66c2a5", "#fc8d62", "#8da0cb", "#e78ac3", "#a6d854"))+
  geom_text(color="black",  size=2,   position = position_stack(vjust = .55))+
  labs(title = "",
       subtitle= "Número actas de defunción que mencionan COVID o neumonía atípica, Juzgado 51, 2020 ",
       x= "",
       y= "",
       caption = "Fuente: Elaboración propia con datos de la Dirección General del Registro Civil.
       *Mencionan frases que contienen COV o CORONAVIRUS.
       ** Mencionan frases que contienen COV o  CORONAVIRUS y frases que contienen (NEU y ATI).")+
  guides(fill=guide_legend(title=""))+
  theme_light()

p <- p1/p2

plot(p)
