library(data.table)
library(lubridate)
library(patchwork)
library(ggplot2)

df <- fread("defun.csv")
df$fecha_defun <- dmy(df$fecha_defun)
df$month <- month(df$fecha_defun)

# Filtra decesos de abril y mayo 2020
df_pandemia <- df[df$fecha_defun >= "2020-04-01" & df$fecha_defun <= "2020-05-25"]

# Decesos por mes COVID-19 VS Otros
decesos_mensuales <- df_pandemia[, .N, by = .(month, juzgado)]
decesos_mensuales$juzgado <- factor(decesos_mensuales$juzgado, levels = c(51, 14, 18, 13, 19, 16), 
                                    labels = c("Juzgado 51", "Juzgado 19", "Juzgado 18", "Juzgado 16", "Juzgado 14", "Juzgado 13"))

ggplot(decesos_mensuales) +
  aes(x = factor(month, labels = c("abril", "mayo")), N, fill = juzgado, label = scales::comma(N)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(labels = scales::comma) +
  scale_fill_manual(values = c("#fca2a2","#8da0cb", "#fc8d62","#a6d851ff",  "#66c2a5",  "#e78ac3")) +
  geom_text(color = "black",  size = 4, position = position_stack(vjust = .55)) +
  labs(title = "Analizamos 20,900 actas de defunción emitidas en la Ciudad de México durante la pandemia",
       subtitle = "Actas de defunción emitidas en la Ciudad de México, abril y mayo 2020",
       x = "",
       y = "",
       caption = "Fuente: Elaboración propia con datos de la Dirección General del Registro Civil.")+
  guides(fill = guide_legend(title = "")) +
  theme_light()

