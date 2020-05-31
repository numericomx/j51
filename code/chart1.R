library(data.table)
library(lubridate)
library(ggplot2)

df <- fread("defun.csv")
df$fecha_defun <- dmy(df$fecha_defun)
df$month <- month(df$fecha_defun)
df$year <- year(df$fecha_defun)

enfermedades <- c("Covid", "Insuficiencia_respiratoria", "Neumonia_atipica", "Neumonia_viral")
enf_factor <- factor(enfermedades, levels = enfermedades,  labels = c("COVID-19", 
                                                                      "Insuficiencia respiratoria",
                                                                      "Neumonía atípica", 
                                                                      "Neumonía viral"))

# Filtra registros de abril y mayo 2020
df_pandemia <- df[df$year == 2020 & df$month %in% c(4, 5)]

# Tabla de frecuencias de menciones por enfermedad
freq_ <- as.numeric(lapply(df_pandemia[, enfermedades, with = FALSE], sum))
freq <- data.table(causa = enf_factor, N = as.numeric(freq_))

ggplot(freq, 
       aes(x = causa, y = N, fill = causa, label =  scales::comma(N))) +
  geom_bar(stat = "identity") +
  scale_y_continuous(labels = scales::comma) +
  scale_fill_manual(values = c("#66c2a5", "#fc8d62", "#8da0cb", "#e78ac3")) +
  geom_text(color = "black", size = 4, position = position_stack(vjust = .8)) +
  labs(title = "Hay 6,488 actas de defunción de la CDMX que mencionan COVID-19 como causa de muerte.",
       subtitle = "Número de actas de defunción del que mencionan*: COVID-19, insuficiencia respiratoria, neumonía atípica y neumonía viral\nabril y mayo 2020, CMDX",
       caption = "Fuente: Elaboración propia con datos de la Dirección General del Registro Civil.\n*Contienen términos (COV o CORONAVIRUS), (NEU y ATIP), (NEU y VIRAL) o (INSUF y RESP) respectivamente.\nUn acta de defunción puede mencionar una o varias causas de muerte.",
       x= "",
       y= "") +
  guides(fill = FALSE) +
  theme_light() 
