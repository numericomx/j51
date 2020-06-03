library(data.table)
library(lubridate)
library(ggplot2)

df <- fread("defun.csv")
df$fecha_defun <- dmy(df$fecha_defun)

causas <- c("Covid", "Insuficiencia_respiratoria", "Neumonia_atipica", "Neumonia_viral", "Otro")
enf_factor <- factor(causas, levels = causas,  labels = c("COVID-19", "Insuficiencia respiratoria", "Neumonía atípica", "Neumonía viral", "Otras"))

# Filtra decesos de abril y mayo 2020
df_pandemia <- df[df$fecha_defun >= "2020-04-01" & df$fecha_defun <= "2020-05-25"]

# Decesos por causa
decesos_por_causa_ <- as.numeric(lapply(df_pandemia[, causas, with = FALSE], sum))
decesos_por_causa <- data.table(causa = enf_factor, N = as.numeric(decesos_por_causa_))

ggplot(decesos_por_causa, 
       aes(x = causa, y = N, fill = causa, label =  scales::comma(N))) +
  geom_bar(stat = "identity") +
  scale_y_continuous(labels = scales::comma) +
  scale_fill_manual(values = c("#66c2a5", "#fc8d62", "#8da0cb", "#e78ac3", "#f0f0b1")) +
  geom_text(color = "black", size = 4, position = position_stack(vjust = .8)) +
  labs(title = "Hay 7,198 actas de defunción de la CDMX que mencionan COVID-19 como causa de muerte.",
       subtitle = "Número de actas de defunción del que mencionan*: COVID-19, insuficiencia respiratoria, neumonía atípica y neumonía viral\nabril y mayo 2020, CMDX",
       caption = "Fuente: Elaboración propia con datos de la Dirección General del Registro Civil.\n*Contienen términos (COV o CORONAVIRUS), (NEU y ATIP), (NEU y VIRAL) o (INSUF y RESP) respectivamente.\nUn acta de defunción puede mencionar una o varias causas de muerte.",
       x = "",
       y = "") +
  guides(fill = FALSE) +
  theme_light() 
