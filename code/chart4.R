library(data.table)
library(lubridate)
library(patchwork)
library(ggplot2)

df <- fread("defun.csv")
df$fecha_defun <- dmy(df$fecha_defun)
df$month <- month(df$fecha_defun)
df$year <- year(df$fecha_defun)

# Filtra registros de abril y mayo 2020
df_j51 <- df[df$year == 2020 & df$month %in% c(4, 5)]

df_j51$covneum <- df_j51$Covid == 1

# Muertes por mes COVID-19 VS Otros
deaths_by_month <- df_j51[, .N, by = .(month, covneum)]
deaths_by_month$covid <- factor(deaths_by_month$covneum, levels = c(TRUE, FALSE), labels = c("Mencionan COVID-19", "Sin mención de COVID-19"))

p1 <- ggplot(deaths_by_month) +
  aes(x = factor(month, labels = c("abril", "mayo")), N, fill = covid, label = scales::comma(N)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(labels = scales::comma) +
  scale_fill_manual(values = c("#66c2a5", "#b6e2d4")) +
  geom_text(color = "black",  size = 3,   position = position_stack(vjust = .55)) +
  labs(title = "¿En cuántas actas de defunción se menciona a COVID-19?",
       subtitle = "Número actas de defunción que mencionan* a COVID-19",
       x = "",
       y = "",
       caption = "")+
  guides(fill = guide_legend(title = "")) +
  theme_light()

# Filtra registros de abril y mayo 2020
df_j51 <- df[df$year == 2020 & df$month %in% c(4, 5)]

df_j51$covneum <- df_j51$Covid | df_j51$Neumonia_atipica |  df_j51$Neumonia_viral 

# Muertes por mes COVID-19 + Neumonía atípica + Neumonía viral VS Otros
deaths_by_month <- df_j51[, .N, by = .(month, covneum)]
deaths_by_month$covid <- factor(deaths_by_month$covneum, levels = c(TRUE, FALSE), labels = c("COVID-19, \nNeumonía atípica o \nNeumonía viral", "Sin mención de las anteriores"))

p2 <- ggplot(deaths_by_month) +
  aes(x = factor(month, labels = c("abril", "mayo")), N, fill = covid, label = scales::comma(N, accuracy=1)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(labels = scales::comma) +
  scale_fill_manual(values = c("#66c2a5", "#b6e2d4")) +
  geom_text(color = "black",  size = 3,   position = position_stack(vjust = .55)) +
  labs(title = "",
       subtitle = "Número actas de defunción que mencionan* a COVID-19, neumonía atípica o neumonía viral",
       x = "",
       y = "",
       caption = "Fuente: Elaboración propia con datos de la Dirección General del Registro Civil.\n*Contienen términos (COV o CORONAVIRUS), (NEU y ATIP), o (NEU y VIRAL) respectivamente.")+
  guides(fill = guide_legend(title = "")) +
  theme_light()

p <- p1/p2

plot(p)
