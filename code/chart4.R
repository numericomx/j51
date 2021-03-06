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
decesos_mensuales <- df_pandemia[, .N, by = .(month, Covid)]
decesos_mensuales$Covid <- factor(decesos_mensuales$Covid, levels = c(1, 0), labels = c("Mencionan COVID-19", "Sin menci�n de COVID-19"))

p1 <- ggplot(decesos_mensuales) +
  aes(x = factor(month, labels = c("abril", "mayo")), N, fill = Covid, label = scales::comma(N)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(labels = scales::comma) +
  scale_fill_manual(values = c("#66c2a5", "#b6e2d4")) +
  geom_text(color = "black",  size = 3,   position = position_stack(vjust = .55)) +
  labs(title = "�En cu�ntas actas de defunci�n se menciona a COVID-19?",
       subtitle = "N�mero actas de defunci�n que mencionan* a COVID-19",
       x = "",
       y = "",
       caption = "") +
  guides(fill = guide_legend(title = "")) +
  theme_light()

# Decesos por mes COVID-19 + Neumon�a at�pica + Neumon�a viral VS Otros
df_pandemia$Covid_o_neumonia <- df_pandemia$Covid | df_pandemia$Neumonia_atipica |  df_pandemia$Neumonia_viral 
decesos_mensuales <- df_pandemia[, .N, by = .(month, Covid_o_neumonia)]
decesos_mensuales$Covid_o_neumonia <- factor(decesos_mensuales$Covid_o_neumonia, levels = c(TRUE, FALSE), labels = c("COVID-19, \nNeumon�a at�pica o \nNeumon�a viral", "Sin menci�n de las anteriores"))

p2 <- ggplot(decesos_mensuales) +
  aes(x = factor(month, labels = c("abril", "mayo")), N, fill = Covid_o_neumonia, label = scales::comma(N, accuracy=1)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(labels = scales::comma) +
  scale_fill_manual(values = c("#66c2a5", "#b6e2d4")) +
  geom_text(color = "black",  size = 3,   position = position_stack(vjust = .55)) +
  labs(title = "",
       subtitle = "N�mero actas de defunci�n que mencionan* a COVID-19, neumon�a at�pica o neumon�a viral",
       x = "",
       y = "",
       caption = "Fuente: Elaboraci�n propia con datos de la Direcci�n General del Registro Civil.\n*Contienen t�rminos (COV o CORONAVIRUS), (NEU y ATIP), o (NEU y VIRAL) respectivamente.")+
  guides(fill = guide_legend(title = "")) +
  theme_light()

p <- p1/p2

plot(p)
