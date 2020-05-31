library(data.table)
library(lubridate)
library(patchwork)
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

# Filtra registros de enero, febreo y marzo 2020 del juzgado 51
df_pre <- df[df$year == 2020 & df$month %in% c(2, 3) & df$juzgado == 51]

# Tabla de frecuencias de menciones por enfermedad pre-pandemia
freq1_ <- as.numeric(lapply(df_pre[, enfermedades, with = FALSE], sum))
freq1 <- data.table(causa = enf_factor, N = as.numeric(freq1_))

p1 <- ggplot(freq1, 
             aes(x = causa, y = N, fill = causa, label = N)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(labels = scales::comma, limits = c(0, 2500)) +
  scale_fill_manual(values = c("#66c2a5", "#fc8d62", "#8da0cb", "#e78ac3")) +
  geom_text(color = "black", size = 4, position = position_stack(vjust = .8)) +
  labs(title = "En el Juzgado 51, estas causas de muerte han incrementado considerablemente",
       subtitle = "Número de actas de defunción del Juzgado 51 que mencionan*: COVID-19, insuficiencia respiratoria, neumonía atípica y neumonía viral\n\nfebrero y marzo 2020, Juzgado 51",
       caption = "",
       x= "",
       y= "") +
  guides(fill = FALSE) +
  theme_light() 

# Filtra registros de abril y mayo 2020 del juzgado 51
df_pandemia <- df[df$year == 2020 & df$month %in% c(4, 5) & df$juzgado == 51]

# Tabla de frecuencias de menciones por enfermedad durante pandemia
freq2_ <- as.numeric(lapply(df_pandemia[, enfermedades, with = FALSE], sum))
freq2 <- data.table(causa = enf_factor, N = as.numeric(freq2_))

p2 <- ggplot(freq2, 
             aes(x = causa, y = N, fill = causa, label = scales::comma(N))) +
  geom_bar(stat = "identity") +
  scale_y_continuous(labels = scales::comma, limits = c(0, 2500)) +
  scale_fill_manual(values = c("#66c2a5", "#fc8d62", "#8da0cb", "#e78ac3")) +
  geom_text(color = "black", size = 4, position = position_stack(vjust = .8)) +
  labs(title = "",
       subtitle = "abril y mayo 2020, Juzgado 51",
       caption = "Fuente: Elaboración propia con datos de la Dirección General del Registro Civil.\n*Contienen términos (COV o CORONAVIRUS), (NEU y ATIP), (NEU y VIRAL) o (INSUF y RESP) respectivamente.\nUn acta de defunción puede mencionar una o varias causas de muerte.",
       x= "",
       y= "") +
  guides(fill = FALSE) +
  theme_light() 

p <- p1/p2

plot(p)
