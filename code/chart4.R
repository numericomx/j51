library(lubridate)
library(ggplot2)
library(ggforce)

df <- fread("data/defun.csv")
df$fecha_defun <- dmy(df$fecha_defun)
df$month <- month(df$fecha_defun)
df$year <- year(df$fecha_defun)

# Seleccionar casos del 2020
df <- df[df$year == 2020, ]

df_venn <- data.frame(x = c(0, 0.866, -0.866),
                      y = c(1, -0.5, -0.5),
                      labels = c('COVID-19', 'Neumonía atípica', 'Insuficiencia respiratoria'))

ggplot(df_venn) +
  geom_circle(aes(x0 = x, y0 = y, r = 1.5, fill = labels), alpha = .7, size = 1, colour = 'white') +
  coord_fixed() +
  theme_void() +
  theme(legend.position = 'bottom') +
  scale_fill_manual(values = c('#fc8d62', '#66c2a5', '#8da0cb')) +
  scale_colour_manual(values = c( '#fc8d62', '#66c2a5', '#8da0cb'), guide = FALSE) +
  labs(fill = NULL) +
  annotate("text", 
           x = c(0, 1.2, 0.8, -1.2, -0.8, 0, 0), 
           y = c(1.2, -0.6, 0.5, -0.6, 0.5, -1, 0), 
           label = c(nrow(df[df$Covid == 1 & df$Neumonia_atipica == 0 & df$Insuficiencia_respiratoria == 0, ]), # COVID
                     nrow(df[df$Covid == 0 & df$Neumonia_atipica == 1 & df$Insuficiencia_respiratoria == 0, ]), # Neumonía 
                     nrow(df[df$Covid == 1 & df$Neumonia_atipica == 1 & df$Insuficiencia_respiratoria == 0, ]), # COVID y neumonía
                     nrow(df[df$Covid == 0 & df$Neumonia_atipica == 0 & df$Insuficiencia_respiratoria == 1, ]), # Insuficiencia
                     nrow(df[df$Covid == 1 & df$Neumonia_atipica == 0 & df$Insuficiencia_respiratoria == 1, ]), # COVID e insuficiencia
                     nrow(df[df$Covid == 0 & df$Neumonia_atipica == 1 & df$Insuficiencia_respiratoria == 1, ]), # Insuficiencia y neumonía 
                     nrow(df[df$Covid == 1 & df$Neumonia_atipica == 1 & df$Insuficiencia_respiratoria == 1, ])), # Todos  
           size = 5) + 
  labs(title = "La neumonía atípica y el COVID19",
       subtitle = "Número de actas de defunción por causa de muerte,\ndiagrama de Venn, abr - may 2020",
       caption = "Fuente: Elaboración propia con datos de la Dirección General del Registro Civil.") 

