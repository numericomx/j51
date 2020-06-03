library(data.table)
library(lubridate)
library(patchwork)
library(ggplot2)
library(zoo)

df <- fread("casos_diarios.csv")
df$fecha_defun <- mdy(df$fecha_defun)

# Decesos acumulados

# Sumas acumuladas
df$Covid_acum <- cumsum(df$Covid) 
df$Covid_o_neumonia_acum <- cumsum(df$Covid_o_neumonia)
df$DGE_acum <- cumsum(df$DGE)

p1 <- ggplot(df, aes(x = fecha_defun)) +
  geom_line(aes(y = Covid_acum), color = "#66c2a5", size = 0.75) +
  geom_line(aes(y = Covid_o_neumonia_acum), color = "#fc8d62", size = 0.75) +
  geom_line(aes(y = DGE_acum), color = "#b58f5bff", size = 0.75) +
  scale_color_discrete(name = "Y series", labels = c("Y2", "Y1")) +
  scale_y_continuous(labels = scales::comma) +
  labs(title = "Una curva alternativa",
       subtitle = "abril 1° 2020 - mayo 25 2020, CDMX\n\nFallecimientos acumulados diarios",
       caption = "",
       x = "",
       y = "") +
  theme_light()

# Decesos diarios

# Promedios móviles de 3 días
df$Covid_rms <- rollmean(df$Covid, 3, na.pad = TRUE) 
df$Covid_o_neumonia_rms <- rollmean(df$Covid_o_neumonia, 3, na.pad = TRUE)
df$DGE_rms <- rollmean(df$DGE, 3, na.pad = TRUE)

p2 <- ggplot(df, aes(x = fecha_defun)) + 
  geom_line(aes(y = df$Covid_rms), color = "#66c2a5", size = 0.75) + 
  geom_line(aes(y = df$Covid_o_neumonia_rms), color = "#fc8d62", size = 0.75) +
  geom_line(aes(y = df$DGE_rms), color = "#b58f5bff", size = 0.75) +
  labs(title = "",
       subtitle = "Fallecimientos diarios (media móvil de tres días)",
       caption = "La línea rayada en la segunda gráfica representa el día con mayor número de fallecimientos.\nFuente: Elaboración propia con datos de la Dirección General del Registro Civil y datos abiertos de la Dirección General de Epidemiología.\n*Contienen términos (COV o CORONAVIRUS).\n**Contienen términos (COV o CORONAVIRUS), (NEU y ATIP) o (NEU y VIRAL).",
       x = "",
       y = "") +
  geom_vline(xintercept = as.Date("2020-05-18"), color = "#555555", linetype = "dashed", size = 0.5) +
  theme_light() 

p <- p1/p2

plot(p)
