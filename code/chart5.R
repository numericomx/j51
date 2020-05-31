library(data.table)
library(lubridate)
library(patchwork)
library(ggplot2)
library(zoo)

df <- fread("defun.csv")
df$fecha_defun <- dmy(df$fecha_defun)
df$day <- month(df$fecha_defun)
df$month <- month(df$fecha_defun)
df$year <- year(df$fecha_defun)

# Filtra del 31 de marzo del 2020 y del 15 de mayo del 2020
df_ts <- df[df$fecha_defun > "2020-03-31" & df$fecha_defun < "2020-05-16"]

##### Casos acumulados #####

# COVID-19 
covid <- aggregate(Covid ~ fecha_defun, df_ts, sum) # Agregar
covid$Covid <- cumsum(covid$Covid) # Acumular

# COVID-19 y neumonía
df_ts[, Cov_or_neu := ifelse(Covid == 1 | Neumonia_atipica == 1 | Neumonia_viral == 1, 1, 0)]

covid_neu <- aggregate(Cov_or_neu  ~ fecha_defun, df_ts, sum) # Agregar
covid_neu$Cov_or_neu <- cumsum(covid_neu$Cov_or_neu) # Acumular

# Datos DGE
df_mx <- fread("Casos_Diarios_Estado_Nacional_Defunciones_20200529.csv")
df_cdmx <- t(df_mx[nombre == "DISTRITO FEDERAL"])

# Reordenar datos
dates <- rownames(df_cdmx)
df_cdmx <- as.data.table(df_cdmx)
df_cdmx$fecha_defun <- dates
df_cdmx <- df_cdmx[-c(1:3), ]

# Filtra del 31 de marzo del 2020 y del 15 de mayo del 2020
df_cdmx$fecha_defun <- dmy(df_cdmx$fecha_defun)
df_cdmx <- df_cdmx[df_cdmx$fecha_defun > "2020-03-31" & df_cdmx$fecha_defun < "2020-05-16"]

setnames(df_cdmx, old = "V1", "Covid")
df_cdmx$Covid <- as.numeric(df_cdmx$Covid)

covid_gob <- aggregate(Covid ~ fecha_defun, df_cdmx, sum) # Agregar
covid_gob$Covid_agg <- cumsum(covid_gob$Covid) # Acumular

# Merge en un único datatable
merged <- merge(covid, covid_gob, by = "fecha_defun")
merged <- merge(merged, covid_neu, by = "fecha_defun")

p1 <- ggplot(merged, aes(x = fecha_defun)) +
  geom_line(aes(y = Covid.x), color = "#66c2a5", size = 0.75) +
  geom_line(aes(y = Covid.y), color = "#b58f5bff", size = 0.75) +
  geom_line(aes(y = Cov_or_neu), color = "#fc8d62", size = 0.75) +
  scale_color_discrete(name = "Y series", labels = c("Y2", "Y1")) +
  scale_y_continuous(labels = scales::comma) +
  labs(title = "Una curva alternativa",
       subtitle = "abril 1° 2020 - mayo 15 2020, CDMX\n\nFallecimientos acumulados diarios",
       caption = "",
       x= "",
       y= "") +
  theme_light()

##### Casos diarios #####

# COVID-19
covid <- aggregate(Covid ~ fecha_defun, df_ts, sum) # Agregar

# Promedio móvil de 3 días
rms <- rollmean(covid$Covid, 3) 
covid <- as.data.table(covid[3:nrow(covid), "fecha_defun"])
covid$covid_rms <- rms

# COVID-19 y neumonía
df_ts[, Cov_or_neu := ifelse(Covid == 1 | Neumonia_atipica == 1 | Neumonia_viral == 1, 1, 0)]

covid_neu <- aggregate(Cov_or_neu  ~ fecha_defun, df_ts, sum) # Agregar

# Promedio móvil de 3 días
rms <- rollmean(covid_neu$Cov_or_neu, 3)
covid_neu <- as.data.table(covid_neu[3:nrow(covid_neu), "fecha_defun"])
covid_neu$Cov_or_neu <- rms

# Datos DGE
df_mx <- fread("Casos_Diarios_Estado_Nacional_Defunciones_20200529.csv")
df_cdmx <- t(df_mx[nombre == "DISTRITO FEDERAL"])

# Reordenar datos
dates <- rownames(df_cdmx)
df_cdmx <- as.data.table(df_cdmx)
df_cdmx$fecha_defun <- dates
df_cdmx <- df_cdmx[-c(1:3), ]

# Filtra del 31 de marzo del 2020 y del 15 de mayo del 2020
df_cdmx$fecha_defun <- dmy(df_cdmx$fecha_defun)
df_cdmx <- df_cdmx[df_cdmx$fecha_defun > "2020-03-31" & df_cdmx$fecha_defun < "2020-05-16"]

setnames(df_cdmx, old = "V1", "Covid")
df_cdmx$Covid <- as.numeric(df_cdmx$Covid)

covid_gob <- aggregate(Covid ~ fecha_defun, df_cdmx, sum) # Agregar

# Promedio móvil de 3 días
rms <- rollmean(covid_gob$Covid, 3)
covid_gob <- as.data.table(covid_gob[3:nrow(covid_gob), "fecha_defun"])
covid_gob$covid_rms <- rms

# Merge en un único datatable
merged <- merge(covid, covid_gob, by = "V1")
merged <- merge(merged, covid_neu, by = "V1")

p2 <- ggplot(merged, aes(x = V1)) + 
  geom_line(aes(y = covid_rms.x), color = "#66c2a5", size = 0.75) + 
  geom_line(aes(y = covid_rms.y), color = "#b58f5bff", size = 0.75) +
  geom_line(aes(y = Cov_or_neu), color = "#fc8d62", size = 0.75) +
  labs(title = "",
       subtitle = "Fallecimientos diarios (media móvil de tres días)",
       caption = "La línea rayada en la segunda gráfica representa el día con mayor número de fallecimientos.\nFuente: Elaboración propia con datos de la Dirección General del Registro Civil y datos abiertos de la Dirección General de Epidemiología.\n*Contienen términos (COV o CORONAVIRUS).\n**Contienen términos (COV o CORONAVIRUS), (NEU y ATIP) o (NEU y VIRAL).",
       x= "",
       y= "") +
  geom_vline(xintercept = as.Date("2020-05-06"), color= "#555555", linetype="dashed", size = 0.5) +
  theme_light() 

p <- p1/p2

plot(p)
