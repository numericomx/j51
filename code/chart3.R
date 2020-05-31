library(data.table)
library(lubridate)
library(ggplot2)
library(UpSetR)

df <- fread("defun.csv")
df$fecha_defun <- dmy(df$fecha_defun)
df$month <- month(df$fecha_defun)
df$year <- year(df$fecha_defun)

# Filtra registros de abril y mayo 2020
df <- df[df$year == 2020 & df$month %in% c(4, 5)]

enfermedades <- c("COVID-19", "Insuficiencia respiratoria", "Neumonía atípica", "Neumonía viral")
setnames(df, old = c("Covid", "Insuficiencia_respiratoria", "Neumonia_atipica", "Neumonia_viral"), new = enfermedades)

upset(df, 
      sets = enfermedades, 
      sets.bar.color = "black",
      order.by = "freq", 
      point.size = 3.5, 
      line.size = 2, 
      mainbar.y.label = "Tamaño de la insersección", sets.x.label = "Total de actas por causa", 
      text.scale = c(1.25, 1.5, 1.25, 1.5, 1.5, 1.5))
