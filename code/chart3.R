library(data.table)
library(lubridate)
library(ggplot2)
library(UpSetR)

df <- fread("defun.csv")
df$fecha_defun <- dmy(df$fecha_defun)

# Filtra decesos de abril y mayo 2020
df_pandemia <- df[df$fecha_defun >= "2020-04-01" & df$fecha_defun <= "2020-05-25"]

enfermedades <- c("COVID-19", "Insuficiencia respiratoria", "Neumonía atípica", "Neumonía viral")
setnames(df_pandemia, old = c("Covid", "Insuficiencia_respiratoria", "Neumonia_atipica", "Neumonia_viral"), new = enfermedades)

upset(df_pandemia, 
      sets = enfermedades, 
      sets.bar.color = "#b3b3b3ff",
      main.bar.color = "#66c2a5ff",
      order.by = "freq", 
      point.size = 3.5, 
      line.size = 2, 
      mainbar.y.label = "Tamaño de la insersección", sets.x.label = "Total de actas por causa", 
      text.scale = c(1.25, 1.5, 1.25, 1.5, 1.5, 1.5))
