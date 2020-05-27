# --- CHART 1 ----
library(data.table)
library(lubridate)

dfma <- fread("data/months2020.csv")

# Get incremental deaths per month
counts_per_month <- dfma[,.(month, countd=c(count[1],na.omit(diff(count)))), by=.(juzgado)]

# Rename juzgados
counts_per_month$juzgadolite <- "Todos los demás"
counts_per_month[juzgado==14,]$juzgadolite <- "Juzgado 14" 
counts_per_month[juzgado==18,]$juzgadolite <- "Juzgado 18" 
counts_per_month[juzgado==51,]$juzgadolite <- "Juzgado 51" 
counts_per_month[juzgado %in% c(13,16,19),]$juzgadolite <- "Juzgado 13,16 y 19" 

counts_per_month$juzgadolite <- factor(counts_per_month$juzgadolite, levels=rev(c("Todos los demás", 
                                                                                  "Juzgado 13,16 y 19" ,  
                                                                                  "Juzgado 14", 
                                                                                  "Juzgado 18",  
                                                                                  "Juzgado 51")))
# Aggregate totals
df_chart <- counts_per_month[,.(total=sum(countd)), by=.(month,juzgadolite)] 
total_per_month <- df_chart[, .(total_month=sum(total)), by= .(month)]

df_chart_merge <- merge(df_chart, total_per_month, by="month" )

#Plot
ggplot(df_chart_merge)+
  aes(x=factor(month, labels=c("enero", "febrero", "marzo", "abril", "mayo")), 
      y=total, fill=juzgadolite, label=paste0(scales::comma(total),"\n", "(", scales::percent(total/total_month),")"))+
  geom_bar(stat="identity")+
  geom_text(size=2,   position = position_stack(vjust = .5))+
  scale_y_continuous(labels=scales::comma)+
  scale_fill_manual(values=c("#66c2a5", "#fc8d62", "#8da0cb", "#e78ac3", "#a6d854"))+
  theme_light()+
  guides(fill=guide_legend(title=""))+
  xlab("")+
  ylab("")+
  labs(title = "El Juzgado 51 cuenta con alrededor del 32% de las actas de defunción",
       subtitle= "Distribución del número de actas de defunción entre diferentes juzgados, 2020",
       caption = "Fuente: Elaboración propia con datos de Mario Romero y Laurianne Despeghel.\n https://github.com/mariorz/folio-deceso.",
       x= "",
       y= "")