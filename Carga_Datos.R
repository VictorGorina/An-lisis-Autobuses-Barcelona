#Carga de datos

# Paquets a instal.lar/carregar
pckgs<-c("ggthemes","tidyverse","dplyr","lubridate","ggplot2","leaflet","ggmap")
pckgs2Install<-pckgs[!(pckgs %in% library()$results[,1])]
pckgs2Load<-pckgs[!(pckgs %in% (.packages()))]
for(pckg in pckgs2Install) {install.packages(pckg)}
for(pckg in pckgs2Load) {library(pckg,character.only = TRUE)}

#Carga archivo
setwd("C:/Users/iqs07/Desktop/TFG/CODIGO/Archivos/")

system.time(length(readLines("Loc_Parades_03_2016.csv")))

Busos <- read.table("Loc_Parades_03_2016.csv", sep = "\t", header = TRUE)

#Limpieza
Busos <- Busos[Busos$FECHAHORA_ENTRADA_AMBITO_PARADA != "",]
Busos <- Busos[Busos$FECHAHORA_SALIDA_AMBITO_PARADA != "",]
Busos <- Busos[Busos$LINEA !="NO_INICIADO",]
Busos <- Busos[!Busos$LINEA %in% c(101,100,180),]
Busos <- Busos[!Busos$MOTIU %in% c(1000,2000,2001,2002,2003,300,4001,4002,5000),]
Busos <- select(Busos,-(MOTIU:TRAYECTO_ARCO),-(ORDEN_ARCO:METRESENTRADADESTI),-(TRAYECTOSAE:DATADADES),-TURNO,-CONTADOR_INI,-CONTADOR_FI)

#Tratamiento Tiempo.
Busos$FECHAHORA_ENTRADA_AMBITO_PARADA <- as.POSIXct(Busos$FECHAHORA_ENTRADA_AMBITO_PARADA)
Busos$FECHAHORA_SALIDA_AMBITO_PARADA <- as.POSIXct(Busos$FECHAHORA_SALIDA_AMBITO_PARADA)
Busos$FECHAHORA_INICIO_DESPLAZA_A_NEXTPARADA <- as.POSIXct(Busos$FECHAHORA_INICIO_DESPLAZA_A_NEXTPARADA)
Busos$FECHAHORA_FINAL_DESPLAZA_A_NEXTPARADA <- as.POSIXct(Busos$FECHAHORA_FINAL_DESPLAZA_A_NEXTPARADA)
Busos$temps_parades <- Busos$FECHAHORA_SALIDA_AMBITO_PARADA - Busos$FECHAHORA_ENTRADA_AMBITO_PARADA #Tiempo entre que entra el autobus en parada hasta que sale en segundos.
Busos$DiaSemana <- wday(Busos$FECHAHORA_ENTRADA_AMBITO_PARADA, label = TRUE)

#CLUSTERS

stops <- read.table("stops.txt", sep = ",", header = TRUE)

numlinsparada <- group_by(Busos, PARADA) %>% summarise(numlins = length(unique(LINEA)))
Busos <- merge(x = Busos, y = numlinsparada, by = "PARADA")

Busos <- arrange(Busos,PARADA, FECHAHORA_ENTRADA_AMBITO_PARADA)
Busos$lag <- c(NA, Busos$FECHAHORA_ENTRADA_AMBITO_PARADA[-1] - Busos$FECHAHORA_SALIDA_AMBITO_PARADA[-nrow(Busos)])
Busos$lag <- ifelse(duplicated(Busos$PARADA), Busos$lag, NA)

stops_bus <- filter(stops, substr(stop_id, 1, 2) == "2.")
stops_bus$stop_id <- paste("FM", substr(stops_bus$stop_id, 3, 100), sep="")
stops_bus <- select(stops_bus,-(stop_url:wheelchair_boarding))
stops_bus <- merge(stops_bus,numlinsparada, by.x = "stop_id", by.y = "PARADA")

Busos <- merge(Busos, stops_bus, by.x = "PARADA", by.y = "stop_id", all.x=TRUE)
