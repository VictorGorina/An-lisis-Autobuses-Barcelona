
#Analisis Bus Bunching

rm(list=ls())

#1. A cuantas líneas da servicio cada Calca.
CALCALINIA = table(Busos$CALCA, Busos$LINEA)
LineesPerCalca <- apply(CALCALINIA, 1, function(x) sum(x > 0))
LineesPerCalca <- LineesPerCalca - 1
barplot(table(LineesPerCalca))

ggplot(aes(y = , x = )) + geom_bar( fill="#56B4E9", width=.8, stat="identity") +
  guides(fill=FALSE) + xlab("Weekday") + ylab("Buses") + ggtitle("Weekday vs. Buses")

#2. Tiempo entre que entra el autobus en parada hasta que sale en segundos.
Busos$temps_parades <- Busos$FECHAHORA_SALIDA_AMBITO_PARADA - Busos$FECHAHORA_ENTRADA_AMBITO_PARADA 
mutate(Busos, hour =  cut(FECHAHORA_ENTRADA_AMBITO_PARADA, "hour")) %>% 
  group_by(LINEA, hour) %>% 
  summarise(mediana=median(temps_parades))%>%
  ggplot(aes(x=hour, y = mediana, group = LINEA, color = LINEA)) + geom_line()

str(Busos)
group_by(Busos, PARADA) %>% summarise(total = n(), estada_mitjana = mean(temps_parades), 
                                      sd = sd(temps_parades)) %>% arrange(desc(total)) 

group_by(Busos, PARADA) %>% 
  summarise(total = n(), estada_mitjana = mean(temps_parades), sd = sd(temps_parades)) %>% 
  ggplot(aes(y = estada_mitjana, x = PARADA)) + geom_bar(stat="identity")

#3. Tiempo entre dos autobuses por parada.

#numlinsparada <- group_by(Busos, PARADA) %>% summarise(numlins = length(unique(LINEA)))
#Busos <- merge(x = Busos, y = numlinsparada, by = "PARADA")
#Busos <- arrange(Busos,PARADA, FECHAHORA_ENTRADA_AMBITO_PARADA)
#Busos$lag <- c(NA, Busos$FECHAHORA_ENTRADA_AMBITO_PARADA[-1] - Busos$FECHAHORA_SALIDA_AMBITO_PARADA[-nrow(Busos)])
#Busos$lag <- ifelse(duplicated(Busos$PARADA), Busos$lag, NA)

## mitjanes de temps d'espera segons lag>0
group_by(Busos, lag > 0) %>% summarise(mean(temps_parades))

Busos[sample(nrow(Busos), 10000, replace = FALSE),] %>% 
  filter(lag<10000) %>%
  ggplot(aes(x=lag, y = temps_parades, group = numlins)) + geom_point(alpha=0.2) 
+ geom_smooth(method="lm") + facet_wrap(~ numlins)

### resums numÃ¨rics agrupats per numlinies a cada parada

#4. Media lag por num lineas que pasan por parada.
group_by(Busos, numlins) %>% 
  summarise(num = n()/nrow(Busos),
            mean_lag = mean(lag, na.rm=TRUE),
            mean_tempsparada = mean(temps_parades, na.rm=TRUE),
            solapaments = 1- sum(lag>0, na.rm=TRUE)/n()) %>%
  ggplot(aes(x=numlins, y=mean_lag)) + geom_bar(stat="identity")

group_by(Busos, numlins, lag>0) %>% 
  summarise(num = n()/nrow(Busos),
            mean_lag = mean(lag, na.rm=TRUE),
            mean_tempsparada = mean(temps_parades, na.rm=TRUE))

group_by(Busos, PARADA) %>%
  summarise(mean = mean(temps_parades, na.rm=TRUE), numlins = length(unique(LINEA))) %>% 
  ggplot(aes(x=numlins, y=mean)) + geom_point()

a <- tapply(as.numeric(Busos$temps_parades), Busos$lag>0, function(x) quantile(x, seq(0,0.95, 0.05)))

plot(a[[1]], type = "l", col="red")
lines(a[[2]], type = "l")

#Distribución del LAG
filter(Busos, lag > quantile(lag, 0.01, na.rm = TRUE), 
       lag < quantile(lag, 0.91, na.rm = TRUE)) %>% 
  ggplot(aes(lag)) + geom_histogram(fill="#56B4E9")+ 
  xlab("LAG") + ylab("") + ggtitle("Distribución del LAG") + 
  theme(panel.background = element_blank(), text = element_text(size=10), title = element_text(size= 10)) 



# 5. Lag mitjà 

#5.1 Per Linia

group_by(Busos, Linea = LINEA) %>% summarise(Media = mean(lag, na.rm = T))

#5.2 Per Linia & dia setmana

group_by(Busos, Linea = LINEA, DiasSemana = wday(Busos$FECHAHORA_ENTRADA_AMBITO_PARADA, label = TRUE)) %>% 
  summarise(mean = mean(lag, na.rm = T))

#5.3 Per parada & dia setmana

group_by(Busos, Parada = PARADA, DiasSemana = wday(Busos$FECHAHORA_ENTRADA_AMBITO_PARADA, label = TRUE)) %>% 
  summarise(mean = mean(lag, na.rm = T))

#Recuentos
group_by(Busos, CALCA) %>% summarise(a = length(unique(LINEA))) %>%
  ggplot(aes(x=a)) + geom_bar()

group_by(Busos, CALCA) %>% summarise(a = sum(LONGITUD, na.rm=TRUE)) %>%
  ggplot(aes(x=a)) + geom_histogram()

group_by(Busos, CALCA) %>% mutate(a = sum(LONGITUD, na.rm=TRUE)) %>%
  filter(a < 100000) %>%   group_by(LINEA) %>% 
  summarise(length(unique(CALCA))) %>% View()

#CLUSTERS
#stops_bus <- filter(stops, substr(stop_id, 1, 2) == "2.")
#stops_bus$stop_id <- paste("FM", substr(stops_bus$stop_id, 3, 100), sep="")
#stops_bus <- select(stops_bus,-(stop_url:wheelchair_boarding))
#stops_bus <- merge(stops_bus,numlinsparada, by.x = "stop_id", by.y = "PARADA")

Busos <- merge(Busos, stops_bus, by.x = "PARADA", by.y = "stop_id", all.x=TRUE)

### kmeans
Parada_kmeans <- group_by(Busos, PARADA) %>% 
  summarise(median_lag = median(lag, na.rm=TRUE),
            median_tempsparada = median(temps_parades, na.rm=TRUE), numlins = mean(numlins))
Parada_kmeans <-na.omit(Parada_kmeans)
Parada_kmeans$median_tempsparada <- as.numeric(Parada_kmeans$median_tempsparada)

km1 <- kmeans(x = Parada_kmeans[,2:4], centers = 5, nstart = 50)
km1
pairs(Parada_kmeans[,2:4], col = km1$cluster)

Parada_kmeans$cluster <- km1$cluster

stops_cluster <- merge(stops_bus, Parada_kmeans, by.x="stop_id", by.y = "parades")

brrr <- group_by(Busos, PARADA) %>% 
  summarise(median_lag = median(lag, na.rm=TRUE),
            median_tempsparada = median(temps_parades, na.rm=TRUE),mean_lag = mean(lag, na.rm=TRUE),
            mean_tempsparada = mean(temps_parades, na.rm=TRUE),
            numlins = mean(numlins))







#ANALISIS DE NUMLINS, TEMPS_PARADA y LAG

#Tiempo en parada.

IQR(as.numeric(Busos$temps_parades))
quantile(as.numeric(Busos$temps_parades),0.75)
quantile(as.numeric(Busos$temps_parades),0.75) + 1.5*31 

hist(as.numeric(Busos$temps_parades), breaks = c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100, 6075), 
     xlim=c(0, 150))

ggplot(Busos, aes(x=temps_parades)) + geom_histogram(aes(y=..density..), colour="#1A26C1", fill="#56B4E9") +
  geom_density(alpha=.2, fill="#FF6666",color="#FF6666") + xlim(0,150) + xlab("Tiempo en parada") + ylab("") + 
  ggtitle("Distribución del tiempo en parada") + theme(panel.background = element_blank(), text = element_text(size=10), 
                                                       title = element_text(size= 10))

quantile(Busos$temps_parades, 0.25)
quantile(Busos$temps_parades, 0.5)
quantile(Busos$temps_parades, 0.75)

#¿El tiempo en parada es mayor que 100 segundos?
table(Busos$temps_parades>100)

#¿Qué porcentaje respecto al total?
prop.table(table(Busos$temps_parades>150))

#Mediana de los que tienen lag y los que no
group_by(Busos, lag>0) %>% summarise(median(temps_parades),n())

#Porcentaje de valores despreciados en el gráfico.
prop.table(table(Busos$temps_parades>150))

summary(as.numeric(Busos$temps_parades))

#Boxplot con X en la media
boxplot(as.numeric(Busos$temps_parades), ylim=c(0,100),horizontal = TRUE, xlab="Time")
title ("Boxplot del tiempo en parada")
points(y=1, x=50.87, col="red", cex=2, pch=4)
ggplot(Busos, aes(x =temps_parades, y =factor(0) )) + geom_boxplot() +scale_y_discrete(breaks=NULL)

#Boxplot relacionando numero de lineas y tiempo en parada
group_by(Busos, numlins) %>% ggplot(aes(x=as.factor(numlins), as.numeric(temps_parades), group = numlins)) + 
  geom_boxplot() + ylim(0,200)


#Cuartiles por número de línea.
group_by(Busos, numlins) %>% summarise(q1 = quantile(temps_parades, 0.25),q2 = quantile(temps_parades, 0.5),
                                       q3 = quantile(temps_parades, 0.75))


group_by(Busos, numlins) %>% summarise(q1 = quantile(as.numeric(temps_parades), 0.25),q2 = quantile(as.numeric(temps_parades), 0.5)) %>% cor()




#LAG

hist(Busos$lag)
table(Busos$lag == 0)
table(Busos$lag > 0)
prop.table(table(Busos$lag >= 0))
prop.table(table(Busos$lag < 0))
group_by(Busos, numlins) %>% summarise(a = length(lag>0)/n(), b = length(lag<0)/n())


#Porcentaje de LAG segun numero de paradas
group_by(Busos, numlins) %>% summarise(Sin_Solapamiento = sum(lag>=0, na.rm=TRUE)/n(), Con_Solapamiento = sum(lag<0, na.rm=TRUE)/n()) %>% 
  gather(Con_Solapamiento,Sin_Solapamiento,value="percent", key="Solapamiento") %>% 
  ggplot(aes(x=as.factor(numlins), y=percent, fill=Solapamiento)) + geom_bar(stat="identity") +
  xlab("Número de Líneas") + ylab("Porcentaje") + ggtitle("LAG en función del número de líneas") +
  theme(panel.background = element_blank(),text = element_text(size=10))




#COMBINACIONES DE LAS VARIABLES

#LAG con numero de lineas 

#Con whiskers.
ggplot(Busos, aes(x=as.factor(numlins), y=lag, group = numlins)) + 
  geom_boxplot(outlier.size = 0.5, outlier.alpha = 0.5, outlier.colour = "red") + 
  ylim(-500, 1000) + xlab("Número de líneas") + ylab("LAG") + ggtitle("LAG vs. Número de líneas") +
  theme(panel.background = element_blank(),text = element_text(size=10))

#Sin whiskers
ggplot(Busos, aes(x=as.factor(numlins), y=lag, group = numlins)) + 
  geom_boxplot(outlier.shape = NA) + ylim(-500, 1000) + 
  xlab("Número de líneas") + ylab("LAG") + ggtitle("LAG vs. Número de líneas") +
  theme(panel.background = element_blank(),text = element_text(size=10)) +
  geom_hline(yintercept = 0, col="red")

#NumLins con Tiempo en parada
ggplot(Busos, aes(x=as.factor(numlins), y=temps_parades, group = numlins)) + 
  geom_boxplot(outlier.size = 0.5, outlier.alpha = 0.5, outlier.colour = "red") +
  ylim(0, 200) + xlab("Número de líneas") 

#NumLins con tiempo en parada según LAG
filter(Busos, lag>0) %>% ggplot(aes(x=as.factor(numlins), y=temps_parades, group = numlins)) + 
  geom_boxplot(outlier.shape = NA) + ylim(0, 150) + 
  xlab("Número de líneas") + ylab("Tiempo en parada") + ggtitle("Tiempo en parada vs. Número de líneas con LAG>0") +
  theme(panel.background = element_blank(),text = element_text(size=10))

filter(Busos, lag<0) %>% ggplot(aes(x=as.factor(numlins), y=temps_parades, group = numlins)) + 
  geom_boxplot(outlier.shape = NA) + ylim(0, 150) + 
  xlab("Número de líneas") + ylab("Tiempo en parada") + ggtitle("Tiempo en parada vs. Número de líneas con LAG<0") +
  theme(panel.background = element_blank(),text = element_text(size=10))

filter(Busos, lag<0) %>% ggplot(aes(x=as.factor(numlins), y=temps_parades, group = numlins)) + 
  geom_boxplot(outlier.size = 0.5, outlier.alpha = 0.5, outlier.colour = "red") + ylim(0, 200) + 
  xlab("Número de líneas")