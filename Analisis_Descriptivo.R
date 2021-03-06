
#Descripci�n datos:

#1. N�m. Autobuses ---- OK

length(unique(Busos$CALCA)) #a que lineas donen servei?

group_by(Busos, Lineas = Busos$LINEA, Calca = unique(Busos$CALCA)) %>% summarise(n()) #gr�fic barres

#2. Horas y dias activos ----- OK

#Horas
as.numeric(max(strftime(Busos$FECHAHORA_ENTRADA_AMBITO_PARADA, "%H"))) - 
  as.numeric(min(strftime(Busos$FECHAHORA_ENTRADA_AMBITO_PARADA, "%H")))

#Dias 
as.numeric(max(strftime(Busos$FECHAHORA_ENTRADA_AMBITO_PARADA, "%d"))) - 
  as.numeric(min(strftime(Busos$FECHAHORA_ENTRADA_AMBITO_PARADA, "%d")))

#3. N�m. Lineas y autobuses ------ OK

length(unique(Busos$LINEA))

length(unique(Busos$CALCA))

#4. N�m. parades ------- OK

length(unique(Busos$PARADA))

#5. N�m. Lineas que paran por parada ----- OK

filter(tapply(Busos$LINEA, Busos$PARADA, function(x) length(unique(x))) %>% as.data.frame(), !is.na(.)) %>% 
  ggplot(aes(as.factor(.))) + geom_bar(fill="#56B4E9") + xlab("N�mero de l�neas") + 
  ylab("N�mero de paradas") + ylim(0,1500) + ggtitle ("N�mero de paradas vs. N�mero de l�neas") +
  theme(panel.background = element_blank(),text = element_text(size=10))

# autobuses que paran en paradas con 1,2, 3... lineas distintas
ggplot(Busos, aes(as.factor(numlins))) + geom_bar(fill="#56B4E9") + xlab("N�mero de l�neas por parada") + ylab("Pasada por parada") + 
  ggtitle( "N�mero de L�neas por parada vs. Pasadas por parada") + 
  theme(panel.background = element_blank(), text = element_text(size=10), title = element_text(size= 10))


#6. N�m Buses por d�a 

#Por d�a del mes (25 marzo -> viernes santo | 28 marzo -> lunes pascua) ------ OK

group_by(Busos, DiaMes = strftime(Busos$FECHAHORA_ENTRADA_AMBITO_PARADA, "%d"),
         DiasSemana = wday(Busos$FECHAHORA_ENTRADA_AMBITO_PARADA, label = TRUE)) %>% 
  summarise(NumMoviments = n(), NumBusos = length(unique(CALCA))) %>%
  ggplot(aes(y = NumBusos, x = DiaMes)) + geom_bar( fill="#56B4E9", width=.8, stat="identity") +
  guides(fill=FALSE) + xlab("D�a del mes") + ylab("Buses") + ggtitle("Buses vs. D�a del mes") + 
  theme(panel.background = element_blank(), text = element_text(size=20))

#Por d�a de la semana ----------- ESTE NO
group_by(Busos, Dias_semana = wday(Busos$FECHAHORA_ENTRADA_AMBITO_PARADA,label = TRUE)) %>% 
  summarise(NumMoviments = n(), NumBusos = length(unique(CALCA))) %>% 
  arrange(desc(NumBusos)) %>%
  ggplot(aes(y = NumBusos, x = Dias_semana)) + geom_bar( fill="#56B4E9", width=.8, stat="identity") +
  guides(fill=FALSE) + xlab("Weekday") + ylab("Buses") + ggtitle("Weekday vs. Buses")


#7. Autobuses por horas

#Por d�a de la semana y por hora -------- OK
group_by(Busos, DiaSemana = wday(Busos$FECHAHORA_ENTRADA_AMBITO_PARADA, label = TRUE), 
         Hora = strftime(Busos$FECHAHORA_ENTRADA_AMBITO_PARADA, "%H")) %>% 
  summarise(NumBuses = length(unique(CALCA))) %>%
  ggplot(aes(x=Hora, y = NumBuses, group = Hora)) +
  geom_bar(fill="#56B4E9", width=.8, stat="identity") +
  facet_wrap( ~ DiaSemana, ncol=1) +
  theme(panel.background = element_blank(), text = element_text(size=10), title = element_text(size= 10)) +
  xlab("Hora") + ylab("Buses") + ggtitle("Buses vs. Hora")


#8. Media del n�m. de autobuses por hora de todos los dias ----- NO

group_by(Busos, Hora = strftime(Busos$FECHAHORA_ENTRADA_AMBITO_PARADA, "%H")) %>% 
  summarise(NumBuses = length(unique(CALCA)), Media = NumBuses/length(unique(
    wday(Busos$FECHAHORA_ENTRADA_AMBITO_PARADA)))) %>% #Media de n�mero de autobuses (Total / Dias)
  ggplot(aes(y = Media, x = Hora)) + geom_point(stat="identity")


#9. Tiempo medio en parada por dia de la semana y por hora.-------- NO

group_by(Busos, Dia_semana = wday(Busos$FECHAHORA_ENTRADA_AMBITO_PARADA, label = TRUE), 
         Hora = strftime(Busos$FECHAHORA_ENTRADA_AMBITO_PARADA, "%H")) %>% 
  summarise(Mediana = median(temps_parades))


#10. Tiempo medio entre paradas por dia de la semana ------ NO

group_by(Busos, Dia_semana = wday(Busos$FECHAHORA_ENTRADA_AMBITO_PARADA, label = TRUE)) %>%
  summarise(TiempoMedioEntreParadas = median(FECHAHORA_FINAL_DESPLAZA_A_NEXTPARADA 
                                             - FECHAHORA_INICIO_DESPLAZA_A_NEXTPARADA))

#11. Distancia (km per linia) -------- OK

group_by(Busos, LINEA) %>% 
  summarise(Num_Busos = length(unique(CALCA)), Num_Parades = length(unique(PARADA)),
            Distancia_enKM = sum(LONGITUD, na.rm = TRUE)/1000) %>% 
  ggplot(aes(x=as.factor(1), y=Distancia_enKM)) + geom_boxplot() +
  coord_flip() + xlab("") + ylab("Distancia [Km]") + ggtitle("Distancia Recorrida") + 
  theme(axis.title.y=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank(), 
        panel.background = element_blank(), text = element_text(size=10), title = element_text(size= 10)) 

group_by(Busos, LINEA) %>% 
  summarise(Num_Busos = length(unique(CALCA)), Num_Parades = length(unique(PARADA)),
            Distancia_enKM = sum(LONGITUD, na.rm = TRUE)/1000) -> bla

summary(bla$Distancia_enKM)

#12. Tiempo que tardan en llegar a parada terminal NO     HACER: DESDE LA PRIMERA VEZ QUE SE MUEVE HASTA PRIMERA VEZ PARADA TERMINAL Y HACER MEDIA DE TODAS LAS VECES.

group_by(Busos, CALCA,Dia_semana = wday(Busos$FECHAHORA_ENTRADA_AMBITO_PARADA, label = TRUE)) %>%
  summarise(Distancia_enKM = sum(LONGITUD, na.rm = TRUE)/1000, Tiempo = max(FECHAHORA_ENTRADA_AMBITO_PARADA)
            - min(FECHAHORA_ENTRADA_AMBITO_PARADA))

#Agafar valor m�nim de temps i restar-ho al temps de parada terminal. Mediana amb tots autobusos mateixa linea?Mitjana?
#   Separar si cap de setmana i dia normal? NO

group_by(Busos, Linea = LINEA, Dia_semana = wday(Busos$FECHAHORA_ENTRADA_AMBITO_PARADA, label = TRUE)) %>%
  summarise(n())


#13. Num lineas dif que recorre 1 bus en concreto. (AGRUPARLOS POR NUMEROS IGUALES) NO

group_by(Busos, Autobus = CALCA) %>%
  summarise(NumLineas = length(unique(LINEA))) %>%
  ggplot(aes(y =NumLineas, x = reorder(Autobus, - NumLineas))) + geom_bar(stat="identity")

