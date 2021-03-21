###Carga de librerias --------------

if (! ('pacman' %in% installed.packages())) install.packages('pacman')
pacman::p_load(tidyverse, OECD, ggthemes,xlsx)

###Descarga datos PIB (nominal y real) por componentes de demanda --------------

#Se revisa la estructura de la base de datos de PIB

PIB_OECD_sucio <- get_data_structure("SNA_TABLE1")


#Creamos el filtro basado en la estructura de PIB_OECD_sucio$VAR_DESC

filtro <- list( c("CAN","FRA","DEU","ITA","JPN","PRT","ESP","GBR","USA", "GBR", "NLD", "DNK", "AUS", "AUT", "BEL"),
                c("B1_GE","P3_P5","P3","P31S14_S15","P3S13","P5","P51","P52_P53","B11","P6","P7"),
                c("C","VOB","G"),
                c("1995", "1996", "1997", "1998", "1999", "2000", "2001", "2002", "2003", "2004", "2005", "2006", "2007", "2008", "2009", "2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018", "2019","2020")
)

#Descargamos los datos

PIB_OECD <- get_dataset(dataset = "SNA_TABLE1", filter = filtro)

#Hacemos un join para que aparezcan los nombres de los conceptos en lugar de solo los id's

PIB_OECD02 <- PIB_OECD %>% 
  left_join(PIB_OECD_sucio$TRANSACT, by = c("TRANSACT"="id"))

#convertimos la fecha en numérico
PIB_OECD02$obsTime <- as.numeric(PIB_OECD02$obsTime)

#Guardamos los datos
save(PIB_OECD02, file = "PIB_OECD02.RData")


###Descarga datos PIB (real) por actividad economica --------------

#Se revisa la estructura de la base de datos de PIB
PIB_OECD_sucio2 <- get_data_structure("SNA_TABLE6A")

#Cargamos los datos de los códigos de las actividades (obtenidos de la dirección XML de las opciones de descarga de la web de la OECD)
actividades <- read.csv("pelo.txt", header=FALSE)

#La convertimos en un vector
actividades2 <- actividades[1,] %>%
  as.vector() %>%
  as.character()

#Definimos el filtro basado en la estructura de PIB_OECD_sucio$ACTIVITY
filtro2 <- list(c("CAN","FRA","DEU","ITA","JPN","PRT","ESP","GBR","USA", "GBR", "NLD", "DNK", "AUS", "AUT", "BEL"),
                c("B1GA"),
                actividades2,
                c("VOB")
)

#Descargamos los datos
PIB_SECTOR_OECD <- get_dataset(dataset = "SNA_TABLE6A", filter = filtro2)

#Hacemos un join para que aparezcan los nombres de los conceptos en lugar de solo los id's
PIB_SECTOR_OECD2 <- PIB_SECTOR_OECD %>% 
  left_join(PIB_OECD_sucio2$ACTIVITY, by = c("ACTIVITY"="id"))

#convertimos la fecha en numérico
PIB_SECTOR_OECD2$obsTime <- as.numeric(PIB_SECTOR_OECD2$obsTime)

#Guardamos los datos
save(PIB_SECTOR_OECD2, file = "PIB_SECTOR_OECD2.RData")


###Algunos graficos --------------

#Cargamos los datos
load("PIB_OECD02.RData")

#Cambiamos los id's de medida por sus nombres para que aparezcan en los gráficos
measure.labs <- c("Nominal", "Real")
names(measure.labs) <- c("C","VOB")

transact.labs <- c("Consumo privado", "Consumo público", "Inversión", "XN")
names(transact.labs) <- c("P31S14_S15", "P3S13", "P5", "B11")

#Graficos

PIB_OECD02 %>% dplyr::filter(LOCATION == "CAN" & MEASURE %in% c("C", "VOB") & label == "Gross domestic product (expenditure approach)"  & obsTime >=1995 ) %>%
  ggplot(aes(x=obsTime, y= obsValue, group=1))+
  geom_line(colour="#000099")+
  facet_wrap(~MEASURE,nrow = 2, labeller = labeller(MEASURE = measure.labs))+
  #theme_economist_white(gray_bg = FALSE)+
  labs(title = "PIB nominal y real")+
  xlab("Fecha")+
  ylab("PIB")

PIB_OECD02 %>% dplyr::filter(LOCATION == "CAN" & TRANSACT %in% c("P31S14_S15", "P3S13", "P5", "B11") & MEASURE ==  "VOB" & obsTime >=1995 ) %>%
  ggplot(aes(x=obsTime, y= obsValue, group=1))+
  geom_line(colour="#000099")+
  facet_wrap(~TRANSACT, labeller = labeller(TRANSACT=transact.labs), scales = "free")+
  #theme_economist_white(gray_bg = FALSE)+
  labs(title = "Componentes del PIB")+
  xlab("Fecha")+
  ylab("Componentes")+
  scale_x_continuous(breaks = scales::breaks_width(2))

PIB_OECD02 %>% dplyr::filter(LOCATION == "CAN" & MEASURE == "G" & label == "Gross domestic product (expenditure approach)"  & obsTime >=1995 ) %>%
  ggplot(aes(x=obsTime, y= obsValue, group=1))+
  geom_line(colour="#000099")+
  labs(title = "Variación PIB real")+
  xlab("Fecha")+
  ylab("PIB")

PIB_OECD02 %>% dplyr::filter(LOCATION == "CAN" & TRANSACT %in% c("P31S14_S15", "P3S13", "P5", "B11") & MEASURE ==  "G" & obsTime >=1995 ) %>%
  ggplot(aes(x=obsTime, y= obsValue, group=1))+
  geom_line(colour="#000099")+
  facet_wrap(~TRANSACT, labeller = labeller(TRANSACT=transact.labs), scales = "free")+
  #theme_economist_white(gray_bg = FALSE)+
  labs(title = "Componentes del PIB")+
  xlab("Fecha")+
  ylab("Componentes")+
  scale_x_continuous(breaks = scales::breaks_width(2))
