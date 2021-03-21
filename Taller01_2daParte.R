###Cargamos e instalamos las librerias  ------------------------

if (! ('pacman' %in% installed.packages())) install.packages('pacman')
pacman::p_load(tidyverse, OECD, ggthemes, entsoeapi, zoo, covid19mobility, xlsx)


###Datos de movilidad (google)-------------

load("movilidad.RData")

#Paises "DE","DK","FR","IT","ES","PT","NL","AT","GR","GB"

pais <- movilidad %>%
  dplyr::filter(location_code == "ES") %>% 
  pull(location) %>% 
  unique()

movilidad %>%
  dplyr::filter(location_code == "ES") %>% 
  mutate(desplazamiento = case_when(data_type == "retail_and_recreation_perc_ch" ~ "Detal y Recreación",
                                    data_type == "grocery_and_pharmacy_perc_ch" ~ "Supermercados y Farmacias",
                                    data_type == "parks_perc_ch" ~ "Parques",
                                    data_type == "transit_stations_perc_ch" ~ "Estaciones Transporte",
                                    data_type == "workplaces_perc_ch" ~ "Trabajo",
                                    data_type == "residential_perc_ch" ~ "Residencia" 
  )) %>% 
  ggplot(aes(x = date, y = value)) +
  facet_wrap(~desplazamiento) +
  geom_line(colour="#000099") +
  theme_economist_white(gray_bg = FALSE)+
  labs(title = "Datos de Movilidad", subtitle = pais)+
  xlab("Fecha")+
  ylab("Variación % (fecha de referencia)")


#Guardamos los datos en Excel 

movilidad %>%
  dplyr::filter(location_code == "ES") %>% 
  mutate(desplazamiento = case_when(data_type == "retail_and_recreation_perc_ch" ~ "Detal y Recreación",
                                    data_type == "grocery_and_pharmacy_perc_ch" ~ "Supermercados y Farmacias",
                                    data_type == "parks_perc_ch" ~ "Parques",
                                    data_type == "transit_stations_perc_ch" ~ "Estaciones Transporte",
                                    data_type == "workplaces_perc_ch" ~ "Trabajo",
                                    data_type == "residential_perc_ch" ~ "Residencia" 
  )) %>%
  select(date,value,desplazamiento) %>%
  pivot_wider(names_from = desplazamiento) %>% 
  as.data.frame() %>%
  write.xlsx2(row.names = FALSE,file = "movilidad.xlsx")


###Datos de movilidad (Apple)-------------

load("movilidad_apple.RData")

pais <- movilidad_apple %>%
  dplyr::filter(location_code == "ES") %>% 
  pull(location) %>% 
  unique()

movilidad_apple %>%
  dplyr::filter(location_code == "ES") %>% 
  mutate(desplazamiento = case_when(data_type == "driving_req_rel_volume" ~ "Dezplazamiento coche",
                                    data_type == "walking_req_rel_volume" ~ "Dezplazamiento caminando",
                                    data_type == "transit_req_rel_volume" ~ "Desplzamiento transporte"
  )) %>% 
  ggplot(aes(x = date, y = value)) +
  facet_wrap(~desplazamiento) +
  geom_line(colour="#000099") +
  theme_economist_white(gray_bg = FALSE)+
  labs(title = "Datos de Movilidad", subtitle = pais)+
  xlab("Fecha")+
  ylab("Variación % (fecha de referencia)")


#Guardamos los datos en Excel 

movilidad_apple %>%
  dplyr::filter(location_code == "ES") %>% 
  mutate(desplazamiento = case_when(data_type == "driving_req_rel_volume" ~ "Dezplazamiento coche",
                                    data_type == "walking_req_rel_volume" ~ "Dezplazamiento caminando",
                                    data_type == "transit_req_rel_volume" ~ "Desplzamiento transporte"
  )) %>% 
  select(date,value,desplazamiento) %>%
  pivot_wider(names_from = desplazamiento) %>% 
  as.data.frame() %>%
  write.xlsx2(row.names = FALSE,file = "movilidad_apple.xlsx")



###Datos Entsoe (Demanda electrica) -----------

load("EntsoePaises.RData")

#Paises "Spain", "France", "Italy", "Germany", "Portugal", "United Kingdom", "Austria", "Belgium", "Netherlands", "Denmark"

EntsoePaises$ %>%
  mutate(dt = lubridate::floor_date(dt, "hours")) %>%
  group_by(dt) %>%
  summarise(quantity = mean(quantity))%>%
  ungroup() %>%
  mutate(dt = lubridate::floor_date(dt, "days")) %>%
  group_by(dt) %>%
  summarise(quantity = mean(quantity)) %>% #Hasta aca datos crudos
  mutate(media_movil = zoo::rollmean(quantity, k = 7, fill = NA)) %>% #Acá se saca la media movil
  dplyr::filter(!is.na(media_movil)) %>% 
  ggplot(aes(dt, media_movil)) + geom_line()+
  labs(title = "Demanda eléctrica")+
  xlab("Fecha")+
  ylab("Media Móvil (7 días)")

#Guardamos los datos en Excel 

EntsoePaises$ %>% 
  select(dt,quantity) %>% 
  rename(Fecha=dt, Demanda=quantity) %>%
  as.data.frame() %>%
  write.xlsx2(row.names = FALSE,file = "DemandaElectrica.xlsx")

###Datos PIB (OECD) -----------

#Cargamos los datos

load("PIB_OECD02.RData")

# "DNK" "NLD" "PRT" "DEU" "AUT" "ESP" "JPN" "FRA" "ITA" "GBR" "CAN" "USA" "BEL"

PIB_OECD02 %>% 
  dplyr::filter(LOCATION == "ESP" &  MEASURE == "VOB") %>% 
  select(obsTime, obsValue, label) %>% 
  pivot_wider(names_from = label, values_from = obsValue)%>%
  as.data.frame() %>%
  write.xlsx2(row.names = FALSE,file = "PIBcomponentes.xlsx")

###Datos IPC (OECD) -----------

#Cargamos los datos

load("IPC_OECD2.RData")

# "DNK" "NLD" "PRT" "DEU" "AUT" "ESP" "JPN" "FRA" "ITA" "GBR" "CAN" "USA" "BEL"

#Guardamos los datos en Excel 

IPC_OECD2 %>% 
  dplyr::filter(LOCATION == "ESP") %>% 
  select(obsTime, obsValue, UNIT) %>% 
  pivot_wider(names_from = UNIT, values_from = obsValue)%>%
  rename(Indice=IDX, Inflacion=PC) %>% 
  as.data.frame() %>%
  write.xlsx2(row.names = FALSE,file = "IPC_Inflacion.xlsx")

###Datos PIB por sectores (OECD) -----------

#Cargamos los datos

load("PIB_SECTOR_OECD2.RData")

# "DNK" "NLD" "PRT" "DEU" "AUT" "ESP" "JPN" "FRA" "ITA" "GBR" "CAN" "USA" "BEL"

#Colocamos los datos en formato wide
PIB_SECTOR_OECD3 <-  PIB_SECTOR_OECD2 %>% 
  dplyr::filter(LOCATION=="ESP") %>% 
  select(obsTime,obsValue,label) %>% 
  pivot_wider(names_from = label, values_from=obsValue)

#Guardamos en excel
write.xlsx2(as.data.frame(PIB_SECTOR_OECD3),
            row.names = FALSE,
            append = TRUE,
            sheetName = "niveles",
            file = "PIB_SECTOR.xlsx")

#Calculamos la variacion porcentual de la tabla entera:
#- Seleccionamos toda la tabla excepto la columna de fecha
#- Convertimos la sub-tabla anteior a ts (objeto series de tiempo)
#- Calculamos la variación porcentual
#La base de este calculo es sacado de https://stackoverflow.com/questions/14614710/calculate-percentage-change-in-an-r-data-frame

PIB_SECTOR_OECD4 <-  (ts(PIB_SECTOR_OECD3[2:ncol(PIB_SECTOR_OECD3)])/stats::lag(ts(PIB_SECTOR_OECD3[2:ncol(PIB_SECTOR_OECD3)]),-1) - 1)*100

#Renombramos las columnas de la nueva tabla con los nombres de la tabla original (al convertirlo a ts se han cambiado los nombres de las columnas)
colnames(PIB_SECTOR_OECD4) <-  colnames(PIB_SECTOR_OECD3)[2:length(colnames(PIB_SECTOR_OECD3))]

#Añadimos la columna de fecha
#Para poder hacer el bind convertimos la tabla anterior a tibble (era un ts)
PIB_SECTOR_OECD4 <-  bind_cols(PIB_SECTOR_OECD3[2:nrow(PIB_SECTOR_OECD3),1],as_tibble(PIB_SECTOR_OECD4))

#Guardamos los datos en Excel 
write.xlsx2(as.data.frame(PIB_SECTOR_OECD4),
            row.names = FALSE,
            sheetName = "variaciones",
            append = TRUE,
            file = "PIB_SECTOR.xlsx")


###Datos PIB por sectores (OECD) -----------

#Cargamos los datos

load("IPC_OECD_RUBRO_NIVEL.RData")
load("IPC_OECD_RUBRO_INFLACION.RData")

# "DNK" "NLD" "PRT" "DEU" "AUT" "ESP" "JPN" "FRA" "ITA" "GBR" "CAN" "USA" "BEL"

#Colocamos los datos en formato wide
IPC_OECD_RUBRO_NIVEL2 <- IPC_OECD_RUBRO_NIVEL %>%
  dplyr::filter(LOCATION=="ESP") %>% 
  select(obsTime,obsValue,rubro) %>% 
  pivot_wider(names_from = rubro, values_from=obsValue)
  
IPC_OECD_RUBRO_INFLACION2 <- IPC_OECD_RUBRO_INFLACION %>%
  dplyr::filter(LOCATION=="ESP") %>% 
  select(obsTime,obsValue,rubro) %>% 
  pivot_wider(names_from = rubro, values_from=obsValue)

#Guardamos los datos en Excel 
write.xlsx2(as.data.frame(IPC_OECD_RUBRO_NIVEL2),
            row.names = FALSE,
            append = TRUE,
            sheetName = "niveles",
            file = "IPC_SECTOR.xlsx")

write.xlsx2(as.data.frame(IPC_OECD_RUBRO_INFLACION2),
            row.names = FALSE,
            append = TRUE,
            sheetName = "inflacion",
            file = "IPC_SECTOR.xlsx")

