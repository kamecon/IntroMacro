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

#Guardamos los datos en Excel 

load("PIB_OECD02.RData")

PIB_OECD02 %>% 
  dplyr::filter(LOCATION == "ESP" &  MEASURE == "VOB") %>% 
  select(obsTime, obsValue, label) %>% 
  pivot_wider(names_from = label, values_from = obsValue)%>%
  as.data.frame() %>%
  write.xlsx2(row.names = FALSE,file = "PIBcomponentes.xlsx")
