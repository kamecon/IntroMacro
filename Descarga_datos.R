###Cargamos e instalamos las librerias  ------------------------

if (! ('pacman' %in% installed.packages())) install.packages('pacman')
pacman::p_load(tidyverse, OECD, ggthemes, entsoeapi, zoo, covid19mobility, xlsx)

###Datos PIB (OECD) ------------------------

#Creamos un filtro con los paises y conceptos que deseamos descargar

filtro <- list( c("CAN","FRA","DEU","ITA","JPN","PRT","ESP","GBR","USA", "GBR", "NLD", "DNK", "AUS", "AUT", "BEL"),
                c("B1_GE","P3_P5","P3","P31S14_S15","P3S13","P5","P51","P52_P53","B11","P6","P7"),
                c("C","VOB","G"),
                c("1995", "1996", "1997", "1998", "1999", "2000", "2001", "2002", "2003", "2004", "2005", "2006", "2007", "2008", "2009", "2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018", "2019","2020")
)


#Vemos la estructura de la tabla o base de datos que deseamos descargar

PIB_OECD_sucio <- get_data_structure("SNA_TABLE1")

#Descargamos los datos

PIB_OECD <- get_dataset(dataset = "SNA_TABLE1", filter = filtro)

#Hacemos un join (fusión de tablas) para poder tener una columna con el nombre de los conceptos asociados a cada código

PIB_OECD02 <- PIB_OECD %>% 
  left_join(PIB_OECD_sucio$TRANSACT, by = c("TRANSACT"="id"))

#Convertimos la fecha en numérico

PIB_OECD02$obsTime <- as.numeric(PIB_OECD02$obsTime)

#Guardamos los datos para poder acceder a ellos luego

save(PIB_OECD02, file = "PIB_OECD02.RData")


###Datos Movilidad (Google) ------------------------

#Descargamos los datos

goog <- refresh_covid19mobility_google_country()

#Filtramos los datos y nos quedamos con los paises que nos interesan

movilidad <- goog %>% 
  dplyr::filter(location_code %in% c("DE","DK","FR","IT","ES","PT","NL","AT","GR","GB"))

#Guardamos los datos para poder acceder a ellos luego

save(movilidad, file = "movilidad.RData")

###Datos Movilidad (Apple) ------------------------

#Descargamos los datos

apple <- refresh_covid19mobility_apple_country()


#Filtramos los datos y nos quedamos con los paises que nos interesan

movilidad_apple <- apple %>% 
  dplyr::filter(location_code %in% c("DE","DK","FR","IT","ES","PT","NL","AT","GR","GB"))

#Guardamos los datos para poder acceder a ellos luego

save(movilidad_apple, file = "movilidad_apple.RData")


###Datos Entsoe (Demanda electrica) ------------------------

#Seleccionamos los paises. Creamos dos vectores: uno con el nombre de los paises y otro con los códigos usados por Entsoe

paises <- en_eic() %>% 
  filter(AreaTypeCode == "CTY" & AreaName %in% c("Spain", "France", "Italy", "Germany", "Portugal", "United Kingdom", "Austria", "Belgium", "Netherlands", "Denmark")) %>% 
  select(AreaCode, AreaName)

paises_cod <- paises %>% 
  pull(1)

paises_name <- paises %>% 
  pull(2)


#Creamos una lista para rellenar con la información de los paises

EntsoePaises <- list()

#Aplicamos un bucle (loop) para descargar los datos de Entsoe y guardarlos en la lista

for (i in seq_along(paises_cod)) {
  
  EntsoePaises[[paises_name[i]]] <- en_load_actual_total_load(eic = paises_cod[i],
                                                              period_start = lubridate::ymd(as.character(Sys.Date() - 370), tz = "UTC"),
                                                              period_end = lubridate::ymd_hm(paste0(Sys.Date() - 1," 23:00"), tz = "UTC"),
                                                              security_token = "cd15b9e9-ac50-4a2e-b949-1fe1583859f7"
  )
  
}

#Guardamos los datos para poder acceder a ellos luego

save(EntsoePaises, file = "EntsoePaises.RData")


###Datos IPC (OECD) ------------------------

#Vemos la estructura de la tabla o base de datos que deseamos descargar

tabla_IPC <- get_data_structure("PRICES_CPI")

# meses <- cross2(seq(1995,2020), 
#                 0 %>%
#                   paste0(seq(1,9)) %>%
#                   c("11","12")
# ) %>% map_chr(paste, sep = "-", collapse = "-")

#Creamos un filtro con los paises y conceptos que deseamos descargar

filtro <- list( c("CAN","FRA","DEU","ITA","JPN","PRT","ESP","GBR","USA", "GBR", "NLD", "DNK", "AUS", "AUT", "BEL"),
                c("CPALTT01"),
                c("IXOB", "GY"),
                c("M")
)



IPC_OECD <- get_dataset(dataset = "PRICES_CPI", filter = filtro)

save(IPC_OECD,file = "IPC_OECD.RData")

IPC_OECD2 <- IPC_OECD %>%
  dplyr::filter(obsTime>="1995-01")

#Guardamos los datos para poder acceder a ellos luego

save(IPC_OECD2,file = "IPC_OECD2.RData")


filtro2 <- list( c("CAN","FRA","DEU","ITA","JPN","PRT","ESP","GBR","USA", "GBR", "NLD", "DNK", "AUS", "AUT", "BEL"),
                 c("CP010000","CP020000","CP030000","CP040000","CP050000","CP060000","CP070000","CP080000","CP090000","CP100000","CP110000","CP120000","CPSDCTGY"),
                c("IXOB", "GY"),
                c("M")
)

IPC_OECD_RUBRO <- get_dataset(dataset = "PRICES_CPI", filter = filtro2)

#Hacemos un join para que aparezcan los nombres de los conceptos en lugar de solo los id's
#Asimismo, seleccionamos las columnas de interes y nos quedamos con los años de 1995 en adelante

IPC_OECD_RUBRO2 <- IPC_OECD_RUBRO %>% 
  left_join(tabla_IPC$SUBJECT, by = c("SUBJECT"="id")) %>%
  separate(col = label, sep = "- ",into = c("cod","rubro")) %>%  #separar en la definicion de rubro el codigo del nombre
  select(LOCATION, MEASURE, obsTime, obsValue, rubro) %>% 
  dplyr::filter(obsTime >= "1995-01")

#IPC por rubros
IPC_OECD_RUBRO_NIVEL <- IPC_OECD_RUBRO2 %>% 
  dplyr::filter(MEASURE == "IXOB")

#Inflacion por rubros
IPC_OECD_RUBRO_INFLACION <- IPC_OECD_RUBRO2 %>% 
  dplyr::filter(MEASURE == "GY")

#Guardamos los datos para poder acceder a ellos luego

save(IPC_OECD_RUBRO_NIVEL,file = "IPC_OECD_RUBRO_NIVEL.RData")
save(IPC_OECD_RUBRO_INFLACION,file = "IPC_OECD_RUBRO_INFLACION.RData")
