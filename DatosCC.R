
# Cargar librerias --------------------------------------------------------

pacman::p_load(IMFData, tidyverse, xlsx)


# Buscar codigos ----------------------------------------------------------


availableDB <- DataflowMethod()

availableDB %>% 
  dplyr::filter(str_detect(DatabaseID, "^BOP")) 

BOP.available.codes <- DataStructureMethod("BOP")

CodeSearch(BOP.available.codes, "CL_INDICATOR_BOP", "CA") %>% 
  head(10)


# Descarga de datos -------------------------------------------------------

databaseID <- "BOP"
startdate = "2001-01-01"
enddate = "2020-12-31"
checkquery = FALSE

queryfilter_dc <- list(CL_FREQ = "A",
                       CL_Country_PCTOT = c("AR","AT","AU","BE","BR","MX",
                                            "CN","DK","FR","IN","IT","JP",
                                            "DE","IE","NL","PT","CH","TR",
                                            "GB","US","UY","SE","PE","SA"),
                       CL_INDICATOR_BOP= "BCA_BP6_USD")

CAquery_dc <- CompactDataMethod(databaseID, queryfilter_dc, startdate, enddate, 
                                checkquery)


# Convertir lista en data frame -------------------------------------------

#Creamos un vector con el nombre de los paises para utilizarlo para nombrar los
#data frames de la lista

paises <- CAquery_dc$`@REF_AREA` %>% as.vector()

names(CAquery_dc$Obs) <- paises

#Creamos una lista de paises para formar los grupos
paises2 <- BOP.available.codes$CL_AREA_BOP %>%
  dplyr::filter(CodeValue %in% paises)

write.xlsx(x = paises2, file = "paises.xlsx", row.names = FALSE)


#Creamos el data framne

data_ca <- Map(cbind,  CAquery_dc$Obs,
               country = CAquery_dc$`@REF_AREA` %>% as.list()
              ) %>% #En cada df de la lista le añadimos una columna con el pais
  bind_rows() %>%
  as_tibble() %>% 
  select(-`@OFFICIAL_BPM`) %>% 
  rename(time=`@TIME_PERIOD`, value=`@OBS_VALUE` ) %>%
  mutate(value=as.numeric(value)) %>%
  mutate(time=as.numeric(time))


# Guaradar los datos ------------------------------------------------------

save(data_ca, file = "data_ca.RData")
