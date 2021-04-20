pacman::p_load(readxl, tidyverse, rdbnomics)

### Datos AMECO con Excel (no incluye ni JPN ni CAN) --------

brechaPIB <- read_excel("brechaPIB.xlsx",
                        col_types = c("text", "text", "text",
                                      "text", "text", "numeric", "numeric",
                                      "numeric", "numeric", "numeric",
                                      "numeric", "numeric", "numeric",
                                      "numeric", "numeric", "numeric",
                                      "numeric", "numeric", "numeric",
                                      "numeric", "numeric", "numeric",
                                      "numeric", "numeric", "numeric",
                                      "numeric"))

brechaPIB1 <- brechaPIB %>%
  select(-c(Variable, `Unit/Description`, Unit, Year )) %>%
  pivot_longer(names_to = "Año", values_to="Brecha", !Country)

save(brechaPIB1, file = "brechaPIB1.RData")

### Datos DBnomics con WEO (incluye Japón y Canadá pero no media EU) --------

# Creamos una "mascara" para pasarla por el buscador e indicarle los paises y la variable que buscamos
# Quedaría asi: pais01+pais02+pais03.concepto.unidades

country = c("CAN","FRA","DEU","ITA","JPN","PRT","ESP","GBR","USA", "GBR", "NLD", "DNK", "AUS", "AUT", "BEL")
indicator=c("NGAP_NPGDP.pcent_potential_gdp")

mascara <- paste(country, collapse = "+") %>%
  paste(indicator,sep = ".")

gap01 <- rdb(provider_code = "IMF", dataset_code =  "WEO:2021-04", mask = mascara)

brechaPIB2 <-  gap01 %>%
  select(original_period,`weo-country`,value) %>%
  dplyr::filter(original_period>1999 & original_period<2021) %>% 
  as_tibble() %>% 
  rename(country=`weo-country`)

save(brechaPIB2, file = "brechaPIB2.RData")
