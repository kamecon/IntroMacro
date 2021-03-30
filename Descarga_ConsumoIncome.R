if (! ('pacman' %in% installed.packages())) install.packages('pacman')
pacman::p_load(tidyverse, OECD)


renta_sucio <- get_data_structure("NAAG")

filtro <- list( c("CAN","FRA","DEU","ITA","JPN","PRT","ESP","GBR","USA", "GBR", "NLD", "DNK", "AUS", "AUT", "BEL"),
                c("B5NG", "P31S14_S15G")
)

#Descargamos los datos

RENTA_OECD <- get_dataset(dataset = "NAAG", filter = filtro)

RENTA_OECD02 <- RENTA_OECD %>% 
  left_join(renta_sucio$INDICATOR, by = c("INDICATOR"="id"))

save(RENTA_OECD02, file = "RENTA_OECD02.RData")
