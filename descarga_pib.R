if (! ('pacman' %in% installed.packages())) install.packages('pacman')
pacman::p_load(tidyverse, OECD, ggthemes)

PIB_OECD_sucio <- get_data_structure("SNA_TABLE1")

# filtro <- list( c("CAN","FRA","DEU","ITA","JPN","PRT","ESP","GBR","USA"),
#                 c("B1_GE","P3_P5","P3","P31S14_S15","P3S13","P5","P51","P52_P53","B11","P6","P7"),
#                 c("C","VOB","G")
#                 )

filtro <- list( c("CAN","FRA","DEU","ITA","JPN","PRT","ESP","GBR","USA", "GBR", "NLD", "DNK", "AUS", "AUT", "BEL"),
                c("B1_GE","P3_P5","P3","P31S14_S15","P3S13","P5","P51","P52_P53","B11","P6","P7"),
                c("C","VOB","G"),
                c("1995", "1996", "1997", "1998", "1999", "2000", "2001", "2002", "2003", "2004", "2005", "2006", "2007", "2008", "2009", "2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018", "2019","2020")
)



PIB_OECD <- get_dataset(dataset = "SNA_TABLE1", filter = filtro)

PIB_OECD02 <- PIB_OECD %>% 
  left_join(PIB_OECD_sucio$TRANSACT, by = c("TRANSACT"="id"))

PIB_OECD02$obsTime <- as.numeric(PIB_OECD02$obsTime)

save(PIB_OECD02, file = "PIB_OECD02.RData")

load("PIB_OECD02.RData")

measure.labs <- c("Nominal", "Real")
names(measure.labs) <- c("C","VOB")

PIB_OECD02 %>% dplyr::filter(LOCATION == "CAN" & MEASURE %in% c("C", "VOB") & label == "Gross domestic product (expenditure approach)"  & obsTime >=1995 ) %>%
  ggplot(aes(x=obsTime, y= obsValue, group=1))+
  geom_line(colour="#000099")+
  facet_wrap(~MEASURE,nrow = 2, labeller = labeller(MEASURE = measure.labs))+
  #theme_economist_white(gray_bg = FALSE)+
  labs(title = "PIB nominal y real")+
  xlab("Fecha")+
  ylab("PIB")


transact.labs <- c("Consumo privado", "Consumo público", "Inversión", "XN")
names(transact.labs) <- c("P31S14_S15", "P3S13", "P5", "B11")

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


transact.labs <- c("Consumo privado", "Consumo público", "Inversión", "XN")
names(transact.labs) <- c("P31S14_S15", "P3S13", "P5", "B11")

PIB_OECD02 %>% dplyr::filter(LOCATION == "CAN" & TRANSACT %in% c("P31S14_S15", "P3S13", "P5", "B11") & MEASURE ==  "G" & obsTime >=1995 ) %>%
  ggplot(aes(x=obsTime, y= obsValue, group=1))+
  geom_line(colour="#000099")+
  facet_wrap(~TRANSACT, labeller = labeller(TRANSACT=transact.labs), scales = "free")+
  #theme_economist_white(gray_bg = FALSE)+
  labs(title = "Componentes del PIB")+
  xlab("Fecha")+
  ylab("Componentes")+
  scale_x_continuous(breaks = scales::breaks_width(2))


