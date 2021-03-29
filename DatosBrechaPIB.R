pacman::p_load(readxl, tidyverse)

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
