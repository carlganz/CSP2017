library(haven)
library(dplyr)

chis <- read_sas("child.dta")

names(chis) <- toupper(names(chis))

chis <- chis %>% select_(~CA6, ~SRAGE)

saveRDS(chis, "chis.rds")