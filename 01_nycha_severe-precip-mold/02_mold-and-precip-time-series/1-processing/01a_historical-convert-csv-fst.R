
require(data.table)
require(fst)

setwd("/Users/ninaflores/Desktop/projects/Mattlab/F31/Aim 1 - Severe weather and mold work orders/data/DAYMET-2016-2018/historical")
nycha_daymet <- fread("daymet-nycha-historical.csv")
write.fst(nycha_daymet, "daymet-nycha-historical-bin.fst")

nycha_daymet %>% select(tds_nmb, bldng_n) %>% n_distinct()
