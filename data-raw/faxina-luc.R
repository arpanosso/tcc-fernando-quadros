library(tidyverse)
df<-readxl::read_xlsx("data-raw/data_set_luc.xlsx")
glimpse(df)
write_rds(df,"data/luc.rds")
