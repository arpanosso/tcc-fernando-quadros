library(tidyverse)
df<-readxl::read_xlsx("data-raw/data_set_luc.xlsx")
glimpse(df)
write_rds(df,"data/luc.rds")

library(tidyverse)
df<-readxl::read_xlsx("data-raw/grid-kgr_luc.xlsx") |>
  janitor::clean_names() |>
  rename(id=x1)
glimpse(df)

df_l <- df |>
  mutate(across(brasil_cov_2015:brasil_cov_desc_2023,as.character)) |>
  pivot_longer(cols = starts_with("brasil_cov_"),
               names_to = "id_cov", values_to = "cov") |>
  mutate(
    year = str_extract(id_cov,"[0-9]+") |> as.numeric(),
    id_cov = str_extract(id_cov,"[^0-9]+")
  ) |>
  pivot_wider(names_from = id_cov, values_from = cov) |>
  mutate(brasil_cov_  = as.numeric(brasil_cov_ )) |>
  rename(cover = brasil_cov_,
         descricao = brasil_cov_desc_)
glimpse(df_l)
write_rds(df_l,"data/grid-kgr_luc.rds")
