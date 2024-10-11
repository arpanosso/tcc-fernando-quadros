# # Análise do Trabalho de Conclusão de Curso:
# ## Séries temporais de xCO2 no estado de São Paulo
#
# ### Pacotes necessários
library(tidyverse)
library(geobr)

# Filtragem para estado de SP ---------------------------------------------
# ### Base de dados inicial
# data_set <- read_rds("data/nasa-xco2.rds")
# data_set <- data_set |>
#   filter(xco2 > 0) |>
#   mutate(
#     path = str_remove(path, "data-raw/nc4/|\\.nc4"),
#     date = as_date(str_sub(path,12,17)),
#     year = year(date),
#     month = month(date),
#     day = day(date),
#     .after = "time"
#   )
# glimpse(data_set)
#
#
# ### Plotando os pontos
# data_set |>
#   filter(year == 2019) |>
#   ggplot(aes(x = longitude, latitude)) +
#   geom_point()
#
#
#
# ## Função para classificação do ponto
# def_pol <- function(x, y, pol){
#   as.logical(sp::point.in.polygon(point.x = x,
#                                   point.y = y,
#                                   pol.x = pol[,1],
#                                   pol.y = pol[,2]))
# }
#
#
#
# ## Retirando os polígono
# estados <- geobr::read_state(showProgress = FALSE)
# pol_sp <- estados$geom |> pluck(20) |> as.matrix()
# plot(pol_sp)
#
#
# ## Classificando os pontos
# data_set_sp <- data_set |>
#   mutate(
#     flag_sp = def_pol(longitude, latitude, pol_sp)
#   ) |>
#   filter(flag_sp)
#
# data_set_sp |>
#   filter(year == 2022) |>
#   ggplot(aes(x = longitude, latitude)) +
#   geom_point()
#
#
# ## Avaliando o sinal de XCO2
# write_rds(data_set_sp,"data/nasa-xco2-sp.rds")

#CArrega a base filtrada
data_set_sp <- read_rds("data/nasa-xco2-sp.rds") |>
  select(-(flag_br:flag_sp),-path) |>
  filter(
    xco2_quality_flag == 0,
    year >= 2015 & year <= 2023
    ) |>
  mutate(
    season = ifelse(month <=2 | month >=9,"rainy","dry")
  )

# Resumo da base
glimpse(data_set_sp)

# PEGAR ALGUM PARAGRAFO EXPLICANDO SOBRE A INCERTEZA (FLAG = 0)
data_set_sp |>
  filter(year == 2015,
         season == "rainy") |>
  ggplot(aes(x = longitude, latitude)) +
  geom_point()

# Criando a estação por ano
season_year <- vector()
month <- data_set_sp |> pull(month)
year <- data_set_sp |> pull(year)
for( i in 1:nrow(data_set_sp)) {
  if(month[i] <= 2){
    season_year[i] <- paste0("rainy_",year[i]-1,"-",year[i])
  }else if(month[i] > 2 & month[i] < 9){
    season_year[i] <- paste0("dry_",year[i])
  } else{
    season_year[i] <- paste0("rainy_",year[i],"-",year[i]+1)
  }
}
data_set_sp$season_year <- season_year
glimpse(data_set_sp)

# contar as categorias criadas
data_set_sp |>
  pull(season_year) |>
  unique()

#Gráficos
data_set_sp |>
  group_by(season_year) |>
  summarise(
    xco2_mean = mean(xco2, na.rm = TRUE)
  ) |>
  ggplot(aes(x=season_year,y=xco2_mean)) +
  geom_col()

data_set_sp %>%
  ggplot(aes(x=xco2,fill=season_year)) +
  geom_histogram(color="black",
                 bins = 30)






