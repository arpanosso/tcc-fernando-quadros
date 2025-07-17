
<!-- README.md is generated from README.Rmd. Please edit that file -->

## TCC - Fernando Quadros - Variação da concentração de CO<sub>2</sub> atmosférica no estado de São Paulo: Abordagem de Séries temporais e geoestatística

### 1) Preparo dos dados

#### Carregando pacotes

``` r
library(tidyverse)
library(ggridges)
library(ggpubr)
library(geobr)
library(gstat)
library(vegan)
source("r/my-function.R")
#> Polygons loaded [states, citysbiomes, conservarion and indigenous]
#> List of polygons loaded [list_pol]
```

#### Filtrando os dados para o estado de São Paulo

Carregando a base geral

``` r
# data_set <- read_rds("data/nasa-xco2.rds") |>
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
```

Corrigindo o polígono do Estado de São Paulo.

``` r
# pol_sp <- states |> filter(abbrev_state == "SP") |> 
#   pull(geom) |> 
#   pluck(1) |> 
#   as.matrix()
# pol_sp <- pol_sp[(183:4742),]
# pol_sp <- rbind(pol_sp, pol_sp[1,])
# # plot(pol_sp)
```

Classificando os pontos pertencentes ao estado de São Paulo e gerando um
arqui para o estado.

``` r
# data_set_sp <- data_set |>
#   mutate(
#     flag_sp = def_pol(longitude, latitude, pol_sp)
#   ) |>
#   filter(flag_sp)
# write_rds(data_set_sp,"data/nasa-xco2-sp.rds")
data_set_sp <- read_rds("data/nasa-xco2-sp.rds")
```

#### Existe uma tendência regional nos dados, e ela deve ser retirada para esse trabalho

``` r
# data_set_sp |>
#   sample_n(1000) |>
#   drop_na() |>
#   mutate( year = year - min(year)) |>
#   ggplot(aes(x=year, y=xco2)) +
#   geom_point() +
#   geom_point(shape=21,color="black",fill="gray") +
#   geom_smooth(method = "lm") +
#   stat_regline_equation(aes(
#   label =  paste(..eq.label.., ..rr.label.., sep = "*plain(\",\")~~"))) +
#   theme_bw() +
#   labs(x="Ano",y="xco2")
```

#### Análise de regressão linear simples para caracterização da tendência.

``` r
mod_trend_xco2 <- lm(xco2 ~ year, 
          data = data_set_sp |> 
            filter(xco2_quality_flag == 0) |> 
            drop_na() |> 
            mutate( year = year - min(year)) 
          )
mod_trend_xco2
#> 
#> Call:
#> lm(formula = xco2 ~ year, data = mutate(drop_na(filter(data_set_sp, 
#>     xco2_quality_flag == 0)), year = year - min(year)))
#> 
#> Coefficients:
#> (Intercept)         year  
#>     396.109        2.384
summary.lm(mod_trend_xco2)
#> 
#> Call:
#> lm(formula = xco2 ~ year, data = mutate(drop_na(filter(data_set_sp, 
#>     xco2_quality_flag == 0)), year = year - min(year)))
#> 
#> Residuals:
#>     Min      1Q  Median      3Q     Max 
#> -9.6590 -0.9532  0.0602  0.9916  7.5311 
#> 
#> Coefficients:
#>              Estimate Std. Error t value Pr(>|t|)    
#> (Intercept) 3.961e+02  1.505e-02   26317   <2e-16 ***
#> year        2.384e+00  2.848e-03     837   <2e-16 ***
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> 
#> Residual standard error: 1.523 on 37881 degrees of freedom
#> Multiple R-squared:  0.9487, Adjusted R-squared:  0.9487 
#> F-statistic: 7.006e+05 on 1 and 37881 DF,  p-value: < 2.2e-16
```

``` r
a_co2 <- mod_trend_xco2$coefficients[[1]]
b_co2 <- mod_trend_xco2$coefficients[[2]]

data_set_sp <- data_set_sp |>
  filter(xco2_quality_flag == 0,
         year >= 2015 & year <= 2023) |> 
  mutate(
    year_modif = year -min(year),
    xco2_est = a_co2+b_co2*year_modif,
    delta = xco2_est-xco2,
    xco2_detrend = (a_co2-delta) - (mean(xco2) - a_co2)
  ) |> 
  select(-c(flag_br:flag_sp, time, xco2_quality_flag,xco2_incerteza,
           path,year_modif:delta)) |> 
  rename(xco2_trend = xco2,
         xco2 = xco2_detrend) |> 
    mutate(
      season = ifelse(month <=2 | month >=9,"rainy","dry"),
      season_year = ifelse(month <= 2,
                           paste0("rainy_",year-2001,":",year-2000),
                           ifelse(month > 2 & month < 9,
                                  paste0("dry_",year-2000,":",year-1999),                                  paste0("rainy_",year-2000,":",year-1999))),
      epoch = str_remove(season_year,"dry_|rainy_")) |> 
    filter(season_year != "rainy_2014-2015",
         epoch != "14:15") |> 
  group_by(season_year) |> 
  mutate(
    xco2_anomaly = xco2 - median(xco2, na.rm = TRUE),
    .after = xco2
  ) |> 
  ungroup()
```

### 2) Análise Estatística Básica

``` r
data_set_sp %>%
  mutate(
  #   fct_year = fct_rev(as.factor(year)),
  #   classe = ifelse(tratamento ==
  #            "UC_desm" | tratamento == "TI_desm",
  #                   "Des","Con")
  ) %>%
  ggplot(aes(y=epoch)) +
  geom_density_ridges(rel_min_height = 0.03,
                      aes(x=xco2, fill=season),
                      alpha = .6, color = "black"
  ) +
  scale_fill_cyclical(values = c("#ff8080","#238B45"),
                      name = "classe", guide = "legend") +
  theme_ridges()
```

![](README_files/figure-gfm/unnamed-chunk-9-1.png)<!-- -->

``` r
data_set_sp %>%
  mutate(
  #   fct_year = fct_rev(as.factor(year)),
  #   classe = ifelse(tratamento ==
  #            "UC_desm" | tratamento == "TI_desm",
  #                   "Des","Con")
  ) %>%
  ggplot(aes(y=epoch)) +
  geom_density_ridges(rel_min_height = 0.03,
                      aes(x=xco2_anomaly, fill=season),
                      alpha = .6, color = "black"
  ) + 
  scale_fill_viridis_d(option = "inferno") +
  theme_ridges()
```

![](README_files/figure-gfm/unnamed-chunk-10-1.png)<!-- -->

``` r
data_set_sp |>
  group_by(season_year) |>
  summarise(
    N = length(xco2),
    MEAN = mean(xco2),
    MEDIAN = median(xco2),
    STD_DV = sd(xco2),
    SKW = agricolae::skewness(xco2),
    KRT = agricolae::kurtosis(xco2),
  ) |>
  writexl::write_xlsx("output/estat-desc.xlsx")

data_set_sp |>
  group_by(epoch, season) |> 
   # summarise(
   #  xco2 = mean(xco2)) |> 
  ggplot(aes(x=epoch, y=xco2, fill = season)) +
  # geom_col(position = "dodge") +
  geom_boxplot() +
  coord_cartesian(ylim = c(380,395))+
  theme_bw() +
  theme(legend.position = "bottom") +
  labs(fill="") +
  scale_fill_viridis_d(option = "mako") + 
  facet_wrap(~season)
```

![](README_files/figure-gfm/unnamed-chunk-11-1.png)<!-- -->

``` r
data_set_sp |>
  group_by(season_year) |>
  summarise(
    N = length(xco2_anomaly),
    MEAN = mean(xco2_anomaly),
    MEDIAN = median(xco2_anomaly),
    STD_DV = sd(xco2_anomaly),
    SKW = agricolae::skewness(xco2_anomaly),
    KRT = agricolae::kurtosis(xco2_anomaly),
  ) |>
  writexl::write_xlsx("output/estat-desc-anomaly.xlsx")

data_set_sp |>
  group_by(epoch, season) |> 
   # summarise(
   #  xco2 = mean(xco2)) |> 
  ggplot(aes(x=epoch, y=xco2_anomaly, fill = season)) +
  # geom_col(position = "dodge") +
  geom_boxplot() +
  # coord_cartesian(ylim = c(380,395))+
  theme_bw() +
  theme(legend.position = "bottom") +
  labs(fill="") +
  scale_fill_viridis_d(option = "inferno")
```

![](README_files/figure-gfm/unnamed-chunk-12-1.png)<!-- -->

### 3) Análise Geoestatística

A definição do grid a ser estimado para pontos não amostrados para o
estado de SP.

``` r
# vetores para coordenadas x e y selecionadas da base do IBGE1
# x<-pol_sp[,1]
# y<-pol_sp[,2]
# dis <- 0.05 # distância para o adensamento de pontos nos estados
# grid_geral <- expand.grid(
#   X=seq(min(x),max(x),dis), 
#   Y=seq(min(y),max(y),dis))
```

É necessário classificar cada ponto como pertencente ou não ao estado de
SP.

``` r
# grid_geral <- grid_geral |>
#    mutate(
#      flag = def_pol(X,Y,pol_sp)
#    ) |> 
#     filter(flag) |> 
#   select(-flag)
# 
# grid_geral |> 
#   ggplot(aes(X,Y)) +
#   geom_point()
```

``` r
# x_ge <- grid_geral$X
# y_ge <- grid_geral$Y
# v_city <- ""
# obj <- citys |> filter(abbrev_state == "SP")
# name_muni <- citys |> filter(abbrev_state == "SP") |> pull(name_muni)  
# for(i in 1:nrow(grid_geral)){  
#   for(j in seq_along(name_muni)){
#     pol <- obj$geom |> pluck(j) |> as.matrix()
#     lgv <- def_pol(x_ge[i],y_ge[i],pol)
#     if(lgv){
#       v_city[i] <- name_muni[j]
#       break
#     }
#   }
# }
# grid_geral_city <- grid_geral |>
#    add_column(
#      city = v_city
#    )
```

``` r
# Encontrando os pontos mais próximos para os refugos
# refugos <- grid_geral_city |>
#    filter(is.na(city))
#  
# x_ge_nref <- grid_geral_city |> filter(!is.na(city)) |>
#    pull(X)
# y_ge_nref <- grid_geral_city |> filter(!is.na(city)) |>
#   pull(Y)
# c_ge_nref <- grid_geral_city |> filter(!is.na(city)) |>
#   pull(city)
# 
# x_ref <- refugos |> pull(X)
# y_ref <- refugos |> pull(Y)
# city_ref <- 0
# for(i in 1:nrow(refugos)){
#   dist_vec = sqrt((x_ref[i]-x_ge_nref)^2+(y_ref[i]-y_ge_nref)^2)
#   i_min <- which(dist_vec == min(dist_vec))[1]
#   city_ref[i] <- c_ge_nref[i_min]
# }
# 
# refugos$city <- city_ref
#  
# grid_geral_city <- grid_geral_city |>
#   filter(!is.na(city)) |>
#   rbind(refugos)
# 
# write_rds(grid_geral_city,"data/grid-city.rds")
```

``` r
grid_geral <- read_rds("data/grid-city.rds")
# citys |>
#   filter(abbrev_state == "SP") |>
#    ggplot() +
#      geom_sf(aes_string(), color="black",
#               size=.05, show.legend = TRUE) +
#   theme_minimal() +
#   geom_point(
#     data = grid_geral |>
#   sample_n(1000),
#   aes(X,Y),
#   color = "red"
#   )
```

#### Definição do gradeado adensado.

``` r
grid <- grid_geral |> 
  select(X, Y)
sp::gridded(grid) = ~ X + Y
```

#### PASSO 1)

“dry_15:16” “rainy_15:16” “dry_16:17” “rainy_16:17”  
“dry_17:18” “rainy_17:18” “dry_18:19” “rainy_18:19”  
“dry_19:20” “rainy_19:20” “dry_20:21” “rainy_20:21”  
“dry_21:22” “rainy_21:22” “dry_22:23” “rainy_22:23”  
“dry_23:24” “rainy_23:24”

``` r
# my_season = "dry_15:16"
# data_set_aux  <- data_set_sp |>
#   filter(
#     season_year == my_season) |>
#   select(longitude, latitude, xco2)
# 
# vct_xco2 <- vector();dist_xco2 <- vector();
# lon_grid <- vector();lat_grid <- vector();
# for(i in 1:nrow(data_set_aux)){
#   d <- sqrt((data_set_aux$longitude[i] - grid$X)^2 + 
#               (data_set_aux$latitude[i] - grid$Y)^2)
#   min_index <- order(d)[1]
#   vct_xco2[i] <- data_set_aux$xco2[min_index]
#   dist_xco2[i] <- d[order(d)[1]]
#   lon_grid[i] <- grid$X[min_index]
#   lat_grid[i] <- grid$Y[min_index]
# }
# data_set_aux$dist_xco2 <- dist_xco2
# data_set_aux$xco2_new <- vct_xco2
# data_set_aux$lon_grid <- lon_grid
# data_set_aux$lat_grid <- lat_grid
# data_set_aux |> 
#   group_by(lon_grid,lat_grid) |> 
#   summarise(
#     xco2 = mean(xco2),
#     .groups = "drop"
#   ) |> 
#   rename(longitude = lon_grid, latitude = lat_grid) -> data_set_aux
# sp::coordinates(data_set_aux) = ~ longitude + latitude
```

#### PASSO 2 - Construção do Semivariograma Experimental

``` r
# form <- xco2 ~ 1
# vari_exp <- gstat::variogram(form, data = data_set_aux,
#                       cressie = FALSE,
#                       cutoff = 2, # distância máxima 8
#                       width = 0.075) # distancia entre pontos
# vari_exp  |>
#   ggplot(aes(x=dist, y=gamma)) +
#   geom_point() +
#   labs(x="lag (º)",
#        y=expression(paste(gamma,"(h)")))
```

#### PASSO 3) Ajuste dos modelos matemáticos teóricos ao semivariograma experimental

``` r
# patamar=1.4
# alcance=0.2
# epepita=0.5
# modelo_1 <- fit.variogram(vari_exp,vgm(patamar,"Sph",alcance,epepita))
# modelo_2 <- fit.variogram(vari_exp,vgm(patamar,"Exp",alcance,epepita))
# modelo_3 <- fit.variogram(vari_exp,vgm(patamar,"Gau",alcance,epepita))
# plot_my_models(modelo_1,modelo_2,modelo_3)
```

#### PASSO 4) Escolha do melhor modelo

O melhor modelo é aquele que apresenta um coeficiente de regressão o
mais próximo de 01 e o interesepto o mais próximo de 0.

``` r
# conjunto_validacao <- data_set_aux |>
#   as_tibble() |>
#   sample_n(50)
# sp::coordinates(conjunto_validacao) = ~longitude + latitude
# modelos<-list(modelo_1,modelo_2,modelo_3)
# for(j in 1:3){
#   est<-0
#   for(i in 1:nrow(conjunto_validacao)){
#     valid <- gstat::krige(formula=form, conjunto_validacao[-i,], conjunto_validacao, model=modelos[[j]])
#     est[i]<-valid$var1.pred[i]
#   }
#   obs<-as.data.frame(conjunto_validacao)[,3]
#   RMSE<-round((sum((obs-est)^2)/length(obs))^.5,3)
#   mod<-lm(obs~est)
#   b<-round(mod$coefficients[2],3)
#   se<-round(summary(mod)$coefficients[4],3)
#   r2<-round(summary(mod)$r.squared,3)
#   a<-round(mod$coefficients[1],3)
#   plot(est,obs,xlab="Estimado", ylab="Observado",pch=j,col="blue",
#        main=paste("Modelo = ",modelos[[j]][2,1],"; Coef. Reg. = ", b, " (SE = ",se, ", r2 = ", r2,")\ny intersept = ",a,"RMSE = ",RMSE ))
#   abline(lm(obs~est));
#   abline(0,1,lty=3)
# }
```

#### PASSO 5) Definido o melhor modelo, precisamos guardar os valores.

``` r
# modelo <- modelo_1 ## sempre modificar
# # Salvando os parâmetros dos melhores modelo
# model <- modelo |> slice(2) |> pull(model)
# rss <- round(attr(modelo, "SSErr"),4) 
# c0 <- round(modelo$psill[[1]],4) 
# c0_c1 <- round(sum(modelo$psill),4)
# a <- ifelse(model == "Gau", round(modelo$range[[2]]*(3^.5),2),
#             ifelse(model == "Exp",round(3*modelo$range[[2]],2),
#             round(modelo$range[[2]],2)))
# 
# 
# r2 <- vari_exp |> add_column( model = model, a=a, c0 = c0,
#                                   c0_c1 = c0_c1) |> 
#     mutate(
#       gamma_m = ifelse(model == "Sph",
#         ifelse(dist <= a, c0 + (c0_c1 - c0) * (3/2 * (dist/a) - 1/2 * (dist/a)^3),c0_c1), ifelse(model == "Exp", c0 + (c0_c1-c0)*(1-exp(-3*(dist/a))),c0 + (c0_c1-c0)*(1-exp(-(dist/a)^2)))),
#       residuo_total = (gamma-mean(gamma))^2,
#       residuo_mod = (gamma - gamma_m)^2
#     ) |>
#     summarise(
#       r2=(sum(residuo_total) - sum(residuo_mod))/sum(residuo_total)
#     ) |> pull(r2)
# 
# tibble(
#   my_season, model, c0, c0_c1, a, rss, r2
# ) |> mutate(gde = c0/c0_c1, .after = "a") |>
#   rename(season=my_season) |> 
#   write_csv(paste0("output/best-fit/",str_replace(my_season,"\\:","_"),".csv"))
# 
# ls_csv <- list.files("output/best-fit/",full.names = TRUE,pattern = ".csv")
# map_df(ls_csv, read_csv) |> 
#   writexl::write_xlsx("output/semivariogram-models.xlsx")
# png(filename = paste0("output/semivariogram-img/semivar-",
#                       str_replace(my_season,"\\:","_"),".png"),
#     width = 800, height = 600)
# plot(vari_exp,model=modelo,cex.lab=2, col=1,pl=F,pch=16,cex=2.2,ylab=list("Semivariância",cex=2.3),xlab=list("Distância de Separação h (m)",cex=2.3,cex.axis=4))
# dev.off()
```

#### Passo 6 - Krigagem Ordinária - interpolação em locais não amostrados

``` r
# ko_variavel <- krige(formula=form, data_set_aux, grid, model=modelo,
#                      block=c(0.1,0.1),
#                      nsim=0,
#                      na.action=na.pass,
#                      debug.level=-1
# )
```

#### Passo 7 - Visualização dos padrões espaciais e armazenamento dos dados e imagem.

``` r
# mapa <- as_tibble(ko_variavel) |>
#   ggplot(aes(x=X, y=Y)) +
#   geom_tile(aes(fill = var1.pred)) +
#   scale_fill_viridis_c() +
#   coord_equal() +
#   labs(x="Longitude",
#        y="Latitude",
#        fill="xco2") +
#   theme_bw()
# mapa
# ggsave(paste0("output/maps-kgr/kgr-xco2-",str_replace(my_season,"\\:","_"),".png"), plot = mapa, width = 10, height = 8, dpi = 300)
# df <- ko_variavel |>
#   as_tibble() |>
#   mutate(var1.var = sqrt(var1.var))
# write_rds(df,paste0("output/maps-kgr/kgr-xco2-",str_replace(my_season,"\\:","_"),".rds"))
```

#### Compilação de todos os mapas gerados

``` r
list_rds <- list.files("output/maps-kgr/",
           pattern = ".rds$",
           full.names = TRUE)

rds_reader <- function(path){
  readr::read_rds(path) |>
    mutate(
    path = stringr::str_remove(path,"output/maps-kgr/kgr-xco2-|\\.rds"),
    epoch = stringr::str_sub(path,1,1),
    season = stringr::str_sub(path,-9,-5),
    epoch_n = ifelse(epoch == "d",1,2),
    season_year = paste0(season,"_",epoch_n)
    # variable = stringr::str_extract(path, "^[^\\-]+")
     ) |>
    rename(
       xco2 = var1.pred, 
       xco2_std = var1.var
    )  |>
    select(-path,-(epoch:epoch_n))
}

kgr_maps <- map_df(list_rds, rds_reader)

kgr_maps <- kgr_maps |> 
  group_by(season_year) |> 
  mutate(xco2_anomaly = xco2 - median(xco2,na.rm=TRUE),
         .after = xco2)

kgr_maps |> 
  group_by(season_year) |> 
  ggplot(aes(season_year,xco2)) +
  geom_boxplot(fill="gray")+
  theme_bw()
```

![](README_files/figure-gfm/unnamed-chunk-26-1.png)<!-- -->

``` r

kgr_maps |> 
  group_by(season_year) |> 
  ggplot(aes(season_year,xco2_anomaly)) +
  geom_boxplot(fill="orange")+
  theme_bw()
```

![](README_files/figure-gfm/unnamed-chunk-26-2.png)<!-- -->

``` r
kgr_maps <- kgr_maps |> 
  left_join(
    grid_geral,
  by=c("X","Y")
  )
```

### Padrões espaciais de XCO2 para o estado por estação

``` r
season <- kgr_maps |> pull(season_year) |> unique()
map(season,~{
  kgr_maps |> 
    filter( season_year == .x) |> 
    ggplot(aes(x=X, y=Y)) +
  geom_tile(aes(fill = xco2)) +
  scale_fill_viridis_c(option = "mako") +
  coord_equal() +
  labs(x="Longitude",
       y="Latitude",
       fill="xco2",
       title = .x) +
  theme_bw()
})
#> [[1]]
```

![](README_files/figure-gfm/unnamed-chunk-28-1.png)<!-- -->

    #> 
    #> [[2]]

![](README_files/figure-gfm/unnamed-chunk-28-2.png)<!-- -->

    #> 
    #> [[3]]

![](README_files/figure-gfm/unnamed-chunk-28-3.png)<!-- -->

    #> 
    #> [[4]]

![](README_files/figure-gfm/unnamed-chunk-28-4.png)<!-- -->

    #> 
    #> [[5]]

![](README_files/figure-gfm/unnamed-chunk-28-5.png)<!-- -->

    #> 
    #> [[6]]

![](README_files/figure-gfm/unnamed-chunk-28-6.png)<!-- -->

    #> 
    #> [[7]]

![](README_files/figure-gfm/unnamed-chunk-28-7.png)<!-- -->

    #> 
    #> [[8]]

![](README_files/figure-gfm/unnamed-chunk-28-8.png)<!-- -->

    #> 
    #> [[9]]

![](README_files/figure-gfm/unnamed-chunk-28-9.png)<!-- -->

    #> 
    #> [[10]]

![](README_files/figure-gfm/unnamed-chunk-28-10.png)<!-- -->

    #> 
    #> [[11]]

![](README_files/figure-gfm/unnamed-chunk-28-11.png)<!-- -->

    #> 
    #> [[12]]

![](README_files/figure-gfm/unnamed-chunk-28-12.png)<!-- -->

    #> 
    #> [[13]]

![](README_files/figure-gfm/unnamed-chunk-28-13.png)<!-- -->

    #> 
    #> [[14]]

![](README_files/figure-gfm/unnamed-chunk-28-14.png)<!-- -->

    #> 
    #> [[15]]

![](README_files/figure-gfm/unnamed-chunk-28-15.png)<!-- -->

    #> 
    #> [[16]]

![](README_files/figure-gfm/unnamed-chunk-28-16.png)<!-- -->

    #> 
    #> [[17]]

![](README_files/figure-gfm/unnamed-chunk-28-17.png)<!-- -->

    #> 
    #> [[18]]

![](README_files/figure-gfm/unnamed-chunk-28-18.png)<!-- -->

### Padrões espaciais de Anomalia de XCO2 para o estado por estação

``` r
map(season,~{
  kgr_maps |> 
    filter( season_year == .x) |> 
    ggplot(aes(x=X, y=Y)) +
  geom_tile(aes(fill = xco2_anomaly)) +
  scale_fill_viridis_c(option = "inferno") +
  coord_equal() +
  labs(x="Longitude",
       y="Latitude",
       fill="xco2_anomaly",
       title = .x) +
  theme_bw()
})
#> [[1]]
```

![](README_files/figure-gfm/unnamed-chunk-29-1.png)<!-- -->

    #> 
    #> [[2]]

![](README_files/figure-gfm/unnamed-chunk-29-2.png)<!-- -->

    #> 
    #> [[3]]

![](README_files/figure-gfm/unnamed-chunk-29-3.png)<!-- -->

    #> 
    #> [[4]]

![](README_files/figure-gfm/unnamed-chunk-29-4.png)<!-- -->

    #> 
    #> [[5]]

![](README_files/figure-gfm/unnamed-chunk-29-5.png)<!-- -->

    #> 
    #> [[6]]

![](README_files/figure-gfm/unnamed-chunk-29-6.png)<!-- -->

    #> 
    #> [[7]]

![](README_files/figure-gfm/unnamed-chunk-29-7.png)<!-- -->

    #> 
    #> [[8]]

![](README_files/figure-gfm/unnamed-chunk-29-8.png)<!-- -->

    #> 
    #> [[9]]

![](README_files/figure-gfm/unnamed-chunk-29-9.png)<!-- -->

    #> 
    #> [[10]]

![](README_files/figure-gfm/unnamed-chunk-29-10.png)<!-- -->

    #> 
    #> [[11]]

![](README_files/figure-gfm/unnamed-chunk-29-11.png)<!-- -->

    #> 
    #> [[12]]

![](README_files/figure-gfm/unnamed-chunk-29-12.png)<!-- -->

    #> 
    #> [[13]]

![](README_files/figure-gfm/unnamed-chunk-29-13.png)<!-- -->

    #> 
    #> [[14]]

![](README_files/figure-gfm/unnamed-chunk-29-14.png)<!-- -->

    #> 
    #> [[15]]

![](README_files/figure-gfm/unnamed-chunk-29-15.png)<!-- -->

    #> 
    #> [[16]]

![](README_files/figure-gfm/unnamed-chunk-29-16.png)<!-- -->

    #> 
    #> [[17]]

![](README_files/figure-gfm/unnamed-chunk-29-17.png)<!-- -->

    #> 
    #> [[18]]

![](README_files/figure-gfm/unnamed-chunk-29-18.png)<!-- -->

#### Centroide de massa, cold and hotspots

``` r
map(season,~{
  
  # Identificar hotspots 
  hotspots <- kgr_maps |>
    filter(season_year == .x) |> 
    slice_max(xco2_anomaly, n = 5, with_ties = FALSE) |>
    mutate(tipo = "hotspot")  
  
  coldspots <- kgr_maps |>
    filter(season_year == .x) |> 
    slice_min(xco2_anomaly, n = 5, with_ties = FALSE) |>
    mutate(tipo = "coldspot")  
  
  pontos_destaque <- bind_rows(hotspots, coldspots)
  
  kgr_maps |> 
    filter(season_year == .x) |> 
    ggplot(aes(x = X, y = Y)) +
    geom_tile(aes(fill = xco2_anomaly)) +
    geom_point(data = pontos_destaque,
               aes(x = X, y = Y, color = tipo),
               shape = 21, size = 3, fill = "white", stroke = 1.2) +
    scale_color_manual(values = c("hotspot" = "red", "coldspot" = "blue")) +
    geom_point(
      data = kgr_maps |> 
        filter(season_year == .x) |> 
        filter(xco2_anomaly >0) |>
        mutate(
          lon_centro = sum(X*xco2_anomaly, na.rm = TRUE)/sum(xco2_anomaly, na.rm = TRUE),
          lat_centro = sum(Y*xco2_anomaly, na.rm = TRUE)/sum(xco2_anomaly, na.rm = TRUE)),
      aes(x = lon_centro, y = lat_centro), 
      color = "cyan", size = 2.5, shape = 4, stroke = 1.2) +
    scale_fill_viridis_c(option = "inferno") +
    coord_equal() +
    facet_wrap(~ season_year) +
    labs(
      x = "Longitude", y = "Latitude", fill = "xco2_anomaly",
      title = .x
    ) +
    theme_bw()
})
#> [[1]]
```

![](README_files/figure-gfm/unnamed-chunk-30-1.png)<!-- -->

    #> 
    #> [[2]]

![](README_files/figure-gfm/unnamed-chunk-30-2.png)<!-- -->

    #> 
    #> [[3]]

![](README_files/figure-gfm/unnamed-chunk-30-3.png)<!-- -->

    #> 
    #> [[4]]

![](README_files/figure-gfm/unnamed-chunk-30-4.png)<!-- -->

    #> 
    #> [[5]]

![](README_files/figure-gfm/unnamed-chunk-30-5.png)<!-- -->

    #> 
    #> [[6]]

![](README_files/figure-gfm/unnamed-chunk-30-6.png)<!-- -->

    #> 
    #> [[7]]

![](README_files/figure-gfm/unnamed-chunk-30-7.png)<!-- -->

    #> 
    #> [[8]]

![](README_files/figure-gfm/unnamed-chunk-30-8.png)<!-- -->

    #> 
    #> [[9]]

![](README_files/figure-gfm/unnamed-chunk-30-9.png)<!-- -->

    #> 
    #> [[10]]

![](README_files/figure-gfm/unnamed-chunk-30-10.png)<!-- -->

    #> 
    #> [[11]]

![](README_files/figure-gfm/unnamed-chunk-30-11.png)<!-- -->

    #> 
    #> [[12]]

![](README_files/figure-gfm/unnamed-chunk-30-12.png)<!-- -->

    #> 
    #> [[13]]

![](README_files/figure-gfm/unnamed-chunk-30-13.png)<!-- -->

    #> 
    #> [[14]]

![](README_files/figure-gfm/unnamed-chunk-30-14.png)<!-- -->

    #> 
    #> [[15]]

![](README_files/figure-gfm/unnamed-chunk-30-15.png)<!-- -->

    #> 
    #> [[16]]

![](README_files/figure-gfm/unnamed-chunk-30-16.png)<!-- -->

    #> 
    #> [[17]]

![](README_files/figure-gfm/unnamed-chunk-30-17.png)<!-- -->

    #> 
    #> [[18]]

![](README_files/figure-gfm/unnamed-chunk-30-18.png)<!-- -->

### Cálculos por valor estimado do Beta e das médias das anomalias no período todo

``` r
kgr_maps_nested <- kgr_maps |>
  group_by(season_year,X,Y) |>
  summarise(
    media_xco2 = mean(xco2,na.rm = TRUE),
    media_xco2_anomaly = mean(xco2_anomaly, na.rm = TRUE),
    desv_pad = mean(xco2_std),
    .groups = "drop"
  ) |>
  group_by(X,Y) |>
  nest() |>
  ungroup()
```

Função para calcular o beta ou a média das anomalias por coordenada

``` r
get_my_par <- function(df, par_return = "beta"){
  n_end <- nrow(df)/2-.5
  x <- seq(0,n_end,0.5)
  y <- df$media_xco2
  mod <- lm(y~x)
  beta <- mod$coefficients[[2]]
  if(par_return == "beta") return(beta)
  yanom <- df$media_xco2_anomaly
  anomaly <- mean(yanom,na.rm=TRUE)
  if(par_return == "anomaly") return(anomaly)
}
```

Cálculo e mapeamento de Beta e Média de anomalias

``` r
kgr_maps_beta_anom <- kgr_maps_nested |>
  mutate(
    beta_xco2 = map(data,get_my_par,par_return="beta"),
    anomaly_xco2 = map(data,get_my_par,par_return="anomaly")
  ) |>
  select(-data) |>
  unnest(cols = c(beta_xco2,anomaly_xco2))

kgr_maps_beta_anom <- kgr_maps_beta_anom |> 
  left_join(
    grid_geral,
  by=c("X","Y")
  )
```

Mapa do Beta

``` r
kgr_maps_beta_anom |>
  ggplot(aes(x=X, y=Y)) +
  geom_tile(aes(fill = beta_xco2)) +
  scale_fill_viridis_c() +
  coord_equal() +
  labs(x="Longitude",
       y="Latitude",
       fill="Beta_xco2") +
  theme_bw()
```

![](README_files/figure-gfm/unnamed-chunk-34-1.png)<!-- -->

``` r

kgr_maps_beta_anom  |> 
  rename(longitude = X, latitude = Y) |> 
  select(-beta_xco2, -anomaly_xco2) |> 
  writexl::write_xlsx("output/grid-kgr.xlsx")
```

Agregação de betas e anomalias (médias nos municípios)

``` r
city_kgr_beta_anom <- left_join(
  citys |> filter(abbrev_state == "SP"),
  kgr_maps_beta_anom |>
    group_by(city) |> 
    summarise(
      beta_xco2 = mean(beta_xco2),
      anomaly_xco2 = mean(anomaly_xco2)
    ) |> 
    rename(name_muni = city),
           by="name_muni")
```

Beta municipal

``` r
city_kgr_beta_anom |>
  drop_na() |> 
     ggplot() +
     geom_sf(aes(fill=beta_xco2), color="transparent",
             size=.05, show.legend = TRUE)  +
  geom_sf(data=citys |> filter(abbrev_state == "SP"), fill="transparent", size=3, show.legend = FALSE, lwd=.05, color="black") +
     theme_bw() +
   theme(
     axis.text.x = element_text(size = rel(.9), color = "black"),
     axis.title.x = element_text(size = rel(1.1), color = "black"),
     axis.text.y = element_text(size = rel(.9), color = "black"),
     axis.title.y = element_text(size = rel(1.1), color = "black"),
     legend.text = element_text(size = rel(1), color = "black"),
     legend.title = element_text(face = 'bold', size = rel(1.2))
     ) +
   labs(fill = 'beta_xco2_mean',
         x = 'Longitude',
         y = 'Latitude') +
     scale_fill_viridis_c()
```

![](README_files/figure-gfm/unnamed-chunk-36-1.png)<!-- -->

Mapa da Anomalia média

``` r
kgr_maps_beta_anom |>
  ggplot(aes(x=X, y=Y)) +
  geom_tile(aes(fill = anomaly_xco2)) +
  scale_fill_viridis_c(option = "inferno") +
  coord_equal() +
  labs(x="Longitude",
       y="Latitude",
       fill="mean_anomaly_xco2") +
  theme_bw()
```

![](README_files/figure-gfm/unnamed-chunk-37-1.png)<!-- -->

### Mapa da Anomalia média até 2018

``` r
kgr_maps_nested_period <- kgr_maps |> 
  mutate(
    year = as.numeric(str_sub(season_year,1,2))+2000,
    period = ifelse(year<=2018,"before-2018","after-2018")) |> 
  group_by(period,season_year,X,Y) |>
  summarise(
    media_xco2 = mean(xco2,na.rm = TRUE),
    media_xco2_anomaly = mean(xco2_anomaly, na.rm = TRUE),
    desv_pad = mean(xco2_std),
    .groups = "drop"
  ) |>
  group_by(period,X,Y) |>
  nest() |>
  ungroup()
```

``` r
kgr_maps_beta_anom_period <- kgr_maps_nested_period |>
  mutate(
    beta_xco2 = map(data,get_my_par,par_return="beta"),
    anomaly_xco2 = map(data,get_my_par,par_return="anomaly")
  ) |>
  select(-data) |>
  unnest(cols = c(beta_xco2,anomaly_xco2))

kgr_maps_beta_anom_period <- kgr_maps_beta_anom_period |> 
  left_join(
    grid_geral,
  by=c("X","Y")
  )
```

``` r
kgr_maps_beta_anom_period |>
  filter(period == "before-2018") |> 
  ggplot(aes(x=X, y=Y)) +
  geom_tile(aes(fill = anomaly_xco2)) +
  scale_fill_viridis_c(option = "inferno") +
  coord_equal() +
  labs(x="Longitude",
       y="Latitude",
       fill="mean_anomaly_xco2") +
  theme_bw() +
  labs(title="Anomalia média de XCO2 antes de 2018")
```

![](README_files/figure-gfm/unnamed-chunk-40-1.png)<!-- -->

### Mapa da Anomalia média após 2018

``` r
kgr_maps_beta_anom_period |>
  filter(period == "after-2018") |> 
  ggplot(aes(x=X, y=Y)) +
  geom_tile(aes(fill = anomaly_xco2)) +
  scale_fill_viridis_c(option = "inferno") +
  coord_equal() +
  labs(x="Longitude",
       y="Latitude",
       fill="mean_anomaly_xco2") +
  theme_bw() +
  labs(title="Anomalia média de XCO2 após de 2018")
```

![](README_files/figure-gfm/unnamed-chunk-41-1.png)<!-- -->

### Mapa do Beta até 2018

``` r
kgr_maps_beta_anom_period |>
  filter(period == "before-2018") |> 
  ggplot(aes(x=X, y=Y)) +
  geom_tile(aes(fill = beta_xco2)) +
  scale_fill_viridis_c() +
  coord_equal() +
  labs(x="Longitude",
       y="Latitude",
       fill="mean_anomaly_xco2") +
  theme_bw() +
  labs(title="Beta de XCO2 antes de 2018")
```

![](README_files/figure-gfm/unnamed-chunk-42-1.png)<!-- -->

### Mapa do Beta após 2018

``` r
kgr_maps_beta_anom_period |>
  filter(period == "after-2018") |> 
  ggplot(aes(x=X, y=Y)) +
  geom_tile(aes(fill = beta_xco2)) +
  scale_fill_viridis_c() +
  coord_equal() +
  labs(x="Longitude",
       y="Latitude",
       fill="mean_anomaly_xco2") +
  theme_bw() +
  labs(title="Beta de XCO2 após de 2018")
```

![](README_files/figure-gfm/unnamed-chunk-43-1.png)<!-- -->

Anomalia média municipal

``` r
hotspots <- city_kgr_beta_anom |>
  slice_max(anomaly_xco2, n = 5, with_ties = FALSE) |>
  mutate(tipo = "hotspot")

coldspots <- city_kgr_beta_anom |>
  slice_min(anomaly_xco2, n = 5, with_ties = FALSE) |>
  mutate(tipo = "coldspot")

# Juntar os pontos de interesse
pontos_destaque <- bind_rows(hotspots, coldspots)

city_kgr_beta_anom |>
  drop_na() |> 
  ggplot() +
  geom_sf(aes(fill = anomaly_xco2), color = "transparent", size = .05) +
  geom_sf(data = citys |> filter(abbrev_state == "SP"), 
          fill = "transparent", size = 3, color = "black", lwd = .05) +
  geom_sf(data = pontos_destaque, 
          aes(color = tipo), size = 3, shape = 21, fill = "white", stroke = 1.2) +
  
  # Escalas e temas
  scale_fill_viridis_c(option = "inferno") +
  scale_color_manual(values = c("hotspot" = "red", "coldspot" = "blue")) +
  labs(
    fill = "mean_anomaly_xco2",
    color = "Tipo",
    x = "Longitude",
    y = "Latitude"
  ) +
  theme_bw() +
  theme(
    axis.text.x = element_text(size = rel(.9), color = "black"),
    axis.title.x = element_text(size = rel(1.1), color = "black"),
    axis.text.y = element_text(size = rel(.9), color = "black"),
    axis.title.y = element_text(size = rel(1.1), color = "black"),
    legend.text = element_text(size = rel(1), color = "black"),
    legend.title = element_text(face = 'bold', size = rel(1.2))
  )
```

![](README_files/figure-gfm/unnamed-chunk-44-1.png)<!-- -->

Anomalia para cada estação no período todo (TOP 10 )

``` r
map(season,~{
  hotspots <- left_join(
  citys |> filter(abbrev_state == "SP"),
  kgr_maps |>
    filter( season_year ==.x) |> 
    group_by(city) |> 
    summarise(
      xco2_anomaly = mean(xco2_anomaly,na.rm=TRUE)
    ) |> 
    rename(name_muni = city),
  by="name_muni") |>
    slice_max(xco2_anomaly, n = 10, with_ties = FALSE) |>
    mutate(tipo = "hotspot")
  
  coldspots <- left_join(
  citys |> filter(abbrev_state == "SP"),
  kgr_maps |>
    filter( season_year ==.x) |> 
    group_by(city) |> 
    summarise(
      xco2_anomaly = mean(xco2_anomaly,na.rm=TRUE)
    ) |> 
    rename(name_muni = city),
  by="name_muni") |>
    slice_min(xco2_anomaly, n = 10, with_ties = FALSE) |>
    mutate(tipo = "coldspot")
  
  # Juntar os pontos de interesse
  pontos_destaque <- bind_rows(hotspots, coldspots)
  
  
  left_join(
  citys |> filter(abbrev_state == "SP"),
  kgr_maps |>
    filter( season_year ==.x) |> 
    group_by(city) |> 
    summarise(
      xco2_anomaly = mean(xco2_anomaly,na.rm=TRUE)
    ) |> 
    rename(name_muni = city),
  by="name_muni") |> 
    drop_na() |> 
    ggplot() +
    geom_sf(aes(fill=xco2_anomaly), color="transparent",
            size=.05, show.legend = TRUE)  +
    geom_sf(data=citys |> filter(abbrev_state == "SP"), fill="transparent", size=3, show.legend = FALSE, lwd=.05, color="black") +
    geom_sf(data = pontos_destaque, 
            aes(color = tipo), size = 3, shape = 21, fill = "white", stroke = 1.2) +
    scale_fill_viridis_c(option = "inferno") +
    scale_color_manual(values = c("hotspot" = "red", "coldspot" = "blue")) +
  theme_bw() +
    theme(
      axis.text.x = element_text(size = rel(.9), color = "black"),
      axis.title.x = element_text(size = rel(1.1), color = "black"),
      axis.text.y = element_text(size = rel(.9), color = "black"),
      axis.title.y = element_text(size = rel(1.1), color = "black"),
      legend.text = element_text(size = rel(1), color = "black"),
      legend.title = element_text(face = 'bold', size = rel(1.2))
    ) +
  labs(fill = 'xco2_anomaly',
       x = 'Longitude',
       y = 'Latitude',
       title = .x)})
#> [[1]]
```

![](README_files/figure-gfm/unnamed-chunk-45-1.png)<!-- -->

    #> 
    #> [[2]]

![](README_files/figure-gfm/unnamed-chunk-45-2.png)<!-- -->

    #> 
    #> [[3]]

![](README_files/figure-gfm/unnamed-chunk-45-3.png)<!-- -->

    #> 
    #> [[4]]

![](README_files/figure-gfm/unnamed-chunk-45-4.png)<!-- -->

    #> 
    #> [[5]]

![](README_files/figure-gfm/unnamed-chunk-45-5.png)<!-- -->

    #> 
    #> [[6]]

![](README_files/figure-gfm/unnamed-chunk-45-6.png)<!-- -->

    #> 
    #> [[7]]

![](README_files/figure-gfm/unnamed-chunk-45-7.png)<!-- -->

    #> 
    #> [[8]]

![](README_files/figure-gfm/unnamed-chunk-45-8.png)<!-- -->

    #> 
    #> [[9]]

![](README_files/figure-gfm/unnamed-chunk-45-9.png)<!-- -->

    #> 
    #> [[10]]

![](README_files/figure-gfm/unnamed-chunk-45-10.png)<!-- -->

    #> 
    #> [[11]]

![](README_files/figure-gfm/unnamed-chunk-45-11.png)<!-- -->

    #> 
    #> [[12]]

![](README_files/figure-gfm/unnamed-chunk-45-12.png)<!-- -->

    #> 
    #> [[13]]

![](README_files/figure-gfm/unnamed-chunk-45-13.png)<!-- -->

    #> 
    #> [[14]]

![](README_files/figure-gfm/unnamed-chunk-45-14.png)<!-- -->

    #> 
    #> [[15]]

![](README_files/figure-gfm/unnamed-chunk-45-15.png)<!-- -->

    #> 
    #> [[16]]

![](README_files/figure-gfm/unnamed-chunk-45-16.png)<!-- -->

    #> 
    #> [[17]]

![](README_files/figure-gfm/unnamed-chunk-45-17.png)<!-- -->

    #> 
    #> [[18]]

![](README_files/figure-gfm/unnamed-chunk-45-18.png)<!-- -->

### 4) Análise de reconhecimento de padrões

#### Por season (Dry vs Rainy)

``` r
nome_periodo <- c("Dry","Rainy")
vetor_de_grupos<-c(2,4)
for(i in 1:2){
  kgr_maps_wider <- kgr_maps |> 
    mutate(
      season = str_sub(season_year,1,5),
      period = as.numeric(str_sub(season_year,7,7))
    ) |>
    filter(period == i) |> 
    select(season, X,Y,city,xco2_anomaly) |> 
    group_by(season, city) |> 
    summarise(xco2_anomaly = mean(xco2_anomaly, na.rm = TRUE)) |> 
    pivot_wider(names_from = season,
                values_from = xco2_anomaly,names_prefix = "season_")
  name_muni <- kgr_maps_wider |> pull(city)
  kgr_maps_wider_xco2 <- kgr_maps_wider |> 
    select(season_15_16:season_23_24) #|> 
  # select(ends_with("2"))
  
  mc <- cor(kgr_maps_wider_xco2)
  corrplot::corrplot(mc)
  da_pad<-decostand(kgr_maps_wider_xco2, 
                    method = "standardize",
                    na.rm=TRUE)
  da_pad_euc<-vegdist(da_pad,"euclidean") 
  da_pad_euc_ward<-hclust(da_pad_euc, method="ward.D")
  plot(da_pad_euc_ward, 
       ylab="Distância Euclidiana",
       xlab="Acessos", hang=-1,
       col="blue", las=1,
       cex=.6,lwd=1.5);box()
  grupo<-cutree(da_pad_euc_ward,vetor_de_grupos[i])
  
  city_kgr_beta_group <- city_kgr_beta_anom |> 
    left_join(
      tibble(name_muni, grupo),
      by="name_muni")
  
  plot_cidades_agrupamento <- city_kgr_beta_group  |>
    drop_na() |> 
    ggplot() +
    geom_sf(aes(fill=grupo), color="transparent",
            size=.05, show.legend = TRUE)  +
    geom_sf(data=citys |> filter(abbrev_state == "SP"), fill="transparent", size=3, show.legend = FALSE) +
    theme_bw() +
    theme(
      axis.text.x = element_text(size = rel(.9), color = "black"),
      axis.title.x = element_text(size = rel(1.1), color = "black"),
      axis.text.y = element_text(size = rel(.9), color = "black"),
      axis.title.y = element_text(size = rel(1.1), color = "black"),
      legend.text = element_text(size = rel(1), color = "black"),
      legend.title = element_text(face = 'bold', size = rel(1.2))
    ) +
    labs(fill = 'Agrupamento',
         x = 'Longitude',
         y = 'Latitude',
         title = nome_periodo[i]) +
    scale_fill_viridis_c()
  print(plot_cidades_agrupamento)
}
```

![](README_files/figure-gfm/unnamed-chunk-46-1.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-46-2.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-46-3.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-46-4.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-46-5.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-46-6.png)<!-- -->

#### Por Média anual anomalia

``` r
kgr_maps_wider <- kgr_maps |> 
  select(season_year, X,Y,city,xco2_anomaly) |> 
  mutate(season = str_sub(season_year,1,5)) |> 
  group_by(season, city) |> 
  summarise(xco2_anomaly = mean(xco2_anomaly, na.rm = TRUE)) |> 
  pivot_wider(names_from = season,
              values_from = xco2_anomaly,names_prefix = "season_")

name_muni <- kgr_maps_wider |> pull(city)
kgr_maps_wider_xco2_anomaly <- kgr_maps_wider |> 
  select(season_15_16:season_23_24) #|> 
  # select(ends_with("2"))
  
mc <- cor(kgr_maps_wider_xco2_anomaly)
corrplot::corrplot(mc)
```

![](README_files/figure-gfm/unnamed-chunk-47-1.png)<!-- -->

``` r
da_pad<-decostand(kgr_maps_wider_xco2_anomaly, 
                  method = "standardize",
                  na.rm=TRUE)
da_pad_euc<-vegdist(da_pad,"euclidean") 
da_pad_euc_ward<-hclust(da_pad_euc, method="ward.D")
plot(da_pad_euc_ward, 
     ylab="Distância Euclidiana",
     xlab="Acessos", hang=-1,
     col="blue", las=1,
     cex=.6,lwd=1.5);box()
```

![](README_files/figure-gfm/unnamed-chunk-47-2.png)<!-- -->

``` r
grupo<-cutree(da_pad_euc_ward,4)

city_kgr_beta_group <- city_kgr_beta_anom |> 
  left_join(
    tibble(name_muni, grupo),
           by="name_muni")

city_kgr_beta_group  |>
  mutate( grupo = as_factor(grupo )) |> 
  drop_na() |> 
     ggplot() +
     geom_sf(aes(fill=grupo), color="transparent",
             size=.05, show.legend = TRUE)  +
  geom_sf(data=citys |> filter(abbrev_state == "SP"), fill="transparent", size=3, show.legend = FALSE) +
     theme_bw() +
   theme(
     axis.text.x = element_text(size = rel(.9), color = "black"),
     axis.title.x = element_text(size = rel(1.1), color = "black"),
     axis.text.y = element_text(size = rel(.9), color = "black"),
     axis.title.y = element_text(size = rel(1.1), color = "black"),
     legend.text = element_text(size = rel(1), color = "black"),
     legend.title = element_text(face = 'bold', size = rel(1.2))
     ) +
   labs(fill = 'Agrupamento',
         x = 'Longitude',
         y = 'Latitude') +
     scale_fill_viridis_d()
```

![](README_files/figure-gfm/unnamed-chunk-47-3.png)<!-- -->

#### Por período (até e após 2018)

``` r
nome_periodo <- c("before-2018","after-2018")
vetor_de_grupos<-c(3,3)
for(i in seq_along(nome_periodo)){
  
  kgr_maps_wider <- kgr_maps |> 
    mutate(
      year = as.numeric(str_sub(season_year,1,2))+2000,
      period = ifelse(year<=2018,"before-2018","after-2018")) |> 
    group_by(period,season_year,X,Y) |>
    filter(period == nome_periodo[i]) |> 
    select(period, season_year, X,Y,city,xco2_anomaly) |> 
    group_by(season_year, period, city) |> 
    summarise(xco2_anomaly = mean(xco2_anomaly, na.rm = TRUE),
              .groups = "drop") |> 
    pivot_wider(names_from = season_year,
                values_from = xco2_anomaly,names_prefix = "season_")
  
  name_muni <- kgr_maps_wider |> pull(city)
  kgr_maps_wider_xco2 <- kgr_maps_wider |> 
    select(-c(period, city)) #|> 
  # select(ends_with("2"))
  
  mc <- cor(kgr_maps_wider_xco2)
  corrplot::corrplot(mc)
  da_pad<-decostand(kgr_maps_wider_xco2, 
                    method = "standardize",
                    na.rm=TRUE)
  da_pad_euc<-vegdist(da_pad,"euclidean") 
  da_pad_euc_ward<-hclust(da_pad_euc, method="ward.D")
  plot(da_pad_euc_ward, 
       ylab="Distância Euclidiana",
       xlab="Acessos", hang=-1,
       col="blue", las=1,
       cex=.6,lwd=1.5);box()
  grupo<-cutree(da_pad_euc_ward,vetor_de_grupos[i])
  
  city_kgr_beta_group <- city_kgr_beta_anom |> 
    left_join(
      tibble(name_muni, grupo),
      by="name_muni")
  
  plot_cidades_agrupamento <- city_kgr_beta_group  |>
    drop_na() |> 
    ggplot() +
    geom_sf(aes(fill=grupo), color="transparent",
            size=.05, show.legend = TRUE)  +
    geom_sf(data=citys |> filter(abbrev_state == "SP"), fill="transparent", size=3, show.legend = FALSE) +
    theme_bw() +
    theme(
      axis.text.x = element_text(size = rel(.9), color = "black"),
      axis.title.x = element_text(size = rel(1.1), color = "black"),
      axis.text.y = element_text(size = rel(.9), color = "black"),
      axis.title.y = element_text(size = rel(1.1), color = "black"),
      legend.text = element_text(size = rel(1), color = "black"),
      legend.title = element_text(face = 'bold', size = rel(1.2))
    ) +
    labs(fill = 'Agrupamento',
         x = 'Longitude',
         y = 'Latitude',
         title = nome_periodo[i]) +
    scale_fill_viridis_c()
  print(plot_cidades_agrupamento)
}
```

![](README_files/figure-gfm/unnamed-chunk-48-1.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-48-2.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-48-3.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-48-4.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-48-5.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-48-6.png)<!-- -->

### 6) Uso da terra

``` r
land_use_change <- read_rds("data/grid-kgr_luc.rds") 
glimpse(land_use_change)
#> Rows: 78,210
#> Columns: 7
#> $ id        <dbl> 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 2, 2, 2, 3, 3, …
#> $ longitude <dbl> -53.06011, -53.06011, -53.06011, -53.06011, -53.06011, -53.0…
#> $ latitude  <dbl> -22.61232, -22.61232, -22.61232, -22.61232, -22.61232, -22.6…
#> $ city      <chr> "Rosana", "Rosana", "Rosana", "Rosana", "Rosana", "Rosana", …
#> $ year      <dbl> 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022, 2023, 2015, …
#> $ cover     <dbl> 21, 21, 21, 21, 21, 21, 21, 21, 21, 3, 3, 3, 3, 3, 3, 3, 3, …
#> $ descricao <chr> "Mosaic of Uses", "Mosaic of Uses", "Mosaic of Uses", "Mosai…
```

``` r
kgr_maps_cover <- kgr_maps |> ungroup() |> 
  mutate(
    year = str_sub(season_year,1,2) |> as.numeric() +2000,
    season = str_sub(season_year,-1) |> as.numeric(),
    ) |> 
  filter(season ==1) |> 
  arrange(X,Y) |> 
  add_column(
    land_use_change |> 
      arrange(longitude,latitude) |> 
      rename(YEAR=year, CITY=city)
  ) |> add_row(
    kgr_maps |> 
  mutate(
    year = str_sub(season_year,1,2) |> as.numeric() +2000,
    season = str_sub(season_year,-1) |> as.numeric(),
    ) |> 
  filter(season ==2) |> 
  arrange(X,Y) |> 
  add_column(
    land_use_change |> 
      arrange(longitude,latitude) |> 
      rename(YEAR=year, CITY=city)
  ) 
) |> 
  select(X:season,cover,descricao) |> 
  arrange(year,season,X,Y)

kgr_maps_cover |> 
  filter(year == 2016) |> 
  mutate(cover = cover |> as_factor()) |> 
  ggplot(aes(x=X, y=Y)) +
  geom_tile(aes(fill = cover)) +
  scale_fill_viridis_d(option = "A") +
  coord_equal() +
  labs(x="Longitude",
       y="Latitude",
       fill="Uso do solo") +
  theme_bw()
```

![](README_files/figure-gfm/unnamed-chunk-50-1.png)<!-- --> \###
Estatísticas descritivas por classe de uso do solo

``` r
kgr_maps_cover |>
  mutate(cover = descricao) |>
  group_by(year, cover) |> 
  summarise(
    count= n(),
  ) |> 
  mutate(
    perc = count/sum(count),
    cover = cover |> fct_lump(n=7,w=perc) |> fct_reorder(count)
  ) |> filter(cover != "Other") |> 
  ggplot(aes(x=as_factor(cover),y=count,fill=cover)) +
  geom_col(color="black") +
  scale_fill_viridis_d(option = "A") +
  facet_wrap(~year) +
  theme_bw()+
  theme(
    legend.position = "none",
    axis.text.x = element_text(angle = 90)
  )
```

![](README_files/figure-gfm/unnamed-chunk-51-1.png)<!-- -->

Retirando os top 7 usos do solo no estado.

``` r
my_top_7_cover <- kgr_maps_cover |>
  mutate(cover = descricao) |>
  group_by(year, cover) |> 
  summarise(
    count= n(),
    .groups = "drop"
  ) |> 
  mutate(
    perc = count/sum(count),
    cover = cover |> fct_lump(n=7,w=perc) |> fct_reorder(count)
  ) |> filter(cover != "Other") |> 
  pull(cover) |> unique()
```

Estatística descritiva de xCO2 para cada classe uso por season

``` r
kgr_maps_cover |> 
  mutate(
    cover_top_7 = ifelse(descricao %in% my_top_7_cover,
                         descricao, "Other"),
    season = as_factor(season)
  ) |> 
  filter(cover_top_7 != "Other") |> 
  ggplot(aes(y=as_factor(year))) +
  geom_density_ridges(rel_min_height = 0.03,
                      aes(x=xco2, fill=season),
                      alpha = .6, color = "black"
  ) +
  # scale_fill_cyclical(values = c("#ff8080","#238B45"),
  #                     name = "classe", guide = "legend") +
  theme_ridges()
```

![](README_files/figure-gfm/unnamed-chunk-53-1.png)<!-- -->

``` r
kgr_maps_cover |>
   mutate(
    cover_top_7 = ifelse(descricao %in% my_top_7_cover,
                         descricao, "Other"),
    season = as_factor(season)
  ) |> 
  filter( cover_top_7 != "Other") |> 
  group_by(year,season,cover_top_7) |>
  summarise(
    N = length(xco2),
    MEAN = mean(xco2),
    MEDIAN = median(xco2),
    STD_DV = sd(xco2),
    SKW = agricolae::skewness(xco2),
    KRT = agricolae::kurtosis(xco2),
    .groups = "drop"
  ) |>
  writexl::write_xlsx("output/estat-desc-year-season-cover.xlsx")
```

``` r
kgr_maps_cover |>
   mutate(
    cover_top_7 = ifelse(descricao %in% my_top_7_cover,
                         descricao, "Other"),
    season = as_factor(season)
  ) |> 
  filter( cover_top_7 != "Other") |> 
  group_by(year,season,cover_top_7) |>
  ggplot(aes(x=year, y=xco2, fill = descricao)) +
  geom_violin(trim=FALSE) +
  facet_wrap(~season, ncol=2) +
  theme(
    legend.position = "top"
  ) +
  theme_bw()+
  labs(fill="cover") +
  scale_fill_viridis_d(option = "A")
```

![](README_files/figure-gfm/unnamed-chunk-55-1.png)<!-- -->

``` r
kgr_maps_cover |>
   mutate(
    cover_top_7 = ifelse(descricao %in% my_top_7_cover,
                         descricao, "Other"),
    season = as_factor(season)
  ) |> 
  filter( cover_top_7 != "Other") |> 
  group_by(year,season,cover_top_7) |>
  ggplot(aes(x=year, y=xco2_anomaly, fill = descricao)) +
  geom_violin(trim=FALSE) +
  facet_wrap(~season, ncol=2) +
  theme(
    legend.position = "top"
  ) +
  theme_bw()+
  labs(fill="cover") +
  scale_fill_viridis_d(option = "B")
```

![](README_files/figure-gfm/unnamed-chunk-56-1.png)<!-- -->

### Análise de variância (ANOVA ou Kruskal-Wallis)

Comparação de XCO₂ entre diferentes classes de uso do solo.

``` r
anova_group <- function(df){
  mod <- aov(xco2 ~ cover_top_7,
      data = df |> 
        mutate(
          cover_top_7 = as_factor(cover_top_7)
        ))
    xco2 <- df |> 
        mutate(
          cover_top_7 = as_factor(cover_top_7)
        ) |> pull(xco2)
    cover_top_7 <- df |> 
        mutate(
          cover_top_7 = as_factor(cover_top_7)
        ) |> pull(cover_top_7)
  pcm_t <- agricolae::HSD.test(mod,"cover_top_7", group = TRUE)
  pcm_k <- agricolae::kruskal(xco2,cover_top_7, 
                              p.adj = "bonferroni", 
                              group = TRUE)
  pcm_t$groups[,2] <- pcm_k$groups[,2] 
  tb_out <- tibble(cover = rownames(pcm_t$groups),
                   pcm_t$groups)
  return(tb_out)
}

dd <- kgr_maps_cover |>
   mutate(
    cover_top_7 = ifelse(descricao %in% my_top_7_cover,
                         descricao, "Other"),
    season = as_factor(season)
  ) |> 
  group_by(year, season) |> 
  nest() |> 
  mutate(
    anava = map(data,anova_group)
  ) 

# Identificar todas as categorias únicas de "cover"
categorias_cover <- dd |> 
  select(-data) |> 
  unnest(cols = c(anava)) |> 
  pull(cover) |> 
  unique()

# Criar uma paleta de cores fixa para essas categorias
paleta_cores <- setNames(viridis::viridis(length(categorias_cover),option = "A"), categorias_cover)

# Loop para criar os gráficos
for (i in 2015:2023) {
  for (j in 1:2) {
    xco2 <-  dd |> 
      select(-data) |> 
      unnest(cols = c(anava)) |> 
      filter(season == j, year == i) |> 
      pull(xco2)
    min_xco2 <- min(xco2)
    max_xco2 <- max(xco2)
    
    plot_tukey <- dd |> 
      select(-data) |> 
      unnest(cols = c(anava)) |> 
      filter(season == j, year == i) |> 
      arrange(xco2) |>
      ungroup() |> 
      mutate(
        cover = cover |> fct_reorder(xco2)
      ) |>
      ggplot(aes(y = cover, x = xco2, fill = cover)) +
      geom_col(color = "black") +
      geom_text(aes(label = groups), 
                hjust = -0.5, 
                color = "black", 
                size = 5) +
      coord_cartesian(xlim = c(min_xco2, max_xco2)) +
      theme_bw() +
      scale_fill_manual(values = paleta_cores) +  # Aplicar a paleta de cores fixa
      labs(title = paste0(i,"-",
                          ifelse(j==1,"Dry","Rainy"))) +
      theme(
        legend.position = "none",
        plot.title = element_text(hjust = .5)
        )
    print(plot_tukey)
  }
}
```

![](README_files/figure-gfm/unnamed-chunk-57-1.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-57-2.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-57-3.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-57-4.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-57-5.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-57-6.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-57-7.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-57-8.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-57-9.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-57-10.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-57-11.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-57-12.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-57-13.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-57-14.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-57-15.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-57-16.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-57-17.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-57-18.png)<!-- -->

``` r
kgr_maps_beta_cover_group <- kgr_maps_beta_anom |> 
  left_join(
    kgr_maps_cover |> 
      filter(season == 2, year == 2020),
    by = c("X","Y","city")
  ) |> 
  left_join(
    city_kgr_beta_group |> 
      as_tibble() |> 
      select( name_muni, grupo) |> 
      rename(city = name_muni),
    by=c("city")
    ) |> 
  select(-xco2, -xco2_std, -year, -season_year, -season)
```

``` r
kgr_maps_beta_cover_group |> 
  ggplot(aes(x=X, y=Y)) +
  geom_tile(aes(fill = cover)) +
  scale_fill_viridis_c(option = "A") +
  coord_equal() +
  labs(x="Longitude",
       y="Latitude",
       fill="Uso do solo") +
  theme_bw()
```

![](README_files/figure-gfm/unnamed-chunk-59-1.png)<!-- -->

``` r

kgr_maps_beta_cover_group |> 
  ggplot(aes(x=X, y=Y)) +
  geom_tile(aes(fill = as_factor(grupo))) +
  scale_fill_viridis_d() +
  coord_equal() +
  labs(x="Longitude",
       y="Latitude",
       fill="Cluster") +
  theme_bw()
```

![](README_files/figure-gfm/unnamed-chunk-59-2.png)<!-- -->

O arquivo `kgr_maps_beta_cover_group` associa todos os resultados até
aqui gerados, Betas, krigagem, usos do solo.

``` r
kgr_maps_beta_cover_group |> 
  mutate(
    descricao = ifelse(descricao %in% my_top_7_cover,
                         descricao, "Other")
  ) |> 
  group_by(grupo) |> 
  summarise(
    N = n(),
    MIN = min(beta_xco2),
    MEAN = mean(beta_xco2),
    MEDIAN = median(beta_xco2),
    MAX = max(beta_xco2),
    STD_DV = sd(beta_xco2),
    SKW = agricolae::skewness(beta_xco2),
    KRT = agricolae::kurtosis(beta_xco2),
  )
#> # A tibble: 3 × 9
#>   grupo     N     MIN   MEAN MEDIAN   MAX STD_DV    SKW    KRT
#>   <int> <int>   <dbl>  <dbl>  <dbl> <dbl>  <dbl>  <dbl>  <dbl>
#> 1     1  4027 -0.159  0.0569 0.0605 0.199 0.0524 -0.674  1.30 
#> 2     2  2077 -0.0562 0.111  0.117  0.270 0.0567 -0.225 -0.233
#> 3     3  2586  0.0159 0.114  0.108  0.295 0.0450  0.697  0.523
```

``` r
kgr_maps_beta_cover_group |> 
  mutate(
    descricao = ifelse(descricao %in% my_top_7_cover,
                         descricao, "Other")
  ) |> 
  group_by(grupo,descricao) |> 
  summarise(
    count = n()
  ) |> 
  group_by(grupo) |> 
  mutate(
    perc = count/sum(count)
  ) |> 
  ungroup() |> 
  ggplot(aes(y=perc*100,x=as_factor(grupo),fill=descricao)) +
  geom_col(color = "black") +
  scale_fill_manual(values = paleta_cores) +
  theme_bw()
```

![](README_files/figure-gfm/unnamed-chunk-61-1.png)<!-- -->

``` r
kgr_maps_beta_cover_group |> 
  mutate(
    descricao = ifelse(descricao %in% my_top_7_cover,
                         descricao, "Other")
  ) |> 
  ggplot(aes(x=as_factor(grupo), y=anomaly_xco2, fill=as_factor(grupo))) +
  geom_boxplot() +
  scale_fill_viridis_d() +
  theme_bw()
```

![](README_files/figure-gfm/unnamed-chunk-62-1.png)<!-- -->

``` r
kgr_maps_beta_cover_group |> 
  mutate(
    descricao = ifelse(descricao %in% my_top_7_cover,
                         descricao, "Other")
  ) |> 
  ggplot(aes(y=as_factor(grupo))) +
  geom_density_ridges(rel_min_height = 0.03,
                      aes(x=anomaly_xco2, fill=as_factor(grupo)),
                      alpha = .6, color = "black"
  ) +
  # scale_fill_cyclical(values = c("#ff8080","#238B45"),
  #                     name = "classe", guide = "legend") +
  theme_ridges()
```

![](README_files/figure-gfm/unnamed-chunk-63-1.png)<!-- -->

``` r
# primeiro analisa xCO2
# depois, analisa beta xCO2 período
# beta xCO2 período poderia ser maior ou menor com a MUDANÇA no uso da terra
# se passou de pastagem para cana por exemplo
# tem isso também, a mudança no uso da terra pode estar influenciando na derivada xCO2
# agora, valor médio xCO2, acredito, próximo oceano menor
# do que pro interior
```

### Morans Indices I
