
<!-- README.md is generated from README.Rmd. Please edit that file -->

## TCC - Fernando Quadros - Variação da concentração de CO<sub>2</sub> atmosférica no estado de São Paulo: Abordagem de Séries temporais e geoestatística

### 1) Preparo dos dados

#### Carregando pacotes

``` r
library(tidyverse)
library(ggridges)
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
data_set <- read_rds("data/nasa-xco2.rds") |>
  filter(xco2 > 0) |>
  mutate(
    path = str_remove(path, "data-raw/nc4/|\\.nc4"),
    date = as_date(str_sub(path,12,17)),
    year = year(date),
    month = month(date),
    day = day(date),
    .after = "time"
  )
glimpse(data_set)
#> Rows: 4,359,155
#> Columns: 13
#> $ longitude         <dbl> -42.02715, -42.03557, -42.62973, -42.66177, -42.6776…
#> $ latitude          <dbl> -20.58697, -20.59402, -17.97576, -17.83512, -17.8497…
#> $ time              <dbl> 1410021395, 1410021395, 1410021438, 1410021441, 1410…
#> $ date              <date> 2014-09-06, 2014-09-06, 2014-09-06, 2014-09-06, 201…
#> $ year              <dbl> 2014, 2014, 2014, 2014, 2014, 2014, 2014, 2014, 2014…
#> $ month             <dbl> 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9…
#> $ day               <int> 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6…
#> $ xco2              <dbl> 388.4401, 395.8184, 395.9337, 393.9267, 394.3022, 39…
#> $ xco2_quality_flag <int> 1, 1, 1, 1, 1, 0, 0, 1, 1, 1, 1, 1, 0, 1, 1, 1, 1, 0…
#> $ xco2_incerteza    <dbl> 0.5112882, 0.5306644, 0.4663646, 0.4828992, 0.432497…
#> $ path              <chr> "oco2_LtCO2_140906_B11100Ar_230523232559s.nc4", "oco…
#> $ flag_br           <lgl> TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE…
#> $ flag_nordeste     <lgl> FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FAL…
```

Corrigindo o polígono do Estado de São Paulo.

``` r
pol_sp <- states |> filter(abbrev_state == "SP") |> 
  pull(geom) |> 
  pluck(1) |> 
  as.matrix()
pol_sp <- pol_sp[(183:4742),]
pol_sp <- rbind(pol_sp, pol_sp[1,])
# plot(pol_sp)
```

Classificando os pontos pertencentes ao estado

``` r
# data_set_sp <- data_set |>
#   mutate(
#     flag_sp = def_pol(longitude, latitude, pol_sp)
#   ) |>
#   filter(flag_sp)
# write_rds(data_set_sp,"data/nasa-xco2-sp.rds")
data_set_sp <- read_rds("data/nasa-xco2-sp.rds")
```

#### Existe uma tendência regional nos dados, e ela deve ser retirada

``` r
data_set |>
  sample_n(1000) |>
  drop_na() |>
  mutate( year = year - min(year)) |>
  ggplot(aes(x=year, y=xco2)) +
  geom_point() +
  geom_point(shape=21,color="black",fill="gray") +
  geom_smooth(method = "lm") +
  ggpubr::stat_regline_equation(ggplot2::aes(
  label =  paste(..eq.label.., ..rr.label.., sep = "*plain(\",\")~~"))) +
  theme_bw() +
  labs(x="Ano",y="xco2")
```

![](README_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

#### Análise de regressão linear simples para caracterização da tendência.

``` r
mod_trend_xco2 <- lm(xco2 ~ year, 
          data = data_set |> 
            filter(xco2_quality_flag == 0) |> 
            drop_na() |> 
            mutate( year = year - min(year)) 
          )
mod_trend_xco2
#> 
#> Call:
#> lm(formula = xco2 ~ year, data = mutate(drop_na(filter(data_set, 
#>     xco2_quality_flag == 0)), year = year - min(year)))
#> 
#> Coefficients:
#> (Intercept)         year  
#>     397.108        2.343
summary.lm(mod_trend_xco2)
#> 
#> Call:
#> lm(formula = xco2 ~ year, data = mutate(drop_na(filter(data_set, 
#>     xco2_quality_flag == 0)), year = year - min(year)))
#> 
#> Residuals:
#>      Min       1Q   Median       3Q      Max 
#> -15.8384  -0.8619   0.0406   0.9145   8.7057 
#> 
#> Coefficients:
#>              Estimate Std. Error t value Pr(>|t|)    
#> (Intercept) 3.971e+02  2.289e-03  173484   <2e-16 ***
#> year        2.343e+00  4.307e-04    5440   <2e-16 ***
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> 
#> Residual standard error: 1.473 on 1642363 degrees of freedom
#> Multiple R-squared:  0.9474, Adjusted R-squared:  0.9474 
#> F-statistic: 2.96e+07 on 1 and 1642363 DF,  p-value: < 2.2e-16
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
         epoch != "14:15")
  
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
  scale_fill_viridis_d()
```

![](README_files/figure-gfm/unnamed-chunk-10-1.png)<!-- -->

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

grid_geral <- read_rds("data/grid-city.rds")
citys |> 
  filter(abbrev_state == "SP") |> 
   ggplot() +
     geom_sf(aes_string(), color="black",
              size=.05, show.legend = TRUE) +
  theme_minimal() +
  geom_point(
    data = grid_geral |> 
  sample_n(1000),
  aes(X,Y),
  color = "red"
  )
```

![](README_files/figure-gfm/unnamed-chunk-14-1.png)<!-- -->

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
my_season = "dry_15:16"
data_set_aux  <- data_set_sp |>
  filter(
    season_year == my_season) |>
  dplyr::select(longitude, latitude, xco2)

vct_xco2 <- vector();dist_xco2 <- vector();
lon_grid <- vector();lat_grid <- vector();
for(i in 1:nrow(data_set_aux)){
  d <- sqrt((data_set_aux$longitude[i] - grid$X)^2 + 
              (data_set_aux$lat[i] - grid$Y)^2
  )
  min_index <- order(d)[1]
  vct_xco2[i] <- data_set_aux$xco2[min_index]
  dist_xco2[i] <- d[order(d)[1]]
  lon_grid[i] <- grid$X[min_index]
  lat_grid[i] <- grid$Y[min_index]
}
data_set_aux$dist_xco2 <- dist_xco2
data_set_aux$xco2_new <- vct_xco2
data_set_aux$lon_grid <- lon_grid
data_set_aux$lat_grid <- lat_grid
data_set_aux |> 
  group_by(lon_grid,lat_grid) |> 
  summarise(
    xco2 = mean(xco2)
  ) |> 
  rename(longitude = lon_grid, latitude = lat_grid) -> data_set_aux
sp::coordinates(data_set_aux) = ~ longitude + latitude
```

#### PASSO 2 - Construção do Semivariograma Experimental

``` r
form <- xco2 ~ 1
vari_exp <- gstat::variogram(form, data = data_set_aux,
                      cressie = FALSE,
                      cutoff = 2, # distância máxima 8
                      width = 0.075) # distancia entre pontos
vari_exp  |>
  ggplot(aes(x=dist, y=gamma)) +
  geom_point() +
  labs(x="lag (º)",
       y=expression(paste(gamma,"(h)")))
```

![](README_files/figure-gfm/unnamed-chunk-17-1.png)<!-- -->

#### PASSO 3) Ajuste dos modelos matemáticos teóricos ao semivariograma experimental

``` r
patamar=1.4
alcance=0.2
epepita=0.5
modelo_1 <- fit.variogram(vari_exp,vgm(patamar,"Sph",alcance,epepita))
modelo_2 <- fit.variogram(vari_exp,vgm(patamar,"Exp",alcance,epepita))
modelo_3 <- fit.variogram(vari_exp,vgm(patamar,"Gau",alcance,epepita))
plot_my_models(modelo_1,modelo_2,modelo_3)
```

![](README_files/figure-gfm/unnamed-chunk-18-1.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-18-2.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-18-3.png)<!-- -->
\#### PASSO 4) Escolha do melhor modelo

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
#         ifelse(dist <= a, c0 + (c0_c1 - c0) * (3/2 * (dist/a) - 1/2 * (dist/a)^3),c0_c1), ifelse(model == "Exp", c0 + (c0_c1-c0)*(1-exp(-3*(dist/a))),c0 + (c0_c1-c0)*(1-exp(-3*(dist/a)^2)))),
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
};rds_reader(list_rds[1])
#> # A tibble: 8,690 × 5
#>        X     Y  xco2 xco2_std season_year
#>    <dbl> <dbl> <dbl>    <dbl> <chr>      
#>  1 -48.2 -25.2  388.    0.575 15_16_1    
#>  2 -48.1 -25.2  388.    0.575 15_16_1    
#>  3 -48.1 -25.2  388.    0.575 15_16_1    
#>  4 -48.0 -25.2  388.    0.575 15_16_1    
#>  5 -48.2 -25.2  388.    0.575 15_16_1    
#>  6 -48.1 -25.2  388.    0.575 15_16_1    
#>  7 -48.1 -25.2  388.    0.575 15_16_1    
#>  8 -48.0 -25.2  388.    0.575 15_16_1    
#>  9 -48.0 -25.2  388.    0.575 15_16_1    
#> 10 -48.2 -25.1  388.    0.575 15_16_1    
#> # ℹ 8,680 more rows

kgr_maps <- map_df(list_rds, rds_reader)

kgr_maps |> 
  group_by(season_year) |> 
  ggplot(aes(season_year,xco2)) +
  geom_boxplot()+
  theme_bw()
```

![](README_files/figure-gfm/unnamed-chunk-23-1.png)<!-- -->

``` r
kgr_maps <- kgr_maps |> 
  left_join(
    grid_geral,
  by=c("X","Y")
  )
```

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

![](README_files/figure-gfm/unnamed-chunk-25-1.png)<!-- -->

    #> 
    #> [[2]]

![](README_files/figure-gfm/unnamed-chunk-25-2.png)<!-- -->

    #> 
    #> [[3]]

![](README_files/figure-gfm/unnamed-chunk-25-3.png)<!-- -->

    #> 
    #> [[4]]

![](README_files/figure-gfm/unnamed-chunk-25-4.png)<!-- -->

    #> 
    #> [[5]]

![](README_files/figure-gfm/unnamed-chunk-25-5.png)<!-- -->

    #> 
    #> [[6]]

![](README_files/figure-gfm/unnamed-chunk-25-6.png)<!-- -->

    #> 
    #> [[7]]

![](README_files/figure-gfm/unnamed-chunk-25-7.png)<!-- -->

    #> 
    #> [[8]]

![](README_files/figure-gfm/unnamed-chunk-25-8.png)<!-- -->

    #> 
    #> [[9]]

![](README_files/figure-gfm/unnamed-chunk-25-9.png)<!-- -->

    #> 
    #> [[10]]

![](README_files/figure-gfm/unnamed-chunk-25-10.png)<!-- -->

    #> 
    #> [[11]]

![](README_files/figure-gfm/unnamed-chunk-25-11.png)<!-- -->

    #> 
    #> [[12]]

![](README_files/figure-gfm/unnamed-chunk-25-12.png)<!-- -->

    #> 
    #> [[13]]

![](README_files/figure-gfm/unnamed-chunk-25-13.png)<!-- -->

    #> 
    #> [[14]]

![](README_files/figure-gfm/unnamed-chunk-25-14.png)<!-- -->

    #> 
    #> [[15]]

![](README_files/figure-gfm/unnamed-chunk-25-15.png)<!-- -->

    #> 
    #> [[16]]

![](README_files/figure-gfm/unnamed-chunk-25-16.png)<!-- -->

    #> 
    #> [[17]]

![](README_files/figure-gfm/unnamed-chunk-25-17.png)<!-- -->

    #> 
    #> [[18]]

![](README_files/figure-gfm/unnamed-chunk-25-18.png)<!-- -->

``` r
kgr_maps_nested <- kgr_maps |>
  group_by(season_year,X,Y) |>
  summarise(
    media = mean(xco2,na.rm = TRUE),
    desv_pad = mean(xco2_std)
  ) |>
  group_by(X,Y) |>
  nest() |>
  ungroup()

get_reg_lin <- function(df, par_return = "beta"){
  x <- seq(0,8.5,0.5)
  y <- df$media
  mod <- lm(y~x)
  value <- mod$coefficients[[2]]
  if(par_return == "beta") return(value)
}

kgr_maps_beta <- kgr_maps_nested |>
  mutate(
    beta = map(data,get_reg_lin)
  ) |>
  select(-data) |>
  unnest(cols = c(beta))

kgr_maps_beta <- kgr_maps_beta |> 
  left_join(
    grid_geral,
  by=c("X","Y")
  )
```

``` r
kgr_maps_beta |>
  ggplot(aes(x=X, y=Y)) +
  geom_tile(aes(fill = beta)) +
  scale_fill_viridis_c(option = "inferno") +
  coord_equal() +
  labs(x="Longitude",
       y="Latitude",
       fill="Beta_xco2") +
  theme_bw()
```

![](README_files/figure-gfm/unnamed-chunk-27-1.png)<!-- -->

``` r

kgr_maps_beta  |> 
  rename(longitude = X, latitude = Y) |> 
  select(-beta) |> 
  writexl::write_xlsx("output/grid-kgr.xlsx")
```

``` r
city_kgr_beta <- left_join(
  citys |> filter(abbrev_state == "SP"),
  kgr_maps_beta |>
    group_by(city) |> 
    summarise(
      beta_xco2 = mean(beta)
    ) |> 
    rename(name_muni = city),
           by="name_muni")

city_kgr_beta |>
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
   labs(fill = 'Beta_xco2',
         x = 'Longitude',
         y = 'Latitude') +
     scale_fill_viridis_c(option = "inferno")
```

![](README_files/figure-gfm/unnamed-chunk-28-1.png)<!-- --> \### 4)
Análise de reconhecimento de padrões

#### Por season (Dry vs Rainy)

``` r
nome_periodo <- c("Dry","Rainy")
vetor_de_grupos<-c(2,4)
for( i in 1:2){
  kgr_maps_wider <- kgr_maps |> 
    mutate(
      season = str_sub(season_year,1,5),
      period = as.numeric(str_sub(season_year,7,7))
      ) |>
    filter( period == i) |> 
    select(season, X,Y,city,xco2) |> 
    group_by(season, city) |> 
    summarise(xco2 = mean(xco2, na.rm = TRUE)) |> 
    pivot_wider(names_from = season,
                values_from = xco2,names_prefix = "season_") 
  
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
  
  city_kgr_beta_group <- city_kgr_beta |> 
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

![](README_files/figure-gfm/unnamed-chunk-29-1.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-29-2.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-29-3.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-29-4.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-29-5.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-29-6.png)<!-- -->

#### Por Média anual

``` r
kgr_maps_wider <- kgr_maps |> 
  select(season_year, X,Y,city,xco2) |> 
  mutate(season = str_sub(season_year,1,5)) |> 
  group_by(season, city) |> 
  summarise(xco2 = mean(xco2, na.rm = TRUE)) |> 
  pivot_wider(names_from = season,
              values_from = xco2,names_prefix = "season_")

name_muni <- kgr_maps_wider |> pull(city)
kgr_maps_wider_xco2 <- kgr_maps_wider |> 
  select(season_15_16:season_23_24) #|> 
  # select(ends_with("2"))
  
mc <- cor(kgr_maps_wider_xco2)
corrplot::corrplot(mc)
```

![](README_files/figure-gfm/unnamed-chunk-30-1.png)<!-- -->

``` r
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
```

![](README_files/figure-gfm/unnamed-chunk-30-2.png)<!-- -->

``` r
grupo<-cutree(da_pad_euc_ward,4)

city_kgr_beta_group <- city_kgr_beta |> 
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

![](README_files/figure-gfm/unnamed-chunk-30-3.png)<!-- -->

## Entrando com o uso do solo

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
land_use_change |> 
  filter(year == 2015) |> 
  ggplot(aes(x=longitude, y=latitude)) +
  geom_point()
```

![](README_files/figure-gfm/unnamed-chunk-31-1.png)<!-- -->

``` r

kgr_maps_cover <- kgr_maps |> 
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
  filter(year == 2015) |> 
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

![](README_files/figure-gfm/unnamed-chunk-31-2.png)<!-- --> \###
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

![](README_files/figure-gfm/unnamed-chunk-32-1.png)<!-- -->

Retirando os top 7 usos do solo no estado.

``` r
my_top_7_cover <- kgr_maps_cover |>
  mutate(cover = descricao) |>
  group_by(year, cover) |> 
  summarise(
    count= n(),
  ) |> 
  mutate(
    perc = count/sum(count),
    cover = cover |> fct_lump(n=7,w=perc) |> fct_reorder(count)
  ) |> filter(cover != "Other") |> 
  pull(cover) |> unique()
```

Estatísticia descritiva de xCO2 para cada classe uso por season

``` r
kgr_maps_cover |> 
  mutate(
    cover_top_7 = ifelse(descricao %in% my_top_7_cover,
                         descricao, "Other"),
    season = as_factor(season)
  ) |> 
  ggplot(aes(y=as_factor(year))) +
  geom_density_ridges(rel_min_height = 0.03,
                      aes(x=xco2, fill=season),
                      alpha = .6, color = "black"
  ) +
  # scale_fill_cyclical(values = c("#ff8080","#238B45"),
  #                     name = "classe", guide = "legend") +
  theme_ridges()
```

![](README_files/figure-gfm/unnamed-chunk-34-1.png)<!-- -->

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

![](README_files/figure-gfm/unnamed-chunk-36-1.png)<!-- --> \### Análise
de variância (ANOVA ou Kruskal-Wallis)

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

![](README_files/figure-gfm/unnamed-chunk-37-1.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-37-2.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-37-3.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-37-4.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-37-5.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-37-6.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-37-7.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-37-8.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-37-9.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-37-10.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-37-11.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-37-12.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-37-13.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-37-14.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-37-15.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-37-16.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-37-17.png)<!-- -->![](README_files/figure-gfm/unnamed-chunk-37-18.png)<!-- -->

``` r
kgr_maps_beta_cover_group <- kgr_maps_beta |> 
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

![](README_files/figure-gfm/unnamed-chunk-39-1.png)<!-- -->

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

![](README_files/figure-gfm/unnamed-chunk-39-2.png)<!-- -->

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
    MIN = min(beta),
    MEAN = mean(beta),
    MEDIAN = median(beta),
    MAX = max(beta),
    STD_DV = sd(beta),
    SKW = agricolae::skewness(beta),
    KRT = agricolae::kurtosis(beta),
  )
#> # A tibble: 4 × 9
#>   grupo     N     MIN   MEAN MEDIAN   MAX STD_DV    SKW     KRT
#>   <int> <int>   <dbl>  <dbl>  <dbl> <dbl>  <dbl>  <dbl>   <dbl>
#> 1     1  2140 -0.159  0.0687 0.0698 0.295 0.0514 -0.172  2.11  
#> 2     2  2480 -0.0326 0.125  0.130  0.270 0.0503 -0.200 -0.0986
#> 3     3   805  0.0263 0.105  0.0971 0.210 0.0358  0.559 -0.454 
#> 4     4  3265 -0.157  0.0654 0.0684 0.291 0.0572 -0.328  1.40
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

![](README_files/figure-gfm/unnamed-chunk-41-1.png)<!-- -->

``` r
kgr_maps_beta_cover_group |> 
  mutate(
    descricao = ifelse(descricao %in% my_top_7_cover,
                         descricao, "Other")
  ) |> 
  ggplot(aes(x=as_factor(grupo), y=beta, fill=as_factor(grupo))) +
  geom_boxplot() +
  scale_fill_viridis_d() +
  theme_bw()
```

![](README_files/figure-gfm/unnamed-chunk-42-1.png)<!-- -->

``` r
kgr_maps_beta_cover_group |> 
  mutate(
    descricao = ifelse(descricao %in% my_top_7_cover,
                         descricao, "Other")
  ) |> 
  ggplot(aes(y=as_factor(grupo))) +
  geom_density_ridges(rel_min_height = 0.03,
                      aes(x=beta, fill=as_factor(grupo)),
                      alpha = .6, color = "black"
  ) +
  # scale_fill_cyclical(values = c("#ff8080","#238B45"),
  #                     name = "classe", guide = "legend") +
  theme_ridges()
```

![](README_files/figure-gfm/unnamed-chunk-43-1.png)<!-- -->

``` r
# primeiro analisa xCO2
# depois, analisa beta xCO2 período
# beta xCO2 período poderia ser maior ou menor com a MUDANÇA no uso da terra
# se passou de pastagem para cana por exemplo
# tem isso também, a mudança no uso da terra pode estar influenciando na derivada xCO2
# agora, valor médio xCO2, acredito, próximo oceano menor
# do que pro interior
```

### 5) Caracterização da Série Temporal

### 6) Aprendizado de Máquina não Supervisionado
