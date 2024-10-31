# # Análise do Trabalho de Conclusão de Curso:
# ## Séries temporais de xCO2 no estado de São Paulo
#
# ### Pacotes necessários
library(tidyverse)
library(ggridges)
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
## Função para classificação do ponto
def_pol <- function(x, y, pol){
  as.logical(sp::point.in.polygon(point.x = x,
                                  point.y = y,
                                  pol.x = pol[,1],
                                  pol.y = pol[,2]))
}
#
#
#
## Retirando os polígono
estados <- geobr::read_state(showProgress = FALSE)
pol_sp <- estados$geom |> pluck(20) |> as.matrix()
pol_sp <- pol_sp[(183:4742),]
pol_sp <- rbind(pol_sp, pol_sp[1,])
head(pol_sp)
tail(pol_sp)
plot(pol_sp)

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
    year >= 2015 & year <= 2023,

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
data_set_sp <- data_set_sp |>
  mutate(
    season_year = ifelse(month <= 2, paste0("rainy_",year-2001,":",year-2000),
                           ifelse(month > 2 & month < 9,
                                  paste0("dry_",year-2000,":",year-1999),
                                  paste0("rainy_",year-2000,":",year-1999))),
    epoch = str_remove(season_year,"dry_|rainy_")
      ) |>
  filter(season_year != "rainy_2014-2015",
         epoch != "14:15")
glimpse(data_set_sp)

# contar as categorias criadas
data_set_sp |>
  pull(season_year) |>
  unique()

#Gráficos
# Histogramas
data_set_sp %>%
  ggplot(aes(x=xco2,fill=season)) +
  geom_histogram(color="black",
                 bins = 30) +
  facet_wrap(~epoch)

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

#Estatística Descritiva
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
  group_by(epoch,season) |>
  summarise(
    N = length(xco2),
    MEAN = mean(xco2),
    MEDIAN = median(xco2),
    STD_DV = sd(xco2),
    SKW = agricolae::skewness(xco2),
    KRT = agricolae::kurtosis(xco2),
  ) |>
  writexl::write_xlsx("output/estat-desc-epoch-season.xlsx")


# Criar o adensamento de pontos
x<-data_set_sp$longitude
y<-data_set_sp$latitude
dis <- 0.05 #Distância entre pontos
grid <- expand.grid(X=seq(min(x),max(x),dis), Y=seq(min(y),max(y),dis)) %>%
  mutate(flag = def_pol(X,Y,pol_sp)) %>%
  filter(flag) %>% select(-flag)
sp::gridded(grid) = ~ X + Y
plot(grid$X,grid$Y)
points(x,y,col="red",pch=4)



# Análise geoestatística
my_season = "rainy_15:16"
data_set_aux  <- data_set_sp |>
  filter(season_year == my_season) |>
  select(longitude, latitude, xco2)

data_set_aux %>%
  ggplot(aes(x=longitude,y=latitude,color=xco2)) +
  geom_point()

# Alteração no df
sp::coordinates(data_set_aux) = ~ longitude + latitude
form <- xco2 ~ 1

# Criar um semivariograma
vari_exp <- gstat::variogram(form, data = data_set_aux,
                      cressie = FALSE,
                      cutoff = .55, # distância máxima do semivariograma
                      width = .03) # distancia entre pontos
vari_exp  %>%
  ggplot(aes(x=dist, y=gamma)) +
  geom_point() +
  labs(x="lag (º)",
       y=expression(paste(gamma,"(h)")))


patamar=.85
alcance=.2
epepita=0
modelo_1 <- gstat::fit.variogram(vari_exp,gstat::vgm(patamar,"Sph",alcance,epepita))
modelo_2 <- gstat::fit.variogram(vari_exp,gstat::vgm(patamar,"Exp",alcance,epepita))
modelo_3 <- gstat::fit.variogram(vari_exp,gstat::vgm(patamar,"Gau",alcance,epepita))
sqr.f1<-round(attr(modelo_1, "SSErr"),4); c01<-round(modelo_1$psill[[1]],4); c0_c11<-round(sum(modelo_1$psill),4);a1<-round(modelo_1$range[[2]],2)
sqr.f2<-round(attr(modelo_2, "SSErr"),4); c02<-round(modelo_2$psill[[1]],4); c0_c12<-round(sum(modelo_2$psill),4);a2<-round(3*modelo_2$range[[2]],2)
sqr.f3<-round(attr(modelo_3, "SSErr"),4); c03<-round(modelo_3$psill[[1]],4); c0_c13<-round(sum(modelo_3$psill),4);a3<-round(modelo_3$range[[2]]*(3^.5),2)

df_aux <- vari_exp %>%
  mutate(
    gamma_m1 = ifelse(dist <= a1, c01 + (c0_c11-c01)*(3/2*(dist/a1)-1/2*(dist/a1)^3),c0_c11),
    gamma_m2 = c02 + (c0_c12-c02)*(1-exp(-3*(dist/a2))),
    gamma_m3 = c03 + (c0_c13-c03)*(1-exp(-3*(dist/a3)^2)),
    residuo_total = (gamma-mean(gamma))^2,
    residuo_mod_1 = (gamma - gamma_m1)^2,
    residuo_mod_2 = (gamma - gamma_m2)^2,
    residuo_mod_3 = (gamma - gamma_m3)^2
  ) %>%
  summarise(
    r2_1=(sum(residuo_total) - sum(residuo_mod_1))/sum(residuo_total),
    r2_2=(sum(residuo_total) - sum(residuo_mod_2))/sum(residuo_total),
    r2_3=(sum(residuo_total) - sum(residuo_mod_3))/sum(residuo_total),
  )
r21<-as.vector(round(df_aux[1],4))
r22<-as.vector(round(df_aux[2],4))
r23<-as.vector(round(df_aux[3],4))

plot(vari_exp,
     model=modelo_1,
     col=1,pl=F,
     pch=16,
     cex=1.2,cex.main=7,
     ylab=list("Semivariância",cex=1.3),
     xlab=list("Distância de Separação h (m)",cex=1.3),
     main =paste("Esf(C0= ",c01,"; C0+C1= ",
                 c0_c11, "; a= ", a1,"; r2 = ",
                 r21,")",sep=""))

plot(vari_exp,model=modelo_2, col=1,pl=F,pch=16,cex=1.2,cex.main=7,ylab=list("Semivariância",cex=1.3),xlab=list("Distância de Separação h (m)",cex=1.3),main =paste("Exp(C0= ",c02,"; C0+C1= ", c0_c12, "; a= ", a2,"; r2 = ", r22,")",sep=""))
plot(vari_exp,model=modelo_3, col=1,pl=F,pch=16,cex=1.2,cex.main=7,ylab=list("Semivariância",cex=1.3),xlab=list("Distância de Separação h (m)",cex=1.3),main =paste("Gau(C0= ",c03,"; C0+C1= ", c0_c13, "; a= ", a3,"; r2 = ", r23,")",sep=""))

## Validação Cruzada
conjunto_validacao <- data_set_aux %>%
  as_tibble() %>%
  sample_n(200)
sp::coordinates(conjunto_validacao) = ~longitude + latitude
modelos<-list(modelo_1,modelo_2,modelo_3)
for(j in 1:3){
  est<-0
  # vari<-as.character(form)[2]
  for(i in 1:nrow(conjunto_validacao)){
    valid <- gstat::krige(formula=form, conjunto_validacao[-i,], conjunto_validacao, model=modelos[[j]])
    est[i]<-valid$var1.pred[i]
  }
  obs<-as.data.frame(conjunto_validacao)[,3]
  RMSE<-round((sum((obs-est)^2)/length(obs))^.5,3)
  mod<-lm(obs~est)
  b<-round(mod$coefficients[2],3)
  se<-round(summary(mod)$coefficients[4],3)
  r2<-round(summary(mod)$r.squared,3)
  a<-round(mod$coefficients[1],3)
  plot(est,obs,xlab="Estimado", ylab="Observado",pch=j,col="blue",
       main=paste("Modelo = ",modelos[[j]][2,1],"; Coef. Reg. = ", b, " (SE = ",se, ", r2 = ", r2,")\ny intersept = ",a,"RMSE = ",RMSE ))
  abline(lm(obs~est));
  abline(0,1,lty=3)
}


# selecionar o melhor modelo Modelar o semivariograma
modelo <- modelo_2 ## sempre modificar
plot(vari_exp,model=modelo, col=1,pl=F,pch=16)


ko_variavel <- gstat::krige(formula=form, data_set_aux, grid, model=modelo,
                     block=c(0,0),
                     nsim=0,
                     na.action=na.pass,
                     debug.level=-1,
)

mapa <- as.tibble(ko_variavel) %>%
  ggplot(aes(x=X, y=Y)) +
  geom_tile(aes(fill = var1.pred)) +
  # scale_fill_gradient(low = "yellow", high = "blue") +
  scale_fill_viridis_c() +
  coord_equal() +
  labs(x="Longitude",
       y="Latitude") #+
  # geom_point(data = data_set_sp |>
  #              filter(season_year == "dry_2015"),aes(x=longitude,y=latitude))
mapa

df <- ko_variavel %>%
  as.tibble() %>%
  mutate(var1.var = sqrt(var1.var)) %>%
  rename(
    !!my_season := var1.pred,
    !!paste0(my_season,"_sd") := var1.var,
  )
# write_rds(df,paste0("out/",variavel,"-",ano_analise,".rds"))



