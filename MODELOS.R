###
#  Pacotes utilizados nos Modelos ----

library(tidyverse)
library(haven)
library(readr)
library(readxl)
library(janitor)
library(treemap)
library(readr)
library(sjPlot) 
library(xfun) 
library(performance) 
library(see) 


# Modelos ---- 
##

load ("G:/Meu Drive/Acadêmica/meus.trabalhos.maiores/dissertacao/banco_de_dados/banco_dissertacao_pronto.Rda")


attributes(banco_dissertacao_pronto$pib_per_2010)$label <- NULL
attributes(banco_dissertacao_pronto$pib_per_2012)$label <- NULL
attributes(banco_dissertacao_pronto$pib_per_2016)$label <- NULL
attributes(banco_dissertacao_pronto$pp_trans_2020)$label <- NULL
attributes(banco_dissertacao_pronto$pp_trans_2005)$label <- NULL
attributes(banco_dissertacao_pronto$pp_trans_2006)$label <- NULL
attributes(banco_dissertacao_pronto$pp_trans_2007)$label <- NULL
attributes(banco_dissertacao_pronto$pp_trans_2008)$label <- NULL
attributes(banco_dissertacao_pronto$pp_trans_2009)$label <- NULL
attributes(banco_dissertacao_pronto$pp_trans_2010)$label <- NULL
attributes(banco_dissertacao_pronto$pp_trans_2011)$label <- NULL
attributes(banco_dissertacao_pronto$pp_trans_2012)$label <- NULL
attributes(banco_dissertacao_pronto$pp_trans_2013)$label <- NULL
attributes(banco_dissertacao_pronto$pp_trans_2014)$label <- NULL
attributes(banco_dissertacao_pronto$pp_trans_2015)$label <- NULL
attributes(banco_dissertacao_pronto$pp_trans_2016)$label <- NULL
attributes(banco_dissertacao_pronto$pp_trans_2017)$label <- NULL
attributes(banco_dissertacao_pronto$pp_trans_2018)$label <- NULL
attributes(banco_dissertacao_pronto$pp_trans_2019)$label <- NULL

banco_dissertacao_pronto <- banco_dissertacao_pronto %>% mutate (pop_log_2012 = log(pop_2012_siops),
                                                                 gasto_log_medio_05_08 = log(gasto_medio_05_08), 
                                                                 gasto_log_medio_09_12 = log(gasto_medio_09_12),
                                                                 gasto_log_medio_13_16 = log(gasto_medio_13_16),
                                                                 gasto_log_medio_17_20 = log(gasto_medio_17_20),
                                                                 psb_dummy_2008 = case_when(partido_eleito_2008 == 'PSB' ~ '1',
                                                                                            partido_eleito_2008 != 'PSB' ~ '0'))


nomes_vari_dissertacao <- names(banco_dissertacao_pronto)
nomes_vari_dissertacao <- as.data.frame(nomes_vari_dissertacao)

# 1. Modelo ciclo 05_08 com Partidos ----
##

banco_modelo_1 <- banco_dissertacao_pronto %>% mutate (pop_log_2012 = log(pop_2012_siops),
                                                                 gasto_log_medio_05_08 = log(gasto_medio_05_08), 
                                                                 gasto_log_medio_09_12 = log(gasto_medio_09_12),
                                                                 gasto_log_medio_13_16 = log(gasto_medio_13_16),
                                                                 gasto_log_medio_17_20 = log(gasto_medio_17_20),
                                                                 psb_dummy_2008 = case_when(partido_eleito_2008 == 'PSB' ~ '1',
                                                                                            partido_eleito_2008 != 'PSB' ~ '0')) %>%  
        rename (`Ciclo 05-08`  = gasto_log_medio_05_08,
# `Ciclo 09-12`  = gasto_log_medio_09_12,
# `Ciclo 13-16`  = gasto_log_medio_13_16,
# `Ciclo 17-20`  = ,gasto_log_medio_17_20,
`Log da Populacao` =pop_log_2012, 
 `Centro-Oeste` = centor_oeste_dummy,
 `Nordeste`     = nordeste_dummy,
 `Sudeste`      = sudeste_dummy,
 `Sul`          = sul_dummy,
 `PIB per capita`   = pib_per_2010,
# `PIB per capita`   = pib_per_2012,
 `IDHM`             = idhm_10,
 `Dependencia Financeira`       = pp_trans_2005,
# `Dependencia Financeira`       = pp_trans_2009,
# `Dependencia Financeira`       = pp_trans_2013,
# `Dependencia Financeira`       = pp_trans_2017,
 `Municipio Urbano`             = dummy_urbano, 
`Municipio Polo`                =  municipio_polo,
 `Primeiro Mandato` = pri_mandato_04,
 #`Primeiro Mandato` = pri_mandato_08,
 #`Primeiro Mandato` = pri_mandato_12,
 #`Primeiro Mandato` = pri_mandato_16,
 `Maioria Parlamentar` = dummy_maioria_2004,
 #`Maioria Parlamentar` = dummy_maioria_2012,
 #`Maioria Parlamentar` = dummy_maioria_2016,
 `Prof de Saude`       = prof_saude_dummy_2004,
 #`Prof de Saude`       = prof_saude_dummy_2008,
 #`Prof de Saude`       = prof_saude_dummy_2012,
 #`Prof de Saude`       = prof_saude_dummy_2016,
 `PT` = pt_dummy_2004,
 `PDT`   = pdt_dummy_2004,
 `PFL/DEM`   = pfl_dummy_2004,
 `PL`   = pl_dummy_2004,
 `PMDB`   = pmdb_dummy_2004,
 `PPS`   = pps_dummy_2004,
 `PSB`   = psb_dummy_2004,
 `PSDB`   = psdb_dummy_2004,
 `PTB`   = ptb_dummy_2004,
 `PP`   = pp_dummy_2004)
 
 #`PT`   = pt_dummy_2008,
 #`PDT`   = pdt_dummy_2008,
 #`PFL/DEM`   = dem_dummy_2008,
 #`PMDB`   = pmdb_dummy_2008,
 #`PP`   = pp_dummy_2008,
 #`PPS`   = pps_dummy_2008,
 #`PR`   = pr_dummy_2008,
 #`PSDB`   = psdb_dummy_2008,
 #`PTB`   = ptb_dummy_2008,
 #`PSB`   = psb_dummy_2008,
 
# `PDT`   = pdt_dummy_2012,
# `PFL/DEM`   = dem_dummy_2012,
# `PMDB`   = pmdb_dummy_2012,
# `PP`   = pp_dummy_2012,
# `PSD`   = psd_dummy_2012,
# `PR`   = pr_dummy_2012,
# `PSDB`   = psdb_dummy_2012,
# `PTB`   = ptb_dummy_2012,
# `PSB`   = psb_dummy_2012,
# `PT`   = pt_dummy_2012,
 
 #`PDT`   = pdt_dummy_2016,
 #`PFL/DEM`   = dem_dummy_2016,
 #`PMDB`   = pmdb_dummy_2016,
 #`PP`   = pp_dummy_2016,
 #`PSD`   = psd_dummy_2016,
 #`PSDB`   = psdb_dummy_2016,
 #`PTB`   = ptb_dummy_2016,
 #`PSB`   = psb_dummy_2016,
 #`PR`   = pr_dummy_2016,
 #`PT`   = pt_dummy_2016,
#`Esquerda com maioria` = esquerda_maioria_2004,
#`Direita com maioria`= direita_maioria_2004,
#`Centro com maioria`= centro_maioria_2004,
#`PT com maioria`= pt_maioria_2004,
#`PSDB com maioria`= psdb_maioria_2004,
#`PFL/DEM com maioria`= pfl_maioria_2004,
#`Esquerda com maioria`= esquerda_maioria_2012,
#`Direita com maioria`= direita_maioria_2012,
#`Centro com maioria`= centro_maioria_2012,
#`PT com maioria`= pt_maioria_2012,
#`PSDB com maioria`= psdb_maioria_2012,
#`DEM com maioria`= dem_maioria_2012,
#`Esquerda com maioria`= esquerda_maioria_2016,
#`Direita com maioria`= direita_maioria_2016,
#`Centro com maioria`= centro_maioria_2016,
#`PT com maioria`= pt_maioria_2016,
#`PSDB com maioria`= psdb_maioria_2016,
#`PFL/DEM com maioria`= dem_maioria_2016)

modelo1<- lm(`Ciclo 05-08` ~ + `Centro-Oeste`
                     # +`Ciclo 09-12`, 
                     # +`Ciclo 13-16`,
                     # +`Ciclo 17-20`,
                    
                      +`Nordeste`
                      +`Sudeste` 
                      +`Sul`
                     +`PIB per capita`
             +    `Log da Populacao` 
                     # +`PIB per capita`,
                     +`IDHM`
                     +`Dependencia Financeira` 
             # +`Dependencia Financeira` ,
             # +`Dependencia Financeira`,
             # +`Dependencia Financeira`,
             +`Municipio Urbano`  
             +`Municipio Polo`                
             +`Primeiro Mandato`
             #+`Primeiro Mandato` ,
             #+`Primeiro Mandato` ,
             #+`Primeiro Mandato`,
             +`Maioria Parlamentar` 
             #+`Maioria Parlamentar`  ,
             #+`Maioria Parlamentar`  ,
             +`Prof de Saude` 
             #+`Prof de Saude`  ,
             #+`Prof de Saude`  ,
             #+`Prof de Saude`  ,
             +`PT`  
             +`PDT` 
             +`PFL/DEM` 
             +`PL`    
             +`PMDB` 
             +`PPS`   
             +`PSB`    
             +`PSDB`    
             +`PTB`    
             +`PP`,    
             data = banco_modelo_1)
             #+`PT` ,
             #+`PDT` ,
             #+`PFL/DEM` ,
             #+`PMDB`   ,
             #+`PP`   ,
             #+`PPS`   ,
             #+`PR`    ,
             #+`PSDB`   ,
             #+`PTB`    ,
             #+`PSB`    ,
             
             # +`PDT`   ,
             # +`PFL/DEM` ,
             # +`PMDB`  ,
             # +`PP`   ,
             # +`PSD`   ,
             # +`PR`   ,
             # +`PSDB`   ,
             # +`PTB`   ,
             # +`PSB`  ,
             # +`PT`  ,
             
             #+`PDT`    ,
             #+`PFL/DEM` ,
             #+`PMDB`   ,
             #+`PP`    ,
             #+`PSD`   ,
             #+`PSDB`    ,
             #+`PTB`    ,
             #+`PSB`   ,
             #+`PR`   ,
             #+`PT`  ,
             #+`Esquerda com maioria` ,
             #+`Direita com maioria` ,
             #+`Centro com maioria` ,
             #+`PT com maioria` ,
             #+`PSDB com maioria` ,
             #+`PFL/DEM com maioria` ,
             #+`Esquerda com maioria` ,
             #+`Direita com maioria` ,
             #+`Centro com maioria` ,
             #+`PT com maioria` ,
             #+`PSDB com maioria` ,
             #+`DEM com maioria` ,
             #+`Esquerda com maioria` ,
             #+`Direita com maioria`,
             #+`Centro com maioria` ,
             #+`PT com maioria` ,
             #+`PSDB com maioria` ,
             #+`PFL/DEM com maioria`,
  
tab_model(modelo1)


# 2. Modelo ciclo 09_12 com Partidos  ----
##

banco_modelo_2 <- banco_dissertacao_pronto %>% mutate (pop_log_2012 = log(pop_2012_siops),
                                                       gasto_log_medio_05_08 = log(gasto_medio_05_08), 
                                                       gasto_log_medio_09_12 = log(gasto_medio_09_12),
                                                       gasto_log_medio_13_16 = log(gasto_medio_13_16),
                                                       gasto_log_medio_17_20 = log(gasto_medio_17_20),
                                                       psb_dummy_2008 = case_when(partido_eleito_2008 == 'PSB' ~ '1',
                                                                                  partido_eleito_2008 != 'PSB' ~ '0')) %>%  
        rename (#`Ciclo 05-08`  = gasto_log_medio_05_08,
                 `Ciclo 09-12`  = gasto_log_medio_09_12,
                # `Ciclo 13-16`  = gasto_log_medio_13_16,
                # `Ciclo 17-20`  = ,gasto_log_medio_17_20,
                `Centro-Oeste` = centor_oeste_dummy,
                `Nordeste`     = nordeste_dummy,
                `Sudeste`      = sudeste_dummy,
                `Sul`          = sul_dummy,
                `PIB per capita`   = pib_per_2010,
                `Log da Populacao` =pop_log_2012, 
                # `PIB per capita`   = pib_per_2012,
                `IDHM`             = idhm_10,
                #`Dependencia Financeira`       = pp_trans_2005,
                 `Dependencia Financeira`       = pp_trans_2009,
                # `Dependencia Financeira`       = pp_trans_2013,
                # `Dependencia Financeira`       = pp_trans_2017,
                `Municipio Urbano`             = dummy_urbano, 
                `Municipio Polo`                =  municipio_polo,
                #`Primeiro Mandato` = pri_mandato_04,
                `Primeiro Mandato` = pri_mandato_08,
                #`Primeiro Mandato` = pri_mandato_12,
                #`Primeiro Mandato` = pri_mandato_16,
                #`Maioria Parlamentar` = dummy_maioria_2004,
                #`Maioria Parlamentar` = dummy_maioria_2012,
                #`Maioria Parlamentar` = dummy_maioria_2016,
                #`Prof de Saude`       = prof_saude_dummy_2004,
                `Prof de Saude`       = prof_saude_dummy_2008,
                #`Prof de Saude`       = prof_saude_dummy_2012,
                #`Prof de Saude`       = prof_saude_dummy_2016,
                #`PT` = pt_dummy_2004,
                #`PDT`   = pdt_dummy_2004,
                #`PFL/DEM`   = pfl_dummy_2004,
                #`PL`   = pl_dummy_2004,
                #`PMDB`   = pmdb_dummy_2004,
                #`PPS`   = pps_dummy_2004,
                #`PSB`   = psb_dummy_2004,
                #`PSDB`   = psdb_dummy_2004,
                #`PTB`   = ptb_dummy_2004,
                #`PP`   = pp_dummy_2004,

`PT`   = pt_dummy_2008,
`PDT`   = pdt_dummy_2008,
`PFL/DEM`   = dem_dummy_2008,
`PMDB`   = pmdb_dummy_2008,
`PP`   = pp_dummy_2008,
`PPS`   = pps_dummy_2008,
`PR`   = pr_dummy_2008,
`PSDB`   = psdb_dummy_2008,
`PTB`   = ptb_dummy_2008,
`PSB`   = psb_dummy_2008)
# `PDT`   = pdt_dummy_2012,
# `PFL/DEM`   = dem_dummy_2012,
# `PMDB`   = pmdb_dummy_2012,
# `PP`   = pp_dummy_2012,
# `PSD`   = psd_dummy_2012,
# `PR`   = pr_dummy_2012,
# `PSDB`   = psdb_dummy_2012,
# `PTB`   = ptb_dummy_2012,
# `PSB`   = psb_dummy_2012,
# `PT`   = pt_dummy_2012,

#`PDT`   = pdt_dummy_2016,
#`PFL/DEM`   = dem_dummy_2016,
#`PMDB`   = pmdb_dummy_2016,
#`PP`   = pp_dummy_2016,
#`PSD`   = psd_dummy_2016,
#`PSDB`   = psdb_dummy_2016,
#`PTB`   = ptb_dummy_2016,
#`PSB`   = psb_dummy_2016,
#`PR`   = pr_dummy_2016,
#`PT`   = pt_dummy_2016,
#`Esquerda com maioria` = esquerda_maioria_2004,
#`Direita com maioria`= direita_maioria_2004,
#`Centro com maioria`= centro_maioria_2004,
#`PT com maioria`= pt_maioria_2004,
#`PSDB com maioria`= psdb_maioria_2004,
#`PFL/DEM com maioria`= pfl_maioria_2004,
#`Esquerda com maioria`= esquerda_maioria_2012,
#`Direita com maioria`= direita_maioria_2012,
#`Centro com maioria`= centro_maioria_2012,
#`PT com maioria`= pt_maioria_2012,
#`PSDB com maioria`= psdb_maioria_2012,
#`DEM com maioria`= dem_maioria_2012,
#`Esquerda com maioria`= esquerda_maioria_2016,
#`Direita com maioria`= direita_maioria_2016,
#`Centro com maioria`= centro_maioria_2016,
#`PT com maioria`= pt_maioria_2016,
#`PSDB com maioria`= psdb_maioria_2016,
#`PFL/DEM com maioria`= dem_maioria_2016)

modelo2<- lm(`Ciclo 09-12` ~ + `Centro-Oeste`
             # +`Ciclo 09-12`, 
             # +`Ciclo 13-16`,
             # +`Ciclo 17-20`,
             
             +`Nordeste`
             +`Sudeste` 
             +`Sul`
             +`PIB per capita`
             +    `Log da Populacao` 
             # +`PIB per capita`,
             +`IDHM`
             +`Dependencia Financeira` 
             # +`Dependencia Financeira` ,
             # +`Dependencia Financeira`,
             # +`Dependencia Financeira`,
             +`Municipio Urbano`  
             +`Municipio Polo` 
             +`Primeiro Mandato`
             #+`Primeiro Mandato` ,
             #+`Primeiro Mandato` ,
             #+`Primeiro Mandato`,
            # +`Maioria Parlamentar` 
             #+`Maioria Parlamentar`  ,
             #+`Maioria Parlamentar`  ,
             +`Prof de Saude` 
             #+`Prof de Saude`  ,
             #+`Prof de Saude`  ,
             #+`Prof de Saude`  ,
             +`PT`   
             +`PDT`   
             +`PFL/DEM`
             +`PMDB` 
             +`PP`   
             +`PPS`  
             +`PR`   
             +`PSDB` 
             +`PTB`  
             +`PSB`,    
             data = banco_modelo_2)

#+`PT` ,
#+`PDT` ,
#+`PFL/DEM` ,
#+`PMDB`   ,
#+`PP`   ,
#+`PPS`   ,
#+`PR`    ,
#+`PSDB`   ,
#+`PTB`    ,
#+`PSB`    ,

# +`PDT`   ,
# +`PFL/DEM` ,
# +`PMDB`  ,
# +`PP`   ,
# +`PSD`   ,
# +`PR`   ,
# +`PSDB`   ,
# +`PTB`   ,
# +`PSB`  ,
# +`PT`  ,

#+`PDT`    ,
#+`PFL/DEM` ,
#+`PMDB`   ,
#+`PP`    ,
#+`PSD`   ,
#+`PSDB`    ,
#+`PTB`    ,
#+`PSB`   ,
#+`PR`   ,
#+`PT`  ,
#+`Esquerda com maioria` ,
#+`Direita com maioria` ,
#+`Centro com maioria` ,
#+`PT com maioria` ,
#+`PSDB com maioria` ,
#+`PFL/DEM com maioria` ,
#+`Esquerda com maioria` ,
#+`Direita com maioria` ,
#+`Centro com maioria` ,
#+`PT com maioria` ,
#+`PSDB com maioria` ,
#+`DEM com maioria` ,
#+`Esquerda com maioria` ,
#+`Direita com maioria`,
#+`Centro com maioria` ,
#+`PT com maioria` ,
#+`PSDB com maioria` ,
#+`PFL/DEM com maioria`,

tab_model(modelo2)



# 3. Modelo ciclo 13_16 com Partidos  ----
##
banco_modelo_3 <- banco_dissertacao_pronto %>% mutate (pop_log_2012 = log(pop_2012_siops),
                                                        gasto_log_medio_05_08 = log(gasto_medio_05_08), 
                                                        gasto_log_medio_09_12 = log(gasto_medio_09_12),
                                                        gasto_log_medio_13_16 = log(gasto_medio_13_16),
                                                        gasto_log_medio_17_20 = log(gasto_medio_17_20),
                                                        psb_dummy_2008 = case_when(partido_eleito_2008 == 'PSB' ~ '1',
                                                                                   partido_eleito_2008 != 'PSB' ~ '0')) %>%  
        rename (#`Ciclo 05-08`  = gasto_log_medio_05_08,
                #`Ciclo 09-12`  = gasto_log_medio_09_12,
                 `Ciclo 13-16`  = gasto_log_medio_13_16,
                # `Ciclo 17-20`  = ,gasto_log_medio_17_20,
                `Centro-Oeste` = centor_oeste_dummy,
                `Nordeste`     = nordeste_dummy,
                `Sudeste`      = sudeste_dummy,
                `Sul`          = sul_dummy,
                `PIB per capita`   = pib_per_2010,
                `Log da Populacao` =pop_log_2012, 
                # `PIB per capita`   = pib_per_2012,
                `IDHM`             = idhm_10,
                #`Dependencia Financeira`       = pp_trans_2005,
                #`Dependencia Financeira`       = pp_trans_2009,
                 `Dependencia Financeira`       = pp_trans_2013,
                # `Dependencia Financeira`       = pp_trans_2017,
                `Municipio Urbano`             = dummy_urbano, 
                `Municipio Polo`                =  municipio_polo,
                #`Primeiro Mandato` = pri_mandato_04,
                #`Primeiro Mandato` = pri_mandato_08,
                `Primeiro Mandato` = pri_mandato_12,
                #`Primeiro Mandato` = pri_mandato_16,
                #`Maioria Parlamentar` = dummy_maioria_2004,
                `Maioria Parlamentar` = dummy_maioria_2012,
                #`Maioria Parlamentar` = dummy_maioria_2016,
                #`Prof de Saude`       = prof_saude_dummy_2004,
                #`Prof de Saude`       = prof_saude_dummy_2008,
                `Prof de Saude`       = prof_saude_dummy_2012,
                #`Prof de Saude`       = prof_saude_dummy_2016,
                #`PT` = pt_dummy_2004,
                #`PDT`   = pdt_dummy_2004,
                #`PFL/DEM`   = pfl_dummy_2004,
                #`PL`   = pl_dummy_2004,
                #`PMDB`   = pmdb_dummy_2004,
                #`PPS`   = pps_dummy_2004,
                #`PSB`   = psb_dummy_2004,
                #`PSDB`   = psdb_dummy_2004,
                #`PTB`   = ptb_dummy_2004,
                #`PP`   = pp_dummy_2004,
                
                #`PT`   = pt_dummy_2008,
                #`PDT`   = pdt_dummy_2008,
                #`PFL/DEM`   = dem_dummy_2008,
                #`PMDB`   = pmdb_dummy_2008,
                #`PP`   = pp_dummy_2008,
                #`PPS`   = pps_dummy_2008,
                #`PR`   = pr_dummy_2008,
                #`PSDB`   = psdb_dummy_2008,
                #`PTB`   = ptb_dummy_2008,
                #`PSB`   = psb_dummy_2008)
 `PDT`   = pdt_dummy_2012,
 `PFL/DEM`   = dem_dummy_2012,
 `PMDB`   = pmdb_dummy_2012,
 `PP`   = pp_dummy_2012,
 `PSD`   = psd_dummy_2012,
 `PR`   = pr_dummy_2012,
 `PSDB`   = psdb_dummy_2012,
 `PTB`   = ptb_dummy_2012,
 `PSB`   = psb_dummy_2012,
 `PT`   = pt_dummy_2012)

#`PDT`   = pdt_dummy_2016,
#`PFL/DEM`   = dem_dummy_2016,
#`PMDB`   = pmdb_dummy_2016,
#`PP`   = pp_dummy_2016,
#`PSD`   = psd_dummy_2016,
#`PSDB`   = psdb_dummy_2016,
#`PTB`   = ptb_dummy_2016,
#`PSB`   = psb_dummy_2016,
#`PR`   = pr_dummy_2016,
#`PT`   = pt_dummy_2016,
#`Esquerda com maioria` = esquerda_maioria_2004,
#`Direita com maioria`= direita_maioria_2004,
#`Centro com maioria`= centro_maioria_2004,
#`PT com maioria`= pt_maioria_2004,
#`PSDB com maioria`= psdb_maioria_2004,
#`PFL/DEM com maioria`= pfl_maioria_2004,
#`Esquerda com maioria`= esquerda_maioria_2012,
#`Direita com maioria`= direita_maioria_2012,
#`Centro com maioria`= centro_maioria_2012,
#`PT com maioria`= pt_maioria_2012,
#`PSDB com maioria`= psdb_maioria_2012,
#`DEM com maioria`= dem_maioria_2012,
#`Esquerda com maioria`= esquerda_maioria_2016,
#`Direita com maioria`= direita_maioria_2016,
#`Centro com maioria`= centro_maioria_2016,
#`PT com maioria`= pt_maioria_2016,
#`PSDB com maioria`= psdb_maioria_2016,
#`PFL/DEM com maioria`= dem_maioria_2016)

modelo3<- lm(`Ciclo 13-16` ~ + `Centro-Oeste`
             # +`Ciclo 09-12`, 
             # +`Ciclo 13-16`,
             # +`Ciclo 17-20`,
             
             +`Nordeste`
             +`Sudeste` 
             +`Sul`
             +`PIB per capita`
             +    `Log da Populacao` 
             # +`PIB per capita`,
             +`IDHM`
             +`Dependencia Financeira` 
             # +`Dependencia Financeira` ,
             # +`Dependencia Financeira`,
             # +`Dependencia Financeira`,
             +`Municipio Urbano`  
             +`Municipio Polo` 
             +`Primeiro Mandato`
             #+`Primeiro Mandato` ,
             #+`Primeiro Mandato` ,
             #+`Primeiro Mandato`,
              +`Maioria Parlamentar` 
             #+`Maioria Parlamentar`  ,
             #+`Maioria Parlamentar`  ,
             +`Prof de Saude` 
             #+`Prof de Saude`  ,
             #+`Prof de Saude`  ,
             #+`Prof de Saude`  ,
             # +`PT`   
             #+`PDT`   
             #+`PFL/DEM`
             #+`PMDB` 
             #+`PP`   
             #+`PPS`  
             #+`PR`   
             #+`PSDB` 
             #+`PTB`  
             #+`PSB`,    
             

             #+`PT` 
             #+`PDT` 
             #+`PFL/DEM` 
             #+`PMDB`   
             #+`PP`   
             #+`PPS`   
             #+`PR`    
             #+`PSDB`   
             #+`PTB`    
#+`PSB`  ,  

 +`PDT`   
 +`PFL/DEM` 
 +`PMDB`  
 +`PP`   
 +`PSD`   
 +`PR`   
 +`PSDB`   
 +`PTB`   
 +`PSB`  
 +`PT` , 
banco_modelo_3)

#+`PDT`    ,
#+`PFL/DEM` ,
#+`PMDB`   ,
#+`PP`    ,
#+`PSD`   ,
#+`PSDB`    ,
#+`PTB`    ,
#+`PSB`   ,
#+`PR`   ,
#+`PT`  ,
#+`Esquerda com maioria` ,
#+`Direita com maioria` ,
#+`Centro com maioria` ,
#+`PT com maioria` ,
#+`PSDB com maioria` ,
#+`PFL/DEM com maioria` ,
#+`Esquerda com maioria` ,
#+`Direita com maioria` ,
#+`Centro com maioria` ,
#+`PT com maioria` ,
#+`PSDB com maioria` ,
#+`DEM com maioria` ,
#+`Esquerda com maioria` ,
#+`Direita com maioria`,
#+`Centro com maioria` ,
#+`PT com maioria` ,
#+`PSDB com maioria` ,
#+`PFL/DEM com maioria`,

tab_model(modelo3)


# 4. Modelo ciclo 17_20 com Partidos  ----
##

banco_modelo_4 <- banco_dissertacao_pronto %>% mutate (pop_log_2012 = log(pop_2012_siops),
                                                       gasto_log_medio_05_08 = log(gasto_medio_05_08), 
                                                       gasto_log_medio_09_12 = log(gasto_medio_09_12),
                                                       gasto_log_medio_13_16 = log(gasto_medio_13_16),
                                                       gasto_log_medio_17_20 = log(gasto_medio_17_20),
                                                       psb_dummy_2008 = case_when(partido_eleito_2008 == 'PSB' ~ '1',
                                                                                  partido_eleito_2008 != 'PSB' ~ '0')) %>%  
        rename (#`Ciclo 05-08`  = gasto_log_medio_05_08,
                #`Ciclo 09-12`  = gasto_log_medio_09_12,
                #`Ciclo 13-16`  = gasto_log_medio_13_16,
                 `Ciclo 17-20`  = gasto_log_medio_17_20,
                `Centro-Oeste` = centor_oeste_dummy,
                `Nordeste`     = nordeste_dummy,
                `Sudeste`      = sudeste_dummy,
                `Sul`          = sul_dummy,
                `PIB per capita`   = pib_per_2010,
                `Log da Populacao` =pop_log_2012, 
                # `PIB per capita`   = pib_per_2012,
                `IDHM`             = idhm_10,
                #`Dependencia Financeira`       = pp_trans_2005,
                #`Dependencia Financeira`       = pp_trans_2009,
                #`Dependencia Financeira`       = pp_trans_2013,
                 `Dependencia Financeira`       = pp_trans_2017,
                `Municipio Urbano`             = dummy_urbano, 
                `Municipio Polo`                =  municipio_polo,
                #`Primeiro Mandato` = pri_mandato_04,
                #`Primeiro Mandato` = pri_mandato_08,
                #`Primeiro Mandato` = pri_mandato_12,
                `Primeiro Mandato` = pri_mandato_16,
                #`Maioria Parlamentar` = dummy_maioria_2004,
                #`Maioria Parlamentar` = dummy_maioria_2012,
                `Maioria Parlamentar` = dummy_maioria_2016,
                #`Prof de Saude`       = prof_saude_dummy_2004,
                #`Prof de Saude`       = prof_saude_dummy_2008,
                #`Prof de Saude`       = prof_saude_dummy_2012,
                `Prof de Saude`       = prof_saude_dummy_2016,
                #`PT` = pt_dummy_2004,
                #`PDT`   = pdt_dummy_2004,
                #`PFL/DEM`   = pfl_dummy_2004,
                #`PL`   = pl_dummy_2004,
                #`PMDB`   = pmdb_dummy_2004,
                #`PPS`   = pps_dummy_2004,
                #`PSB`   = psb_dummy_2004,
                #`PSDB`   = psdb_dummy_2004,
                #`PTB`   = ptb_dummy_2004,
                #`PP`   = pp_dummy_2004,
                
                #`PT`   = pt_dummy_2008,
                #`PDT`   = pdt_dummy_2008,
                #`PFL/DEM`   = dem_dummy_2008,
                #`PMDB`   = pmdb_dummy_2008,
                #`PP`   = pp_dummy_2008,
                #`PPS`   = pps_dummy_2008,
                #`PR`   = pr_dummy_2008,
                #`PSDB`   = psdb_dummy_2008,
                #`PTB`   = ptb_dummy_2008,
                #`PSB`   = psb_dummy_2008)
                #`PDT`   = pdt_dummy_2012,
                #`PFL/DEM`   = dem_dummy_2012,
                #`PMDB`   = pmdb_dummy_2012,
                #`PP`   = pp_dummy_2012,
                #`PSD`   = psd_dummy_2012,
                #`PR`   = pr_dummy_2012,
                #`PSDB`   = psdb_dummy_2012,
                #`PTB`   = ptb_dummy_2012,
                #`PSB`   = psb_dummy_2012,
                #`PT`   = pt_dummy_2012)
`PDT`   = pdt_dummy_2016,
`PFL/DEM`   = dem_dummy_2016,
`PMDB`   = pmdb_dummy_2016,
`PP`   = pp_dummy_2016,
`PSD`   = psd_dummy_2016,
`PSDB`   = psdb_dummy_2016,
`PTB`   = ptb_dummy_2016,
`PSB`   = psb_dummy_2016,
`PR`   = pr_dummy_2016,
`PT`   = pt_dummy_2016)
#`Esquerda com maioria` = esquerda_maioria_2004,
#`Direita com maioria`= direita_maioria_2004,
#`Centro com maioria`= centro_maioria_2004,
#`PT com maioria`= pt_maioria_2004,
#`PSDB com maioria`= psdb_maioria_2004,
#`PFL/DEM com maioria`= pfl_maioria_2004,
#`Esquerda com maioria`= esquerda_maioria_2012,
#`Direita com maioria`= direita_maioria_2012,
#`Centro com maioria`= centro_maioria_2012,
#`PT com maioria`= pt_maioria_2012,
#`PSDB com maioria`= psdb_maioria_2012,
#`DEM com maioria`= dem_maioria_2012,
#`Esquerda com maioria`= esquerda_maioria_2016,
#`Direita com maioria`= direita_maioria_2016,
#`Centro com maioria`= centro_maioria_2016,
#`PT com maioria`= pt_maioria_2016,
#`PSDB com maioria`= psdb_maioria_2016,
#`PFL/DEM com maioria`= dem_maioria_2016)

modelo4<- lm(`Ciclo 17-20` ~ + `Centro-Oeste`
             # +`Ciclo 09-12`, 
             # +`Ciclo 13-16`,
             # +`Ciclo 17-20`,
             
             +`Nordeste`
             +`Sudeste` 
             +`Sul`
             +`PIB per capita`
             +    `Log da Populacao` 
             # +`PIB per capita`,
             +`IDHM`
             +`Dependencia Financeira` 
             # +`Dependencia Financeira` ,
             # +`Dependencia Financeira`,
             # +`Dependencia Financeira`,
             +`Municipio Urbano`  
             +`Municipio Polo` 
             +`Primeiro Mandato`
             #+`Primeiro Mandato` ,
             #+`Primeiro Mandato` ,
             #+`Primeiro Mandato`,
              +`Maioria Parlamentar` 
             #+`Maioria Parlamentar`  ,
             #+`Maioria Parlamentar`  ,
             +`Prof de Saude` 
             #+`Prof de Saude`  ,
             #+`Prof de Saude`  ,
             #+`Prof de Saude`  ,
             # +`PT`   
             #+`PDT`   
             #+`PFL/DEM`
             #+`PMDB` 
             #+`PP`   
             #+`PPS`  
             #+`PR`   
             #+`PSDB` 
             #+`PTB`  
             #+`PSB`,    
             
             
             #+`PT` 
             #+`PDT` 
             #+`PFL/DEM` 
             #+`PMDB`   
             #+`PP`   
             #+`PPS`   
             #+`PR`    
             #+`PSDB`   
             #+`PTB`    
             #+`PSB`  ,  
             
             #+`PDT`   
             #+`PFL/DEM` 
             #+`PMDB`  
             #+`PP`   
             #+`PSD`   
             #+`PR`   
             #+`PSDB`   
             #+`PTB`   
             #+`PSB`  
             #+`PT` , 
             

+`PDT`    
+`PFL/DEM` 
+`PMDB`   
+`PP`    
+`PSD`   
+`PSDB`    
+`PTB`    
+`PSB`   
+`PR`   
+`PT`  ,
banco_modelo_4)
#+`Esquerda com maioria` ,
#+`Direita com maioria` ,
#+`Centro com maioria` ,
#+`PT com maioria` ,
#+`PSDB com maioria` ,
#+`PFL/DEM com maioria` ,
#+`Esquerda com maioria` ,
#+`Direita com maioria` ,
#+`Centro com maioria` ,
#+`PT com maioria` ,
#+`PSDB com maioria` ,
#+`DEM com maioria` ,
#+`Esquerda com maioria` ,
#+`Direita com maioria`,
#+`Centro com maioria` ,
#+`PT com maioria` ,
#+`PSDB com maioria` ,
#+`PFL/DEM com maioria`,

tab_model(modelo4)
tab_model(modelo1,modelo2,modelo3,modelo4)

# Testes dos modelos com Partidos. 1 ao 4 ----
##

# Histograma da Variavel Dependente 

banco_dissertacao_pronto %>%
  filter(!(uf %in% "")) %>%
  filter(!(regiao %in% "")) %>%
  ggplot() +
  aes(x = gasto_log_medio_05_08) +
  geom_histogram() +
  labs(x = "Log da media do Gasto medio proprio em Saude (05_08)") +
  theme_bw()

banco_dissertacao_pronto %>%
  filter(!(uf %in% "")) %>%
  filter(!(regiao %in% "")) %>%
  ggplot() +
  aes(x = gasto_log_medio_09_12) +
  geom_histogram() +
  labs(x = "Log da media do Gasto medio proprio em Saude (09_12)") +
  theme_bw()

banco_dissertacao_pronto %>%
  filter(!(uf %in% "")) %>%
  filter(!(regiao %in% "")) %>%
  ggplot() +
  aes(x = gasto_log_medio_13_16) +
  geom_histogram() +
  labs(x = "Log da media do Gasto medio proprio em Saude (13_16)") +
  theme_bw()

banco_dissertacao_pronto %>%
  filter(!(uf %in% "")) %>%
  filter(!(regiao %in% "")) %>%
  ggplot() +
  aes(x = gasto_log_medio_17_20) +
  geom_histogram() +
  labs(x = "Log da media do Gasto medio proprio em Saude (17_20)") +
  theme_bw()

# Teste do presuposto da Homocedasticidade

library(gvlma) # Modelo 1
gvmodel_modelo_1 <- gvlma(modelo1)
summary(gvmodel_modelo_1)

gvmodel_modelo_2 <- gvlma(modelo2) # Modelo 2
summary(gvmodel_modelo_2)

gvmodel_modelo_3 <- gvlma(modelo3) # Modelo 3
summary(gvmodel_modelo_3)

gvmodel_modelo_4 <- gvlma(modelo4) # Modelo 4
summary(gvmodel_modelo_4)

# Teste do pressuposto de normalidade 

 
residos_modelo_1 <- residuals(modelo1) # Modelo 1
qqnorm(residos_modelo_1)


residos_modelo_2 <- residuals(modelo2) # Modelo 2
qqnorm(residos_modelo_2)


residos_modelo_3 <- residuals(modelo3) # Modelo 3
qqnorm(residos_modelo_3)

residos_modelo_4 <- residuals(modelo4) # Modelo 4
qqnorm(residos_modelo_4)

check_heteroscedasticity(modelo1) %>% # Modelo 1
  plot(modelo1) 


check_heteroscedasticity(modelo2) %>% # Modelo 2
  plot(modelo2)                             


check_heteroscedasticity(modelo3) %>% # Modelo 3
  plot(modelo3)                             


check_heteroscedasticity(modelo4) %>% # Modelo 4
  plot(modelo4)    

# Teste do pressuposto de multicollinearity

if (require("see")) {
  modelo_1_multicollinearity <- check_collinearity(modelo1) # Modelo 1
  plot(modelo_1_multicollinearity)+
    coord_flip() 
} 




if (require("see")) {
  modelo_2_multicollinearity <- check_collinearity(modelo2) # Modelo 2
  plot(modelo_2_multicollinearity)+
    coord_flip()
} 



if (require("see")) {
  modelo_3_multicollinearity <- check_collinearity(modelo3) # Modelo 3
  plot(modelo_3_multicollinearity)+
    coord_flip()
} 



if (require("see")) {
  modelo_4_multicollinearity <- check_collinearity(modelo4) # Modelo 4
  plot(modelo_4_multicollinearity)+
    coord_flip()
} 



# OBS: MODELO COM PARTIDO, modelos com partido pt e psdb com maioria, modelo esquerda e direita e centro com maioria sem partido 
# obs: Usar teste como a parada hipotese o troço usado ele usa no covid     

# 5. Modelo ciclo 05_08 com Partidos com Maioria  ----
##

banco_modelo_5 <- banco_dissertacao_pronto %>% mutate (pop_log_2012 = log(pop_2012_siops),
                                                       gasto_log_medio_05_08 = log(gasto_medio_05_08), 
                                                       gasto_log_medio_09_12 = log(gasto_medio_09_12),
                                                       gasto_log_medio_13_16 = log(gasto_medio_13_16),
                                                       gasto_log_medio_17_20 = log(gasto_medio_17_20),
                                                       psb_dummy_2008 = case_when(partido_eleito_2008 == 'PSB' ~ '1',
                                                                                  partido_eleito_2008 != 'PSB' ~ '0')) %>%  
        rename (`Ciclo 05-08`  = gasto_log_medio_05_08,
                # `Ciclo 09-12`  = gasto_log_medio_09_12,
                # `Ciclo 13-16`  = gasto_log_medio_13_16,
                # `Ciclo 17-20`  = ,gasto_log_medio_17_20,
                `Log da Populacao` =pop_log_2012, 
                `Centro-Oeste` = centor_oeste_dummy,
                `Nordeste`     = nordeste_dummy,
                `Sudeste`      = sudeste_dummy,
                `Sul`          = sul_dummy,
                `PIB per capita`   = pib_per_2010,
                # `PIB per capita`   = pib_per_2012,
                `IDHM`             = idhm_10,
                `Dependencia Financeira`       = pp_trans_2005,
                # `Dependencia Financeira`       = pp_trans_2009,
                # `Dependencia Financeira`       = pp_trans_2013,
                # `Dependencia Financeira`       = pp_trans_2017,
                `Municipio Urbano`             = dummy_urbano, 
                `Municipio Polo`                =  municipio_polo,
                `Primeiro Mandato` = pri_mandato_04,
                #`Primeiro Mandato` = pri_mandato_08,
                #`Primeiro Mandato` = pri_mandato_12,
                #`Primeiro Mandato` = pri_mandato_16,
               # `Maioria Parlamentar` = dummy_maioria_2004,
                #`Maioria Parlamentar` = dummy_maioria_2012,
                #`Maioria Parlamentar` = dummy_maioria_2016,
                `Prof de Saude`       = prof_saude_dummy_2004,
                #`Prof de Saude`       = prof_saude_dummy_2008,
                #`Prof de Saude`       = prof_saude_dummy_2012,
                #`Prof de Saude`       = prof_saude_dummy_2016,
                #`PT` = pt_dummy_2004,
                #`PDT`   = pdt_dummy_2004,
                #`PFL/DEM`   = pfl_dummy_2004,
                #`PL`   = pl_dummy_2004,
                #`PMDB`   = pmdb_dummy_2004,
                #`PPS`   = pps_dummy_2004,
                #`PSB`   = psb_dummy_2004,
                #`PSDB`   = psdb_dummy_2004,
                #`PTB`   = ptb_dummy_2004,
                #`PP`   = pp_dummy_2004)
                
                #`PT`   = pt_dummy_2008,
                #`PDT`   = pdt_dummy_2008,
                #`PFL/DEM`   = dem_dummy_2008,
                #`PMDB`   = pmdb_dummy_2008,
                #`PP`   = pp_dummy_2008,
                #`PPS`   = pps_dummy_2008,
                #`PR`   = pr_dummy_2008,
                #`PSDB`   = psdb_dummy_2008,
                #`PTB`   = ptb_dummy_2008,
                #`PSB`   = psb_dummy_2008,
                
                # `PDT`   = pdt_dummy_2012,
                # `PFL/DEM`   = dem_dummy_2012,
                # `PMDB`   = pmdb_dummy_2012,
                # `PP`   = pp_dummy_2012,
                # `PSD`   = psd_dummy_2012,
                # `PR`   = pr_dummy_2012,
                # `PSDB`   = psdb_dummy_2012,
                # `PTB`   = ptb_dummy_2012,
                # `PSB`   = psb_dummy_2012,
                # `PT`   = pt_dummy_2012,
                
                #`PDT`   = pdt_dummy_2016,
                #`PFL/DEM`   = dem_dummy_2016,
                #`PMDB`   = pmdb_dummy_2016,
                #`PP`   = pp_dummy_2016,
                #`PSD`   = psd_dummy_2016,
                #`PSDB`   = psdb_dummy_2016,
                #`PTB`   = ptb_dummy_2016,
                #`PSB`   = psb_dummy_2016,
                #`PR`   = pr_dummy_2016,
                #`PT`   = pt_dummy_2016,
                #`Esquerda com maioria` = esquerda_maioria_2004,
                #`Direita com maioria`= direita_maioria_2004,
                #`Centro com maioria`= centro_maioria_2004,
                `PT com maioria`= pt_maioria_2004,
                `PSDB com maioria`= psdb_maioria_2004,
                `PFL/DEM com maioria`= pfl_maioria_2004)
                #`Esquerda com maioria`= esquerda_maioria_2012,
                #`Direita com maioria`= direita_maioria_2012,
                #`Centro com maioria`= centro_maioria_2012,
                #`PT com maioria`= pt_maioria_2012,
                #`PSDB com maioria`= psdb_maioria_2012,
                #`DEM com maioria`= dem_maioria_2012,
                #`Esquerda com maioria`= esquerda_maioria_2016,
                #`Direita com maioria`= direita_maioria_2016,
                #`Centro com maioria`= centro_maioria_2016,
                #`PT com maioria`= pt_maioria_2016,
                #`PSDB com maioria`= psdb_maioria_2016,
                #`PFL/DEM com maioria`= dem_maioria_2016)
                
                modelo5<- lm(`Ciclo 05-08` ~ + `Centro-Oeste`
                             # +`Ciclo 09-12`, 
                             # +`Ciclo 13-16`,
                             # +`Ciclo 17-20`,
                             
                             +`Nordeste`
                             +`Sudeste` 
                             +`Sul`
                             +`PIB per capita`
                             +    `Log da Populacao` 
                             # +`PIB per capita`,
                             +`IDHM`
                             +`Dependencia Financeira` 
                             # +`Dependencia Financeira` ,
                             # +`Dependencia Financeira`,
                             # +`Dependencia Financeira`,
                             +`Municipio Urbano`  
                             +`Municipio Polo`                
                             +`Primeiro Mandato`
                             #+`Primeiro Mandato` ,
                             #+`Primeiro Mandato` ,
                             #+`Primeiro Mandato`,
                            # +`Maioria Parlamentar` 
                             #+`Maioria Parlamentar`  ,
                             #+`Maioria Parlamentar`  ,
                             +`Prof de Saude` 
                             #+`Prof de Saude`  ,
                             #+`Prof de Saude`  ,
                             #+`Prof de Saude`  ,
                             #+`PT`  
                             #+`PDT` 
                             #+`PFL/DEM` 
                             #+`PL`    
                             #+`PMDB` 
                             #+`PPS`   
                             #+`PSB`    
                             #+`PSDB`    
                             #+`PTB`    
                             #+`PP`,    
                             #+`PT` ,
                             #+`PDT` ,
                             #+`PFL/DEM` ,
                             #+`PMDB`   ,
                             #+`PP`   ,
                             #+`PPS`   ,
                             #+`PR`    ,
                             #+`PSDB`   ,
                             #+`PTB`    ,
                             #+`PSB`    ,
                             
                             # +`PDT`   ,
                             # +`PFL/DEM` ,
                             # +`PMDB`  ,
                             # +`PP`   ,
                             # +`PSD`   ,
                             # +`PR`   ,
                             # +`PSDB`   ,
                             # +`PTB`   ,
                             # +`PSB`  ,
                             # +`PT`  ,
                             
                             #+`PDT`    ,
                             #+`PFL/DEM` ,
                             #+`PMDB`   ,
                             #+`PP`    ,
                             #+`PSD`   ,
                             #+`PSDB`    ,
                             #+`PTB`    ,
                             #+`PSB`   ,
                             #+`PR`   ,
                             #+`PT`  ,
                             #+`Esquerda com maioria` ,
                             #+`Direita com maioria` ,
                             #+`Centro com maioria` ,
                             +`PT com maioria` 
                             +`PSDB com maioria` 
                             +`PFL/DEM com maioria`,
                             data = banco_modelo_5) 
                             #+`Esquerda com maioria` ,
                             #+`Direita com maioria` ,
                             #+`Centro com maioria` ,
                             #+`PT com maioria` ,
                             #+`PSDB com maioria` ,
                             #+`DEM com maioria` ,
                             #+`Esquerda com maioria` ,
                             #+`Direita com maioria`,
                             #+`Centro com maioria` ,
                             #+`PT com maioria` ,
                             #+`PSDB com maioria` ,
                             #+`PFL/DEM com maioria`,
                             
                             tab_model(modelo5)

# 6. Modelo ciclo 09_12 com Partidos com Maioria. NAO TEM  ----
##

# 7. Modelo ciclo 13_16 com Partidos com Maioria  ----
##
                             
                             banco_modelo_7 <- banco_dissertacao_pronto %>% mutate (pop_log_2012 = log(pop_2012_siops),
                                                                                    gasto_log_medio_05_08 = log(gasto_medio_05_08), 
                                                                                    gasto_log_medio_09_12 = log(gasto_medio_09_12),
                                                                                    gasto_log_medio_13_16 = log(gasto_medio_13_16),
                                                                                    gasto_log_medio_17_20 = log(gasto_medio_17_20),
                                                                                    psb_dummy_2008 = case_when(partido_eleito_2008 == 'PSB' ~ '1',
                                                                                                               partido_eleito_2008 != 'PSB' ~ '0')) %>%  
                                     rename (#`Ciclo 05-08`  = gasto_log_medio_05_08,
                                             #`Ciclo 09-12`  = gasto_log_medio_09_12,
                                             `Ciclo 13-16`  = gasto_log_medio_13_16,
                                             # `Ciclo 17-20`  = ,gasto_log_medio_17_20,
                                             `Centro-Oeste` = centor_oeste_dummy,
                                             `Nordeste`     = nordeste_dummy,
                                             `Sudeste`      = sudeste_dummy,
                                             `Sul`          = sul_dummy,
                                             `PIB per capita`   = pib_per_2010,
                                             `Log da Populacao` =pop_log_2012, 
                                             # `PIB per capita`   = pib_per_2012,
                                             `IDHM`             = idhm_10,
                                             #`Dependencia Financeira`       = pp_trans_2005,
                                             #`Dependencia Financeira`       = pp_trans_2009,
                                             `Dependencia Financeira`       = pp_trans_2013,
                                             # `Dependencia Financeira`       = pp_trans_2017,
                                             `Municipio Urbano`             = dummy_urbano, 
                                             `Municipio Polo`                =  municipio_polo,
                                             #`Primeiro Mandato` = pri_mandato_04,
                                             #`Primeiro Mandato` = pri_mandato_08,
                                             `Primeiro Mandato` = pri_mandato_12,
                                             #`Primeiro Mandato` = pri_mandato_16,
                                             #`Maioria Parlamentar` = dummy_maioria_2004,
                                            # `Maioria Parlamentar` = dummy_maioria_2012,
                                             #`Maioria Parlamentar` = dummy_maioria_2016,
                                             #`Prof de Saude`       = prof_saude_dummy_2004,
                                             #`Prof de Saude`       = prof_saude_dummy_2008,
                                             `Prof de Saude`       = prof_saude_dummy_2012,
                                             #`Prof de Saude`       = prof_saude_dummy_2016,
                                             #`PT` = pt_dummy_2004,
                                             #`PDT`   = pdt_dummy_2004,
                                             #`PFL/DEM`   = pfl_dummy_2004,
                                             #`PL`   = pl_dummy_2004,
                                             #`PMDB`   = pmdb_dummy_2004,
                                             #`PPS`   = pps_dummy_2004,
                                             #`PSB`   = psb_dummy_2004,
                                             #`PSDB`   = psdb_dummy_2004,
                                             #`PTB`   = ptb_dummy_2004,
                                             #`PP`   = pp_dummy_2004,
                                             
                                             #`PT`   = pt_dummy_2008,
                                             #`PDT`   = pdt_dummy_2008,
                                             #`PFL/DEM`   = dem_dummy_2008,
                                             #`PMDB`   = pmdb_dummy_2008,
                                             #`PP`   = pp_dummy_2008,
                                             #`PPS`   = pps_dummy_2008,
                                             #`PR`   = pr_dummy_2008,
                                             #`PSDB`   = psdb_dummy_2008,
                                             #`PTB`   = ptb_dummy_2008,
                                             #`PSB`   = psb_dummy_2008)
                                           #  `PDT`   = pdt_dummy_2012,
                                            # `PFL/DEM`   = dem_dummy_2012,
                                             #`PMDB`   = pmdb_dummy_2012,
                                             #`PP`   = pp_dummy_2012,
                                             #`PSD`   = psd_dummy_2012,
                                             #`PR`   = pr_dummy_2012,
                                             #`PSDB`   = psdb_dummy_2012,
                                             #`PTB`   = ptb_dummy_2012,
                                             #`PSB`   = psb_dummy_2012,
                                             #`PT`   = pt_dummy_2012)
                             
                             #`PDT`   = pdt_dummy_2016,
                             #`PFL/DEM`   = dem_dummy_2016,
                             #`PMDB`   = pmdb_dummy_2016,
                             #`PP`   = pp_dummy_2016,
                             #`PSD`   = psd_dummy_2016,
                             #`PSDB`   = psdb_dummy_2016,
                             #`PTB`   = ptb_dummy_2016,
                             #`PSB`   = psb_dummy_2016,
                             #`PR`   = pr_dummy_2016,
                             #`PT`   = pt_dummy_2016,
                             #`Esquerda com maioria` = esquerda_maioria_2004,
                             #`Direita com maioria`= direita_maioria_2004,
                             #`Centro com maioria`= centro_maioria_2004,
                             #`PT com maioria`= pt_maioria_2004,
                             #`PSDB com maioria`= psdb_maioria_2004,
                             #`PFL/DEM com maioria`= pfl_maioria_2004,
                             #`Esquerda com maioria`= esquerda_maioria_2012,
                             #`Direita com maioria`= direita_maioria_2012,
                             #`Centro com maioria`= centro_maioria_2012,
                             `PT com maioria`= pt_maioria_2012,
                             `PSDB com maioria`= psdb_maioria_2012,
                             `PFL/DEM com maioria`= dem_maioria_2012)
                             #`Esquerda com maioria`= esquerda_maioria_2016,
                             #`Direita com maioria`= direita_maioria_2016,
                             #`Centro com maioria`= centro_maioria_2016,
                             #`PT com maioria`= pt_maioria_2016,
                             #`PSDB com maioria`= psdb_maioria_2016,
                             #`PFL/DEM com maioria`= dem_maioria_2016)
                             
                             modelo7<- lm(`Ciclo 13-16` ~ + `Centro-Oeste`
                                          # +`Ciclo 09-12`, 
                                          # +`Ciclo 13-16`,
                                          # +`Ciclo 17-20`,
                                          
                                          +`Nordeste`
                                          +`Sudeste` 
                                          +`Sul`
                                          +`PIB per capita`
                                          +    `Log da Populacao` 
                                          # +`PIB per capita`,
                                          +`IDHM`
                                          +`Dependencia Financeira` 
                                          # +`Dependencia Financeira` ,
                                          # +`Dependencia Financeira`,
                                          # +`Dependencia Financeira`,
                                          +`Municipio Urbano`  
                                          +`Municipio Polo` 
                                          +`Primeiro Mandato`
                                          #+`Primeiro Mandato` ,
                                          #+`Primeiro Mandato` ,
                                          #+`Primeiro Mandato`,
                                        #  +`Maioria Parlamentar` 
                                          #+`Maioria Parlamentar`  ,
                                          #+`Maioria Parlamentar`  ,
                                          +`Prof de Saude` 
                                          #+`Prof de Saude`  ,
                                          #+`Prof de Saude`  ,
                                          #+`Prof de Saude`  ,
                                          # +`PT`   
                                          #+`PDT`   
                                          #+`PFL/DEM`
                                          #+`PMDB` 
                                          #+`PP`   
                                          #+`PPS`  
                                          #+`PR`   
                                          #+`PSDB` 
                                          #+`PTB`  
                                          #+`PSB`,    
                                          
                                          
                                          #+`PT` 
                                          #+`PDT` 
                                          #+`PFL/DEM` 
                                          #+`PMDB`   
                                          #+`PP`   
                                          #+`PPS`   
                                          #+`PR`    
                                          #+`PSDB`   
                                          #+`PTB`    
                                          #+`PSB`  ,  
                                          
                                          #+`PDT`   
                                          #+`PFL/DEM` 
                                          #+`PMDB`  
                                          #+`PP`   
                                          #+`PSD`   
                                          #+`PR`   
                                          #+`PSDB`   
                                          #+`PTB`   
                                          #+`PSB`  
                                          #+`PT` 

                             
                             #+`PDT`    ,
                             #+`PFL/DEM` ,
                             #+`PMDB`   ,
                             #+`PP`    ,
                             #+`PSD`   ,
                             #+`PSDB`    ,
                             #+`PTB`    ,
                             #+`PSB`   ,
                             #+`PR`   ,
                             #+`PT`  ,
                             #+`Esquerda com maioria` ,
                             #+`Direita com maioria` ,
                             #+`Centro com maioria` ,
                             +`PT com maioria` 
                             +`PSDB com maioria` 
                             +`PFL/DEM com maioria` ,
                             banco_modelo_7)
                             #+`Esquerda com maioria` ,
                             #+`Direita com maioria` ,
                             #+`Centro com maioria` ,
                             #+`PT com maioria` ,
                             #+`PSDB com maioria` ,
                             #+`DEM com maioria` ,
                             #+`Esquerda com maioria` ,
                             #+`Direita com maioria`,
                             #+`Centro com maioria` ,
                             #+`PT com maioria` ,
                             #+`PSDB com maioria` ,
                             #+`PFL/DEM com maioria`,
                             
                             tab_model(modelo7)

# 8. Modelo ciclo 17_20 com Partidos com Maioria  ----
##
                             
                             
                             banco_modelo_8 <- banco_dissertacao_pronto %>% mutate (pop_log_2012 = log(pop_2012_siops),
                                                                                    gasto_log_medio_05_08 = log(gasto_medio_05_08), 
                                                                                    gasto_log_medio_09_12 = log(gasto_medio_09_12),
                                                                                    gasto_log_medio_13_16 = log(gasto_medio_13_16),
                                                                                    gasto_log_medio_17_20 = log(gasto_medio_17_20),
                                                                                    psb_dummy_2008 = case_when(partido_eleito_2008 == 'PSB' ~ '1',
                                                                                                               partido_eleito_2008 != 'PSB' ~ '0')) %>%  
                                     rename (#`Ciclo 05-08`  = gasto_log_medio_05_08,
                                             #`Ciclo 09-12`  = gasto_log_medio_09_12,
                                             #`Ciclo 13-16`  = gasto_log_medio_13_16,
                                             `Ciclo 17-20`  = gasto_log_medio_17_20,
                                             `Centro-Oeste` = centor_oeste_dummy,
                                             `Nordeste`     = nordeste_dummy,
                                             `Sudeste`      = sudeste_dummy,
                                             `Sul`          = sul_dummy,
                                             `PIB per capita`   = pib_per_2010,
                                             `Log da Populacao` =pop_log_2012, 
                                             # `PIB per capita`   = pib_per_2012,
                                             `IDHM`             = idhm_10,
                                             #`Dependencia Financeira`       = pp_trans_2005,
                                             #`Dependencia Financeira`       = pp_trans_2009,
                                             #`Dependencia Financeira`       = pp_trans_2013,
                                             `Dependencia Financeira`       = pp_trans_2017,
                                             `Municipio Urbano`             = dummy_urbano, 
                                             `Municipio Polo`                =  municipio_polo,
                                             #`Primeiro Mandato` = pri_mandato_04,
                                             #`Primeiro Mandato` = pri_mandato_08,
                                             #`Primeiro Mandato` = pri_mandato_12,
                                             `Primeiro Mandato` = pri_mandato_16,
                                             #`Maioria Parlamentar` = dummy_maioria_2004,
                                             #`Maioria Parlamentar` = dummy_maioria_2012,
                                            # `Maioria Parlamentar` = dummy_maioria_2016,
                                             #`Prof de Saude`       = prof_saude_dummy_2004,
                                             #`Prof de Saude`       = prof_saude_dummy_2008,
                                             #`Prof de Saude`       = prof_saude_dummy_2012,
                                             `Prof de Saude`       = prof_saude_dummy_2016,
                                             #`PT` = pt_dummy_2004,
                                             #`PDT`   = pdt_dummy_2004,
                                             #`PFL/DEM`   = pfl_dummy_2004,
                                             #`PL`   = pl_dummy_2004,
                                             #`PMDB`   = pmdb_dummy_2004,
                                             #`PPS`   = pps_dummy_2004,
                                             #`PSB`   = psb_dummy_2004,
                                             #`PSDB`   = psdb_dummy_2004,
                                             #`PTB`   = ptb_dummy_2004,
                                             #`PP`   = pp_dummy_2004,
                                             
                                             #`PT`   = pt_dummy_2008,
                                             #`PDT`   = pdt_dummy_2008,
                                             #`PFL/DEM`   = dem_dummy_2008,
                                             #`PMDB`   = pmdb_dummy_2008,
                                             #`PP`   = pp_dummy_2008,
                                             #`PPS`   = pps_dummy_2008,
                                             #`PR`   = pr_dummy_2008,
                                             #`PSDB`   = psdb_dummy_2008,
                                             #`PTB`   = ptb_dummy_2008,
                                             #`PSB`   = psb_dummy_2008)
                                             #`PDT`   = pdt_dummy_2012,
                                             #`PFL/DEM`   = dem_dummy_2012,
                                             #`PMDB`   = pmdb_dummy_2012,
                                             #`PP`   = pp_dummy_2012,
                                             #`PSD`   = psd_dummy_2012,
                                             #`PR`   = pr_dummy_2012,
                                             #`PSDB`   = psdb_dummy_2012,
                                             #`PTB`   = ptb_dummy_2012,
                                             #`PSB`   = psb_dummy_2012,
                                             #`PT`   = pt_dummy_2012)
                                            # `PDT`   = pdt_dummy_2016,
                                             #`PFL/DEM`   = dem_dummy_2016,
                                             #`PMDB`   = pmdb_dummy_2016,
                                             #`PP`   = pp_dummy_2016,
                                             #`PSD`   = psd_dummy_2016,
                                             #`PSDB`   = psdb_dummy_2016,
                                             #`PTB`   = ptb_dummy_2016,
                                             #`PSB`   = psb_dummy_2016,
                                             #`PR`   = pr_dummy_2016,
                                             #`PT`   = pt_dummy_2016)
                             #`Esquerda com maioria` = esquerda_maioria_2004,
                             #`Direita com maioria`= direita_maioria_2004,
                             #`Centro com maioria`= centro_maioria_2004,
                             #`PT com maioria`= pt_maioria_2004,
                             #`PSDB com maioria`= psdb_maioria_2004,
                             #`PFL/DEM com maioria`= pfl_maioria_2004,
                             #`Esquerda com maioria`= esquerda_maioria_2012,
                             #`Direita com maioria`= direita_maioria_2012,
                             #`Centro com maioria`= centro_maioria_2012,
                             #`PT com maioria`= pt_maioria_2012,
                             #`PSDB com maioria`= psdb_maioria_2012,
                             #`DEM com maioria`= dem_maioria_2012,
                             #`Esquerda com maioria`= esquerda_maioria_2016,
                             #`Direita com maioria`= direita_maioria_2016,
                             #`Centro com maioria`= centro_maioria_2016,
                             `PT com maioria`= pt_maioria_2016,
                             `PSDB com maioria`= psdb_maioria_2016,
                             `PFL/DEM com maioria`= dem_maioria_2016)
                             
                             modelo8<- lm(`Ciclo 17-20` ~ + `Centro-Oeste`
                                          # +`Ciclo 09-12`, 
                                          # +`Ciclo 13-16`,
                                          # +`Ciclo 17-20`,
                                          
                                          +`Nordeste`
                                          +`Sudeste` 
                                          +`Sul`
                                          +`PIB per capita`
                                          +    `Log da Populacao` 
                                          # +`PIB per capita`,
                                          +`IDHM`
                                          +`Dependencia Financeira` 
                                          # +`Dependencia Financeira` ,
                                          # +`Dependencia Financeira`,
                                          # +`Dependencia Financeira`,
                                          +`Municipio Urbano`  
                                          +`Municipio Polo` 
                                          +`Primeiro Mandato`
                                          #+`Primeiro Mandato` ,
                                          #+`Primeiro Mandato` ,
                                          #+`Primeiro Mandato`,
                                          #+`Maioria Parlamentar` 
                                          #+`Maioria Parlamentar`  ,
                                          #+`Maioria Parlamentar`  ,
                                          +`Prof de Saude` 
                                          #+`Prof de Saude`  ,
                                          #+`Prof de Saude`  ,
                                          #+`Prof de Saude`  ,
                                          # +`PT`   
                                          #+`PDT`   
                                          #+`PFL/DEM`
                                          #+`PMDB` 
                                          #+`PP`   
                                          #+`PPS`  
                                          #+`PR`   
                                          #+`PSDB` 
                                          #+`PTB`  
                                          #+`PSB`,    
                                          
                                          
                                          #+`PT` 
                                          #+`PDT` 
                                          #+`PFL/DEM` 
                                          #+`PMDB`   
                                          #+`PP`   
                                          #+`PPS`   
                                          #+`PR`    
                                          #+`PSDB`   
                                          #+`PTB`    
                                          #+`PSB`  ,  
                                          
                                          #+`PDT`   
                                          #+`PFL/DEM` 
                                          #+`PMDB`  
                                          #+`PP`   
                                          #+`PSD`   
                                          #+`PR`   
                                          #+`PSDB`   
                                          #+`PTB`   
                                          #+`PSB`  
                                          #+`PT` , 
                                          
                                          
                                          #+`PDT`    
                                          #+`PFL/DEM` 
                                          #+`PMDB`   
                                          #+`PP`    
                                          #+`PSD`   
                                          #+`PSDB`    
                                          #+`PTB`    
                                          #+`PSB`   
                                          #+`PR`   
                                          #+`PT`  ,
                             #+`Esquerda com maioria` ,
                             #+`Direita com maioria` ,
                             #+`Centro com maioria` ,
                             #+`PT com maioria` ,
                             #+`PSDB com maioria` ,
                             #+`PFL/DEM com maioria` ,
                             #+`Esquerda com maioria` ,
                             #+`Direita com maioria` ,
                             #+`Centro com maioria` ,
                             #+`PT com maioria` ,
                             #+`PSDB com maioria` ,
                             #+`DEM com maioria` ,
                             #+`Esquerda com maioria` ,
                             #+`Direita com maioria`,
                             #+`Centro com maioria` ,
                             +`PT com maioria` 
                             +`PSDB com maioria` 
                             +`PFL/DEM com maioria`,
                             banco_modelo_8)
                             
                             tab_model(modelo8)
                             tab_model(modelo5,modelo7,modelo8)

# Testes dos modelos com Partidos com Maioria. 5 ao 8 ----
##

                             
                             # Teste do presuposto da Homocedasticidade
                             
                             library(gvlma) # Modelo 5
                             gvmodel_modelo_5 <- gvlma(modelo5)
                             summary(gvmodel_modelo_5)
                             
                             check_heteroscedasticity(modelo5) %>% 
                             plot(modelo5) 
                             
                             
                             gvmodel_modelo_7 <- gvlma(modelo7) # Modelo 7
                             summary(gvmodel_modelo_7)

                             check_heteroscedasticity(modelo7) %>% 
                             plot(modelo7)                             
                             
                             gvmodel_modelo_8 <- gvlma(modelo8) # Modelo 8
                             summary(gvmodel_modelo_8)

                             check_heteroscedasticity(modelo8) %>% 
                             plot(modelo8)     
                             
                      
                             # Teste do pressuposto de normalidade 
                             
                             residos_modelo_5 <- residuals(modelo5) # Modelo 5
                             qqnorm(residos_modelo_5)
                             
                             residos_modelo_7 <- residuals(modelo7) # Modelo 7
                             qqnorm(residos_modelo_7)
                             
                             residos_modelo_8 <- residuals(modelo8) # Modelo 8
                             qqnorm(residos_modelo_8)
                             
                             # Teste do pressuposto de multicollinearity
                             
                             if (require("see")) {
                               modelo_5_multicollinearity <- check_collinearity(modelo5) # Modelo 5
                               plot(x)+
                                 coord_flip()
                             }                            
                             
                             if (require("see")) {
                               modelo_7_multicollinearity <- check_collinearity(modelo7) # Modelo 7
                               plot(x)+
                                 coord_flip()
                             }
                             
                              if (require("see")) {
                                modelo_8_multicollinearity <- check_collinearity(modelo8) # Modelo 8
                               plot(x)+
                                 coord_flip()
                             }
                            

# 9. Modelo ciclo 05_08 com Ideologia com maioria   ----
##

banco_modelo_9 <- banco_dissertacao_pronto %>% mutate (pop_log_2012 = log(pop_2012_siops),
                                                       gasto_log_medio_05_08 = log(gasto_medio_05_08), 
                                                       gasto_log_medio_09_12 = log(gasto_medio_09_12),
                                                       gasto_log_medio_13_16 = log(gasto_medio_13_16),
                                                       gasto_log_medio_17_20 = log(gasto_medio_17_20),
                                                       psb_dummy_2008 = case_when(partido_eleito_2008 == 'PSB' ~ '1',
                                                                                  partido_eleito_2008 != 'PSB' ~ '0')) %>%  
  rename (`Ciclo 05-08`  = gasto_log_medio_05_08,
          # `Ciclo 09-12`  = gasto_log_medio_09_12,
          # `Ciclo 13-16`  = gasto_log_medio_13_16,
          # `Ciclo 17-20`  = ,gasto_log_medio_17_20,
          `Log da Populacao` = pop_log_2012, 
          `Centro-Oeste` = centor_oeste_dummy,
          `Nordeste`     = nordeste_dummy,
          `Sudeste`      = sudeste_dummy,
          `Sul`          = sul_dummy,
          `PIB per capita`   = pib_per_2010,
          # `PIB per capita`   = pib_per_2012,
          `IDHM`             = idhm_10,
          `Dependencia Financeira`       = pp_trans_2005,
          # `Dependencia Financeira`       = pp_trans_2009,
          # `Dependencia Financeira`       = pp_trans_2013,
          # `Dependencia Financeira`       = pp_trans_2017,
          `Municipio Urbano`             = dummy_urbano, 
          `Municipio Polo`                =  municipio_polo,
          `Primeiro Mandato` = pri_mandato_04,
          #`Primeiro Mandato` = pri_mandato_08,
          #`Primeiro Mandato` = pri_mandato_12,
          #`Primeiro Mandato` = pri_mandato_16,
          # `Maioria Parlamentar` = dummy_maioria_2004,
          #`Maioria Parlamentar` = dummy_maioria_2012,
          #`Maioria Parlamentar` = dummy_maioria_2016,
          `Prof de Saude`       = prof_saude_dummy_2004,
          #`Prof de Saude`       = prof_saude_dummy_2008,
          #`Prof de Saude`       = prof_saude_dummy_2012,
          #`Prof de Saude`       = prof_saude_dummy_2016,
          #`PT` = pt_dummy_2004,
          #`PDT`   = pdt_dummy_2004,
          #`PFL/DEM`   = pfl_dummy_2004,
          #`PL`   = pl_dummy_2004,
          #`PMDB`   = pmdb_dummy_2004,
          #`PPS`   = pps_dummy_2004,
          #`PSB`   = psb_dummy_2004,
          #`PSDB`   = psdb_dummy_2004,
          #`PTB`   = ptb_dummy_2004,
          #`PP`   = pp_dummy_2004)
          
          #`PT`   = pt_dummy_2008,
          #`PDT`   = pdt_dummy_2008,
          #`PFL/DEM`   = dem_dummy_2008,
          #`PMDB`   = pmdb_dummy_2008,
          #`PP`   = pp_dummy_2008,
          #`PPS`   = pps_dummy_2008,
          #`PR`   = pr_dummy_2008,
          #`PSDB`   = psdb_dummy_2008,
          #`PTB`   = ptb_dummy_2008,
          #`PSB`   = psb_dummy_2008,
          
          # `PDT`   = pdt_dummy_2012,
          # `PFL/DEM`   = dem_dummy_2012,
          # `PMDB`   = pmdb_dummy_2012,
          # `PP`   = pp_dummy_2012,
          # `PSD`   = psd_dummy_2012,
          # `PR`   = pr_dummy_2012,
          # `PSDB`   = psdb_dummy_2012,
          # `PTB`   = ptb_dummy_2012,
          # `PSB`   = psb_dummy_2012,
          # `PT`   = pt_dummy_2012,
          
          #`PDT`   = pdt_dummy_2016,
          #`PFL/DEM`   = dem_dummy_2016,
          #`PMDB`   = pmdb_dummy_2016,
          #`PP`   = pp_dummy_2016,
          #`PSD`   = psd_dummy_2016,
          #`PSDB`   = psdb_dummy_2016,
          #`PTB`   = ptb_dummy_2016,
          #`PSB`   = psb_dummy_2016,
          #`PR`   = pr_dummy_2016,
          #`PT`   = pt_dummy_2016,
          `Esquerda com maioria` = esquerda_maioria_2004,
          `Direita com maioria`= direita_maioria_2004,
          `Centro com maioria`= centro_maioria_2004)
          #`PT com maioria`= pt_maioria_2004,
          #`PSDB com maioria`= psdb_maioria_2004,
          #`PFL/DEM com maioria`= pfl_maioria_2004,
          #`Esquerda com maioria` = esquerda_maioria_2004,
          #`Centro com maioria`  = centro_maioria_2004,
          #`Direita com maioria`  = direita_maioria_2004,
#`Esquerda com maioria`= esquerda_maioria_2012,
#`Direita com maioria`= direita_maioria_2012,
#`Centro com maioria`= centro_maioria_2012,
#`PT com maioria`= pt_maioria_2012,
#`PSDB com maioria`= psdb_maioria_2012,
#`DEM com maioria`= dem_maioria_2012,
#`Esquerda com maioria`= esquerda_maioria_2016,
#`Direita com maioria`= direita_maioria_2016,
#`Centro com maioria`= centro_maioria_2016,
#`PT com maioria`= pt_maioria_2016,
#`PSDB com maioria`= psdb_maioria_2016,
#`PFL/DEM com maioria`= dem_maioria_2016)



modelo9<- lm(`Ciclo 05-08` ~ + `Centro-Oeste`
             # +`Ciclo 09-12`, 
             # +`Ciclo 13-16`,
             # +`Ciclo 17-20`,
     
             +`Nordeste`
             +`Sudeste` 
             +`Sul`
             +`PIB per capita`
             +    `Log da Populacao` 
             # +`PIB per capita`,
             +`IDHM`
             +`Dependencia Financeira` 
             # +`Dependencia Financeira` ,
             # +`Dependencia Financeira`,
             # +`Dependencia Financeira`,
             +`Municipio Urbano`  
             +`Municipio Polo`                
             +`Primeiro Mandato`
             #+`Primeiro Mandato` ,
             #+`Primeiro Mandato` ,
             #+`Primeiro Mandato`,
             # +`Maioria Parlamentar` 
             #+`Maioria Parlamentar`  ,
             #+`Maioria Parlamentar`  ,
             +`Prof de Saude` 
             #+`Prof de Saude`  ,
             #+`Prof de Saude`  ,
             #+`Prof de Saude`  ,
             #+`PT`  
             #+`PDT` 
             #+`PFL/DEM` 
             #+`PL`    
             #+`PMDB` 
             #+`PPS`   
             #+`PSB`    
             #+`PSDB`    
             #+`PTB`    
             #+`PP`,    
             #+`PT` ,
             #+`PDT` ,
             #+`PFL/DEM` ,
             #+`PMDB`   ,
             #+`PP`   ,
             #+`PPS`   ,
             #+`PR`    ,
             #+`PSDB`   ,
             #+`PTB`    ,
             #+`PSB`    ,
             
             # +`PDT`   ,
             # +`PFL/DEM` ,
             # +`PMDB`  ,
             # +`PP`   ,
             # +`PSD`   ,
             # +`PR`   ,
             # +`PSDB`   ,
             # +`PTB`   ,
             # +`PSB`  ,
             # +`PT`  ,
             
             #+`PDT`    ,
             #+`PFL/DEM` ,
             #+`PMDB`   ,
             #+`PP`    ,
             #+`PSD`   ,
             #+`PSDB`    ,
             #+`PTB`    ,
             #+`PSB`   ,
             #+`PR`   ,
             #+`PT`  ,
             #+`Esquerda com maioria` ,
             #+`Direita com maioria` ,
             #+`Centro com maioria` ,
             #+`PT com maioria` 
             #+`PSDB com maioria` 
             #+`PFL/DEM com maioria`,
+`Esquerda com maioria` 
+`Direita com maioria` 
+`Centro com maioria` ,
data = banco_modelo_9)
#+`PT com maioria` ,
#+`PSDB com maioria` ,
#+`DEM com maioria` ,
#+`Esquerda com maioria` ,
#+`Direita com maioria`,
#+`Centro com maioria` ,
#+`PT com maioria` ,
#+`PSDB com maioria` ,
#+`PFL/DEM com maioria`,

tab_model(modelo9)

# 10. Modelo ciclo 09_12 com Ideologia. NAO TEM  ----
##

# 11. Modelo ciclo 13_16 com Ideologia  com maioria ----
##

banco_modelo_11 <- banco_dissertacao_pronto %>% mutate (pop_log_2012 = log(pop_2012_siops),
                                                       gasto_log_medio_05_08 = log(gasto_medio_05_08), 
                                                       gasto_log_medio_09_12 = log(gasto_medio_09_12),
                                                       gasto_log_medio_13_16 = log(gasto_medio_13_16),
                                                       gasto_log_medio_17_20 = log(gasto_medio_17_20),
                                                       psb_dummy_2008 = case_when(partido_eleito_2008 == 'PSB' ~ '1',
                                                                                  partido_eleito_2008 != 'PSB' ~ '0')) %>%  
  rename (#`Ciclo 05-08`  = gasto_log_medio_05_08,
    #`Ciclo 09-12`  = gasto_log_medio_09_12,
    `Ciclo 13-16`  = gasto_log_medio_13_16,
    # `Ciclo 17-20`  = ,gasto_log_medio_17_20,
    `Centro-Oeste` = centor_oeste_dummy,
    `Nordeste`     = nordeste_dummy,
    `Sudeste`      = sudeste_dummy,
    `Sul`          = sul_dummy,
    `PIB per capita`   = pib_per_2010,
    `Log da Populacao` =pop_log_2012, 
    # `PIB per capita`   = pib_per_2012,
    `IDHM`             = idhm_10,
    #`Dependencia Financeira`       = pp_trans_2005,
    #`Dependencia Financeira`       = pp_trans_2009,
    `Dependencia Financeira`       = pp_trans_2013,
    # `Dependencia Financeira`       = pp_trans_2017,
    `Municipio Urbano`             = dummy_urbano, 
    `Municipio Polo`                =  municipio_polo,
    #`Primeiro Mandato` = pri_mandato_04,
    #`Primeiro Mandato` = pri_mandato_08,
    `Primeiro Mandato` = pri_mandato_12,
    #`Primeiro Mandato` = pri_mandato_16,
    #`Maioria Parlamentar` = dummy_maioria_2004,
    # `Maioria Parlamentar` = dummy_maioria_2012,
    #`Maioria Parlamentar` = dummy_maioria_2016,
    #`Prof de Saude`       = prof_saude_dummy_2004,
    #`Prof de Saude`       = prof_saude_dummy_2008,
    `Prof de Saude`       = prof_saude_dummy_2012,
    #`Prof de Saude`       = prof_saude_dummy_2016,
    #`PT` = pt_dummy_2004,
    #`PDT`   = pdt_dummy_2004,
    #`PFL/DEM`   = pfl_dummy_2004,
    #`PL`   = pl_dummy_2004,
    #`PMDB`   = pmdb_dummy_2004,
    #`PPS`   = pps_dummy_2004,
    #`PSB`   = psb_dummy_2004,
    #`PSDB`   = psdb_dummy_2004,
    #`PTB`   = ptb_dummy_2004,
    #`PP`   = pp_dummy_2004,
    
    #`PT`   = pt_dummy_2008,
    #`PDT`   = pdt_dummy_2008,
    #`PFL/DEM`   = dem_dummy_2008,
    #`PMDB`   = pmdb_dummy_2008,
    #`PP`   = pp_dummy_2008,
    #`PPS`   = pps_dummy_2008,
    #`PR`   = pr_dummy_2008,
    #`PSDB`   = psdb_dummy_2008,
    #`PTB`   = ptb_dummy_2008,
    #`PSB`   = psb_dummy_2008)
    #  `PDT`   = pdt_dummy_2012,
    # `PFL/DEM`   = dem_dummy_2012,
    #`PMDB`   = pmdb_dummy_2012,
    #`PP`   = pp_dummy_2012,
    #`PSD`   = psd_dummy_2012,
    #`PR`   = pr_dummy_2012,
    #`PSDB`   = psdb_dummy_2012,
    #`PTB`   = ptb_dummy_2012,
    #`PSB`   = psb_dummy_2012,
    #`PT`   = pt_dummy_2012)
    
    #`PDT`   = pdt_dummy_2016,
    #`PFL/DEM`   = dem_dummy_2016,
    #`PMDB`   = pmdb_dummy_2016,
    #`PP`   = pp_dummy_2016,
    #`PSD`   = psd_dummy_2016,
    #`PSDB`   = psdb_dummy_2016,
    #`PTB`   = ptb_dummy_2016,
    #`PSB`   = psb_dummy_2016,
    #`PR`   = pr_dummy_2016,
    #`PT`   = pt_dummy_2016,
    #`Esquerda com maioria` = esquerda_maioria_2004,
    #`Direita com maioria`= direita_maioria_2004,
    #`Centro com maioria`= centro_maioria_2004,
    #`PT com maioria`= pt_maioria_2004,
    #`PSDB com maioria`= psdb_maioria_2004,
    #`PFL/DEM com maioria`= pfl_maioria_2004,
    #`Esquerda com maioria`= esquerda_maioria_2012,
    #`Direita com maioria`= direita_maioria_2012,
    #`Centro com maioria`= centro_maioria_2012,
   # `PT com maioria`= pt_maioria_2012,
  #  `PSDB com maioria`= psdb_maioria_2012,
   # `PFL/DEM com maioria`= dem_maioria_2012,
  `Esquerda com maioria` = esquerda_maioria_2012,
  `Direita com maioria`= direita_maioria_2012,
  `Centro com maioria`= centro_maioria_2012)
#`Esquerda com maioria`= esquerda_maioria_2016,
#`Direita com maioria`= direita_maioria_2016,
#`Centro com maioria`= centro_maioria_2016,
#`PT com maioria`= pt_maioria_2016,
#`PSDB com maioria`= psdb_maioria_2016,
#`PFL/DEM com maioria`= dem_maioria_2016)

modelo11<- lm(`Ciclo 13-16` ~ + `Centro-Oeste`
             # +`Ciclo 09-12`, 
             # +`Ciclo 13-16`,
             # +`Ciclo 17-20`,
             
             +`Nordeste`
             +`Sudeste` 
             +`Sul`
             +`PIB per capita`
             +    `Log da Populacao` 
             # +`PIB per capita`,
             +`IDHM`
             +`Dependencia Financeira` 
             # +`Dependencia Financeira` ,
             # +`Dependencia Financeira`,
             # +`Dependencia Financeira`,
             +`Municipio Urbano`  
             +`Municipio Polo` 
             +`Primeiro Mandato`
             #+`Primeiro Mandato` ,
             #+`Primeiro Mandato` ,
             #+`Primeiro Mandato`,
             #  +`Maioria Parlamentar` 
             #+`Maioria Parlamentar`  ,
             #+`Maioria Parlamentar`  ,
             +`Prof de Saude` 
             #+`Prof de Saude`  ,
             #+`Prof de Saude`  ,
             #+`Prof de Saude`  ,
             # +`PT`   
             #+`PDT`   
             #+`PFL/DEM`
             #+`PMDB` 
             #+`PP`   
             #+`PPS`  
             #+`PR`   
             #+`PSDB` 
             #+`PTB`  
             #+`PSB`,    
             
             
             #+`PT` 
             #+`PDT` 
             #+`PFL/DEM` 
             #+`PMDB`   
             #+`PP`   
             #+`PPS`   
             #+`PR`    
             #+`PSDB`   
             #+`PTB`    
             #+`PSB`  ,  
             
             #+`PDT`   
             #+`PFL/DEM` 
             #+`PMDB`  
             #+`PP`   
             #+`PSD`   
             #+`PR`   
             #+`PSDB`   
             #+`PTB`   
             #+`PSB`  
             #+`PT` 
             
             
             #+`PDT`    ,
             #+`PFL/DEM` ,
             #+`PMDB`   ,
             #+`PP`    ,
             #+`PSD`   ,
             #+`PSDB`    ,
             #+`PTB`    ,
             #+`PSB`   ,
             #+`PR`   ,
             #+`PT`  ,
             #+`Esquerda com maioria` ,
             #+`Direita com maioria` ,
             #+`Centro com maioria` ,
            # +`PT com maioria` 
            # +`PSDB com maioria` 
            # +`PFL/DEM com maioria` ,

+`Esquerda com maioria` 
+`Direita com maioria` 
+`Centro com maioria` ,
data =banco_modelo_11)
#+`PT com maioria` ,
#+`PSDB com maioria` ,
#+`DEM com maioria` ,
#+`Esquerda com maioria` ,
#+`Direita com maioria`,
#+`Centro com maioria` ,
#+`PT com maioria` ,
#+`PSDB com maioria` ,
#+`PFL/DEM com maioria`,

tab_model(modelo11)

# 12. Modelo ciclo 17_20 com Ideologia com maioria  ----
##


banco_modelo_12 <- banco_dissertacao_pronto %>% mutate (pop_log_2012 = log(pop_2012_siops),
                                                       gasto_log_medio_05_08 = log(gasto_medio_05_08), 
                                                       gasto_log_medio_09_12 = log(gasto_medio_09_12),
                                                       gasto_log_medio_13_16 = log(gasto_medio_13_16),
                                                       gasto_log_medio_17_20 = log(gasto_medio_17_20),
                                                       psb_dummy_2008 = case_when(partido_eleito_2008 == 'PSB' ~ '1',
                                                                                  partido_eleito_2008 != 'PSB' ~ '0')) %>%  
  rename (#`Ciclo 05-08`  = gasto_log_medio_05_08,
    #`Ciclo 09-12`  = gasto_log_medio_09_12,
    #`Ciclo 13-16`  = gasto_log_medio_13_16,
    `Ciclo 17-20`  = gasto_log_medio_17_20,
    `Centro-Oeste` = centor_oeste_dummy,
    `Nordeste`     = nordeste_dummy,
    `Sudeste`      = sudeste_dummy,
    `Sul`          = sul_dummy,
    `PIB per capita`   = pib_per_2010,
    `Log da Populacao` =pop_log_2012, 
    # `PIB per capita`   = pib_per_2012,
    `IDHM`             = idhm_10,
    #`Dependencia Financeira`       = pp_trans_2005,
    #`Dependencia Financeira`       = pp_trans_2009,
    #`Dependencia Financeira`       = pp_trans_2013,
    `Dependencia Financeira`       = pp_trans_2017,
    `Municipio Urbano`             = dummy_urbano, 
    `Municipio Polo`                =  municipio_polo,
    #`Primeiro Mandato` = pri_mandato_04,
    #`Primeiro Mandato` = pri_mandato_08,
    #`Primeiro Mandato` = pri_mandato_12,
    `Primeiro Mandato` = pri_mandato_16,
    #`Maioria Parlamentar` = dummy_maioria_2004,
    #`Maioria Parlamentar` = dummy_maioria_2012,
    # `Maioria Parlamentar` = dummy_maioria_2016,
    #`Prof de Saude`       = prof_saude_dummy_2004,
    #`Prof de Saude`       = prof_saude_dummy_2008,
    #`Prof de Saude`       = prof_saude_dummy_2012,
    `Prof de Saude`       = prof_saude_dummy_2016,
    #`PT` = pt_dummy_2004,
    #`PDT`   = pdt_dummy_2004,
    #`PFL/DEM`   = pfl_dummy_2004,
    #`PL`   = pl_dummy_2004,
    #`PMDB`   = pmdb_dummy_2004,
    #`PPS`   = pps_dummy_2004,
    #`PSB`   = psb_dummy_2004,
    #`PSDB`   = psdb_dummy_2004,
    #`PTB`   = ptb_dummy_2004,
    #`PP`   = pp_dummy_2004,
    
    #`PT`   = pt_dummy_2008,
    #`PDT`   = pdt_dummy_2008,
    #`PFL/DEM`   = dem_dummy_2008,
    #`PMDB`   = pmdb_dummy_2008,
    #`PP`   = pp_dummy_2008,
    #`PPS`   = pps_dummy_2008,
    #`PR`   = pr_dummy_2008,
    #`PSDB`   = psdb_dummy_2008,
    #`PTB`   = ptb_dummy_2008,
    #`PSB`   = psb_dummy_2008)
    #`PDT`   = pdt_dummy_2012,
    #`PFL/DEM`   = dem_dummy_2012,
    #`PMDB`   = pmdb_dummy_2012,
    #`PP`   = pp_dummy_2012,
    #`PSD`   = psd_dummy_2012,
    #`PR`   = pr_dummy_2012,
    #`PSDB`   = psdb_dummy_2012,
    #`PTB`   = ptb_dummy_2012,
    #`PSB`   = psb_dummy_2012,
    #`PT`   = pt_dummy_2012)
    # `PDT`   = pdt_dummy_2016,
    #`PFL/DEM`   = dem_dummy_2016,
    #`PMDB`   = pmdb_dummy_2016,
    #`PP`   = pp_dummy_2016,
    #`PSD`   = psd_dummy_2016,
    #`PSDB`   = psdb_dummy_2016,
    #`PTB`   = ptb_dummy_2016,
    #`PSB`   = psb_dummy_2016,
    #`PR`   = pr_dummy_2016,
    #`PT`   = pt_dummy_2016)
    #`Esquerda com maioria` = esquerda_maioria_2004,
    #`Direita com maioria`= direita_maioria_2004,
    #`Centro com maioria`= centro_maioria_2004,
    #`PT com maioria`= pt_maioria_2004,
    #`PSDB com maioria`= psdb_maioria_2004,
    #`PFL/DEM com maioria`= pfl_maioria_2004,
    #`Esquerda com maioria`= esquerda_maioria_2012,
    #`Direita com maioria`= direita_maioria_2012,
    #`Centro com maioria`= centro_maioria_2012,
    #`PT com maioria`= pt_maioria_2012,
    #`PSDB com maioria`= psdb_maioria_2012,
    #`DEM com maioria`= dem_maioria_2012,
    `Esquerda com maioria`= esquerda_maioria_2016,
    `Direita com maioria`= direita_maioria_2016,
    `Centro com maioria`= centro_maioria_2016)
    #`PT com maioria`= pt_maioria_2016,
    #`PSDB com maioria`= psdb_maioria_2016,
    #`PFL/DEM com maioria`= dem_maioria_2016,)

modelo12<- lm(`Ciclo 17-20` ~ + `Centro-Oeste`
             # +`Ciclo 09-12`, 
             # +`Ciclo 13-16`,
             # +`Ciclo 17-20`,
             
             +`Nordeste`
             +`Sudeste` 
             +`Sul`
             +`PIB per capita`
             +    `Log da Populacao` 
             # +`PIB per capita`,
             +`IDHM`
             +`Dependencia Financeira` 
             # +`Dependencia Financeira` ,
             # +`Dependencia Financeira`,
             # +`Dependencia Financeira`,
             +`Municipio Urbano`  
             +`Municipio Polo` 
             +`Primeiro Mandato`
             #+`Primeiro Mandato` ,
             #+`Primeiro Mandato` ,
             #+`Primeiro Mandato`,
             #+`Maioria Parlamentar` 
             #+`Maioria Parlamentar`  ,
             #+`Maioria Parlamentar`  ,
             +`Prof de Saude` 
             #+`Prof de Saude`  ,
             #+`Prof de Saude`  ,
             #+`Prof de Saude`  ,
             # +`PT`   
             #+`PDT`   
             #+`PFL/DEM`
             #+`PMDB` 
             #+`PP`   
             #+`PPS`  
             #+`PR`   
             #+`PSDB` 
             #+`PTB`  
             #+`PSB`,    
             
             
             #+`PT` 
             #+`PDT` 
             #+`PFL/DEM` 
             #+`PMDB`   
             #+`PP`   
             #+`PPS`   
             #+`PR`    
             #+`PSDB`   
             #+`PTB`    
             #+`PSB`  ,  
             
             #+`PDT`   
             #+`PFL/DEM` 
             #+`PMDB`  
             #+`PP`   
             #+`PSD`   
             #+`PR`   
             #+`PSDB`   
             #+`PTB`   
             #+`PSB`  
             #+`PT` , 
             
             
             #+`PDT`    
             #+`PFL/DEM` 
             #+`PMDB`   
             #+`PP`    
             #+`PSD`   
             #+`PSDB`    
             #+`PTB`    
             #+`PSB`   
             #+`PR`   
             #+`PT`  ,
             #+`Esquerda com maioria` ,
             #+`Direita com maioria` ,
             #+`Centro com maioria` ,
             #+`PT com maioria` ,
             #+`PSDB com maioria` ,
             #+`PFL/DEM com maioria` ,
             #+`Esquerda com maioria` ,
             #+`Direita com maioria` ,
             #+`Centro com maioria` ,
             #+`PT com maioria` ,
             #+`PSDB com maioria` ,
             #+`DEM com maioria` ,
             +`Esquerda com maioria` 
             +`Direita com maioria`
             +`Centro com maioria` ,
             data= banco_modelo_12)
             #+`PT com maioria` 
             #+`PSDB com maioria` 
             #+`PFL/DEM com maioria`

tab_model(modelo12)
tab_model(modelo9,modelo11,modelo12)

# Testes dos modelos com Ideologia. 9 ao 12 ----
##

# Teste do presuposto da Homocedasticidade

library(gvlma) # Modelo 9
gvmodel_modelo_9 <- gvlma(modelo9)
summary(gvmodel_modelo_9)

gvmodel_modelo_11 <- gvlma(modelo11) # Modelo 11
summary(gvmodel_modelo_11)

gvmodel_modelo_12 <- gvlma(modelo12) # Modelo 12
summary(gvmodel_modelo_12)

check_heteroscedasticity(modelo9) %>% # Modelo 9
  plot(modelo9) 

check_heteroscedasticity(modelo11) %>% # Modelo 11
  plot(modelo11)                             

check_heteroscedasticity(modelo12) %>% # Modelo 12
  plot(modelo12)

# Teste do pressuposto de normalidade 

residos_modelo_9 <- residuals(modelo9) # Modelo 9
qqnorm(residos_modelo_9)

residos_modelo_11 <- residuals(modelo11) # Modelo 11
qqnorm(residos_modelo_11)

residos_modelo_12 <- residuals(modelo12) # Modelo 12
qqnorm(residos_modelo_12)

# Teste do pressuposto de multicollinearity

if (require("see")) {
  modelo_9_multicollinearity <- check_collinearity(modelo9) # Modelo 9
  plot(modelo_9_multicollinearity)+
    coord_flip()
}                            

if (require("see")) {
  modelo_11_multicollinearity <- check_collinearity(modelo11) # Modelo 11
  plot(modelo_11_multicollinearity)+
    coord_flip()
}                            

if (require("see")) {
  modelo_12_multicollinearity <- check_collinearity(modelo12) # Modelo 12
  plot(modelo_12_multicollinearity)+
    coord_flip()
}                            


# 13. Modelo ciclo 05_08 com Ideologia  ----
##

banco_modelo_13 <- banco_dissertacao_pronto %>% mutate (pop_log_2012 = log(pop_2012_siops),
                                                       gasto_log_medio_05_08 = log(gasto_medio_05_08), 
                                                       gasto_log_medio_09_12 = log(gasto_medio_09_12),
                                                       gasto_log_medio_13_16 = log(gasto_medio_13_16),
                                                       gasto_log_medio_17_20 = log(gasto_medio_17_20),
                                                       psb_dummy_2008 = case_when(partido_eleito_2008 == 'PSB' ~ '1',
                                                                                  partido_eleito_2008 != 'PSB' ~ '0')) %>%  
  rename (`Ciclo 05-08`  = gasto_log_medio_05_08,
          # `Ciclo 09-12`  = gasto_log_medio_09_12,
          # `Ciclo 13-16`  = gasto_log_medio_13_16,
          # `Ciclo 17-20`  = ,gasto_log_medio_17_20,
          `Log da Populacao` = pop_log_2012, 
          `Centro-Oeste` = centor_oeste_dummy,
          `Nordeste`     = nordeste_dummy,
          `Sudeste`      = sudeste_dummy,
          `Sul`          = sul_dummy,
          `PIB per capita`   = pib_per_2010,
          # `PIB per capita`   = pib_per_2012,
          `IDHM`             = idhm_10,
          `Dependencia Financeira`       = pp_trans_2005,
          # `Dependencia Financeira`       = pp_trans_2009,
          # `Dependencia Financeira`       = pp_trans_2013,
          # `Dependencia Financeira`       = pp_trans_2017,
          `Municipio Urbano`             = dummy_urbano, 
          `Municipio Polo`                =  municipio_polo,
          `Primeiro Mandato` = pri_mandato_04,
          #`Primeiro Mandato` = pri_mandato_08,
          #`Primeiro Mandato` = pri_mandato_12,
          #`Primeiro Mandato` = pri_mandato_16,
          # `Maioria Parlamentar` = dummy_maioria_2004,
          #`Maioria Parlamentar` = dummy_maioria_2012,
          #`Maioria Parlamentar` = dummy_maioria_2016,
          `Prof de Saude`       = prof_saude_dummy_2004,
          #`Prof de Saude`       = prof_saude_dummy_2008,
          #`Prof de Saude`       = prof_saude_dummy_2012,
          #`Prof de Saude`       = prof_saude_dummy_2016,
          #`PT` = pt_dummy_2004,
          #`PDT`   = pdt_dummy_2004,
          #`PFL/DEM`   = pfl_dummy_2004,
          #`PL`   = pl_dummy_2004,
          #`PMDB`   = pmdb_dummy_2004,
          #`PPS`   = pps_dummy_2004,
          #`PSB`   = psb_dummy_2004,
          #`PSDB`   = psdb_dummy_2004,
          #`PTB`   = ptb_dummy_2004,
          #`PP`   = pp_dummy_2004)
          
          #`PT`   = pt_dummy_2008,
          #`PDT`   = pdt_dummy_2008,
          #`PFL/DEM`   = dem_dummy_2008,
          #`PMDB`   = pmdb_dummy_2008,
          #`PP`   = pp_dummy_2008,
          #`PPS`   = pps_dummy_2008,
          #`PR`   = pr_dummy_2008,
          #`PSDB`   = psdb_dummy_2008,
          #`PTB`   = ptb_dummy_2008,
          #`PSB`   = psb_dummy_2008,
          
          # `PDT`   = pdt_dummy_2012,
          # `PFL/DEM`   = dem_dummy_2012,
          # `PMDB`   = pmdb_dummy_2012,
          # `PP`   = pp_dummy_2012,
          # `PSD`   = psd_dummy_2012,
          # `PR`   = pr_dummy_2012,
          # `PSDB`   = psdb_dummy_2012,
          # `PTB`   = ptb_dummy_2012,
          # `PSB`   = psb_dummy_2012,
          # `PT`   = pt_dummy_2012,
          
          #`PDT`   = pdt_dummy_2016,
          #`PFL/DEM`   = dem_dummy_2016,
          #`PMDB`   = pmdb_dummy_2016,
          #`PP`   = pp_dummy_2016,
          #`PSD`   = psd_dummy_2016,
          #`PSDB`   = psdb_dummy_2016,
          #`PTB`   = ptb_dummy_2016,
          #`PSB`   = psb_dummy_2016,
          #`PR`   = pr_dummy_2016,
          #`PT`   = pt_dummy_2016,
          `Esquerda` = esquerda_2004,
          `Direita`= direita_2004)
         # `Centro`= centro_2004)
#`PT com maioria`= pt_maioria_2004,
#`PSDB com maioria`= psdb_maioria_2004,
#`PFL/DEM com maioria`= pfl_maioria_2004,
#`Esquerda com maioria` = esquerda_maioria_2004,
#`Centro com maioria`  = centro_maioria_2004,
#`Direita com maioria`  = direita_maioria_2004,
#`Esquerda com maioria`= esquerda_maioria_2012,
#`Direita com maioria`= direita_maioria_2012,
#`Centro com maioria`= centro_maioria_2012,
#`PT com maioria`= pt_maioria_2012,
#`PSDB com maioria`= psdb_maioria_2012,
#`DEM com maioria`= dem_maioria_2012,
#`Esquerda com maioria`= esquerda_maioria_2016,
#`Direita com maioria`= direita_maioria_2016,
#`Centro com maioria`= centro_maioria_2016,
#`PT com maioria`= pt_maioria_2016,
#`PSDB com maioria`= psdb_maioria_2016,
#`PFL/DEM com maioria`= dem_maioria_2016)



modelo13<- lm(`Ciclo 05-08` ~ + `Centro-Oeste`
             # +`Ciclo 09-12`, 
             # +`Ciclo 13-16`,
             # +`Ciclo 17-20`,
             +`Nordeste`
             +`Sudeste` 
             +`Sul`
             +`PIB per capita`
             +    `Log da Populacao` 
             # +`PIB per capita`,
             +`IDHM`
             +`Dependencia Financeira` 
             # +`Dependencia Financeira` ,
             # +`Dependencia Financeira`,
             # +`Dependencia Financeira`,
             +`Municipio Urbano`  
             +`Municipio Polo`                
             +`Primeiro Mandato`
             #+`Primeiro Mandato` ,
             #+`Primeiro Mandato` ,
             #+`Primeiro Mandato`,
             # +`Maioria Parlamentar` 
             #+`Maioria Parlamentar`  ,
             #+`Maioria Parlamentar`  ,
             +`Prof de Saude`
             +`Esquerda` 
             +`Direita`,
             data = banco_modelo_13)
             #+`Prof de Saude`  ,
             #+`Prof de Saude`  ,
             #+`Prof de Saude`  ,
             #+`PT`  
             #+`PDT` 
             #+`PFL/DEM` 
             #+`PL`    
             #+`PMDB` 
             #+`PPS`   
             #+`PSB`    
             #+`PSDB`    
             #+`PTB`    
             #+`PP`,    
             #+`PT` ,
             #+`PDT` ,
             #+`PFL/DEM` ,
             #+`PMDB`   ,
             #+`PP`   ,
             #+`PPS`   ,
             #+`PR`    ,
             #+`PSDB`   ,
             #+`PTB`    ,
             #+`PSB`    ,
             
             # +`PDT`   ,
             # +`PFL/DEM` ,
             # +`PMDB`  ,
             # +`PP`   ,
             # +`PSD`   ,
             # +`PR`   ,
             # +`PSDB`   ,
             # +`PTB`   ,
             # +`PSB`  ,
             # +`PT`  ,
             
             #+`PDT`    ,
             #+`PFL/DEM` ,
             #+`PMDB`   ,
             #+`PP`    ,
             #+`PSD`   ,
             #+`PSDB`    ,
             #+`PTB`    ,
             #+`PSB`   ,
             #+`PR`   ,
             #+`PT`  ,
             #+`Esquerda com maioria` ,
             #+`Direita com maioria` ,
             #+`Centro com maioria` ,
             #+`PT com maioria` 
             #+`PSDB com maioria` 
             #+`PFL/DEM com maioria`,

            # +`Centro`,
           
#+`PT com maioria` ,
#+`PSDB com maioria` ,
#+`DEM com maioria` ,
#+`Esquerda com maioria` ,
#+`Direita com maioria`,
#+`Centro com maioria` ,
#+`PT com maioria` ,
#+`PSDB com maioria` ,
#+`PFL/DEM com maioria`,

tab_model(modelo13)

# 14. Modelo ciclo 09_12 com Ideologia  ----
##


banco_modelo_14 <- banco_dissertacao_pronto %>% mutate (pop_log_2012 = log(pop_2012_siops),
                                                       gasto_log_medio_05_08 = log(gasto_medio_05_08), 
                                                       gasto_log_medio_09_12 = log(gasto_medio_09_12),
                                                       gasto_log_medio_13_16 = log(gasto_medio_13_16),
                                                       gasto_log_medio_17_20 = log(gasto_medio_17_20),
                                                       psb_dummy_2008 = case_when(partido_eleito_2008 == 'PSB' ~ '1',
                                                                                  partido_eleito_2008 != 'PSB' ~ '0')) %>%  
  rename (#`Ciclo 05-08`  = gasto_log_medio_05_08,
    `Ciclo 09-12`  = gasto_log_medio_09_12,
    # `Ciclo 13-16`  = gasto_log_medio_13_16,
    # `Ciclo 17-20`  = ,gasto_log_medio_17_20,
    `Centro-Oeste` = centor_oeste_dummy,
    `Nordeste`     = nordeste_dummy,
    `Sudeste`      = sudeste_dummy,
    `Sul`          = sul_dummy,
    `PIB per capita`   = pib_per_2010,
    `Log da Populacao` =pop_log_2012, 
    # `PIB per capita`   = pib_per_2012,
    `IDHM`             = idhm_10,
    #`Dependencia Financeira`       = pp_trans_2005,
    `Dependencia Financeira`       = pp_trans_2009,
    # `Dependencia Financeira`       = pp_trans_2013,
    # `Dependencia Financeira`       = pp_trans_2017,
    `Municipio Urbano`             = dummy_urbano, 
    `Municipio Polo`                =  municipio_polo,
    #`Primeiro Mandato` = pri_mandato_04,
    `Primeiro Mandato` = pri_mandato_08,
    #`Primeiro Mandato` = pri_mandato_12,
    #`Primeiro Mandato` = pri_mandato_16,
    #`Maioria Parlamentar` = dummy_maioria_2004,
    #`Maioria Parlamentar` = dummy_maioria_2012,
    #`Maioria Parlamentar` = dummy_maioria_2016,
    #`Prof de Saude`       = prof_saude_dummy_2004,
    `Prof de Saude`       = prof_saude_dummy_2008,
    `Esquerda` = esquerda_2012,
    `Direita`= direita_2012,
    `Centro`= centro_2012)
    #`Prof de Saude`       = prof_saude_dummy_2012,
    #`Prof de Saude`       = prof_saude_dummy_2016,
    #`PT` = pt_dummy_2004,
    #`PDT`   = pdt_dummy_2004,
    #`PFL/DEM`   = pfl_dummy_2004,
    #`PL`   = pl_dummy_2004,
    #`PMDB`   = pmdb_dummy_2004,
    #`PPS`   = pps_dummy_2004,
    #`PSB`   = psb_dummy_2004,
    #`PSDB`   = psdb_dummy_2004,
    #`PTB`   = ptb_dummy_2004,
    #`PP`   = pp_dummy_2004,
    
   #`PT`   = pt_dummy_2008,
    #`PDT`   = pdt_dummy_2008,
    #`PFL/DEM`   = dem_dummy_2008,
    #`PMDB`   = pmdb_dummy_2008,
    #`PP`   = pp_dummy_2008,
    #`PPS`   = pps_dummy_2008,
    #`PR`   = pr_dummy_2008,
    #`PSDB`   = psdb_dummy_2008,
    #`PTB`   = ptb_dummy_2008,
    #`PSB`   = psb_dummy_2008)
# `PDT`   = pdt_dummy_2012,
# `PFL/DEM`   = dem_dummy_2012,
# `PMDB`   = pmdb_dummy_2012,
# `PP`   = pp_dummy_2012,
# `PSD`   = psd_dummy_2012,
# `PR`   = pr_dummy_2012,
# `PSDB`   = psdb_dummy_2012,
# `PTB`   = ptb_dummy_2012,
# `PSB`   = psb_dummy_2012,
# `PT`   = pt_dummy_2012,

#`PDT`   = pdt_dummy_2016,
#`PFL/DEM`   = dem_dummy_2016,
#`PMDB`   = pmdb_dummy_2016,
#`PP`   = pp_dummy_2016,
#`PSD`   = psd_dummy_2016,
#`PSDB`   = psdb_dummy_2016,
#`PTB`   = ptb_dummy_2016,
#`PSB`   = psb_dummy_2016,
#`PR`   = pr_dummy_2016,
#`PT`   = pt_dummy_2016,
#`Esquerda com maioria` = esquerda_maioria_2004,
#`Direita com maioria`= direita_maioria_2004,
#`Centro com maioria`= centro_maioria_2004,
#`PT com maioria`= pt_maioria_2004,
#`PSDB com maioria`= psdb_maioria_2004,
#`PFL/DEM com maioria`= pfl_maioria_2004,
#`Esquerda com maioria`= esquerda_maioria_2012,
#`Direita com maioria`= direita_maioria_2012,
#`Centro com maioria`= centro_maioria_2012,
#`PT com maioria`= pt_maioria_2012,
#`PSDB com maioria`= psdb_maioria_2012,
#`DEM com maioria`= dem_maioria_2012,
#`Esquerda com maioria`= esquerda_maioria_2016,
#`Direita com maioria`= direita_maioria_2016,
#`Centro com maioria`= centro_maioria_2016,
#`PT com maioria`= pt_maioria_2016,
#`PSDB com maioria`= psdb_maioria_2016,
#`PFL/DEM com maioria`= dem_maioria_2016)

modelo14<- lm(`Ciclo 09-12` ~ + `Centro-Oeste`
             # +`Ciclo 09-12`, 
             # +`Ciclo 13-16`,
             # +`Ciclo 17-20`,
             
             +`Nordeste`
             +`Sudeste` 
             +`Sul`
             +`PIB per capita`
             +    `Log da Populacao` 
             # +`PIB per capita`,
             +`IDHM`
             +`Dependencia Financeira` 
             # +`Dependencia Financeira` ,
             # +`Dependencia Financeira`,
             # +`Dependencia Financeira`,
             +`Municipio Urbano`  
             +`Municipio Polo` 
             +`Primeiro Mandato`
             #+`Primeiro Mandato` ,
             #+`Primeiro Mandato` ,
             #+`Primeiro Mandato`,
             # +`Maioria Parlamentar` 
             #+`Maioria Parlamentar`  ,
             #+`Maioria Parlamentar`  ,
             +`Prof de Saude` 
             +`Esquerda`
             +`Direita`,
             data = banco_modelo_14)
             #+`Prof de Saude`  ,
             #+`Prof de Saude`  ,
             #+`Prof de Saude`  ,
             #+`PT`   
             #+`PDT`   
             #+`PFL/DEM`
             #+`PMDB` 
             #+`PP`   
             #+`PPS`  
             #+`PR`   
             #+`PSDB` 
             #+`PTB`  
             #+`PSB`,    
          

#+`PT` ,
#+`PDT` ,
#+`PFL/DEM` ,
#+`PMDB`   ,
#+`PP`   ,
#+`PPS`   ,
#+`PR`    ,
#+`PSDB`   ,
#+`PTB`    ,
#+`PSB`    ,

# +`PDT`   ,
# +`PFL/DEM` ,
# +`PMDB`  ,
# +`PP`   ,
# +`PSD`   ,
# +`PR`   ,
# +`PSDB`   ,
# +`PTB`   ,
# +`PSB`  ,
# +`PT`  ,

#+`PDT`    ,
#+`PFL/DEM` ,
#+`PMDB`   ,
#+`PP`    ,
#+`PSD`   ,
#+`PSDB`    ,
#+`PTB`    ,
#+`PSB`   ,
#+`PR`   ,
#+`PT`  ,
#+`Esquerda com maioria` ,
#+`Direita com maioria` ,
#+`Centro com maioria` ,
#+`PT com maioria` ,
#+`PSDB com maioria` ,
#+`PFL/DEM com maioria` ,
#+`Esquerda com maioria` ,
#+`Direita com maioria` ,
#+`Centro com maioria` ,
#+`PT com maioria` ,
#+`PSDB com maioria` ,
#+`DEM com maioria` ,
#+`Esquerda com maioria` ,
#+`Direita com maioria`,
#+`Centro com maioria` ,
#+`PT com maioria` ,
#+`PSDB com maioria` ,
#+`PFL/DEM com maioria`,

tab_model(modelo14)

# 15. Modelo ciclo 13_16 com Ideologia ----
##

banco_modelo_15 <- banco_dissertacao_pronto %>% mutate (pop_log_2012 = log(pop_2012_siops),
                                                        gasto_log_medio_05_08 = log(gasto_medio_05_08), 
                                                        gasto_log_medio_09_12 = log(gasto_medio_09_12),
                                                        gasto_log_medio_13_16 = log(gasto_medio_13_16),
                                                        gasto_log_medio_17_20 = log(gasto_medio_17_20),
                                                        psb_dummy_2008 = case_when(partido_eleito_2008 == 'PSB' ~ '1',
                                                                                   partido_eleito_2008 != 'PSB' ~ '0')) %>%  
  rename (#`Ciclo 05-08`  = gasto_log_medio_05_08,
    #`Ciclo 09-12`  = gasto_log_medio_09_12,
    `Ciclo 13-16`  = gasto_log_medio_13_16,
    # `Ciclo 17-20`  = ,gasto_log_medio_17_20,
    `Centro-Oeste` = centor_oeste_dummy,
    `Nordeste`     = nordeste_dummy,
    `Sudeste`      = sudeste_dummy,
    `Sul`          = sul_dummy,
    `PIB per capita`   = pib_per_2010,
    `Log da Populacao` =pop_log_2012, 
    # `PIB per capita`   = pib_per_2012,
    `IDHM`             = idhm_10,
    #`Dependencia Financeira`       = pp_trans_2005,
    #`Dependencia Financeira`       = pp_trans_2009,
    `Dependencia Financeira`       = pp_trans_2013,
    # `Dependencia Financeira`       = pp_trans_2017,
    `Municipio Urbano`             = dummy_urbano, 
    `Municipio Polo`                =  municipio_polo,
    #`Primeiro Mandato` = pri_mandato_04,
    #`Primeiro Mandato` = pri_mandato_08,
    `Primeiro Mandato` = pri_mandato_12,
    #`Primeiro Mandato` = pri_mandato_16,
    #`Maioria Parlamentar` = dummy_maioria_2004,
    # `Maioria Parlamentar` = dummy_maioria_2012,
    #`Maioria Parlamentar` = dummy_maioria_2016,
    #`Prof de Saude`       = prof_saude_dummy_2004,
    #`Prof de Saude`       = prof_saude_dummy_2008,
    `Prof de Saude`       = prof_saude_dummy_2012,
    #`Prof de Saude`       = prof_saude_dummy_2016,
    #`PT` = pt_dummy_2004,
    #`PDT`   = pdt_dummy_2004,
    #`PFL/DEM`   = pfl_dummy_2004,
    #`PL`   = pl_dummy_2004,
    #`PMDB`   = pmdb_dummy_2004,
    #`PPS`   = pps_dummy_2004,
    #`PSB`   = psb_dummy_2004,
    #`PSDB`   = psdb_dummy_2004,
    #`PTB`   = ptb_dummy_2004,
    #`PP`   = pp_dummy_2004,
    
    #`PT`   = pt_dummy_2008,
    #`PDT`   = pdt_dummy_2008,
    #`PFL/DEM`   = dem_dummy_2008,
    #`PMDB`   = pmdb_dummy_2008,
    #`PP`   = pp_dummy_2008,
    #`PPS`   = pps_dummy_2008,
    #`PR`   = pr_dummy_2008,
    #`PSDB`   = psdb_dummy_2008,
    #`PTB`   = ptb_dummy_2008,
    #`PSB`   = psb_dummy_2008)
    #  `PDT`   = pdt_dummy_2012,
    # `PFL/DEM`   = dem_dummy_2012,
    #`PMDB`   = pmdb_dummy_2012,
    #`PP`   = pp_dummy_2012,
    #`PSD`   = psd_dummy_2012,
    #`PR`   = pr_dummy_2012,
    #`PSDB`   = psdb_dummy_2012,
    #`PTB`   = ptb_dummy_2012,
    #`PSB`   = psb_dummy_2012,
    #`PT`   = pt_dummy_2012)
    
    #`PDT`   = pdt_dummy_2016,
    #`PFL/DEM`   = dem_dummy_2016,
    #`PMDB`   = pmdb_dummy_2016,
    #`PP`   = pp_dummy_2016,
    #`PSD`   = psd_dummy_2016,
    #`PSDB`   = psdb_dummy_2016,
    #`PTB`   = ptb_dummy_2016,
    #`PSB`   = psb_dummy_2016,
    #`PR`   = pr_dummy_2016,
    #`PT`   = pt_dummy_2016,
    #`Esquerda com maioria` = esquerda_maioria_2004,
    #`Direita com maioria`= direita_maioria_2004,
    #`Centro com maioria`= centro_maioria_2004,
    #`PT com maioria`= pt_maioria_2004,
    #`PSDB com maioria`= psdb_maioria_2004,
    #`PFL/DEM com maioria`= pfl_maioria_2004,
    #`Esquerda com maioria`= esquerda_maioria_2012,
    #`Direita com maioria`= direita_maioria_2012,
    #`Centro com maioria`= centro_maioria_2012,
    # `PT com maioria`= pt_maioria_2012,
    #  `PSDB com maioria`= psdb_maioria_2012,
    # `PFL/DEM com maioria`= dem_maioria_2012,
    `Esquerda` = esquerda_2012,
    `Direita`= direita_2012,
    `Centro`= centro_2012)
#`Esquerda com maioria`= esquerda_maioria_2016,
#`Direita com maioria`= direita_maioria_2016,
#`Centro com maioria`= centro_maioria_2016,
#`PT com maioria`= pt_maioria_2016,
#`PSDB com maioria`= psdb_maioria_2016,
#`PFL/DEM com maioria`= dem_maioria_2016)

modelo15<- lm(`Ciclo 13-16` ~ + `Centro-Oeste`
              # +`Ciclo 09-12`, 
              # +`Ciclo 13-16`,
              # +`Ciclo 17-20`,
              
              +`Nordeste`
              +`Sudeste` 
              +`Sul`
              +`PIB per capita`
              +    `Log da Populacao` 
              # +`PIB per capita`,
              +`IDHM`
              +`Dependencia Financeira` 
              # +`Dependencia Financeira` ,
              # +`Dependencia Financeira`,
              # +`Dependencia Financeira`,
              +`Municipio Urbano`  
              +`Municipio Polo` 
              +`Primeiro Mandato`
              #+`Primeiro Mandato` ,
              #+`Primeiro Mandato` ,
              #+`Primeiro Mandato`,
              #  +`Maioria Parlamentar` 
              #+`Maioria Parlamentar`  ,
              #+`Maioria Parlamentar`  ,
              +`Prof de Saude` 
              #+`Prof de Saude`  ,
              #+`Prof de Saude`  ,
              #+`Prof de Saude`  ,
              # +`PT`   
              #+`PDT`   
              #+`PFL/DEM`
              #+`PMDB` 
              #+`PP`   
              #+`PPS`  
              #+`PR`   
              #+`PSDB` 
              #+`PTB`  
              #+`PSB`,    
              
              
              #+`PT` 
              #+`PDT` 
              #+`PFL/DEM` 
              #+`PMDB`   
              #+`PP`   
              #+`PPS`   
              #+`PR`    
              #+`PSDB`   
              #+`PTB`    
              #+`PSB`  ,  
              
              #+`PDT`   
              #+`PFL/DEM` 
              #+`PMDB`  
              #+`PP`   
              #+`PSD`   
              #+`PR`   
              #+`PSDB`   
              #+`PTB`   
              #+`PSB`  
              #+`PT` 
              
              
              #+`PDT`    ,
              #+`PFL/DEM` ,
              #+`PMDB`   ,
              #+`PP`    ,
              #+`PSD`   ,
              #+`PSDB`    ,
              #+`PTB`    ,
              #+`PSB`   ,
              #+`PR`   ,
              #+`PT`  ,
              #+`Esquerda com maioria` ,
              #+`Direita com maioria` ,
              #+`Centro com maioria` ,
              # +`PT com maioria` 
              # +`PSDB com maioria` 
              # +`PFL/DEM com maioria` ,
              
              +`Esquerda` 
              +`Direita`,
              data =banco_modelo_15)
              #+`Centro` 
#+`PT com maioria` ,
#+`PSDB com maioria` ,
#+`DEM com maioria` ,
#+`Esquerda com maioria` ,
#+`Direita com maioria`,
#+`Centro com maioria` ,
#+`PT com maioria` ,
#+`PSDB com maioria` ,
#+`PFL/DEM com maioria`,

tab_model(modelo15)

# 16. Modelo ciclo 17_20 com Ideologia   ----
##


banco_modelo_16 <- banco_dissertacao_pronto %>% mutate (pop_log_2012 = log(pop_2012_siops),
                                                        gasto_log_medio_05_08 = log(gasto_medio_05_08), 
                                                        gasto_log_medio_09_12 = log(gasto_medio_09_12),
                                                        gasto_log_medio_13_16 = log(gasto_medio_13_16),
                                                        gasto_log_medio_17_20 = log(gasto_medio_17_20),
                                                        psb_dummy_2008 = case_when(partido_eleito_2008 == 'PSB' ~ '1',
                                                                                   partido_eleito_2008 != 'PSB' ~ '0')) %>%  
  rename (#`Ciclo 05-08`  = gasto_log_medio_05_08,
    #`Ciclo 09-12`  = gasto_log_medio_09_12,
    #`Ciclo 13-16`  = gasto_log_medio_13_16,
    `Ciclo 17-20`  = gasto_log_medio_17_20,
    `Centro-Oeste` = centor_oeste_dummy,
    `Nordeste`     = nordeste_dummy,
    `Sudeste`      = sudeste_dummy,
    `Sul`          = sul_dummy,
    `PIB per capita`   = pib_per_2010,
    `Log da Populacao` =pop_log_2012, 
    # `PIB per capita`   = pib_per_2012,
    `IDHM`             = idhm_10,
    #`Dependencia Financeira`       = pp_trans_2005,
    #`Dependencia Financeira`       = pp_trans_2009,
    #`Dependencia Financeira`       = pp_trans_2013,
    `Dependencia Financeira`       = pp_trans_2017,
    `Municipio Urbano`             = dummy_urbano, 
    `Municipio Polo`                =  municipio_polo,
    #`Primeiro Mandato` = pri_mandato_04,
    #`Primeiro Mandato` = pri_mandato_08,
    #`Primeiro Mandato` = pri_mandato_12,
    `Primeiro Mandato` = pri_mandato_16,
    #`Maioria Parlamentar` = dummy_maioria_2004,
    #`Maioria Parlamentar` = dummy_maioria_2012,
    # `Maioria Parlamentar` = dummy_maioria_2016,
    #`Prof de Saude`       = prof_saude_dummy_2004,
    #`Prof de Saude`       = prof_saude_dummy_2008,
    #`Prof de Saude`       = prof_saude_dummy_2012,
    `Prof de Saude`       = prof_saude_dummy_2016,
    #`PT` = pt_dummy_2004,
    #`PDT`   = pdt_dummy_2004,
    #`PFL/DEM`   = pfl_dummy_2004,
    #`PL`   = pl_dummy_2004,
    #`PMDB`   = pmdb_dummy_2004,
    #`PPS`   = pps_dummy_2004,
    #`PSB`   = psb_dummy_2004,
    #`PSDB`   = psdb_dummy_2004,
    #`PTB`   = ptb_dummy_2004,
    #`PP`   = pp_dummy_2004,
    
    #`PT`   = pt_dummy_2008,
    #`PDT`   = pdt_dummy_2008,
    #`PFL/DEM`   = dem_dummy_2008,
    #`PMDB`   = pmdb_dummy_2008,
    #`PP`   = pp_dummy_2008,
    #`PPS`   = pps_dummy_2008,
    #`PR`   = pr_dummy_2008,
    #`PSDB`   = psdb_dummy_2008,
    #`PTB`   = ptb_dummy_2008,
    #`PSB`   = psb_dummy_2008)
    #`PDT`   = pdt_dummy_2012,
    #`PFL/DEM`   = dem_dummy_2012,
    #`PMDB`   = pmdb_dummy_2012,
    #`PP`   = pp_dummy_2012,
    #`PSD`   = psd_dummy_2012,
    #`PR`   = pr_dummy_2012,
    #`PSDB`   = psdb_dummy_2012,
    #`PTB`   = ptb_dummy_2012,
    #`PSB`   = psb_dummy_2012,
    #`PT`   = pt_dummy_2012)
    # `PDT`   = pdt_dummy_2016,
    #`PFL/DEM`   = dem_dummy_2016,
    #`PMDB`   = pmdb_dummy_2016,
    #`PP`   = pp_dummy_2016,
    #`PSD`   = psd_dummy_2016,
    #`PSDB`   = psdb_dummy_2016,
    #`PTB`   = ptb_dummy_2016,
    #`PSB`   = psb_dummy_2016,
    #`PR`   = pr_dummy_2016,
    #`PT`   = pt_dummy_2016)
    #`Esquerda com maioria` = esquerda_maioria_2004,
    #`Direita com maioria`= direita_maioria_2004,
    #`Centro com maioria`= centro_maioria_2004,
    #`PT com maioria`= pt_maioria_2004,
    #`PSDB com maioria`= psdb_maioria_2004,
    #`PFL/DEM com maioria`= pfl_maioria_2004,
    #`Esquerda com maioria`= esquerda_maioria_2012,
    #`Direita com maioria`= direita_maioria_2012,
    #`Centro com maioria`= centro_maioria_2012,
    #`PT com maioria`= pt_maioria_2012,
    #`PSDB com maioria`= psdb_maioria_2012,
    #`DEM com maioria`= dem_maioria_2012,
    `Esquerda`= esquerda_2016,
    `Direita`= direita_2016)
#`Centro`= centro_2016)
#`PT com maioria`= pt_maioria_2016,
#`PSDB com maioria`= psdb_maioria_2016,
#`PFL/DEM com maioria`= dem_maioria_2016,)

modelo16<- lm(`Ciclo 17-20` ~ + `Centro-Oeste`
              # +`Ciclo 09-12`, 
              # +`Ciclo 13-16`,
              # +`Ciclo 17-20`,
              
              +`Nordeste`
              +`Sudeste` 
              +`Sul`
              +`PIB per capita`
              +    `Log da Populacao` 
              # +`PIB per capita`,
              +`IDHM`
              +`Dependencia Financeira` 
              # +`Dependencia Financeira` ,
              # +`Dependencia Financeira`,
              # +`Dependencia Financeira`,
              +`Municipio Urbano`  
              +`Municipio Polo` 
              +`Primeiro Mandato`
              #+`Primeiro Mandato` ,
              #+`Primeiro Mandato` ,
              #+`Primeiro Mandato`,
              #+`Maioria Parlamentar` 
              #+`Maioria Parlamentar`  ,
              #+`Maioria Parlamentar`  ,
              +`Prof de Saude` 
              #+`Prof de Saude`  ,
              #+`Prof de Saude`  ,
              #+`Prof de Saude`  ,
              # +`PT`   
              #+`PDT`   
              #+`PFL/DEM`
              #+`PMDB` 
              #+`PP`   
              #+`PPS`  
              #+`PR`   
              #+`PSDB` 
              #+`PTB`  
              #+`PSB`,    
              
              
              #+`PT` 
              #+`PDT` 
              #+`PFL/DEM` 
              #+`PMDB`   
              #+`PP`   
              #+`PPS`   
              #+`PR`    
              #+`PSDB`   
              #+`PTB`    
              #+`PSB`  ,  
              
              #+`PDT`   
              #+`PFL/DEM` 
              #+`PMDB`  
              #+`PP`   
              #+`PSD`   
              #+`PR`   
              #+`PSDB`   
              #+`PTB`   
              #+`PSB`  
              #+`PT` , 
              
              
              #+`PDT`    
              #+`PFL/DEM` 
              #+`PMDB`   
              #+`PP`    
              #+`PSD`   
              #+`PSDB`    
              #+`PTB`    
              #+`PSB`   
              #+`PR`   
              #+`PT`  ,
              #+`Esquerda com maioria` ,
              #+`Direita com maioria` ,
              #+`Centro com maioria` ,
              #+`PT com maioria` ,
              #+`PSDB com maioria` ,
              #+`PFL/DEM com maioria` ,
              #+`Esquerda com maioria` ,
              #+`Direita com maioria` ,
              #+`Centro com maioria` ,
              #+`PT com maioria` ,
              #+`PSDB com maioria` ,
              #+`DEM com maioria` ,
              +`Esquerda` 
              +`Direita`,
              data= banco_modelo_16)
#+`PT com maioria` 
#+`PSDB com maioria` 
#+`PFL/DEM com maioria`

tab_model(modelo16)
tab_model(modelo13,modelo14,modelo15,modelo16)

# Testes dos modelos com Ideologia. 13 ao 16 ----
##

# Teste do presuposto da Homocedasticidade


library(gvlma) # Modelo 13
gvmodel_modelo_13 <- gvlma(modelo13)
summary(gvmodel_modelo_13)

gvmodel_modelo_14 <- gvlma(modelo14) # Modelo 14
summary(gvmodel_modelo_14)

gvmodel_modelo_15 <- gvlma(modelo15) # Modelo 15
summary(gvmodel_modelo_15)

gvmodel_modelo_16 <- gvlma(modelo16) # Modelo 16
summary(gvmodel_modelo_16)

check_heteroscedasticity(modelo13) %>% # Modelo 13
  plot(modelo13) 



check_heteroscedasticity(modelo14) %>% # Modelo 14
  plot(modelo14)                             



check_heteroscedasticity(modelo15) %>% # Modelo 15
  plot(modelo15)



check_heteroscedasticity(modelo16) %>% # Modelo 16
  plot(modelo16)



# Teste do pressuposto de normalidade 

residos_modelo_13 <- residuals(modelo13) # Modelo 13
qqnorm(residos_modelo_13)



residos_modelo_14 <- residuals(modelo14) # Modelo 14
qqnorm(residos_modelo_14)



residos_modelo_15 <- residuals(modelo15) # Modelo 15
qqnorm(residos_modelo_15)



residos_modelo_16 <- residuals(modelo16) # Modelo 16
qqnorm(residos_modelo_16)



# Teste do pressuposto de multicollinearity

if (require("see")) {
  modelo_13_multicollinearity <- check_collinearity(modelo13) # Modelo 13
  plot(modelo_13_multicollinearity)+
    coord_flip()
}       



if (require("see")) {
  modelo_14_multicollinearity <- check_collinearity(modelo14) # Modelo 14
  plot(modelo_14_multicollinearity)+
    coord_flip()
}                            



if (require("see")) {
  modelo_15_multicollinearity <- check_collinearity(modelo15) # Modelo 15
  plot(modelo_15_multicollinearity)+
    coord_flip()
}               




if (require("see")) {
  modelo_16_multicollinearity <- check_collinearity(modelo16) # Modelo 16
  plot(modelo_16_multicollinearity)+
    coord_flip()
}           

