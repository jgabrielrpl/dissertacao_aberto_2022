###
#  Resultados obtidos no capitulo I ----

###
#  Pacotes utilizados o capitulo I ----

library(tidyverse)
library(haven)
library(readr)
library(readxl)
library(janitor)
library(treemap)
library(readr)

###
# Carregamento dos bancos para gerar os resultados do capitulo I ----

banco_sociodemografico <- read_csv("C:/Users/Larissa/codigos_dissertacao/banco_de_dados/bancos_base/banco_sociodemografico.csv", 
                                   locale = locale(encoding = "ISO-8859-1"))

library(haven)
recursos_financiamento <- read_sav("C:/Users/Larissa/codigos_dissertacao/banco_de_dados/bancos_base/banco.codigos.agrupados.RECURSOS.FINANCIAMENTO.sav")

###
# Banco que preciso para fazer as visualizacoes graficas do capitulo I ----

bd <- banco_sociodemografico
d <- recursos_financiamento
q <- d %>% select(IBGE7, PP_I_2010)

###
#  Juntei os bancos necessarios para fazer o capitulo I ----

f <- left_join(bd, q, by= c('ibge7'='IBGE7'))

###
# R.Distribuicao percentual dos municipios por regiao ----

r <- f%>%group_by(regiao) %>% summarise(casos=n())

r <- r%>%filter( casos > "1")

rr <- r %>% mutate(per = casos/sum(casos)*100)  

rr$`Percentual de municipios por Regiao` <- rr$per

###
# R.Tmap da Distribuicao percentual dos municipios por regiao ----

library(tidyverse)
install.packages('treemap')
library(treemap)

treemap(rr,
        index="regiao",
        vSize="Percentual de municipios por regiao",
        type="index",
        palette="-RdGy")

treemap <- treemap(rr,
        index="regiao",
        vSize="Percentual de municipios por regiao",
        type="index",
        palette="-RdGy")

ggsave("C:/Users/Larissa/codigos_dissertacao/resultados/imagem_capitulo_I/treemap.png", width = 20, height = 15, units = "cm")

###
# R.Tabela com a Distribuicao percentual dos municipios por categoria ----

w <- f%>%group_by(pop_cat_2012) %>% summarise(n=n(),casos=sum(pop_2012_siops, na.rm = T))

ww <- w %>% mutate(per = casos/sum(casos)*100)  

###
# R.Tabela com a distribuicao percentual dos municipios por regiao ---

dd <- f%>%group_by(regiao) %>% summarise(n=n(),casos=sum(pop_2012_siops, na.rm = T))

ddd <- dd %>% mutate(per = casos/sum(casos)*100)  

###
# R.Dispersao por percentual de receita propria por categoria  ----

m1 <- f %>% mutate(cat_12_n = case_when(pop_cat_2012 == "ate 5 mil habitantes" ~ 1,
                                        pop_cat_2012 == "entre 5 e 10 mil habitantes" ~ 2,
                                        pop_cat_2012 == "entre 10 e 20 mil habitantes" ~ 3,
                                        pop_cat_2012 == "acima de 500 mil  habitantes" ~ 7,
                                        pop_cat_2012 ==  "entre 100 e 500 mil habitantes" ~ 6,
                                        pop_cat_2012 ==  "entre 20 e 50 mil habitantes"  ~  4,
                                        pop_cat_2012 ==  "entre 50 e 100 mil habitantes"  ~ 5))       

m1  %>%
  filter(!(pop_cat_2012 %in% "")) %>%
  filter(!is.na(pop_cat_2012)) %>% 
  ggplot() +
  aes(x=fct_reorder(pop_cat_2012, cat_12_n), y = PP_I_2010) +
  labs(y = "Percentual de receita própria", x = "Tamanho Populacional") +
  geom_jitter(adjust = 1L, scale = "area", fill = "#0c4c8a", alpha= 0.7) +
  labs(y = "Percentual de receita própria", x = "Tamanho Populacional")+
  theme_minimal()

dispersao_reeita_propria_categoria <- m1  %>%
  filter(!(pop_cat_2012 %in% "")) %>%
  filter(!is.na(pop_cat_2012)) %>% 
  ggplot() +
  aes(x=fct_reorder(pop_cat_2012, cat_12_n), y = PP_I_2010) +
  labs(y = "Percentual de receita própria", x = "Tamanho Populacional") +
  geom_jitter(adjust = 1L, scale = "area", fill = "#0c4c8a", alpha= 0.7) +
  labs(y = "Percentual de receita própria", x = "Tamanho Populacional")+
  theme_minimal()

ggsave("C:/Users/Larissa/codigos_dissertacao/resultados/imagem_capitulo_I/dispersao_reeita_propria_categoria.png", width = 20, height = 15, units = "cm")

###
# R.Dispersao por percentual de receita propria por regiao  ---- 

m1  %>%
  filter(!(regiao %in% "")) %>%
  filter(!is.na(regiao)) %>% 
  ggplot() +
  aes(x=regiao, y = PP_I_2010) +
  labs(y = "Percentual de receita propria", x = "Regiao") +
  geom_jitter(adjust = 1L, scale = "area", fill = "#0c4c8a", alpha= 0.7) +
  labs(y = "Percentual de receita própria", x = "Regiao")+
  theme_minimal()

dispersao_reeita_propria_regiao <- m1  %>%
  filter(!(regiao %in% "")) %>%
  filter(!is.na(regiao)) %>% 
  ggplot() +
  aes(x=regiao, y = PP_I_2010) +
  labs(y = "Percentual de receita propria", x = "Regiao") +
  geom_jitter(adjust = 1L, scale = "area", fill = "#0c4c8a", alpha= 0.7) +
  labs(y = "Percentual de receita propria", x = "Regiao")+
  theme_minimal()

ggsave("C:/Users/Larissa/codigos_dissertacao/resultados/imagem_capitulo_I/dispersao_reeita_propria_regiao.png", width = 20, height = 15, units = "cm")

###
# R.Dispersao entre IDHM e populacao ----

f %>%
  filter(!(regiao %in% "") | is.na(regiao)) %>%
  filter(!is.na(pop_2010_siops)) %>%
  ggplot() +
  aes(x = log(pop_2010_siops), y = idhm_10, colour = regiao) +
  geom_point() +
  scale_color_viridis_d(option = "viridis") +
  theme_minimal() +
  labs(x = "Log da populacao em 2010", y = "IDHM") +
  theme_classic()+
  geom_smooth(formula= y ~ x, colour= 'black', method='lm', span = 0.8,se=F)

dispersao_idhm_pop <- f %>%
  filter(!(regiao %in% "") | is.na(regiao)) %>%
  filter(!is.na(pop_2010_siops)) %>%
  ggplot() +
  aes(x = log(pop_2010_siops), y = idhm_10, colour = regiao) +
  geom_point() +
  scale_color_viridis_d(option = "viridis") +
  theme_minimal() +
  labs(x = "Log da populacao em 2010", y = "IDHM") +
  theme_classic()+
  geom_smooth(formula= y ~ x, colour= 'black', method='lm', span = 0.8,se=F)

  ggsave("C:/Users/Larissa/codigos_dissertacao/resultados/imagem_capitulo_I/dispersao_idhm_pop.png", width = 20, height = 15, units = "cm")

###
#  Resultados Obtidos no capitulo II ----

###
# Pacotes Utilizados no capitulo II -----

library(tidyverse)

###
# Banco do capitulo II -----

library(readxl)
partidos <- read_excel("C:/Users/Larissa/codigos_dissertacao/resultados/imagem_capitulo_II/partidos.xlsx")

# R.Grafico dos partidos -----
###

partido <- ggplot(partidos) +
  aes(x = reorder(Década, o), weight = `Número total de Partidos`) +
  geom_bar(fill = "#8E9094") +
  theme_bw() +
  #theme_minimal()+
  labs(x = "Década", y = "Número total de partidos Politicos")


ggsave("C:/Users/Larissa/codigos_dissertacao/resultados/imagem_capitulo_II/partido.png", width = 20, height = 15, units = "cm")

###
#  Resultados Obtidos no capitulo III ----

###
# Pacotes Utilizados no capitulo III -----

load("codigos_dissertacao/banco_de_dados/banco_dissertacao_pronto.Rda")

# Numero de Cadeiras conquistado por partido em 2004, 2008, 2012, 2020 ----
###

table(banco_dissertacao_pronto$partidos_cat_2004)
table(banco_dissertacao_pronto$partidos_cat_2008)
table(banco_dissertacao_pronto$partidos_cat_12)
table(banco_dissertacao_pronto$partidos_cat_2016)

# Numero de Partido por partido em 2004, 2008, 2012, 2020 ----
###

table(banco_dissertacao_pronto$class_ideo_cat_2004)
table(banco_dissertacao_pronto$class_ideo_cat_2008)
table(banco_dissertacao_pronto$class_ideo_cat_2012)
table(banco_dissertacao_pronto$class_ideo_cat_2016)

# Numero de Partido por ideologia em 2004

parti_ideo_2004 <- table(banco_dissertacao_pronto$partido_eleito_2004, banco_dissertacao_pronto$class_ideo_cat_2004)
parti_ideo_2004 <- as.data.frame(parti_ideo_2004)
parti_ideo_2004 <- parti_ideo_2004 %>% 
pivot_wider(names_from = Var2, values_from = Freq)
parti_ideo_2004 <- parti_ideo_2004 %>%rename(`Partido Eleito em 2004` = Var1)

# Numero de Partido por ideologia em 2008

parti_ideo_2008 <- table(banco_dissertacao_pronto$partido_eleito_2008, banco_dissertacao_pronto$class_ideo_cat_2008)
parti_ideo_2008 <- as.data.frame(parti_ideo_2008)
parti_ideo_2008 <- parti_ideo_2008 %>% 
pivot_wider(names_from = Var2, values_from = Freq)
parti_ideo_2008 <- parti_ideo_2008 %>%rename(`Partido Eleito em 2008` = Var1)

# Numero de Partido por ideologia em 2012

parti_ideo_2012 <- table(banco_dissertacao_pronto$partido_eleito_2012, banco_dissertacao_pronto$class_ideo_cat_2012)
parti_ideo_2012 <- as.data.frame(parti_ideo_2012)
parti_ideo_2012 <- parti_ideo_2012 %>% 
  pivot_wider(names_from = Var2, values_from = Freq)
parti_ideo_2012 <- parti_ideo_2012 %>%rename(`Partido Eleito em 2012` = Var1)

# Numero de Partido por ideologia em 2016

parti_ideo_2016 <- table(banco_dissertacao_pronto$partido_eleito_2016, banco_dissertacao_pronto$class_ideo_cat_2016)
parti_ideo_2016 <- as.data.frame(parti_ideo_2016)
parti_ideo_2016 <- parti_ideo_2016 %>% 
  pivot_wider(names_from = Var2, values_from = Freq)
parti_ideo_2016 <- parti_ideo_2016 %>%rename(`Partido Eleito em 2016` = Var1)

# salvar bancos 

write.csv(parti_ideo_2004, "C:/Users/Larissa/codigos_dissertacao/resultados/imagem_capitulo_III/parti_ideo_2004.csv") 
write.csv(parti_ideo_2008, "C:/Users/Larissa/codigos_dissertacao/resultados/imagem_capitulo_III/parti_ideo_2008.csv") 
write.csv(parti_ideo_2012, "C:/Users/Larissa/codigos_dissertacao/resultados/imagem_capitulo_III/parti_ideo_2012.csv") 
write.csv(parti_ideo_2016, "C:/Users/Larissa/codigos_dissertacao/resultados/imagem_capitulo_III/parti_ideo_2016.csv") 

# Pronto est? numa tabeal do excell

# R.Histograma. Variavel dependente segundo cada legislatura ----
###

histograma <- banco_dissertacao_pronto %>% select (gasto_medio_05_08, gasto_medio_09_12, gasto_medio_13_16,gasto_medio_17_20) %>% 
              gather(Ciclos, Gasto_Medio_Saude) %>% 
              mutate (Ciclos = case_when(Ciclos == 'gasto_medio_05_08' ~ 'Ciclo_2005_2008',
                                         Ciclos == 'gasto_medio_09_12' ~ 'Ciclo_2009_2012',
                                         Ciclos == 'gasto_medio_13_16' ~ 'Ciclo_2013_2016',
                                         Ciclos == 'gasto_medio_17_20' ~ 'Ciclo_2017_2020'))
ggplot(histograma) +
  aes(x = Gasto_Medio_Saude) +
  geom_histogram(bins = 30L, fill = "#112446") +
  theme_bw() +
  facet_wrap(vars(Ciclos)) + labs(x = "Media do Gasto Proprio per capita em Saude", y = "Valores ")

# salvar

histograma <- ggplot(histograma) +
  aes(x = Gasto_Medio_Saude) +
  geom_histogram(bins = 30L, fill = "#112446") +
  theme_bw() +
  facet_wrap(vars(Ciclos)) + labs(x = "Media do Gasto Proprio per capita em Saude", y = "Valores ") 

  ggsave("C:/Users/Larissa/codigos_dissertacao/resultados/imagem_capitulo_III/histograma.png", width = 20, height = 15, units = "cm")

# R.Boxplot. Gasto Proprio com saude ao longo dos 15 anos de trabalho   ----
### 

box_plot <- banco_dissertacao_pronto %>% select (saude_prop_hab_real_2005,saude_prop_hab_real_2006,saude_prop_hab_real_2007,saude_prop_hab_real_2008,
                                                 saude_prop_hab_real_2009,saude_prop_hab_real_2010,saude_prop_hab_real_2011,saude_prop_hab_real_2012,
                                                 saude_prop_hab_real_2013,saude_prop_hab_real_2014,saude_prop_hab_real_2015,saude_prop_hab_real_2016,
                                                 saude_prop_hab_real_2017,saude_prop_hab_real_2018,saude_prop_hab_real_2019,saude_prop_hab_real_2020) %>% 
  gather(Anos, Gasto_Saude) %>% 
  mutate (Anos = case_when(Anos == 'saude_prop_hab_real_2005' ~ '2005',
                         Anos ==   'saude_prop_hab_real_2006' ~ '2006',
                         Anos == 'saude_prop_hab_real_2007' ~ '2007',
                         Anos == 'saude_prop_hab_real_2008' ~ '2008',
                         Anos == 'saude_prop_hab_real_2009' ~ '2009',
                         Anos == 'saude_prop_hab_real_2010' ~ '2010',
                         Anos == 'saude_prop_hab_real_2011' ~ '2011',
                         Anos == 'saude_prop_hab_real_2012' ~ '2012',
                         Anos == 'saude_prop_hab_real_2013' ~ '2013',
                         Anos == 'saude_prop_hab_real_2014' ~ '2014',
                         Anos == 'saude_prop_hab_real_2015' ~ '2015',
                         Anos == 'saude_prop_hab_real_2016' ~ '2016',
                         Anos == 'saude_prop_hab_real_2017' ~ '2017',
                         Anos == 'saude_prop_hab_real_2018' ~ '2018',
                         Anos == 'saude_prop_hab_real_2019' ~ '2019',
                         Anos == 'saude_prop_hab_real_2020' ~ '2020')) 
  box_plot %>% 
  filter(Gasto_Saude <= 2000) %>%   
  ggplot() +
  aes(x = Anos, y = Gasto_Saude) +
  geom_boxplot() +
  theme_bw() +
  stat_summary(fun=mean, geom="point", shape=20, size=2, color="blue", fill="blue")

# Salvar 
  
    boxplot_saude_quinze_anos <-  box_plot %>% 
    filter(Gasto_Saude <= 2000) %>%   
    ggplot() +
    aes(x = Anos, y = Gasto_Saude) +
    geom_boxplot() +
    theme_bw() +
    stat_summary(fun=mean, geom="point", shape=20, size=2, color="blue", fill="blue")
  
  ggsave("C:/Users/Larissa/codigos_dissertacao/resultados/imagem_capitulo_III/boxplot_saude_quinze_anos.png", width = 20, height = 15, units = "cm")
  
# PAREI AQUI R.Boxplot. Variavel dependente, regiao por ciclo   ----
###

vari_regi <- banco_dissertacao_pronto %>% select (gasto_medio_05_08, gasto_medio_09_12, gasto_medio_13_16, gasto_medio_17_20, regiao) %>% 
             gather(Ciclos, Media_Gasto_Saude, - regiao)   %>% 
  mutate (Ciclos = case_when(Ciclos == 'gasto_medio_05_08' ~ 'Ciclo 2005 a 2008',
                             Ciclos == 'gasto_medio_09_12' ~ 'Ciclo 2009 a 2012',
                             Ciclos == 'gasto_medio_13_16' ~ 'Ciclo 2013 a 2016',
                             Ciclos == 'gasto_medio_17_20' ~ 'Ciclo 2017 a 2020'))

  vari_regi %>%
  filter(!(regiao %in% "")) %>%
  ggplot() +
  aes(x = regiao, y = Media_Gasto_Saude) +
  geom_boxplot() +
  facet_wrap(vars(Ciclos))+ 
  theme_bw() +
  stat_summary(fun=mean, geom="point", shape=20, size=2, color="blue", fill="blue") +
  labs(x = "Regiao", y = "Media do Gasto Proprio per capita em Saude")+
  ylim(0,2000) # Inserido outlier 
  
  
  vari_regi %>%
  filter(!(regiao %in% ""), Media_Gasto_Saude <= 2000) %>%  # Sem outlier
  ggplot() +
  aes(x = regiao, y = Media_Gasto_Saude) +
  geom_boxplot() +
  facet_wrap(vars(Ciclos))+ 
  theme_bw() +
  stat_summary(fun=mean, geom="point", shape=20, size=2, color="blue", fill="blue") +
  labs(x = "Regiao", y = "Media do Gasto Proprio em Saude")

# R.Boxplot. Variavel dependente, tamanh por ciclo   ----
###
    
  banco_dissertacao_pronto$pop_cat_2012
  vari_pop <- banco_dissertacao_pronto %>% select (gasto_medio_05_08, gasto_medio_09_12, gasto_medio_13_16, gasto_medio_17_20, pop_cat_2012) %>% 
  gather(Ciclos, Media_Gasto_Saude, - pop_cat_2012)   %>% 
  mutate (Ciclos = case_when(Ciclos == 'gasto_medio_05_08' ~ 'Ciclo 2005 a 2008',
          Ciclos == 'gasto_medio_09_12' ~ 'Ciclo 2009 a 2012',
          Ciclos == 'gasto_medio_13_16' ~ 'Ciclo 2013 a 2016',
          Ciclos == 'gasto_medio_17_20' ~ 'Ciclo 2017 a 2020'), 
          cat_12_n = case_when(pop_cat_2012   == "até 5 mil habitantes" ~ 1,
          pop_cat_2012 == "entre 5 e 10 mil habitantes" ~ 2,
          pop_cat_2012 == "entre 10 e 20 mil habitantes" ~ 3,
          pop_cat_2012 == "acima de 500 mil  habitantes" ~ 7,
          pop_cat_2012 ==  "entre 100 e 500 mil habitantes" ~ 6,
          pop_cat_2012 ==  "entre 20 e 50 mil habitantes"  ~  4,
          pop_cat_2012 ==  "entre 50 e 100 mil habitantes"  ~ 5))   
  
  vari_pop %>%
  filter(!(pop_cat_2012 %in% ""), Media_Gasto_Saude <= 1500) %>% # outliner 
  filter(!is.na(cat_12_n)) %>%
  ggplot() +
  aes(x = fct_reorder(pop_cat_2012, cat_12_n), y = Media_Gasto_Saude) +
  geom_boxplot() +
  theme_minimal() +
  facet_wrap(vars(Ciclos))+ 
  theme_bw() +
  stat_summary(fun=mean, geom="point", shape=20, size=2, color="blue", fill="blue") +
  labs(x = "Porte Populacional", y = "Media do Gasto Proprio per capita em Saude") +
  coord_flip()  
  
# R.Boxplot. Variavel dependente, partido por ciclo   ----
###

  long_saude_partido <- banco_dissertacao_pronto %>% 
    select(ibge7, 91:94, partidos_cat_12, partidos_cat_2004, partidos_cat_2008, partidos_cat_2016) %>% 
    pivot_longer(names_to = "ciclo", cols = starts_with("gasto"), values_to = "gasto") %>% 
    mutate(partido = ifelse(ciclo == "gasto_medio_05_08", partidos_cat_2004,
                            ifelse(ciclo == "gasto_medio_09_12", partidos_cat_2008,
                                   ifelse(ciclo == "gasto_medio_13_16", partidos_cat_12,
                                          ifelse(ciclo == "gasto_medio_17_20", partidos_cat_2016, "NA")))),
           ciclo = case_when(ciclo == 'gasto_medio_05_08' ~ 'Ciclo 2005 a 2008',
                             ciclo == 'gasto_medio_09_12' ~ 'Ciclo 2009 a 2012',
                             ciclo == 'gasto_medio_13_16' ~ 'Ciclo 2013 a 2016',
                             ciclo == 'gasto_medio_17_20' ~ 'Ciclo 2017 a 2020'))
  
    long_saude_partido <- long_saude_partido %>% 
    filter(!is.na(gasto)) %>% 
    group_by(ibge7,ciclo,partido) %>% 
    mutate(media_partido = mean(gasto,na.rm= TRUE))
  
    long_saude_partido %>%
    filter(!(partido%in% ""),gasto <= 2000) %>% # outliner 
    filter(!is.na(partido)) %>%
    ggplot() +
    aes(x = fct_reorder(partido, media_partido), y = gasto) +
    geom_boxplot() +
    theme_minimal() +
    facet_wrap(vars(ciclo))+ 
    theme_bw() +
    stat_summary(fun=mean, geom="point", shape=20, size=2, color="blue", fill="blue") +
    labs(x = "Partido", y = "Media do Gasto Proprio per capita em Saude") +
    coord_flip() 
  
# R.Boxplot. Variavel dependente, ideologia por ciclo   ----
###
      
    long_saude_ideologia <- banco_dissertacao_pronto %>% 
      select(ibge7, gasto_medio_05_08, gasto_medio_09_12, gasto_medio_13_16, gasto_medio_17_20, class_ideo_cat_2004, class_ideo_cat_2008, class_ideo_cat_2012, class_ideo_cat_2016) %>% 
      pivot_longer(names_to = "ciclo", cols = starts_with("gasto"), values_to = "gasto") %>% 
      mutate(ideologia = ifelse(ciclo == "gasto_medio_05_08", class_ideo_cat_2004,
                              ifelse(ciclo == "gasto_medio_09_12", class_ideo_cat_2008,
                                     ifelse(ciclo == "gasto_medio_13_16", class_ideo_cat_2012,
                                            ifelse(ciclo == "gasto_medio_17_20", class_ideo_cat_2016, "NA")))),
             ciclo = case_when(ciclo == 'gasto_medio_05_08' ~ 'Ciclo 2005 a 2008',
                               ciclo == 'gasto_medio_09_12' ~ 'Ciclo 2009 a 2012',
                               ciclo == 'gasto_medio_13_16' ~ 'Ciclo 2013 a 2016',
                               ciclo == 'gasto_medio_17_20' ~ 'Ciclo 2017 a 2020'),
             ideologia_d_e_c= case_when(ideologia == "Direita" ~  "Direita", 
                                                          ideologia == "Extrema-direita" ~  "Direita",
                                                          ideologia == "Centro-direita" ~  "Direita",
                                                          ideologia == "Centro" ~  "Centro",
                                                          ideologia == "Esquerda" ~  "Esquerda",
                                                          ideologia == "Centro-esquerda" ~  "Esquerda",
                                                          ideologia == "Extrema-esquerda" ~  "Esquerda"))
    
    long_saude_ideologia <- long_saude_ideologia %>% 
      filter(!is.na(gasto)) %>% 
      group_by(ibge7,ciclo,ideologia) %>% 
      mutate(media_ideologia = mean(gasto,na.rm= TRUE))
    
    long_saude_ideologia %>%
      filter(!(ideologia_d_e_c%in% ""),gasto <= 2000) %>% # outliner 
      filter(!is.na(ideologia_d_e_c)) %>%
      ggplot() +
      aes(x = fct_reorder(ideologia_d_e_c, media_ideologia), y = gasto) +
      geom_boxplot() +
      theme_minimal() +
      facet_wrap(vars(ciclo))+ 
      theme_bw() +
      stat_summary(fun=mean, geom="point", shape=20, size=2, color="blue", fill="blue") +
      labs(x = "ideologia do Partido", y = "Media do Gasto Proprio per capita em Saude") 
      
    
# R.Boxplot.Variavel Dependente no ultimo ciclo por partido segundo a regiao----
##
    
    long_saude_ideologia_regiao <- banco_dissertacao_pronto %>% 
      select(ibge7, regiao, pop_2012_siops, gasto_medio_17_20,class_ideo_cat_2016) %>% 
      pivot_longer(names_to = "ciclo", cols = starts_with("gasto"), values_to = "gasto") %>% 
      mutate(ideologia = ifelse(ciclo == "gasto_medio_17_20", class_ideo_cat_2016, "NA"),
             ciclo = case_when(ciclo == 'gasto_medio_17_20' ~ 'Ciclo 2017 a 2020'),
             ideologia_d_e_c= case_when(ideologia == "Direita" ~  "Direita", 
                                                ideologia == "Extrema-direita" ~  "Direita",
                                                ideologia == "Centro-direita" ~  "Direita",
                                                ideologia == "Centro" ~  "Centro",
                                                ideologia == "Esquerda" ~  "Esquerda",
                                                ideologia == "Centro-esquerda" ~  "Esquerda",
                                                ideologia == "Extrema-esquerda" ~  "Esquerda"))

    long_saude_ideologia_regiao <- long_saude_ideologia_regiao %>% 
      filter(!is.na(gasto)) %>% 
      group_by(ibge7,regiao,ideologia) %>% 
      mutate(media_ideologia = mean(gasto,na.rm= TRUE))
    
    long_saude_ideologia_regiao %>%
      filter(!(ideologia_d_e_c%in% ""),gasto <= 2000) %>% # outliner 
      filter(!is.na(ideologia_d_e_c)) %>%
      ggplot() +
      aes(x = fct_reorder(ideologia_d_e_c, media_ideologia), y = gasto) +
      geom_boxplot() +
      theme_minimal() +
      facet_wrap(vars(regiao))+ 
      theme_bw() +
      stat_summary(fun=mean, geom="point", shape=20, size=2, color="blue", fill="blue") +
      labs(x = "ideologia do Partido", y = "Media do Gasto Proprio per capita em Saude") 
  
# R.Boxplot. Variavel Dependente por primeiro mandato segundo o ciclo ----
## 

    long_saude_primeiro_mandato <- banco_dissertacao_pronto %>% 
      select(ibge7, gasto_medio_05_08, gasto_medio_09_12, gasto_medio_13_16, gasto_medio_17_20, pri_mandato_04, pri_mandato_08, pri_mandato_12, pri_mandato_16) %>% 
      pivot_longer(names_to = "ciclo", cols = starts_with("gasto"), values_to = "gasto") %>% 
      mutate(primeiro_mandato = ifelse(ciclo == "gasto_medio_05_08", pri_mandato_04,
                                ifelse(ciclo == "gasto_medio_09_12", pri_mandato_08,
                                       ifelse(ciclo == "gasto_medio_13_16", pri_mandato_12,
                                              ifelse(ciclo == "gasto_medio_17_20", pri_mandato_16, "NA")))),
             ciclo = case_when(ciclo == 'gasto_medio_05_08' ~ 'Ciclo 2005 a 2008',
                               ciclo == 'gasto_medio_09_12' ~ 'Ciclo 2009 a 2012',
                               ciclo == 'gasto_medio_13_16' ~ 'Ciclo 2013 a 2016',
                               ciclo == 'gasto_medio_17_20' ~ 'Ciclo 2017 a 2020'),
             primeiro_mandato_e= case_when(primeiro_mandato == 1 ~  "Primeiro Mandato", 
                                           primeiro_mandato == 0 ~  "Segundo Mandato"))
 
    long_saude_primeiro_mandato <-long_saude_primeiro_mandato %>% 
      filter(!is.na(gasto)) %>% 
      group_by(ibge7,ciclo,primeiro_mandato) %>% 
      mutate(media_primeiro_mandato = mean(gasto,na.rm= TRUE))
    
    long_saude_primeiro_mandato %>%
      filter(!(primeiro_mandato_e%in% ""),gasto <= 2000) %>% # outliner 
      filter(!is.na(primeiro_mandato_e)) %>%
      ggplot() +
      aes(x = fct_reorder(primeiro_mandato_e, media_primeiro_mandato), y = gasto) +
      geom_boxplot() +
      theme_minimal() +
      facet_wrap(vars(ciclo))+ 
      theme_bw() +
      stat_summary(fun=mean, geom="point", shape=20, size=2, color="blue", fill="blue") +
      labs(x = "Situa??o do Prefeito", y = "Media do Gasto Proprio per capita em Saude") 
    
# R.Boxplot. Gasto no ?litmo Ano de Governo por primeiro mandato segundo o ciclo ----
## 

    long_saude_primeiro_mandato_ano <- banco_dissertacao_pronto %>% 
      select(ibge7, saude_prop_hab_real_2008, saude_prop_hab_real_2012, saude_prop_hab_real_2016, saude_prop_hab_real_2020, pri_mandato_04, pri_mandato_08, pri_mandato_12, pri_mandato_16) %>% 
      pivot_longer(names_to = "Ano_Eleitoral", cols = starts_with("saude"), values_to = "gasto") %>% 
      mutate(primeiro_mandato = ifelse(Ano_Eleitoral == "saude_prop_hab_real_2008", pri_mandato_04,
                                       ifelse(Ano_Eleitoral == "saude_prop_hab_real_2012", pri_mandato_08,
                                              ifelse(Ano_Eleitoral == "saude_prop_hab_real_2016", pri_mandato_12,
                                                     ifelse(Ano_Eleitoral == "saude_prop_hab_real_2020", pri_mandato_16, "NA")))),
             Ano_Eleitoral = case_when(Ano_Eleitoral == 'saude_prop_hab_real_2008' ~ '2008',
                               Ano_Eleitoral == 'saude_prop_hab_real_2012' ~ '2012',
                               Ano_Eleitoral == 'saude_prop_hab_real_2016' ~ '2016',
                               Ano_Eleitoral == 'saude_prop_hab_real_2020' ~ '2020'),
             primeiro_mandato_e= case_when(primeiro_mandato == 1 ~  "Primeiro Mandato", 
                                           primeiro_mandato == 0 ~  "Segundo Mandato"))
    
    long_saude_primeiro_mandato_ano <-long_saude_primeiro_mandato_ano %>% 
      filter(!is.na(gasto)) %>% 
      group_by(ibge7,Ano_Eleitoral,primeiro_mandato) %>% 
      mutate(media_primeiro_mandato = mean(gasto,na.rm= TRUE))
    
    long_saude_primeiro_mandato_ano %>%
      filter(!(primeiro_mandato_e%in% ""),gasto <= 2000) %>% # outliner 
      filter(!is.na(primeiro_mandato_e)) %>%
      ggplot() +
      aes(x = fct_reorder(primeiro_mandato_e, media_primeiro_mandato), y = gasto) +
      geom_boxplot() +
      theme_minimal() +
      facet_wrap(vars(Ano_Eleitoral))+ 
      theme_bw() +
      stat_summary(fun=mean, geom="point", shape=20, size=2, color="blue", fill="blue") +
      labs(x = "Situa??o do Prefeito", y = "Gasto medio per capita em Saude") 
    
# R.Boxplot. Variavel Dependente por profissional de saude segundo o ciclo ----
## 

    long_prof_saude <- banco_dissertacao_pronto %>% 
      select(ibge7, gasto_medio_05_08, gasto_medio_09_12, gasto_medio_13_16, gasto_medio_17_20, prof_saude_dummy_2004, prof_saude_dummy_2008, prof_saude_dummy_2012, prof_saude_dummy_2016) %>% 
      pivot_longer(names_to = "ciclo", cols = starts_with("gasto"), values_to = "gasto") %>% 
      mutate(prof_saude = ifelse(ciclo == "gasto_medio_05_08", prof_saude_dummy_2004,
                                       ifelse(ciclo == "gasto_medio_09_12", prof_saude_dummy_2008,
                                              ifelse(ciclo == "gasto_medio_13_16", prof_saude_dummy_2012,
                                                     ifelse(ciclo == "gasto_medio_17_20", prof_saude_dummy_2016, "NA")))),
             ciclo = case_when(ciclo == 'gasto_medio_05_08' ~ 'Ciclo 2005 a 2008',
                               ciclo == 'gasto_medio_09_12' ~ 'Ciclo 2009 a 2012',
                               ciclo == 'gasto_medio_13_16' ~ 'Ciclo 2013 a 2016',
                               ciclo == 'gasto_medio_17_20' ~ 'Ciclo 2017 a 2020'),
            prof_saude_e= case_when(prof_saude == 1 ~  "Profissional de Saude", 
                                    prof_saude == 0 ~  "Demais profissionais"))
    
    long_prof_saude <-long_prof_saude %>% 
      filter(!is.na(gasto)) %>% 
      group_by(ibge7,ciclo,prof_saude) %>% 
      mutate(media_prof_saude = mean(gasto,na.rm= TRUE))
    
    long_prof_saude %>%
      filter(!(prof_saude_e%in% ""),gasto <= 2000) %>% # outlier 
      filter(!is.na(prof_saude_e)) %>%
      ggplot() +
      aes(x = fct_reorder(prof_saude_e, media_prof_saude), y = gasto) +
      geom_boxplot() +
      theme_minimal() +
      facet_wrap(vars(ciclo))+ 
      theme_bw() +
      stat_summary(fun=mean, geom="point", shape=20, size=2, color="blue", fill="blue") +
      labs(x = "Profiss?o do Prefeito", y = "Media do Gasto Proprio per capita em Saude") 

# R.Boxplot. Variavel Dependente por maioria parlamentar segundo o ciclo ----
## 
   
       maioria <- banco_dissertacao_pronto %>% 
       select(ibge7, gasto_medio_05_08, gasto_medio_13_16, gasto_medio_17_20, dummy_maioria_2004,dummy_maioria_2012,dummy_maioria_2016) %>% 
       pivot_longer(names_to = "ciclo", cols = starts_with("gasto"), values_to = "gasto") %>% 
       mutate(maioria = ifelse(ciclo == "gasto_medio_05_08", dummy_maioria_2004,
                 ifelse(ciclo == "gasto_medio_13_16", dummy_maioria_2012,
                 ifelse(ciclo == "gasto_medio_17_20", dummy_maioria_2016, "NA"))),
       ciclo = case_when(ciclo == 'gasto_medio_05_08' ~ 'Ciclo 2005 a 2008',
                         ciclo == 'gasto_medio_13_16' ~ 'Ciclo 2013 a 2016',
                         ciclo == 'gasto_medio_17_20' ~ 'Ciclo 2017 a 2020'),
       maioria_e = case_when(maioria == 1 ~  "Com Maioria", 
                             maioria == 0 ~  "Sem Tem"))
    
       maioria <- maioria %>% 
       filter(!is.na(gasto)) %>% 
       group_by(ibge7,ciclo,maioria) %>% 
       mutate(media_maioria = mean(gasto,na.rm= TRUE))
    
       maioria %>%
       filter(!(maioria_e%in% ""),gasto <= 2000) %>% # outlier 
       filter(!is.na(maioria_e)) %>%
       ggplot() +
       aes(x = fct_reorder(maioria_e, media_maioria), y = gasto) +
       geom_boxplot() +
       theme_minimal() +
       facet_wrap(vars(ciclo))+ 
       theme_bw() +
       stat_summary(fun=mean, geom="point", shape=20, size=2, color="blue", fill="blue") +
       labs(x = "Prefeito com Maioria e sem", y = "Media do Gasto Proprio per capita em Saude") 
    