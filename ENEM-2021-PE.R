# write.csv(dados,"dados_limpo.csv", row.names = FALSE)

setwd("C:../microdados_enem_2021/DADOS") #path ate pasta dados
getwd()
library(tidyverse)
library(tidyr)
library(data.table)
library(ggthemes)
library(dplyr)
library(ggplot2)

dados = as.data.frame(fread("MICRODADOS_ENEM_2021.csv",sep=";", header = TRUE))
# dados = dados[,c(1,3,4,6,16,23:27,32:35,45,51)]
dados_bruto = dados[,c(1,3,4,6,23:27,32:35,45,51)]
# dados_na_removido = na.omit(dados_bruto)
rm(dados)
gc(verbose=F)
dados_bruto %>% head

# Total de Inscritos
paste("N? Total de Inscritos no Enem:",round(count(dados_bruto)/1000000,1),"milh?es")

# Dados para cria??o de gr?ficos
dados_box_CN = dados_bruto %>%
  filter(NU_NOTA_CN>0 & SG_UF_PROVA == 'PE') %>%
  drop_na() %>%
  select(TP_SEXO, NU_NOTA_CN)

dados_box_MT = dados_bruto %>%
  filter(NU_NOTA_MT>0 & SG_UF_PROVA == 'PE') %>%
  select(TP_SEXO, NU_NOTA_MT)

dados_box_CH = dados_bruto %>%
  filter(NU_NOTA_CH>0 & SG_UF_PROVA == 'PE') %>%
  drop_na() %>%
  select(TP_SEXO, NU_NOTA_CH)

dados_box_LC = dados_bruto %>%
  filter(NU_NOTA_LC>0 & SG_UF_PROVA == 'PE') %>%
  drop_na() %>%
  select(TP_SEXO, NU_NOTA_LC)

dados_box_REDACAO = dados_bruto %>%
  filter(NU_NOTA_REDACAO>0 & SG_UF_PROVA == 'PE') %>%
  drop_na() %>%
  select(TP_SEXO, NU_NOTA_REDACAO)

dados_box_REDACAOMT = dados_bruto %>%
  filter(NU_NOTA_REDACAO>0 & NU_NOTA_MT>0 & SG_UF_PROVA == 'PE') %>%
  drop_na() %>%
  select(TP_SEXO, NU_NOTA_REDACAO, NU_NOTA_MT)

dados_box_REDACAOCN = dados_bruto %>%
  filter(NU_NOTA_REDACAO>0 & NU_NOTA_CN>0 & SG_UF_PROVA == 'PE') %>%
  drop_na() %>%
  select(TP_SEXO, NU_NOTA_REDACAO, NU_NOTA_CN)

dados_box_REDACAOCH = dados_bruto %>%
  filter(NU_NOTA_REDACAO>0 & NU_NOTA_CH>0 & SG_UF_PROVA == 'PE') %>%
  drop_na() %>%
  select(TP_SEXO, NU_NOTA_REDACAO, NU_NOTA_CH)

# Regress?o Linear das Notas de Matem?tica vs Reda??o
g <- ggplot(dados_box_REDACAOMT,aes(x=NU_NOTA_REDACAO,y=NU_NOTA_MT, fill=TP_SEXO)) + geom_point() + 
  scale_fill_brewer(name="Sexo", 
                    palette = "RdBu",
                    labels=c("Feminino", "Masculino")) +
  labs(title = "Notas em Reda??o vs Matem?tica") +
  stat_summary(fun=mean, geom="point", shape=21, size=2)
g
rm(dados_box_REDACAOMT)


# Regress?o Linear das Notas de Ci?ncias da Natureza vs Reda??o
g <- ggplot(dados_box_REDACAOCN,aes(x=NU_NOTA_REDACAO,y=NU_NOTA_CN, fill=TP_SEXO)) + geom_point() +
  scale_fill_brewer(name="Sexo", 
                    palette = "Spectral",
                    labels=c("Feminino", "Masculino")) +
  labs(title = "Notas em Reda??o vs Ci?ncias da Natureza") +
  stat_summary(fun=mean, geom="point", shape=23, size=2)
g
rm(dados_box_REDACAOCN)
# Regress?o Linear das Notas de Ci?ncias Humanas vs Reda??o
g <- ggplot(dados_box_REDACAOCH,aes(x=NU_NOTA_REDACAO,y=NU_NOTA_CH, fill=TP_SEXO)) + geom_point() +
  scale_fill_brewer(name="Sexo", 
                    palette = "Spectral",
                    labels=c("Feminino", "Masculino")) +
  labs(title = "Notas em Reda??o vs Ci?ncias Humanas") +
  stat_summary(fun=mean, geom="point", shape=23, size=2)
g
rm(dados_box_REDACAOCH)
#  Histograma das Notas de Reda??o
g <- ggplot(data=dados_box_REDACAO, aes(dados_box_REDACAO$NU_NOTA_REDACAO)) + 
  geom_histogram(aes(y =..density..), 
                 bins = 50, 
                 #col="blue", 
                 #fill="green", 
                 alpha = .8) + 
  # geom_density(col=1, size=0.6, alpha = .6) + 
  labs(title="Histogram das Nota de Reda??o") +
  labs(x="Notas", y="qtd")
g

rm(dados_box_REDACAO)
#  BoxPlot das Notas de Matem?tica
p <- ggplot(dados_box_MT, aes(x=TP_SEXO, y=NU_NOTA_MT, fill=TP_SEXO)) + 
  geom_boxplot() + ylab("Nota em Matem?tica") + xlab("BRASIL") +
  theme(legend.position = c(0.086, 0.92), 
        legend.background = element_rect(fill="lightgray",
                                         size=0.4, linetype="solid",
                                         colour ="darkgray")) +
  scale_fill_brewer(name="Sexo", 
                    palette = "Spectral",
                    labels=c("Feminino", "Masculino")) +
  labs(title = "Boxplot das Notas em Matem?tica") +
  stat_summary(fun=mean, geom="point", shape=23, size=1)
p

#  BoxPlot das Notas de Matem?tica
p <- ggplot(dados_box_MT, aes(x=TP_SEXO, y=NU_NOTA_MT, fill=TP_SEXO)) + 
  geom_boxplot() + ylab("Nota em Matem?tica") + xlab("BRASIL") +
  theme(legend.position = c(0.086, 0.92), 
        legend.background = element_rect(fill="lightgray",
                                         size=0.4, linetype="solid",
                                         colour ="darkgray")) +
  scale_fill_brewer(name="Sexo", 
                    palette = "Spectral",
                    labels=c("Feminino", "Masculino")) +
  labs(title = "Boxplot das Notas em Matem?tica") +
  stat_summary(fun=mean, geom="point", shape=23, size=1)
p

#  BoxPlot das Notas de Ci?ncias da Natureza por Unidade Federativa (UF)
p <- ggplot(dados_box_CN, aes(x=TP_SEXO, y=NU_NOTA_CN, fill=TP_SEXO)) + 
  geom_boxplot() + ylab("Nota em Ci?ncias da Natureza") + xlab("BRASIL") +
  theme(legend.position = c(0.086, 0.92), 
        legend.background = element_rect(fill="lightgray",
                                         size=0.4, linetype="solid",
                                         colour ="darkgray")) +
  scale_fill_brewer(name="Sexo", 
                    palette = "Spectral",
                    labels=c("Feminino", "Masculino")) +
  labs(title = "Boxplot das Nota em Ci?ncias da Natureza") +
  stat_summary(fun=mean, geom="point", shape=23, size=1)
p

#  BoxPlot das Notas de Ci?ncias Humanas
p <- ggplot(dados_box_CH, aes(x=TP_SEXO, y=NU_NOTA_CH, fill=TP_SEXO)) + 
  geom_boxplot() + ylab("Nota em Ci?ncias Humanas") + xlab("BRASIL") +
  theme(legend.position = c(0.086, 0.92), 
        legend.background = element_rect(fill="lightgray",
                                         size=0.4, linetype="solid",
                                         colour ="darkgray")) +
  scale_fill_brewer(name="Sexo", 
                    palette = "Spectral",
                    labels=c("Feminino", "Masculino")) +
  labs(title = "Boxplot das Notas em Ci?ncias Humanas por Sexo nas UFs") +
  stat_summary(fun=mean, geom="point", shape=23, size=1)
p



#  BoxPlot das Notas de Linguagens e C?digos
p <- ggplot(dados_box_LC, aes(x=TP_SEXO, y=NU_NOTA_LC, fill=TP_SEXO)) + 
  geom_boxplot() + ylab("Nota em Linguagens e C?digos") + xlab("BRASIL") +
  theme(legend.position = c(0.086, 0.92), 
        legend.background = element_rect(fill="lightgray",
                                         size=0.4, linetype="solid",
                                         colour ="darkgray")) +
  scale_fill_brewer(name="Sexo", 
                    palette = "Spectral",
                    labels=c("Feminino", "Masculino")) +
  labs(title = "Boxplot das Notas em Linguagens e C?digos por Sexo nas UFs") +
  stat_summary(fun=mean, geom="point", shape=23, size=1)
p




