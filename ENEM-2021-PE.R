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
paste("Nº Total de Inscritos no Enem:",round(count(dados_bruto)/1000000,1),"milhões")

# Dados para criação de gráficos
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

# Regressão Linear das Notas de Matemática vs Redação
g <- ggplot(dados_box_REDACAOMT,aes(x=NU_NOTA_REDACAO,y=NU_NOTA_MT, fill=TP_SEXO)) + geom_point() + 
  scale_fill_brewer(name="Sexo", 
                    palette = "RdBu",
                    labels=c("Feminino", "Masculino")) +
  labs(title = "Notas em Redação vs Matemática") +
  stat_summary(fun=mean, geom="point", shape=21, size=2)
g
rm(dados_box_REDACAOMT)


# Regressão Linear das Notas de Ciências da Natureza vs Redação
g <- ggplot(dados_box_REDACAOCN,aes(x=NU_NOTA_REDACAO,y=NU_NOTA_CN, fill=TP_SEXO)) + geom_point() +
  scale_fill_brewer(name="Sexo", 
                    palette = "Spectral",
                    labels=c("Feminino", "Masculino")) +
  labs(title = "Notas em Redação vs Ciências da Natureza") +
  stat_summary(fun=mean, geom="point", shape=23, size=2)
g
rm(dados_box_REDACAOCN)
# Regressão Linear das Notas de Ciências Humanas vs Redação
g <- ggplot(dados_box_REDACAOCH,aes(x=NU_NOTA_REDACAO,y=NU_NOTA_CH, fill=TP_SEXO)) + geom_point() +
  scale_fill_brewer(name="Sexo", 
                    palette = "Spectral",
                    labels=c("Feminino", "Masculino")) +
  labs(title = "Notas em Redação vs Ciências Humanas") +
  stat_summary(fun=mean, geom="point", shape=23, size=2)
g
rm(dados_box_REDACAOCH)
#  Histograma das Notas de Redação
g <- ggplot(data=dados_box_REDACAO, aes(dados_box_REDACAO$NU_NOTA_REDACAO)) + 
  geom_histogram(aes(y =..density..), 
                 bins = 50, 
                 #col="blue", 
                 #fill="green", 
                 alpha = .8) + 
  # geom_density(col=1, size=0.6, alpha = .6) + 
  labs(title="Histogram das Nota de Redação") +
  labs(x="Notas", y="qtd")
g

rm(dados_box_REDACAO)
#  BoxPlot das Notas de Matemática
p <- ggplot(dados_box_MT, aes(x=TP_SEXO, y=NU_NOTA_MT, fill=TP_SEXO)) + 
  geom_boxplot() + ylab("Nota em Matemática") + xlab("BRASIL") +
  theme(legend.position = c(0.086, 0.92), 
        legend.background = element_rect(fill="lightgray",
                                         size=0.4, linetype="solid",
                                         colour ="darkgray")) +
  scale_fill_brewer(name="Sexo", 
                    palette = "Spectral",
                    labels=c("Feminino", "Masculino")) +
  labs(title = "Boxplot das Notas em Matemática") +
  stat_summary(fun=mean, geom="point", shape=23, size=1)
p

#  BoxPlot das Notas de Matemática
p <- ggplot(dados_box_MT, aes(x=TP_SEXO, y=NU_NOTA_MT, fill=TP_SEXO)) + 
  geom_boxplot() + ylab("Nota em Matemática") + xlab("BRASIL") +
  theme(legend.position = c(0.086, 0.92), 
        legend.background = element_rect(fill="lightgray",
                                         size=0.4, linetype="solid",
                                         colour ="darkgray")) +
  scale_fill_brewer(name="Sexo", 
                    palette = "Spectral",
                    labels=c("Feminino", "Masculino")) +
  labs(title = "Boxplot das Notas em Matemática") +
  stat_summary(fun=mean, geom="point", shape=23, size=1)
p

#  BoxPlot das Notas de Ciências da Natureza por Unidade Federativa (UF)
p <- ggplot(dados_box_CN, aes(x=TP_SEXO, y=NU_NOTA_CN, fill=TP_SEXO)) + 
  geom_boxplot() + ylab("Nota em Ciências da Natureza") + xlab("BRASIL") +
  theme(legend.position = c(0.086, 0.92), 
        legend.background = element_rect(fill="lightgray",
                                         size=0.4, linetype="solid",
                                         colour ="darkgray")) +
  scale_fill_brewer(name="Sexo", 
                    palette = "Spectral",
                    labels=c("Feminino", "Masculino")) +
  labs(title = "Boxplot das Nota em Ciências da Natureza") +
  stat_summary(fun=mean, geom="point", shape=23, size=1)
p

#  BoxPlot das Notas de Ciências Humanas
p <- ggplot(dados_box_CH, aes(x=TP_SEXO, y=NU_NOTA_CH, fill=TP_SEXO)) + 
  geom_boxplot() + ylab("Nota em Ciências Humanas") + xlab("BRASIL") +
  theme(legend.position = c(0.086, 0.92), 
        legend.background = element_rect(fill="lightgray",
                                         size=0.4, linetype="solid",
                                         colour ="darkgray")) +
  scale_fill_brewer(name="Sexo", 
                    palette = "Spectral",
                    labels=c("Feminino", "Masculino")) +
  labs(title = "Boxplot das Notas em Ciências Humanas por Sexo nas UFs") +
  stat_summary(fun=mean, geom="point", shape=23, size=1)
p



#  BoxPlot das Notas de Linguagens e Códigos
p <- ggplot(dados_box_LC, aes(x=TP_SEXO, y=NU_NOTA_LC, fill=TP_SEXO)) + 
  geom_boxplot() + ylab("Nota em Linguagens e Códigos") + xlab("BRASIL") +
  theme(legend.position = c(0.086, 0.92), 
        legend.background = element_rect(fill="lightgray",
                                         size=0.4, linetype="solid",
                                         colour ="darkgray")) +
  scale_fill_brewer(name="Sexo", 
                    palette = "Spectral",
                    labels=c("Feminino", "Masculino")) +
  labs(title = "Boxplot das Notas em Linguagens e Códigos por Sexo nas UFs") +
  stat_summary(fun=mean, geom="point", shape=23, size=1)
p




