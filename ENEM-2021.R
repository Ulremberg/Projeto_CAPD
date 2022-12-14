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
  filter(NU_NOTA_CN>0) %>%
  drop_na() %>%
  select(SG_UF_PROVA, TP_SEXO, NU_NOTA_CN)

dados_box_MT = dados_bruto %>%
  filter(NU_NOTA_MT>0) %>%
  select(SG_UF_PROVA, TP_SEXO, NU_NOTA_MT)

dados_box_CH = dados_bruto %>%
  filter(NU_NOTA_CH>0) %>%
  drop_na() %>%
  select(SG_UF_PROVA, TP_SEXO, NU_NOTA_CH)

dados_box_LC = dados_bruto %>%
  filter(NU_NOTA_LC>0) %>%
  drop_na() %>%
  select(SG_UF_PROVA, TP_SEXO, NU_NOTA_LC)

dados_box_REDACAO = dados_bruto %>%
  filter(NU_NOTA_REDACAO>0) %>%
  drop_na() %>%
  select(SG_UF_PROVA, TP_SEXO, NU_NOTA_REDACAO)

dados_box_REDACAOMT = dados_bruto %>%
  filter(NU_NOTA_REDACAO>0 & NU_NOTA_MT>0) %>%
  drop_na() %>%
  select(SG_UF_PROVA, TP_SEXO, NU_NOTA_REDACAO, NU_NOTA_MT)

dados_box_REDACAOCN = dados_bruto %>%
  filter(NU_NOTA_REDACAO>0 & NU_NOTA_CN>0) %>%
  drop_na() %>%
  select(SG_UF_PROVA, TP_SEXO, NU_NOTA_REDACAO, NU_NOTA_CN)

dados_box_REDACAOCH = dados_bruto %>%
  filter(NU_NOTA_REDACAO>0 & NU_NOTA_CH>0) %>%
  drop_na() %>%
  select(SG_UF_PROVA, TP_SEXO, NU_NOTA_REDACAO, NU_NOTA_CH)


# Regress?o Linear das Notas de Matem?tica vs Reda??o
g <- ggplot(dados_box_REDACAOMT,aes(x=NU_NOTA_REDACAO,y=NU_NOTA_MT, fill=TP_SEXO)) + geom_point() + 
  scale_fill_brewer(name="Sexo", 
                    palette = "Dark2",
                    labels=c("Feminino", "Masculino")) +
  labs(title = "Notas em Reda??o vs Matem?tica") +
  stat_summary(fun=mean, geom="point", shape=21, size=2)
g
rm(dados_box_REDACAOMT)
# Regress?o Linear das Notas de Ci?ncias da Natureza vs Reda??o
g <- ggplot(dados_box_REDACAOCN,aes(x=NU_NOTA_REDACAO,y=NU_NOTA_CN, fill=TP_SEXO)) + geom_point() +
  scale_fill_brewer(name="Sexo", 
                    palette = "Dark2",
                    labels=c("Feminino", "Masculino")) +
  labs(title = "Notas em Reda??o vs Ci?ncias da Natureza") +
  stat_summary(fun=mean, geom="point", shape=21, size=2)
g

# Regress?o Linear das Notas de Ci?ncias Humanas vs Reda??o
g <- ggplot(dados_box_REDACAOCH,aes(x=NU_NOTA_REDACAO,y=NU_NOTA_CH, fill=TP_SEXO)) + geom_point() +
  scale_fill_brewer(name="Sexo", 
                    palette = "Dark2",
                    labels=c("Feminino", "Masculino")) +
  labs(title = "Notas em Reda??o vs Ci?ncias Humanas") +
  stat_summary(fun=mean, geom="point", shape=21, size=2)
g

#  Histograma das Notas de Reda??o
g <- ggplot(data=dados_box_REDACAO, aes(dados_box_REDACAO$NU_NOTA_REDACAO)) + 
  geom_histogram(aes(y =..density..), 
                 bins = 50, 
                 colour = 1,
                 fill = "white") + 
  geom_density(lwd = 1, colour = 4,
               fill = 4, alpha = 0.25)+ 
  labs(title="Histogram das Nota de Reda??o") +
  labs(x="Notas", y="densidade")
g

#  BoxPlot das Notas de Reda??o Brasil
p <- ggplot(dados_box_REDACAO, aes(x=TP_SEXO, y=NU_NOTA_REDACAO, fill=TP_SEXO)) + 
  geom_boxplot(alpha=1.8) + ylab("Nota em Reda??o") + xlab("BRASIL") +
  theme(legend.position = c(0.086, 0.92), 
        legend.background = element_rect(fill="lightgray",
                                         size=0.4, linetype="solid",
                                         colour ="darkgray")) +
  scale_fill_brewer(name="Sexo", 
                    palette = "Dark2",
                    labels=c("Feminino", "Masculino")) +
  labs(title = "Boxplot das Notas em Reda??o") +
  stat_summary(fun=mean, geom="point", shape=21, size=1)
p

#  BoxPlot das Notas de Reda??o por Unidade Federativa (UF)
p <- ggplot(dados_box_REDACAO, aes(x=SG_UF_PROVA, y=NU_NOTA_REDACAO, fill=TP_SEXO)) + 
  geom_boxplot(alpha=1.8) + ylab("Nota em Reda??o") + xlab("Estados")+ 
  theme(legend.position = c(0.086, 0.92), 
        legend.background = element_rect(fill="lightgray",
                                         size=0.4, linetype="solid",
                                         colour ="darkgray")) +
  scale_fill_brewer(name="Sexo", 
                    palette = "Dark2",
                    labels=c("Feminino", "Masculino")) +
  labs(title = "Boxplot das Notas em Reda??o") +
  stat_summary(fun=mean, geom="point", shape=21, size=1)
p


#  Histograma das Notas de Matem?tica
g <- ggplot(dados_box_MT, aes(dados_box_MT$NU_NOTA_MT)) + 
  geom_histogram(aes(y =..density..), 
                 bins = 50, 
                 colour = 1,
                 fill = "white") + 
  geom_density(lwd = 1, colour = 4,
               fill = 4, alpha = 0.25)+
  labs(title="Histogram das Nota de Matem?tica") +
  labs(x="Notas", y="qtd")
g

#  BoxPlot das Notas de Matem?tica
p <- ggplot(dados_box_MT, aes(x=TP_SEXO, y=NU_NOTA_MT, fill=TP_SEXO)) + 
  geom_boxplot(alpha=1.8) + ylab("Nota em Matem?tica") + xlab("BRASIL") +
  theme(legend.position = c(0.086, 0.92), 
        legend.background = element_rect(fill="lightgray",
                                         size=0.4, linetype="solid",
                                         colour ="darkgray")) +
  scale_fill_brewer(name="Sexo", 
                    palette = "Dark2",
                    labels=c("Feminino", "Masculino")) +
  labs(title = "Boxplot das Notas em Matem?tica") +
  stat_summary(fun=mean, geom="point", shape=21, size=1)
p



#  Histograma das Notas de Ci?ncias da Natureza
g <- ggplot(dados_box_CN, aes(dados_box_CN$NU_NOTA_CN)) + 
  geom_histogram(aes(y =..density..), 
                 bins = 50, 
                 colour = 1,
                 fill = "white") + 
  geom_density(lwd = 1, colour = 4,
               fill = 4, alpha = 0.25)+
  labs(title="Histogram das Nota de Ci?ncias da Natureza") +
  labs(x="Notas", y="densidade")
g

#  BoxPlot das Notas de Ci?ncias da Natureza por Unidade Federativa (UF)
p <- ggplot(dados_box_CN, aes(x=SG_UF_PROVA, y=NU_NOTA_CN, fill=TP_SEXO)) + 
  geom_boxplot(alpha=1.8) + ylab("Nota em Ci?ncias da Natureza") + xlab("Estados") +
  theme(legend.position = c(0.086, 0.92), 
        legend.background = element_rect(fill="lightgray",
                                         size=0.4, linetype="solid",
                                         colour ="darkgray")) +
  scale_fill_brewer(name="Sexo", 
                    palette = "Dark2",
                    labels=c("Feminino", "Masculino")) +
  labs(title = "Boxplot das Nota em Ci?ncias da Natureza") +
  stat_summary(fun=mean, geom="point", shape=21, size=1)
p

#  BoxPlot das Notas de Ci?ncias da Natureza por por Sexo
p <- ggplot(dados_box_CN, aes(x=TP_SEXO, y=NU_NOTA_CN, fill=TP_SEXO)) + 
  geom_boxplot(alpha=1.8) + ylab("Nota em Ci?ncias da Natureza") + xlab("Sexo") +
  theme(legend.position = c(0.086, 0.92), 
        legend.background = element_rect(fill="lightgray",
                                         size=0.4, linetype="solid",
                                         colour ="darkgray")) +
  scale_fill_brewer(name="Sexo", 
                    palette = "Dark2",
                    labels=c("Feminino", "Masculino")) +
  labs(title = "Boxplot das Nota em Ci?ncias da Natureza") +
  stat_summary(fun=mean, geom="point", shape=21, size=1)
p

#  Histograma Notas de Ci?ncias Humanas
g <- ggplot(dados_box_CH, aes(dados_box_CH$NU_NOTA_CH)) + 
  geom_histogram(aes(y =..density..), 
                 bins = 50, 
                 colour = 1,
                 fill = "white") + 
  geom_density(lwd = 1, colour = 4,
               fill = 4, alpha = 0.25)+
  labs(title="Histogram das Nota deCi?ncias Humanas") +
  labs(x="Notas", y="densidade")
g

#  BoxPlot das Notas de Ci?ncias Humanas
p <- ggplot(dados_box_CH, aes(x=TP_SEXO, y=NU_NOTA_CH, fill=TP_SEXO)) + 
  geom_boxplot(alpha=1.8) + ylab("Nota em Ci?ncias Humanas") + xlab("BRASIL") +
  theme(legend.position = c(0.086, 0.92), 
        legend.background = element_rect(fill="lightgray",
                                         size=0.4, linetype="solid",
                                         colour ="darkgray")) +
  scale_fill_brewer(name="Sexo", 
                    palette = "Dark2",
                    labels=c("Feminino", "Masculino")) +
  labs(title = "Boxplot das Notas em Ci?ncias Humanas por Sexo nas UFs") +
  stat_summary(fun=mean, geom="point", shape=21, size=1)
p

#  BoxPlot das Notas de Ci?ncias Humanas por Unidade Federativa (UF)
p <- ggplot(dados_box_CH, aes(x=SG_UF_PROVA, y=NU_NOTA_CH, fill=TP_SEXO)) + 
  geom_boxplot(alpha=1.8) + ylab("Nota em Ci?ncias Humanas") + xlab("Estados")+ 
  theme(legend.position = c(0.086, 0.92), 
        legend.background = element_rect(fill="lightgray",
                                         size=0.4, linetype="solid",
                                         colour ="darkgray")) +
  scale_fill_brewer(name="Sexo", 
                    palette = "Dark2",
                    labels=c("Feminino", "Masculino")) +
  labs(title = "Boxplot das Notas em Ci?ncias Humanas por Sexo nas UFs") +
  stat_summary(fun=mean, geom="point", shape=21, size=1)
p

#  BoxPlot das Notas de Linguagens e C?digos
p <- ggplot(dados_box_LC, aes(x=TP_SEXO, y=NU_NOTA_LC, fill=TP_SEXO)) + 
  geom_boxplot(alpha=1.8) + ylab("Nota em Linguagens e C?digos") + xlab("BRASIL") +
  theme(legend.position = c(0.086, 0.92), 
        legend.background = element_rect(fill="lightgray",
                                         size=0.4, linetype="solid",
                                         colour ="darkgray")) +
  scale_fill_brewer(name="Sexo", 
                    palette = "Dark2",
                    labels=c("Feminino", "Masculino")) +
  labs(title = "Boxplot das Notas em Linguagens e C?digos por Sexo nas UFs") +
  stat_summary(fun=mean, geom="point", shape=21, size=1)
p

#  BoxPlot das Notas de Linguagens e C?digos por Unidade Federativa (UF)
p <- ggplot(dados_box_LC, aes(x=SG_UF_PROVA, y=NU_NOTA_LC, fill=TP_SEXO)) + 
  geom_boxplot(alpha=1.8) + ylab("Nota em Linguagens e C?digos") + xlab("Estados")+ 
  theme(legend.position = c(0.086, 0.92), 
        legend.background = element_rect(fill="lightgray",
                                         size=0.4, linetype="solid",
                                         colour ="darkgray")) +
  scale_fill_brewer(name="Sexo", 
                    palette = "Dark2",
                    labels=c("Feminino", "Masculino")) +
  labs(title = "Boxplot das Notas em Linguagens e C?digos por Sexo nas UFs") +
  stat_summary(fun=mean, geom="point", shape=21, size=1)
p

regiao = read.csv2("C:/Dados/regiaoUF.csv", sep=";")

d4 = dados_bruto %>% 
  group_by(TP_COR_RACA,SG_UF_PROVA) %>%
  summarise(n=n_distinct(NU_INSCRICAO))

d4 = left_join(d4,regiao,by="SG_UF_PROVA")
d4 = d4 %>% group_by(TP_COR_RACA,REGIAO) %>% summarise(n=sum(n))
d4$TP_COR_RACA = as.factor(d4$TP_COR_RACA)

d6 <- dados_bruto %>% 
  group_by(TP_SEXO,SG_UF_PROVA) %>%
  summarise(n=n_distinct(NU_INSCRICAO))

g6 <- ggplot(d6, aes(SG_UF_PROVA, n, fill = TP_SEXO)) +
  # t?tulo xlab e ylab
  xlab("Estados (UF)") + ylab ("Quantidade de Alunos") + 
  geom_bar(stat = "identity") +
  theme_few() + 
  scale_y_continuous(labels = comma)+
  scale_fill_brewer(name="Sexo", 
                    palette = "Dark2",
                    labels=c("Feminino", "Masculino")) + 
  labs(title = "Distribui??o por Sexo nas UFs")
#Legenda do Gr?fico
g6 <- g6 + theme(legend.position = c(0.13, 0.84), 
                 legend.background = element_rect(fill="lightgray",
                                                  size=0.4, linetype="solid",
                                                  colour ="darkgray"))
g6

rm(g6)
rm(d6)
d7 <- dados_bruto %>% 
  group_by(TP_SEXO) %>%
  count() %>% 
  ungroup() %>% 
  mutate(per=n/sum(n)) %>% 
  arrange(desc(TP_SEXO))
d7$label <- scales::percent(d7$per)

ggplot(d7)+
  geom_bar(aes(x=TP_SEXO, y=per, fill=TP_SEXO), stat="identity", width = 1)+
  theme_void()+ xlab("") + ylab("") +
  theme_few() +
  scale_fill_brewer(name="Sexo", 
                    palette = "Dark2",
                    labels=c("Feminino", "Masculino")) + 
  labs(title = "Inscritos Enem 2021") +
  theme(
    legend.background = element_rect(fill="lightgray",
                                     size=0.2, linetype="solid",
                                     colour ="darkgray")) + 
  geom_text(aes(x=TP_SEXO, y = per, label=label), position = position_stack(vjust = 0.5))

rm(d7)

dados_box_REDACAO = left_join(dados_box_REDACAO, regiao, by="SG_UF_PROVA")

# box plot
p <- ggplot(dados_box_REDACAO, aes(x=REGIAO, y=NU_NOTA_REDACAO, fill=TP_SEXO)) + 
  geom_boxplot(alpha=1.8) + ylab("Nota em Reda??o") + xlab("Regi?es")+ 
  theme(legend.position = c(0.086, 0.92), 
        legend.background = element_rect(fill="lightgray",
                                         size=0.4, linetype="solid",
                                         colour ="darkgray")) +
  scale_fill_brewer(name="Sexo", 
                    palette = "Dark2",
                    labels=c("Feminino", "Masculino")) +
  labs(title = "Boxplot das Notas em Reda??o") +
  stat_summary(fun=mean, geom="point", shape=23, size=1)
p
rm(dados_box_REDACAO)


dados_box_MT = left_join(dados_box_MT, regiao, by="SG_UF_PROVA")

# box plot
p <- ggplot(dados_box_MT, aes(x=REGIAO, y=NU_NOTA_MT, fill=TP_SEXO)) + 
  geom_boxplot(alpha=1.8) + ylab("Nota em Matem?tica") + xlab("Regi?es")+ 
  theme(legend.position = c(0.086, 0.92), 
        legend.background = element_rect(fill="lightgray",
                                         size=0.4, linetype="solid",
                                         colour ="darkgray")) +
  scale_fill_brewer(name="Sexo", 
                    palette = "Dark2",
                    labels=c("Feminino", "Masculino")) +
  labs(title = "Boxplot das Notas em Matem?tica") +
  stat_summary(fun=mean, geom="point", shape=23, size=1)
p

rm(dados_box_MT)
dados_box_CN = left_join(dados_box_CN, regiao, by="SG_UF_PROVA")

# box plot
p <- ggplot(dados_box_CN, aes(x=REGIAO, y=NU_NOTA_CN, fill=TP_SEXO)) + 
  geom_boxplot(alpha=1.8) + ylab("Nota em Ci?ncias da Natureza") + xlab("Regi?es")+ 
  theme(legend.position = c(0.086, 0.92), 
        legend.background = element_rect(fill="lightgray",
                                         size=0.4, linetype="solid",
                                         colour ="darkgray")) +
  scale_fill_brewer(name="Sexo", 
                    palette = "Dark2",
                    labels=c("Feminino", "Masculino")) +
  labs(title = "Boxplot das Notas em Ci?ncias da Natureza") +
  stat_summary(fun=mean, geom="point", shape=23, size=1)
p

rm(dados_box_CN)
dados_box_CH = left_join(dados_box_CH, regiao, by="SG_UF_PROVA")

# box plot
p <- ggplot(dados_box_CH, aes(x=REGIAO, y=NU_NOTA_CH, fill=TP_SEXO)) + 
  geom_boxplot(alpha=1.8) + ylab("Nota em Ci?ncias Humanas") + xlab("Regi?es")+ 
  theme(legend.position = c(0.086, 0.92), 
        legend.background = element_rect(fill="lightgray",
                                         size=0.4, linetype="solid",
                                         colour ="darkgray")) +
  scale_fill_brewer(name="Sexo", 
                    palette = "Dark2",
                    labels=c("Feminino", "Masculino")) +
  labs(title = "Boxplot das Notas em Ci?ncias Humanas") +
  stat_summary(fun=mean, geom="point", shape=23, size=1)
p
rm(dados_box_CH)


dados_box_LC = left_join(dados_box_LC, regiao, by="SG_UF_PROVA")

# box plot
p <- ggplot(dados_box_LC, aes(x=REGIAO, y=NU_NOTA_LC, fill=TP_SEXO)) + 
  geom_boxplot(alpha=1.8) + ylab("Nota em Linguagens e C?digos") + xlab("Regi?es")+ 
  theme(legend.position = c(0.086, 0.92), 
        legend.background = element_rect(fill="lightgray",
                                         size=0.4, linetype="solid",
                                         colour ="darkgray")) +
  scale_fill_brewer(name="Sexo", 
                    palette = "Dark2",
                    labels=c("Feminino", "Masculino")) +
  labs(title = "Boxplot das Notas em Linguagens e C?digos") +
  stat_summary(fun=mean, geom="point", shape=21, size=1)
p

rm(dados_box_LC)