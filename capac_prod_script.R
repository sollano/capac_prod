## ---

## title: Script para estimação de capacidade produtiva no R

## author: 
## - Sollano Rabelo Braga 
## - Ana Carolina Araujo

## date: Outubro, 2016

## output:

##    pdf_document:

##      toc: true

##      toc_depth: 4

##      highlight: tango

##    html_document:

##      toc: true

##      toc_depth: 4

##      highlight: tango

## ---
## \pagebreak
##

## # 1) Carregar pacotes e dados ####

## Primeiro carrega-se os pacotes que serão utilizados:
library(tidyverse)
library(readxl)

## Carrega-se os dados do excel utilizando readxl
dados <- read_excel("dados.xlsx")

## Visualiza-se os dados com head e tail:
head(dados)
tail(dados)

## # 2) Verificar tendência dos dados ####

## Verifica-se a tendência de altura, 
## gerando um gráfico de idade em relação a altura dominante.

ggplot(dados, aes(idade, HD) ) + 
  geom_point() + 
  stat_smooth(method="lm", formula=y~log(x) ) + 
  labs(x      = "Idade (meses)",  
       y      = "Altura dominante (m)"   ) + 
  theme(axis.title   = element_text(size = 14), 
        axis.text    = element_text(size = 12) )  

## # 3) Estimar altura dominante ####

## ## 3.1) Ajuste do modelo linear ####

## Primeiro, cria-se variáveis necessárias para regressão:

dados$LN_HD <- log(dados$HD) 
dados$INV_I <- 1/dados$idade

## Em seguida, ajusta-se o modelo, utilizando a função lm:

reg <- lm(LN_HD ~ INV_I, dados)

## resumo sobre a regressã:
summary(reg)

## 
summary(reg)[[9]]

## ## 3.2) Estimação de HD ####

## Primeiro, cria-se objetos que contém os coeficientes

b0 <- coef(reg)[[1]] 
b1 <- coef(reg)[[2]]
b0
b1

## Em seguida, estima-se HD:
dados$HD_EST <- exp(b0 + b1 * dados$INV_I)

## Visualiza-se o resultado com head:
head(dados)
## Em seguida, remove-se as variáveis utilizadas na estimação:

dados[c("LN_HD", "INV_I")] <- NULL


## ## 3.3) Analises da qualidade do ajuste ####

## Correlação
cor(dados$HD, dados$HD_EST)

## Bias
sum(dados$HD - dados$HD_EST)/nrow(dados)

## RQEM - raiz quadrada do erro médio (RMSE - root mean squared error )
1/mean(dados$HD) * ( sqrt( sum((dados$HD - dados$HD_EST)^2)/nrow(dados) )  ) * 100

## Visualiza-se o resultado com head:
head(dados)

## ## 3.3) Análise de erro do ajuste ####

## Primeiro, calcula-se o erro em porcentagem:

dados$erro <- ((dados$HD_EST - dados$HD)/dados$HD) * 100

## Visualiza-se o resultado com head:
head(dados)

## Agora, gera-se o gráfico de residuos:
ggplot(dados, aes(HD, erro)) + 
  geom_hline(yintercept = 0, color = "gray45") +
  geom_point() +
  ylim(c(-110, 110)) +
  labs(x = "Altura dominante (m)",
       y = "Residuo (%)" ) + 
  theme(axis.title   = element_text(size = 14), 
        axis.text    = element_text(size = 12) )  


## # 4) Gerar curva-guia - metodo dos fatores ####

## O primeiro passo é calcular a razão entre 
## a altura observada e a altura estimada:
dados$fator <- dados$HD / dados$HD_EST
head(dados)

## Em seguida definimos a idade índice que será utilizada:

idade_indice <- 72

## E entao calcula-se a altura dominante na idade índice,
## para ser utilizada futuramente:
HD_EST_II <- exp( b0 + b1*( 1/ idade_indice )  )


## Em seguida calcula-se os limites inferior e superior das classes de site;
## para isso multiplica-se os valores máximo e mínimo do fator pela altura dominante
## na idade índice:
lim_inf <- HD_EST_II * min( dados$fator )
lim_sup <- HD_EST_II * max( dados$fator )

lim_inf
lim_sup

## O intervalo de classe pode ser calculado, com base no numero de classes:
nc <- 5
intervalo <- ceiling(  (lim_sup - lim_inf ) / nc  )
intervalo

mround <- function(x,base){ 
  base*round(x/base) 
} 

## A seguir, serão calculados os limites inferior e superior para todas as classes;
## serão criados varios dataframes, um para cada classe, que serão unidos em seguida por rbind.
## os dataframes serão compostos por 3 colunas: nível, limites, e fator.

## Primeiro utiliza-se o limite inferior como base:
c1 <- data.frame(
                 classe = as.character(as.roman(1)),
  
                 nivel = c("inf","sup"), 
                 
                 limites = c(mround(floor(lim_inf), 5) , 
                             mround(floor(lim_inf), 5) + intervalo), 
                 
                 fator = c(mround(floor(lim_inf), 5) / HD_EST_II,
                           (mround(floor(lim_inf), 5) + intervalo ) / HD_EST_II )  )
c1

## Agora o limite inferior da próxima classe será igual 
## ao limite superior da classe anterior, portanto:

c2 <- data.frame(
                 classe = as.character(as.roman(2)),
  
                 nivel = c("inf","sup"), 
  
                 limites = c(c1$limites[2], 
                             c1$limites[2] + intervalo), 
                 
                 fator = c(c1$limites[2] / HD_EST_II,
                           (c1$limites[2]  + intervalo ) / HD_EST_II )  )
c2

## e assim sucessivamente, ate atingir-se o número de classes desejado:
c3 <- data.frame(
                 classe = as.character(as.roman(3)),
  
                 nivel = c("inf","sup"), 
  
                 limites = c(c2$limites[2], 
                             c2$limites[2] + intervalo), 
                 
                 fator = c(c2$limites[2] / HD_EST_II,
                           (c2$limites[2]  + intervalo ) / HD_EST_II )  )
c3

c4 <- data.frame(
                 classe = as.character(as.roman(4)),
  
                 nivel = c("inf","sup"), 
  
                 limites = c(c3$limites[2], 
                             c3$limites[2] + intervalo), 
                 
                 fator = c(c3$limites[2] / HD_EST_II,
                           (c3$limites[2]  + intervalo ) / HD_EST_II )  )
c4

c5 <- data.frame(
                 classe = as.character(as.roman(5)),
  
                 nivel = c("inf","sup"), 
  
                 limites = c(c4$limites[2], 
                             c4$limites[2] + intervalo), 
                 
                 fator = c(c4$limites[2] / HD_EST_II,
                           (c4$limites[2]  + intervalo ) / HD_EST_II )  )
c5

## Em seguida, utilizando rbind, cria-se a tabela de classes,
## unindo todas as classes em um unico objeto:
classes <- rbind(c1,c2,c3,c4,c5)
classes

## Uma forma mais pratica de realizar esta ação é utilizando um loop for.
##
## Segue-se o mesmo raciocínio anteriormente, porem define-se que a operação será repetida
## por um numero de vezes, ou seja, um loop. 
##
## como será utilizado um loop for a seguir,
## cria-se uma lista que contém o número de entradas referente
## ao número de classes desejado:
nc <- 5
list <- vector("list", nc)

## Cria-se uma lista vazia com 5 elementos:
list

## Os primeiros limites são calculados separadamente do loop,
## e são salvos no primeiro elemento da lista:
## Isto é importante e e feito separadamente, pois os calculos feitos no loop
## irão se iniciar utilizando o primeiro elemento da lista.
list[[1]] <- data.frame(
                        classe = as.character(as.roman(1)),
  
                        nivel = c("inf","sup"), 
                        
                        limites = c(mround(floor(lim_inf), 5) , 
                                    mround(floor(lim_inf), 5) + intervalo), 
                        
                        fator = c(mround(floor(lim_inf), 5) / HD_EST_II,
                                  (mround(floor(lim_inf), 5) + intervalo ) / HD_EST_II )  )

## Agora aplica-se o loop. No loop for (para), se determina uma  variável,
## chamada de "i" neste caso, que representa a posição
## onde será aplicado o loop. Entao neste caso o loop vai de 2 a 5, como pode-se ver abaixo.
## A operação será repetida i vezes, ate que se chegue ao numero final.
##
## Anteriormente o mesmo processo foi repetido 5 vezes, mudando-se apenas a posição,
## pois primeiro foi feito para o segundo limite, depois para o terceiro, ate o quinto.
## Portando pode-se aplicar este padrao aqui, substituindo estas posições que foram mudando
## por "i", para que o loop faça o processo automaticamente.
##
## Cada interção e salva em um elemento de uma lista. Portanto, a primeira operação,
## que anteriormente foi chamada de c1, e era uma matriz, agora é um elemento de uma lista,
## e pode ser chamado por:
list[[1]]

## Os próximos elementos serão salvos nas posições 2, 3, 4 e 5.
## Portanto, onde anteriormente foi utilizado c, utiliza-se list[[]].
## Como i representa a posição atual do loop, se o calculo for para a posição 2,
## i terá o valor 2. Substituindo abaixo, temos que:
## list tera o elemento 2 criado;
## e os elementos e os fatores serão criados utilizando list[[2 - 1]] ou seja, 1.
## Isto quando se determina os fatores e os limites sempre se utiliza a informação do limite anterior.
## O processo então é repetido para as demais classes.
for(i in 2:(nc)){
  
  list[[i]] <-   data.frame(classe = as.character(as.roman(i)),
                            nivel = c("inf","sup"), 
                            limites = c(list[[i-1]] [[3]] [[2]], 
                                        list[[i-1]] [[3]] [[2]] + intervalo), 
                            
                            fator = c(list[[i-1]] [[3]] [[2]] / HD_EST_II,
                                      (list[[i-1]] [[3]] [[2]]+ intervalo ) / HD_EST_II )  )
  
}

## transforma-se a lista em matriz e em seguida em data frame:
classes2 <- do.call(rbind, list)

## Os dois objetos criados sao idênticos:
classes
classes2

## O próximo passo é unir os dados originais de altura dominante e idade,
## com as classes criadas. Para uni-los mantendo a ordem dos limites,
## é necessario criar uma variavel auxiliar que seja comum aos dois data frames:
dados$aux <- 1
classes$aux <- 1

## em seguida, une-se os dados utilizando merge, com base na variável aux:
tab_curva <- merge( dados[c("HD", "HD_EST", "idade", "aux")]  , classes , by = "aux")

## Agora pode-se remover a variável auxiliar:
tab_curva$aux <- NULL

## Visualiza-se o resultado com head:
head(tab_curva, 15)

## Já que os limites serão utilizados para classificar os dados,
## é interessante transformá-los em fator:
# transforma-se os limites em fator, para que o gráfico possa ser plotado corretamente:
tab_curva$limites <- factor(tab_curva$limites)
class(tab_curva$limites)

# Em seguida calcula-se do HD com base no fator calculado:
tab_curva$HD_CURVA <- tab_curva$HD_EST * tab_curva$fator

## Visualiza-se o resultado com head:
head(tab_curva, 15)

curvas <- ggplot(tab_curva ) +  # cria-se a base para o grafico
  geom_point(aes(idade, HD)) + # plota-se os dados originais como pontos
  geom_line(aes( idade, 
                 HD_CURVA, 
                 color = limites ), 
            size = 1.5 ) + # plota-se as linhas utilizando 
  labs(x = "Idade (meses)",
       y = "Altura dominante (m)",
       color = "Site") +
  guides(color= guide_legend(reverse = T)) + 
  theme(  #com theme muda-se o tamanho e estilo de letra de cada parte do grafico
    axis.title   = element_text(size = 14), 
    axis.text    = element_text(size = 12))
curvas

## # 5) Converter tabela dos limites para o padrão ####

tab_curva_cor <- tab_curva %>% 
  unite(C, classe, nivel) %>% 
  select(idade,C, HD_CURVA)%>% 
  group_by(idade,C) %>%  
  mutate(aux=row_number()) %>% 
  spread(C, HD_CURVA, sep = "_")%>% 
  summarise_at(vars(contains("_")),mean)

tab_curva_cor

## # 5) Exportar resultados ####

ggsave("curvas.png", curvas, width = 12,height = 8)
write.csv2(tab_curva_cor, "tab_curva_cor.csv", row.names = F)
