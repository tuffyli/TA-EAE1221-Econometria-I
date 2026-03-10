# ---------------------------------------------------------------------------- #
#' Primeiro Contato com o R
#' Data da última edição: 10/03/2026
#' Autor: Tuffy Licciardi Issa
#' --------------------------------------------------------------------------- #

# ---------------------------------------------------------------------------- #
# Introdução ----
# ---------------------------------------------------------------------------- #

#' Para comentar no R, usamos o símbolo de #.
#' Todo o texto após esta marcação não será rodado no console.

# No R, as operações básicas são marcadas por símbolos específicos:

1 + 1
2 - 1
3 * 4
10 / 2

# ---------------------------------------------------------------------------- #
# Como declarar objetos no R?

objeto <- 1 #Lembre-se de usar o símbolo de atribuição "<-" para criar objetos no R.

#Podemos criar vetores usando a função c().
vetor <- c(1, 2, 3, 4, 5)


#Com os vetores podemos criar um DataFrame
df <- data.frame(
  id = c(1, 2, 3),
  nome = c("Alice", "Bob", "Charlie"),
  idade = c(25, 30, 35)
)

#Para observar nosso vetor ou DataFrame, basta digitar o nome do objeto no console.
vetor
df

#Outro modo de vizualizar o DataFrame é usando o print()
print(df)

# ---------------------------------------------------------------------------- #
# Pacotes ----
# ---------------------------------------------------------------------------- #
# Para usar funções específicas, precisamos carregar os pacotes correspondentes.
# Mas, antes é necessário instalar os pacotes usando a função install.packages().

install.packages("tidyverse") #Instalando o pacote dplyr
install.packages("wooldridge")
install.packages("ggplot2")
install.packages("readxl")

# Não esqueça de carregar os pacotes baixados
library(tidyverse) #Carregando o pacote dplyr
library(wooldridge) #Carregando o pacote wooldridge
library(ggplot2) #Carregando o pacote ggplot2
library(readxl) #Carregando o pacote readxl


#Podemos agora abrir uma das bases de dados disponíveis no pacote do wooldridge:

data("traffic1") #Carregando a base de dados wage1)

summary(traffic1)


# E se quiser baixar algum dado da internet? 
# https://www.ibge.gov.br/estatisticas/economicas/precos-e-custos/9256-indice-nacional-de-precos-ao-consumidor-amplo.html?t=series-historicas&utm_source=landing&utm_medium=explica&utm_campaign=inflacao#plano-real-mes
df_inflacao <- read.csv2("C:/Users/tuffy/Documents/Monitoria/EAE-1221/Intro_R/Bases/inflacao_anual.csv",
                         header = T,
                         skip = 1
                         )
df_inflacao #Como podemos ver o IBGE sempre deixa os dados deles no formato wide
#Não sei por que, mas é o que temos.
#Para isso, precisamos transformar os dados para o formato long usando a função
#pivot_longer() do pacote tidyr.


df_long <- df_inflacao %>% #Isto são pipes, uma forma de encadear operações no R.
  filter(X == "Brasil") %>%
  mutate(across(-X, as.numeric)) %>%
  pivot_longer(
    cols = -X,
    names_to = c("mes", "ano"),
    names_sep = "\\.",
    values_to = "inflacao"
  ) %>% 
  select(-X)

df_long #Data final no formato long
