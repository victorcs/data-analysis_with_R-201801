# Carregue a biblioteca tidyverse. Lembre que outras bibliotecas serão carregadas junto ao tidyverse

library(tidyverse)


# Crie um dataframe com o conteúdo do arquivo ted_main.csv.gz. 

dtframe_1 <- read_csv("aula-05/data/ted_main.csv.gz")


# Visualize o resumo dos dados do dataframe. Verifique os mínimos, máximos, médias e medianas das variáveis numéricas.
# As variáveis duration, film_date e published_date estão no tipo de dados apropriado?

#film_date      = int ----> errado
#duration       = int ----> certo
#published_date = int-----> errado

  head(dtframe_1, 20)
  str(dtframe_1)
  summary(dtframe_1)

  
  
# Converta as seguintes variáveis utilizando o pacote Lubridate:
#     * duration, para duração (em segundos). Experimente utilizar as funções as.duration e duration. Mantenha aquela que considerar mais apropriada.
#     * film_date, para data, com a função as_datetime.
#     * published_date, para data, com a função as_datetime..


library(lubridate)


duracao <- duration(dtframe_1$duration, "seconds")
head(duracao,10)


dataFilme <- as_datetime(dtframe_1$film_date)
head(dataFilme,10)


dataPublicacao <- as_datetime(dtframe_1$published_date)
head(dataPublicacao,10)


# Converta as seguintes variáveis character para variáveis categóricas com a função factor.
#     * event
#     * speaker_occupation

evento <- factor(dtframe_1$event)
head(evento,10)

ocupacao <- factor(dtframe_1$speaker_occupation)
head(ocupacao,10)

# Retire do dataframe a variável name

dtframe_1$name <- NULL
head(dtframe_1,10)



# Visualize novamente o resumo dos dados do dataframe. Verifique os mínimos, máximos, médias e medianas das variáveis numéricas. Verifique as contagens das variáveis categóricas
str(dtframe_1)
summary(dtframe_1)


# Verifique quais registros possuem a menor quantidade de línguas. Corrija para que possuam no mínimo 1 idioma.

dtframe_1 %>%
  mutate(languages = ifelse(languages == 0,1,languages))%>%
  arrange(languages)%>%
  head(10)


# Verifique os 15 registros com menor data de filmagem. 

dtframe_1 %>%
  arrange(film_date)%>%
  head(15)


# Crie um dataframe com a contagem de apresentações por ano de filmagem e visualize todo o seu conteúdo
apresentacoes_ano <- dtframe_1 %>%
  group_by(year(film_date)) %>%
  summarise(count = n())
View(apresentacoes_ano)


# Analise os 10 quantis da quantidade de apresentações por ano.
# Descarte, do data frame de apresentações do TED Talks, aqueles cujo ano de filmagem tiver quantidade de apresentações menor ou igual à quantidade do quarto quantil.

quantile(apresentacoes_ano$count, probs = seq(0,1,1/9))
quarto_quartil = quantile(apresentacoes_ano$count, probs = seq(0,1,1/9))[4]



# Verifique novamente o resumo dos dados do dataframe




# Verifique os 10 registros com maior duração.




# Existem apresentações com duração maior que 3 desvios padrão acima da média? Liste elas




# Calcule os 4 quartis e o IQR da duração das apresentações. Liste as apresentações cuja duração supera 1.5 * o IQR + o terceiro quartil




# Visualize os 10 quantis da quantidade de visualizações




# Compare as seguintes estatísticas descritivas da quantidade de visualizações:
#   * Média e Mediana. Qual é maior?
#   * Desvio Absoluto da Mediana e Desvio Padrão. Qual é maior?
#   * Desvio Absoluto da Mediana e IQR. Quantas vezes o IQR é maior que o Desvio Absoluto da Mediana?
#   * Com base na média e na mediana, e na razão entre o IQR e o Desvio Absoluto da Mediana, 
#     você conclui que as quantidades de visualização estão distribuidas de forma simétrica em torno da média?




# Calcule a média, o desvio padrão, a mediana e o IQR da quantidade de línguas dos seguintes grupos:
#     * 10% de vídeos com maior número de visualizações
#     * 10% de vídeos com menor número de visualizações




# Determine a quantidade de apresentações por evento cujo nome inicie com TED. Utilize a função str_detect para este filtro




# Determine, por evento cujo nome inicie com TED e que a quantidade de visualizações dos vídeos foi maior que a mediana calculada anteriormente.
#   * a quantidade de apresentações resultante do filtro, por evento
#   * o ano do evento (utilizar o menor ano da data de publicação)
#   * a quantidade média de línguas das apresentações
#   * o desvio padrão da quantidade de línguas
#   * o coeficiente de variação da quantidade de línguas
### EXIBA SOMENTE OS EVENTOS COM MAIS DE 10 APRESENTAÇÕES




# Calcule e classifique as seguintes correlações
#     * Quantidade de visualizações e Quantidade de línguas
#     * Quantidade de visualizações e Duração
#     * Quantidade de visualizações e Quantidade de Comentários
#     * Quantidade de Comentários e Quantidade de línguas




# Descarte os vídeos cuja duração seja maior que 3 desvios padrões da média. Calcule novamente as 5 correlações solicitadas




# Utilizando o data frame original, crie um dataframe com a mediana da duração dos vídeos por ano de filmagem. Calcule a correlação entre o ano e a mediana da duração
# e interprete o resultado




