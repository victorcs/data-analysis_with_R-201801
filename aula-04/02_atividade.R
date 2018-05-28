library(tidyverse)
library(lubridate)

## Nesta atividade você deve utilizar o resultado do exercício 01 da Atividade da aula 03 (remuneração em dólares convertida para reais)
## Utilize o código daquele exercício como ponto de partida para esta atividade. 
## Sempre utilize o caminho relativo, não o caminho absoluto, pois não funcionará na correção do exercício.

### IMPORTANTE ###
## Se você utilizar alguma função própria ou do material de aula, o código da(s) função(ões) deve estar neste arquivo da atividade.


### 1 ####
## 
## Correlação de ano de ingresso por cargo
## - Determine o coeficiente de correlação entre o tempo em anos desde a DATA_INGRESSO_ORGAO e o tempo em anos desde a DATA_DIPLOMA_INGRESSO_SERVICOPUBLICO
##   para todos os cargos que possuem no mínimo 200 servidores.
## - Crie uma coluna que determina se a correlação é positiva ou negativa, e outra coluna que define a força da correlação de acordo com 
##   o material visto em aula sobre interpretação do coeficiente.
## - O resultado desta atividade deve ser um Data Frame com as variáveis de Cargo, Coeficiente de Correlação, Direção da Correlação e Força da Correlação
## 
### # ####

calc_grau_correlacao <- function(x)
{
  if (x <= 0.3) 
  {
    "Desprezível"
  } else if (x <= 0.5) 
  {
    "Fraca"
  } else if (x <= 0.7) 
  {
    "Moderada"
  } else if (x <= 0.9) 
  {
    "Forte"
  } else 
  {
    "Muito forte"
  }
}
salarios <- read_csv("aula-03/data/201802_dados_salarios_servidores.csv.gz")
salarios$REMUNERACAO_FINAL <- salarios$REMUNERACAO_REAIS + (salarios$REMUNERACAO_DOLARES * 3.2421)
salarios %>%
  filter(REMUNERACAO_FINAL >= 900) -> resultado_salarios


resultado_salarios %>%
  group_by(DESCRICAO_CARGO) %>%
  filter(n() >= 200) %>%
  summarise(COEFICIENTE_CORRELACAO = cor(year(DATA_INGRESSO_ORGAO), year(DATA_DIPLOMA_INGRESSO_SERVICOPUBLICO)),
            CORRELACAO_POSITIVA = COEFICIENTE_CORRELACAO > 0,
            GRAU_CORRELACAO = calc_grau_correlacao(abs(COEFICIENTE_CORRELACAO))) %>%
  ungroup() %>%
  select(DESCRICAO_CARGO, COEFICIENTE_CORRELACAO, CORRELACAO_POSITIVA, GRAU_CORRELACAO) -> resultado_final


resultado_final




### 2 ###
##
## - A partir do dataset do exercício anterior, selecione os 10 cargos de correlação mais forte (seja positiva ou negativa) e os 
##   10 cargos de correlação mais fraca (de novo, independente de ser positiva ou negativa)
## - Para estes 20 cargos, determine a Moda do órgão de lotação (ORGSUP_LOTACAO) e de exercício (ORGSUP_EXERCICIO)
## - Reponda se existe diferença entre as modas e se existe relação entre a Força da Correlação e a diferença entre as modas 
##   (caso haja diferença)
##
### # ###


resultado_final %>%
  arrange(desc(abs(COEFICIENTE_CORRELACAO))) %>%
  head(10) -> top10_fortes

resultado_final %>%
  arrange(abs(COEFICIENTE_CORRELACAO)) %>%
  head(10) -> top10_fracos

(merge(top10_fortes, top10_fracos, all = TRUE)) -> res_fracos_fortes


cargos <- res_fracos_fortes %>% pull(DESCRICAO_CARGO)


MODA_ORGSUP_LOTACAO <- resultado_salarios %>%
  filter(DESCRICAO_CARGO %in% cargos) %>%
  group_by(ORGSUP_LOTACAO) %>%
  summarise(qtd = n()) %>%
  ungroup() %>%
  arrange(desc(qtd)) %>%
  head(1) %>% 
  select(ORGSUP_LOTACAO)

MODA_ORGSUP_EXERCICIO <- resultado_salarios %>%
  filter(DESCRICAO_CARGO %in% cargos) %>%
  group_by(ORGSUP_EXERCICIO) %>%
  summarise(qtd = n()) %>%
  ungroup() %>%
  arrange(desc(qtd)) %>%
  head(1) %>% 
  select(ORGSUP_EXERCICIO)

paste("Moda ORGSUP_LOTACAO: ", MODA_ORGSUP_LOTACAO)


paste("Moda ORGSUP_EXERCICIO: ", MODA_ORGSUP_EXERCICIO)


print("Há diferença entre as MODAS")

