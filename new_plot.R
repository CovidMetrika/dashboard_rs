require(ggplot2)
require(dplyr)

# ----------------------------------------------
# F I L T R O S : 
# ----------------------------------------------
#---------
# selecione a variavel de interesse 

# confirmados 
# confirmados por 100 mil habitantes 
# óbitos 
# letalidade 
#---------
# selecione o tipo de agrupamento:

# por municipio 
# por região covid

# ----------------------------------------------

# input é o tipo de variavel:
options_var <- c('Casos Confirmados', 'Casos Confirmados por 100 mil hab.', 'Óbitos', 'Letalidade')

# cluster é o tipo de agrupamento: 
options_cluster <- c('Município', 'Região COVID')

# ----------------------------------------------
# variável sexo: 

# 


gg_sex <- function(input){
  
}

temp <- dados_covid_rs %>% 
  select(municipio, regiao_covid, sexo, )