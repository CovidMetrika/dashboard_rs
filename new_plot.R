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

# default é casos confirmados e por município
gg_sex <- function(input = options_var[1], cluster = options_cluster[1]){
  
  # definir se o banco é de municipio ou regiao covid:
  
  if(cluster != options_cluster[1]){
    
    temp <- dados_covid_rs %>% 
      select(regiao_covid, sexo, evolucao) 
    
  } else {
     
    temp <- dados_covid_rs %>% 
      select(municipio, sexo, evolucao)
  }
  
  names(temp)[1] <- 'variavel'
  
  # agregar os dados: 
  df <- data.frame(table(temp$variavel, temp$sexo, temp$evolucao))
  names(df) <- c(names(temp), 'Contagem')
  
  
  
}


ggplot(temp, aes(y = variavel)) +
  geom_bar(aes(fill = sexo), position = position_stack(reverse = TRUE)) +
  theme(legend.position = "top")







