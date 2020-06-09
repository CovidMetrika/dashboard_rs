require(ggplot2)
require(tidyr)
require(dplyr)
#------------------------------------------------ 

# erros observados no banco de dados: 

# variavél sexo possui um escrito 'femininio'

dados_covid_rs$sexo <- ifelse(dados_covid_rs$sexo == 'Femininio', 'Feminino', dados_covid_rs$sexo)
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
gg <- function(input = options_var[1], cluster = options_cluster[1], var = 'sexo'){
  
  dados <- dados_covid_rs
  
  # definir se o banco é de sexo ou faixa etária:
  if(var != 'sexo'){
    names(dados)[5] <- 'variavel'
  } else {
    names(dados)[4] <- 'variavel'
  }
  
  
  # definir se o banco é de municipio ou regiao covid:
  if(cluster != options_cluster[1]){
    temp <- dados %>% 
      select(regiao_covid, variavel, evolucao) 
  } else {
    temp <- dados %>% 
      select(municipio, variavel, evolucao)
  }
  
  # agregar os dados: 
  df <- data.frame(table(temp$municipio, temp$variavel, temp$evolucao))
  names(df) <- c(names(temp), 'Contagem')
  
  # chamar o banco com as populações 
  pops <- readxl::read_excel(here::here('dados', 'FEE2017.xlsx'))
  
  if(var != 'sexo'){
    pops <- pops %>% 
      select(-sexo)
  } else {
    pops <- pops %>% 
      select(-faixa)
  }
  
  names(pops)[4] <- 'variavel'
  
  # arrumar o banco de dados para ter todas as possíveis combinações 
  # mesmo que isso seja zero. 
  
  aux <- expand.grid(municipio = unique(pops$municipio), 
                     variavel = unique(pops$variavel), 
                     evolucao = as.vector(na.omit(unique(dados_covid_rs$evolucao)))
                     )
  
  # acima não to considerando os casos em que a evolução são casos NA, se quiser
  # considerar a gente pode retirar o as.vector e o na.omit
  
  # talvez deixar especificado no banco de dados que existem um número x de 
  # caso que não estão sendo considerados por causa que não sabemos a evolução 
  
  # eu tenho o banco de pop pelas duas variaveis, aqui eu junto porque 
  # eu to trabalhando só com uma delas:
  
  pops <- pops %>% 
    group_by(codigo, municipio, variavel) %>%
    summarise(populacao = sum(populacao))
  
  
  # pegando a populacao e juntando no banco com as contagens:
  df <- aux %>% 
    left_join(df, by = c("municipio", "variavel", "evolucao")) %>% 
    mutate(Contagem = replace_na(Contagem, 0))
  
  df <- df %>%
    left_join(pops, by = c("municipio", "variavel")) %>% 
    mutate(contagem = Contagem) %>%
    select(codigo, municipio, populacao, variavel, evolucao, contagem)
  
  # mudar a variável de evolução para colunas, pra poder fazer os gráficos 
  # separado pras variaveis (4)
  
  df$contagem <- as.numeric(df$contagem)
  
  df <- df %>% 
    tidyr::spread(key = evolucao, value = contagem)
  
  # juntar CURA + EM ACOMPANHAMENTO = CASOS CONFIRMADOS 
  
  names(df)[5:7] <- c('cura', 'acompanhamento', 'mortes')
  
  dados <- df %>% 
    mutate(casos = cura + acompanhamento) %>% 
    mutate(taxa = casos/populacao*100000) %>% 
    mutate(letalidade = mortes/casos) %>% 
    select(codigo, municipio, variavel, casos, taxa, mortes, letalidade)
  
  dados$letalidade <- ifelse(is.na(dados$letalidade) == 'TRUE', 0, round(dados$letalidade, 3))
  
}


ggplot(temp, aes(y = variavel)) +
  geom_bar(aes(fill = sexo), position = position_stack(reverse = TRUE)) +
  theme(legend.position = "top")






