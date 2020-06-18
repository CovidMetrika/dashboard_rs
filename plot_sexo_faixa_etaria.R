require(ggplot2)
require(tidyr)
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
options_dados <- c('Casos Confirmados', 'Óbitos', 'Recuperados', 'Em Acompanhamento')

# cluster é o tipo de agrupamento: 
options_cluster <- c('Município', 'Região COVID')

# variável: 
options_var <- c('Sexo', 'Faixa Etária')

# formato, se taxa, numero abs ou a tabela com os dados
options_format <- c('Números Absolutos', 'Taxa', 'Tabela')

gg <- function(input, cluster, var, formato){
  
  dados <- dados_covid_rs
  
  # definir se o banco é de sexo ou faixa etária:
  if(var != options_var[1]){
    names(dados)[5] <- 'variavel'
    
    dados$variavel[which(dados$variavel %in% c("<1","01 a 04"))] <- "00 a 04"
    dados$variavel[which(dados$variavel %in% c("05 a 09"))] <- "05 a 09"
    
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
  
  names(temp)[1] <- 'regiao'
  
  # agregar os dados: 
  df <- data.frame(table(temp$regiao, temp$variavel, temp$evolucao))
  names(df) <- c(names(temp), 'Contagem')
  
  # chamar o banco com as populações 
  pops <- readxl::read_excel(here::here('dados', 'FEE2017.xlsx'))
  
  # selecionar so os dados necessarios:
  if(var != options_var[1]){
    pops <- pops %>% 
      select(-sexo)
    
    pops$faixa[which(pops$faixa %in% c("0 a 04"))] <- "00 a 04"
    pops$faixa[which(pops$faixa %in% c("5 a 09"))] <- "05 a 09"
    
  } else {
    pops <- pops %>% 
      select(-faixa)
  }
  
  names(pops)[4] <- 'variavel'
  
  # selecionar só a regiao correta:
  if(cluster != options_cluster[1]){
    pops <- pops %>% 
      select(-municipio) %>%
      select(populacao, variavel, reg_covid)
    
  } else {
    pops <- pops %>% 
      select(-reg_covid) %>%
      select(populacao, variavel, municipio)
  }
  
  
  names(pops)[3] <- 'regiao'
  
  # eu tenho o banco de pop pelas duas variaveis, aqui eu junto porque 
  # eu to trabalhando só com uma delas:
  
  pops <- pops %>% 
    group_by(regiao, variavel) %>%
    summarise(populacao = sum(populacao))

  
  # arrumar o banco de dados para ter todas as possíveis combinações 
  # mesmo que isso seja zero. 
  
  aux <- expand.grid(regiao = unique(pops$regiao), 
                     variavel = unique(pops$variavel), 
                     evolucao = as.vector(na.omit(unique(dados_covid_rs$evolucao)))
                     )
  
  # acima não to considerando os casos em que a evolução são casos NA, se quiser
  # considerar a gente pode retirar o as.vector e o na.omit
  
  # talvez deixar especificado no banco de dados que existem um número x de 
  # caso que não estão sendo considerados por causa que não sabemos a evolução 
  
  # pegando a populacao e juntando no banco com as contagens:
  df <- aux %>% 
    left_join(df, by = c("regiao", "variavel", "evolucao")) %>% 
    mutate(Contagem = replace_na(Contagem, 0))
  
  df <- df %>%
    left_join(pops, by = c("regiao", "variavel")) %>% 
    mutate(contagem = Contagem) %>%
    select(regiao, variavel, evolucao, populacao, contagem)
  
  # mudar a variável de evolução para colunas, pra poder fazer os gráficos 
  # separado pras variaveis (4)
  
  df$contagem <- as.numeric(df$contagem)
  
  df <- df %>% 
    tidyr::spread(key = evolucao, value = contagem)
  
  # juntar CURA + EM ACOMPANHAMENTO + OBITOS = CASOS CONFIRMADOS 
  
  names(df)[4:6] <- c('cura', 'acompanhamento', 'mortes')
  
  dados <- df %>% 
    mutate(casos = cura + acompanhamento + mortes) %>% 
    mutate(taxa_casos = casos/populacao*100000) %>% 
    mutate(taxa_cura = cura/populacao*100000) %>% 
    mutate(taxa_acomp = acompanhamento/populacao*100000) %>% 
    mutate(taxa_morte = mortes/populacao*100000) %>% 
    mutate(letalidade = mortes/casos*100) %>% 
    select(regiao, variavel, casos, taxa_casos, cura, taxa_cura, acompanhamento, taxa_acomp, 
           mortes, taxa_morte, letalidade)
  
  # deixando os dados ajustados: 
  dados$taxa_cura <- round(dados$taxa_cura, 3)
  dados$taxa_acomp <- round(dados$taxa_acomp, 3)
  dados$taxa_casos <- round(dados$taxa_casos, 3)
  dados$taxa_morte <- round(dados$taxa_morte, 3)
  dados$letalidade <- round(dados$letalidade, 3)
  
  # agora que o banco de dados tá pronto 
  # agora faz o filtro pra escolher qual variável quer enxergar 
  
  if(input == options_dados[1]){
    df <- dados %>%
      select(regiao, variavel, casos, taxa_casos)
  } else { 
    if(input == options_dados[2]) {
      df <- dados %>%
        select(regiao, variavel, mortes, taxa_morte)
    } else {
      if(input == options_dados[3]) { 
        df <- dados %>%
          select(regiao, variavel, cura, taxa_cura)
      }
      df <- dados %>%
        select(regiao, variavel, acompanhamento, taxa_acomp)
    }
  }
  
  df <- df %>% 
    mutate(regiao = tools::toTitleCase(tolower(df$regiao)))
    
  names(df) <- c('regiao', 'variavel', 'abs', 'taxa')
  
  df$variavel <- factor(df$variavel, levels = unique(df$variavel))
  df$regiao <- factor(df$regiao, levels = unique(df$regiao))
  df <- df[with(df, order(regiao, variavel)),]
  
  
  plot_abs <- ggplot(df, aes(x = abs, y = regiao)) +
    geom_col(aes(fill = variavel), 
             position = position_stack(reverse = TRUE)) +
    theme(legend.position = "top") + 
    labs(y = NULL, 
         x = paste0(options_dados[which(input == options_dados)]), 
         fill = paste0(tools::toTitleCase(var), ':')) + 
    theme(legend.position = 'bottom')
  
  plot_tx <- ggplot(df, aes(x = taxa, y = regiao)) +
    geom_col(aes(fill = variavel), 
             position = position_stack(reverse = TRUE)) +
    theme(legend.position = "top") + 
    labs(y = NULL, 
         x = paste0(options_dados[which(input == options_dados)], '*'), 
         fill = paste0(tools::toTitleCase(var), ':'), 
         caption = '* por 100.000 habitantes') + 
    theme(legend.position = 'bottom')
  
  table <- df
  names(table) <- c(cluster, var , input, paste0('Taxa de ', input, ' (Por 100 mil hab.)'))
  
  results <- if(formato == options_format[1]){
    plot_abs
  } else {
    if(formato == options_format[2]){
      plot_tx
    } else {
      table
    }
  }
  
  print(results)
  
} # final da função


gg(input = options_dados[3], 
   cluster = options_cluster[2], 
   var = options_var[1], 
   formato = options_format[1])

