library(tidyverse)
library(rgeos)
library(httr)
library(jsonlite)
library(readxl)
library(sf)
library(jsonlite)
library(abjutils)
library(lubridate)

# esse script serve para organizar todos objetos de banco de dados que utilizarei no aplicativo
# são 3 principais:
# - o de casos do rs obtido através da ses agora
# - dois shapefiles do rs(municipio e mesoregião) com dados sobre os casos de corona(confirmaodos, 
# incidencia, mortes, etc)
# - um arquivo com latitudes e longitudes das cidades/hospitais e seus leitos
# - dois shapefiles do rs(municipio e mesoregião) com dados sobre os leitos

# lendo shapefiles RS municipios

mapa_rs_shp <- sf::st_read("dados/shapefiles/43MUE250GC_SIR.shp", quiet = TRUE) %>%
  mutate(municipio = str_to_title(NM_MUNICIP),
         codigo_ibge = as.character(CD_GEOCMU))

mapa_rs_shp[mapa_rs_shp$municipio=="Westfalia","municipio"] <- "Westfália"
mapa_rs_shp[mapa_rs_shp$municipio=="Vespasiano Correa","municipio"] <- "Vespasiano Corrêa"

# lendo shapefiles regiões covid

mapa_reg_rs <- sf::st_read("dados/shapefiles/regioes_covid/rs_regiao_20_saude_2020.shp", 
                           quiet = TRUE) %>%
  mutate(codigo_regiao_covid = 1:20)

# criando um objeto para atribuir códigos ibge às cidades

codigos_cidades <- tibble(municipio = mapa_rs_shp$municipio, codigo_ibge = mapa_rs_shp$codigo_ibge)

codigos_cidades_sem_acento <- codigos_cidades %>%
  mutate(municipio = rm_accent(municipio))

# lendo mesoregiões e região covid

regiao_covid_mun <- read_csv("dados/shapefiles/regioes_covid/banco_mun_regiao_covid.csv") %>%
  mutate(codigo_ibge = as.character(codigo_ibge))

rs_mesoregiao_microregiao <- read_csv("dados/mesoregiao/rs_mesoregiao_microregiao.csv") %>%
  mutate(municipio = str_to_title(municipio),
         mesorregiao = str_to_title(mesorregiao)) %>%
  left_join(codigos_cidades, by = "municipio") %>%  # atribuindo o código
  select(-municipio)

# lendo dados da SES-RS

dados_ses <- NULL
dados_ses <- try(read_csv2("http://ti.saude.rs.gov.br/covid19/download", 
                           locale = readr::locale(encoding = "latin1")))


path <- "http://ti.saude.rs.gov.br/covid19/download"
request <- GET(url = path)

if(request$status_code == 404) {
  dados_ses <- read_csv("dados/covid/ses_reserva.csv")
} else {
  write_csv(dados_ses,"dados/covid/ses_reserva.csv")
}

names(dados_ses) <- c("codigo_ibge_6_digitos","municipio","codigo_regiao_covid","regiao_covid",
                      "sexo","faixa_etaria","tipo_teste",
                      "data_confirmacao","data_sintomas","data_evolucao","evolucao","hospitalizacao",
                      "sintoma_febre","sintoma_tosse",
                      "sintoma_garganta","sintoma_dispneia","sintomas_outros","comorbidades")

dados_covid_rs <- dados_ses %>%
  mutate(data_confirmacao = as_date(data_confirmacao, format = "%d/%m/%y"),
         data_sintomas = as_date(data_sintomas, format = "%d/%m/%y"),
         data_evolucao = as_date(data_evolucao, format = "%d/%m/%y"),
         municipio = str_to_title(municipio))

# arrumando os 3 municipios com inconssistências nos nomes

dados_covid_rs[dados_covid_rs$municipio=="Westfalia","municipio"] <- "Westfália"
dados_covid_rs[dados_covid_rs$municipio=="Vespasiano Correa","municipio"] <- "Vespasiano Corrêa"
dados_covid_rs[dados_covid_rs$municipio=="Santana Do Livramento","municipio"] <- "Sant'ana Do Livramento"

# pegando o código ibge de 7 dígitos pelo nome do municipio

dados_covid_rs <- dados_covid_rs %>%
  left_join(codigos_cidades, by = c("municipio")) %>%
  left_join(rs_mesoregiao_microregiao, by = "codigo_ibge") %>%
  select(-codigo_ibge_6_digitos)

# pegando os dados de estimativas populacionais dos municipios

estimativas_ibge <- read_excel("dados/mesoregiao/estimativas_populacionais_ibge.xls", 
                               sheet = "Municípios", skip = 1, n_max = 5570) %>%
  filter(UF == "RS") %>%
  mutate(codigo_ibge = str_c(`COD. UF`,`COD. MUNIC`),
         populacao_estimada_municipio = `POPULAÇÃO ESTIMADA`) %>%
  select(codigo_ibge, populacao_estimada_municipio)


pop_regiao <- estimativas_ibge %>%
  left_join(regiao_covid_mun, by = "codigo_ibge") %>%
  group_by(regiao_covid) %>%
  summarise(populacao_estimada_regiao_covid = sum(as.numeric(populacao_estimada_municipio)))

dados_covid_rs <- dados_covid_rs %>%
  left_join(estimativas_ibge, by = c("codigo_ibge")) %>%
  left_join(pop_regiao, by = c("regiao_covid"))

# adicionando as datas de recuperação para os casos não hospitalizados e sem data

dados_covid_rs <- dados_covid_rs %>%
  mutate(data_evolucao = ifelse(evolucao == "CURA", ifelse(hospitalizacao == "Nao", ifelse(is.na(data_evolucao),data_sintomas+days(14),data_evolucao),data_evolucao),data_evolucao)) %>%
  mutate(data_evolucao = as_date(data_evolucao))

# fazendo um banco de join para o mapa do rs

dados_covid_join <- dados_covid_rs %>%
  mutate(obitos = ifelse(evolucao == "OBITO", 1, 0),
         acompanhamento = ifelse(evolucao == "EM ACOMPANHAMENTO", 1, 0),
         recuperados = ifelse(evolucao == "CURA", 1, 0)) %>% 
  group_by(municipio, codigo_ibge) %>%
  summarise(confirmados = n(), confirmados_taxa = n()*100000/as.numeric(first(populacao_estimada_municipio)),
            obitos = sum(obitos, na.rm = T), obitos_taxa = sum(obitos, na.rm = T)*100000/as.numeric(first(populacao_estimada_municipio)), 
            acompanhamento = sum(acompanhamento, na.rm = T), acompanhamento_taxa  = sum(acompanhamento, na.rm = T)*100000/as.numeric(first(populacao_estimada_municipio)),
            recuperados = sum(recuperados, na.rm = T), recuperados_taxa = sum(recuperados, na.rm = T)*100000/as.numeric(first(populacao_estimada_municipio)),
            populacao_estimada_municipio = first(populacao_estimada_municipio)) %>%
  ungroup() %>%
  select(-c(municipio))

dados_covid_join_reg <- dados_covid_rs %>%
  mutate(obitos = ifelse(evolucao == "OBITO", 1, 0),
         acompanhamento = ifelse(evolucao == "EM ACOMPANHAMENTO", 1, 0),
         recuperados = ifelse(evolucao == "CURA", 1, 0)) %>% 
  group_by(regiao_covid, codigo_regiao_covid) %>%
  summarise(confirmados = n(), confirmados_taxa = n()*100000/as.numeric(first(populacao_estimada_municipio)),
            obitos = sum(obitos, na.rm = T), obitos_taxa = sum(obitos, na.rm = T)*100000/as.numeric(first(populacao_estimada_municipio)), 
            acompanhamento = sum(acompanhamento, na.rm = T), acompanhamento_taxa  = sum(acompanhamento, na.rm = T)*100000/as.numeric(first(populacao_estimada_municipio)),
            recuperados = sum(recuperados, na.rm = T), recuperados_taxa = sum(recuperados, na.rm = T)*100000/as.numeric(first(populacao_estimada_municipio)),
            populacao_estimada_regiao_covid = sum(as.numeric(populacao_estimada_regiao_covid))) %>%
  ungroup()

# fazendo o join dos dados covid ao shp

# shp municipio

dados_mapa_rs <- mapa_rs_shp %>%
  left_join(dados_covid_join, by = c("codigo_ibge")) %>%
  left_join(regiao_covid_mun, by = "codigo_ibge") %>%
  mutate(codigo_ibge = factor("CD_GEOCMU", levels = levels(mapa_rs_shp$CD_GEOCMU)))

# shp mesoregiao

dados_mapa_rs_reg <- mapa_reg_rs %>%
  left_join(dados_covid_join_reg, by = c("codigo_regiao_covid"))

#################################
# lendo leitos UTI do site da SES
#################################

hospital_municipio <- read_csv("dados/leitos/outros/hospital_municipio.csv") %>%
  mutate(codigo_ibge = as.character(codigo_ibge))

dados_cnes <- read_csv("dados/leitos/outros/base_cnes_atualizada.csv") %>%
  select(CNES, LATITUDE, LONGITUDE)

pasta <- "dados/leitos/"
arquivos <- list.files(pasta, pattern = ".csv")
caminhos <- str_c(pasta, arquivos)

# arrumando problema da base deles em que em vez da coluna com dados de hospitais ter o nome "Hosital"
# ta com nome municipios(começou no dia 04/05/2020 e ta indo até hj 07/05/2020)
# ah e também a coluna Cód q possui o código cnes do hospital está escrita como Cód IBGE agr
# incrivel como sempre conseguem arranjar algum novo problema com esses dados da SES

arquivos_troca_nome <- c("leitos_dados_ses_05_05.csv","leitos_dados_ses_06_05.csv","leitos_dados_ses_07_05.csv",
                         "leitos_dados_ses_08_05.csv","leitos_dados_ses_09_05.csv","leitos_dados_ses_10_05.csv",
                         "leitos_dados_ses_11_05.csv","leitos_dados_ses_12_05.csv","leitos_dados_ses_13_05.csv",
                         "leitos_dados_ses_14_05.csv","leitos_dados_ses_15_05.csv","leitos_dados_ses_16_05.csv",
                         "leitos_dados_ses_17_05.csv","leitos_dados_ses_18_05.csv","leitos_dados_ses_19_05.csv",
                         "leitos_dados_ses_20_05.csv","leitos_dados_ses_21_05.csv","leitos_dados_ses_22_05.csv",
                         "leitos_dados_ses_23_05.csv","leitos_dados_ses_24_05.csv","leitos_dados_ses_25_05.csv",
                         "leitos_dados_ses_26_05.csv","leitos_dados_ses_27_05.csv","leitos_dados_ses_28_05.csv",
                         "leitos_dados_ses_29_05.csv","leitos_dados_ses_30_05.csv","leitos_dados_ses_31_05.csv",
                         "leitos_dados_ses_01_06.csv","leitos_dados_ses_02_06.csv","leitos_dados_ses_03_06.csv",
                         "leitos_dados_ses_04_06.csv","leitos_dados_ses_05_06.csv","leitos_dados_ses_06_06.csv",
                         "leitos_dados_ses_07_06.csv","leitos_dados_ses_08_06.csv","leitos_dados_ses_09_06.csv",
                         "leitos_dados_ses_10_06.csv")
caminhos_troca_nome <- str_c(pasta,arquivos_troca_nome)

arruma_nome <- map(caminhos_troca_nome, read_csv) %>%
  map(dplyr::mutate, Cód = `Cód IBGE`, Hospital = Município_1) %>%
  map(dplyr::select, -(`Taxa Ocupação`)) %>%
  bind_rows() 

caminhos <- caminhos[!(caminhos %in% caminhos_troca_nome)]

leitos_uti <- map(caminhos, read_csv) %>%
  map(dplyr::select, -(`Taxa Ocupação`)) %>%
  bind_rows() %>%
  bind_rows(arruma_nome) %>% # adicionando arquivos bugados
  left_join(hospital_municipio, by = c("Cód" = "cnes")) %>%
  left_join(regiao_covid_mun, by = "codigo_ibge") %>%
  mutate(data_atualizacao = lubridate::as_date(`Últ Atualização`, format = "%d/%m/%Y",  tz = "America/Sao_Paulo"),
         Hospital = str_to_title(Hospital)) %>%
  distinct(`Cód`, data_atualizacao, .keep_all = T) %>%
  select(data_atualizacao = data_atualizacao, cnes = Cód, hospital = Hospital, codigo_ibge = codigo_ibge, municipio, leitos_internacoes = Pacientes,
         leitos_total = Leitos, leitos_covid = Confirmados, regiao_covid, codigo_regiao_covid, data_hora_atualizacao = `Últ Atualização`) %>%
  left_join(dados_cnes, by = c("cnes" = "CNES")) %>%
  filter(data_atualizacao > "2020-04-27") %>%
  select(-data_hora_atualizacao) %>%
  arrange(data_atualizacao)

# resolvendo problema da mudança de nome no hospital de uruguaiana
# nome antigo: Santa Casa De Uruguaiana
# nome novo: Hospital Santa Casa De Uruguaiana

leitos_uti <- leitos_uti %>%
  mutate(hospital = ifelse(cnes == 2248190,"Hospital Santa Casa De Uruguaiana",hospital))

# resolvendo problema dos dados incompletos
# pegando dados de dias anteriores para os dias sem dado

# colocando as datas como se fosse colunas para verificar quais os dados que estão faltando para cada hospital

aux_total <- leitos_uti %>%
  select(-c(leitos_internacoes,leitos_covid)) %>%
  pivot_wider(names_from = data_atualizacao, values_from = leitos_total) %>%
  as.data.frame()

aux_internados <- leitos_uti %>%
  select(-c(leitos_total,leitos_covid)) %>%
  pivot_wider(names_from = data_atualizacao, values_from = leitos_internacoes) %>%
  as.data.frame()

aux_covid <- leitos_uti %>%
  select(-c(leitos_total,leitos_internacoes)) %>%
  pivot_wider(names_from = data_atualizacao, values_from = leitos_covid) %>%
  as.data.frame()

# e a partir dai caso um dado esteja sem atualização para aquele dia, então pega-se o dado do dia anterior
# foi bem hardcore mas acho que deu tudo certo, mas acho que seria importante avisar disso em algum momento,
# que estamos supondo a mesma situação do dia anterior para aqueles dias em que não temos dados(tudo a partir de quando começamos a coletar os dados no dia 28/04/20)

for (i in 10:ncol(aux_total)) {
  aux_total[,i] <- ifelse(is.na(aux_total[,i]),aux_total[,(i-1)],aux_total[,i])
  aux_internados[,i] <- ifelse(is.na(aux_internados[,i]),aux_internados[,(i-1)],aux_internados[,i])
  aux_covid[,i] <- ifelse(is.na(aux_covid[,i]),aux_covid[,(i-1)],aux_covid[,i])
}

aux_todos <- aux_total %>%
  pivot_longer(-names(aux_total)[1:8],names_to = "data_atualizacao", values_to = "leitos_total")

aux_internados <- aux_internados %>%
  pivot_longer(-names(aux_internados)[1:8],names_to = "data_atualizacao", values_to = "leitos_internacoes") %>%
  select(cnes,data_atualizacao,leitos_internacoes)

aux_covid <- aux_covid %>%
  pivot_longer(-names(aux_covid)[1:8],names_to = "data_atualizacao", values_to = "leitos_covid") %>%
  select(cnes,data_atualizacao,leitos_covid)

leitos_uti <- aux_todos %>%
  left_join(aux_internados, by = c("cnes","data_atualizacao")) %>%
  left_join(aux_covid, by = c("cnes","data_atualizacao")) %>%
  mutate(data_atualizacao = lubridate::as_date(data_atualizacao)) %>%
  mutate(lotacao = ifelse(leitos_total == 0, NA, leitos_internacoes/leitos_total),
         leitos_disponiveis = leitos_total - leitos_internacoes)


leitos_join_mun <- leitos_uti %>%
  group_by(cnes) %>%
  filter(data_atualizacao == max(data_atualizacao)) %>%
  group_by(codigo_ibge) %>%
  summarise(leitos_internacoes = sum(leitos_internacoes), leitos_total = sum(leitos_total), leitos_covid = sum(leitos_covid),
            lotacao = ifelse(sum(leitos_total)==0, NA, sum(leitos_internacoes)/sum(leitos_total)), leitos_disponiveis = leitos_total - leitos_internacoes)

leitos_join_reg <- leitos_uti %>%
  group_by(cnes) %>%
  filter(data_atualizacao == max(data_atualizacao)) %>%
  group_by(codigo_regiao_covid,regiao_covid) %>%
  summarise(leitos_internacoes = sum(leitos_internacoes), leitos_total = sum(leitos_total), leitos_covid = sum(leitos_covid),
            lotacao = ifelse(sum(leitos_total)==0, NA, sum(leitos_internacoes)/sum(leitos_total)), leitos_disponiveis = leitos_total - leitos_internacoes)
  

# fazendo o join dos dados covid ao shp

# shp municipio

leitos_mapa_mun_rs <- mapa_rs_shp %>%
  left_join(leitos_join_mun, by = c("codigo_ibge")) %>%
  left_join(regiao_covid_mun, by = "codigo_ibge") %>%
  mutate(codigo_ibge = factor("CD_GEOCMU", levels = levels(mapa_rs_shp$CD_GEOCMU)))

# shp mesoregiao

leitos_mapa_reg_rs <- mapa_reg_rs %>%
  left_join(leitos_join_reg, by = "codigo_regiao_covid")

# lendo arquivo com semana epidemoilógica para adicionar ao banco

semana <- read_csv("dados/semana_epidemio_dia.csv")

dados_covid_rs <- dados_covid_rs %>%
  left_join(semana, by = c("data_confirmacao" = "dia")) %>%
  mutate(semana_epidemiologica_confirmacao = semana_epidemiologica) %>%
  select(-semana_epidemiologica) %>%
  left_join(semana, by = c("data_sintomas" = "dia")) %>%
  mutate(semana_epidemiologica_sintomas = semana_epidemiologica) %>%
  select(-semana_epidemiologica) %>%
  left_join(semana, by = c("data_evolucao" = "dia")) %>%
  mutate(semana_epidemiologica_evolucao = semana_epidemiologica) %>%
  select(-semana_epidemiologica)

leitos_uti <- leitos_uti %>%
  left_join(semana, by = c("data_atualizacao" = "dia"))

# deixando só os objetos essenciais

rm(list=setdiff(ls(),c("leitos_mapa_mun_rs","leitos_mapa_reg_rs","leitos_uti","dados_mapa_rs_reg",
                       "dados_mapa_rs","dados_covid_rs","pop_regiao")))

#------------------------------------------------ 

# erros observados no banco de dados: 

# variavél sexo possui um escrito 'femininio'

dados_covid_rs$sexo <- ifelse(dados_covid_rs$sexo == 'Femininio', 'Feminino', dados_covid_rs$sexo)

