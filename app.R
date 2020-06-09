library(shiny)
library(tidyverse)
library(sf)
library(stringr)
library(readxl)
library(leaflet)
library(RColorBrewer)
library(DT)
library(leafpop)
library(readxl)
library(rgeos) #https://statistique-et-logiciel-r.com/premiers-pas-en-cartographie-avec-r/
library(viridis)
library(rainbow)
library(httr)
library(curl)
library(abjutils)
library(shinydashboard)
library(plotly)
library(scales)
library(shinydashboardPlus)
library(shinyEffects)
library(lubridate)
library(ggthemes)
library(shinyalert)
library(shinyBS)

options(OutDec = ",") # Muda de ponto para virgula nos decimais! 

# rodando o script de data_wrangling para obtenção dos dados

source("data_wrangling.R", encoding = "UTF-8")

# Criando variáveis globais

opcoes <- list(
  "confirmados" = list("cor" = "#dd4b39", "paleta" = "Reds", "texto" = "Confirmados"),
  "incidencia" = list("cor" = "#dd4b39", "paleta" = "Reds", "texto" = "Incidência"),
  "obitos" = list("cor" = "#605ca8", "paleta" = "Purples", "texto" = "Óbitos"),
  "mortalidade" = list("cor" = "#605ca8", "paleta" = "Purples", "texto" = "Mortalidade"),
  "recuperados" = list("cor" = "#0073b7", "paleta" = "Blues", "texto" = "Recuperados"),
  "acompanhamento" = list("cor" = "#ff851b", "paleta" = "Oranges", "texto" = "Em acompanhamento")
)

####
# Funções criadas
####

# criando função personalizada para a caixa do usuário 



#data_hora_atual <- str_c("Última atualização em ",format(with_tz(httr::GET("http://www.google.com/")$date, "America/Sao_Paulo"), "%H:%M %d/%m/%Y"))

##############################################################################################
# Aplicativo
##############################################################################################


header <- dashboardHeader(
  title = "Dados COVID19 no Rio Grande do Sul",
  titleWidth = 500
)

sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("COVID-19 RS", tabName = "mapa_covid_rs"),
    menuItem("Leitos UTI - Adulto RS", tabName = "mapa_leitos_rs"),
    menuItem("Fonte de dados", tabName = "fonte"),
    menuItem("CovidMetrika", tabName = "sobre")
  ),
  width = 180
)


body <- dashboardBody(
  tabItems(
    tabItem("mapa_covid_rs",
            fluidPage(
              # incluindo o script do google analytics para acompanhamento de dados
              
              tags$head(includeHTML(("google_analytics.html"))),
              
              # para a mensagem de popup
              
              useShinyalert(),
              
              titlePanel(
                fluidRow(
                  column(
                    width = 6,
                    h1("COVID-19 Rio Grande do Sul"),
                    # h5(em(data_hora_atual))
                  ),
                  column(
                    tags$img(src = "logos.png", 
                             width = "100%"),
                    width = 6
                  )
                )
              ),
              
              # reescrevendo as cores default para as que eu quero nas boxes de óbitos cartório
              tags$style(".small-box.bg-lime { background-color: #757474 !important; color: #FFFFFF !important; }"),
              
              fluidRow(
                valueBoxOutput("box_conf", width = 2),
                valueBoxOutput("box_inci", width = 2),
                valueBoxOutput("box_obit", width = 2),
                valueBoxOutput("box_mort", width = 2),
                valueBoxOutput("box_recu", width = 2),
                valueBoxOutput("box_acom", width = 2)
              ),
              bsModal("modal_incidencia", "Incidência", "box_inci", size = "small", "Número de casos confirmados de COVID-19 por 100 000 habitantes na população de determinado espaço geográfico"),
              bsModal("modal_mortalidade", "Mortalidade", "box_mort", size = "small", "Número de óbitos confirmados por COVID-19 por 100 000 habitantes na população de determinado espaço geográfico"),
              setShadow(id = "box_inci", class = "small-box"),
              setShadow(id = "box_mort", class = "small-box"),
              fluidRow(
                column(
                  width = 3,
                  h3("Selecione a variável de interesse"),
                  radioButtons("var_covid",
                               label = NULL,
                               choices = list("Confirmados" = "confirmados","Incidência" = "incidencia","Óbitos" = "obitos","Mortalidade" = "mortalidade",
                                              "Recuperados" = "recuperados", "Em acompanhamento" = "acompanhamento"),
                               selected = "confirmados",
                               inline = T)
                ),
                column(
                  width = 3,
                  h3("Selecione o tipo de agrupamento"),
                  radioButtons("agrup_covid",
                               label = NULL,
                               choices = list("Municípios" = "municipio", "Regiões COVID" = "regiao_covid"),
                               selected = "municipio",
                               inline = T),
                ),
                column(
                  width = 6,
                  h3("Digite as regiões de interesse"),
                  selectizeInput("filtro_covid",
                                 label = NULL,
                                 choices = levels(as.factor(dados_covid_rs$regiao_covid)),
                                 selected = levels(as.factor(dados_covid_rs$regiao_covid)),
                                 multiple = T,
                                 width = "100%"),
                )
              ),
              fluidRow(
                column(
                  width = 7,
                  mainPanel(
                    leafletOutput("mapa_covid", height = "500px"),
                    HTML("<br><br><br>"), # para dar um espaço entre os gráficos
                    width = 12
                  )
                ),
                column(
                  width = 5,
                  box(
                    plotlyOutput("grafico_covid", height = "500px"),
                    width = 12
                  )
                )
              ),
              fluidRow(
                column(
                  width = 12,
                  uiOutput("ui_serie_covid")
                ),
                column(
                  width = 12,
                  uiOutput("ui_filtro_quadradinhos")
                ),
                column(
                  width = 9,
                  box(
                    dataTableOutput("table_covid", height = "480px"),
                    width = 12
                  )
                )
              )
            )
    ),
    tabItem("mapa_leitos_rs",
            fluidPage(
              # pegando de alguma forma htmlzada a versão mais recente dos ícones font awesome
              
              tags$script(src = "https://kit.fontawesome.com/ab15947b75.js", crossorigin="anonymous"), 
              titlePanel(
                fluidRow(
                  column(
                    width = 6,
                    h1("Leitos UTI - Adulto no Rio Grande do Sul"),
                    # h5(em(data_hora_atual))
                  ),
                  column(
                    tags$img(src = "logos.png", 
                             width = "100%"),
                    width = 6
                  )
                )
              ),
              fluidRow(
                valueBoxOutput("box_tot", width = 3),
                valueBoxOutput("box_int", width = 3),
                valueBoxOutput("box_lot", width = 3),
                valueBoxOutput("box_cov", width = 3)
              ),
              fluidRow(
                column(
                  width = 3,
                  h3("Selecione a variável de interesse"),
                  radioButtons("var_leitos",
                               label = NULL,
                               choices = list("Leitos totais" = "leitos_total","Leitos disponíveis" = "leitos_disponiveis","Lotação" = "lotacao", "Leitos ocupados COVID-19" = "leitos_covid"),
                               selected = "leitos_disponiveis",
                               inline = T)
                ),
                column(
                  width = 4,
                  h3("Selecione o tipo de agrupamento"),
                  radioButtons("agrup_leitos",
                               label = NULL,
                               choices = list("Hospital" = "hospital", "Municípios" = "municipio", "Regiões COVID" = "regiao_covid"),
                               selected = "municipio",
                               inline = T),
                ),
                column(
                  width = 5,
                  h3("Digite as regiões de interesse"),
                  selectizeInput("filtro_leitos",
                                 label = NULL,
                                 choices = levels(as.factor(leitos_uti$regiao_covid)),
                                 selected = levels(as.factor(leitos_uti$regiao_covid)),
                                 multiple = T,
                                 width = "100%"),
                ),
                column(
                  width = 7,
                  mainPanel(
                    leafletOutput("mapa_leitos", height = "500px"),
                    HTML("<br><br><br>"), # para dar um espaço entre os gráficos
                    width = 12
                  )
                ),
                column(
                  width = 5,
                  plotlyOutput("grafico_leitos", height = "500px")
                ),
                uiOutput("ui_serie_leitos"),
              )
            )
    ),
    tabItem("fonte",
            fluidPage(
              fluidRow(
                
                # pegando de alguma forma htmlzada a versão mais recente dos ícones font awesome
                
                tags$script(src = "https://kit.fontawesome.com/ab15947b75.js", crossorigin="anonymous"), 
                
                setShadow(id = "dados_covid",class = "small-box"), # dando um zoomzin quando passa o mouse nos links com base de dados
                setShadow(id = "dados_leitos",class = "small-box"),
                setShadow(id = "licenca",class = "small-box"),
                
                column(
                  width = 6,
                  valueBoxOutput("dados_covid",width = 12)
                ),
                column(
                  width = 6,
                  valueBoxOutput("dados_leitos",width = 12)
                ),
                column(
                  width = 6,
                  valueBoxOutput("dados_regioes_covid",width = 12)
                ),
                column(
                  width = 6,
                  valueBoxOutput("licenca", width = 12)
                )
              )
              
              
            )
    ),
    tabItem("sobre",
            fluidPage(
              fluidRow(
                column(
                  width = 6,
                  valueBoxOutput("covidMetrika",width = 12)
                ),
                column(
                  width = 6,
                  valueBoxOutput("git_covidMetrika", width = 12)
                ),
                column(
                  width = 6,
                  valueBoxOutput("dashboard_poa",width = 12)
                ),
                column(
                  width = 6,
                  valueBoxOutput("dashboard_br", width = 12)
                )
              ),
              fluidRow(
                widgetUserBox(
                  title = tags$b("Franciele Lobo Pallaoro"),
                  subtitle = "Estudante de Estatística da UFRGS",
                  type = 2,
                  width = 4,
                  src = 'franciele.jpg',
                  color = "red",
                  "Contato: franpallaoro@gmail.com",
                  footer_padding = F
                ),
                
                widgetUserBox(
                  title = tags$b("Gabriel Holmer Saul"),
                  subtitle = "Estudante de Estatística da UFRGS",
                  type = 2,
                  width = 4,
                  src = "gabriel.jpg",
                  color = "red",
                  "Contato: gabrielholmersaul@gmail.com",
                  footer_padding = F
                )
                ,
                
                widgetUserBox(
                  title = tags$b("Gustavo Machado Utpott"),
                  subtitle = "Estudante de Estatística da UFRGS",
                  type = 2,
                  width = 4,
                  src = "gustavo.png",
                  color = "red",
                  "Contato: gustavo.utpott@gmail.com",
                  footer_padding = F
                ),
                
                widgetUserBox(
                  title = tags$b("Juliana Sena de Souza"),
                  subtitle = "Estudante de Pós-Graduação em Epidemiologia da UFRGS",
                  type = 2,
                  width = 4,
                  src =  "juliana.jpeg",
                  color = "red",
                  "Contato: julianass.estatistica@gmail.com",
                  footer_padding = F
                ),
                
                
                widgetUserBox(
                  title = tags$b("Márcia Helena Barbian"),
                  subtitle = "Professora do Departamento de Estatística da UFRGS",
                  type = 2,
                  width = 4,
                  src = "marcia.png",
                  color = "red",
                  "Contato: mhbarbian@ufrgs.br",
                  footer_padding = F
                ), 
                
                widgetUserBox(
                  title = tags$b("Rodrigo Citton P. dos Reis"),
                  subtitle = "Professor do Departamento de Estatística da UFRGS",
                  type = 2,
                  width = 4,
                  src = "rodrigo.jpg",
                  color = "red",
                  "Contato: citton.padilha@ufrgs.br",
                  footer_padding = F
                ), 
                
                tags$img(src = "logos.png", 
                         width = "100%")
              
              )
              
                
            )
    )
  )
)

ui <- dashboardPage(header = header, sidebar = sidebar, body = body, skin = "red")

########################################################################
# Server
########################################################################


server <- function(input, output) {
  
  shinyalert("Olá", "Caso você esteja acessando o dashboard pelo celular, sugerimos que o coloque na posição horizontal para uma melhor visualização dos gráficos!", type = "info")
  
  ###############################
  ####### first tabItem #########
  ###############################
  
  # caixas com numeros gerais
  
  # caixa de confirmados
  output$box_conf <- renderValueBox({
    aux <- dados_covid_rs %>%
      filter(regiao_covid %in% input$filtro_covid)
    
    total <- nrow(aux)
    
    valueBox(
      total,
      "Confirmados",
      icon = icon("virus"),
      color = "red" 
    )
  })
  # caixa incidência por 100 mil habitantes
  output$box_inci <- renderValueBox({

    aux <- dados_covid_rs %>%
      filter(regiao_covid %in% input$filtro_covid) %>%
      group_by(regiao_covid) %>%
      summarise(confirmados = n(), populacao_estimada_regiao_covid = first(populacao_estimada_regiao_covid))
        
    incidencia <- sum(aux$confirmados)*100000/sum(aux$populacao_estimada_regiao_covid)
      
    valueBox(
      round(incidencia,2),
      "Incidência*",
      icon = icon("virus"),
      color = "red",
      href = NULL
    )
  })
  # caixa de óbitos
  output$box_obit <- renderValueBox({
    aux <- dados_covid_rs %>%
      filter(regiao_covid %in% input$filtro_covid) %>%
      filter(evolucao == "OBITO")
    
    obitos <- nrow(aux)
    
    valueBox(
      obitos,
      "Óbitos",
      icon = icon("heartbeat"),
      color = "purple"
    )
  })
  # caixas de mortalidade
  output$box_mort <- renderValueBox({
    aux <- dados_covid_rs %>%
      filter(regiao_covid %in% input$filtro_covid) %>%
      filter(evolucao == "OBITO") %>%
      group_by(regiao_covid) %>%
      summarise(obitos = n(), populacao_estimada_regiao_covid = first(populacao_estimada_regiao_covid))
    
    mortalidade <- sum(aux$obitos)*100000/sum(aux$populacao_estimada_regiao_covid)

    valueBox(
      round(mortalidade,2),
      "Mortalidade*",
      icon = icon("heartbeat"),
      color = "purple"
    )
  })
  # caixa de recuperados
  output$box_recu <- renderValueBox({
    aux <- dados_covid_rs %>%
      filter(regiao_covid %in% input$filtro_covid) %>%
      filter(evolucao == "CURA")
    
    recuperados <- nrow(aux)
    
    valueBox(
      recuperados,
      "Recuperados",
      icon = icon("virus-slash"),
      color = "blue",
      href = NULL 
    )
  })
  # caixa de em acompanhamento
  output$box_acom <- renderValueBox({
    aux <- dados_covid_rs %>%
      filter(regiao_covid %in% input$filtro_covid) %>%
      filter(evolucao == "EM ACOMPANHAMENTO")
    
    acompanhamento <- nrow(aux)
    
    valueBox(
      acompanhamento,
      "Em acompanhamento",
      icon = icon("clinic-medical"),
      color = "yellow"
    )
  })

  
  
  #####################
  # Mapa_covid
  
  output$mapa_covid <- renderLeaflet({
    
    var <- rlang::sym(input$var_covid)
    
    if(input$agrup_covid=="municipio") {
      aux_mapa <- dados_mapa_rs %>%
        mutate(var = replace_na(!!var, 0)) %>%
        filter(regiao_covid %in% input$filtro_covid)
    } else {
      aux_mapa <- dados_mapa_rs_reg %>%
        mutate(var = replace_na(!!var, 0)) %>%
        filter(regiao_covid %in% input$filtro_covid)
    }
      
    y_quantidade <- aux_mapa$var
    
    variavel <- as.data.frame(aux_mapa)[,input$agrup_covid]
    
    # criando intervalo com uma função muito boa
    
    intervalos <- classInt::classIntervals(var = y_quantidade, n = 7, style = "fisher")
    
    intervalos[["brks"]][1:2] <- c(0,1)
    
    pal <- colorBin(palette=opcoes[[input$var_covid]][["paleta"]], domain = y_quantidade, bins = intervalos[["brks"]])
    
    leaflet(aux_mapa) %>%
      addTiles(urlTemplate = "http://mt0.google.com/vt/lyrs=m&hl=en&x={x}&y={y}&z={z}&s=Ga", attribution = 'Google') %>%
      addPolygons(fillColor = ~pal(y_quantidade), 
                  weight = 1.5,
                  opacity = 0.7,
                  fillOpacity = 0.7,
                  color = "gray",
                  highlight = highlightOptions(
                    weight = 5,
                    color = "#666",
                    fillOpacity = 0.7,
                    bringToFront = TRUE),
                  label = sprintf("%s - %s", variavel, y_quantidade),
                  labelOptions = labelOptions(
                    style = list("font-weight" = "normal", padding = "6px 11px"),
                    textsize = "15px",
                    direction = "auto"))   %>%
      addLegend(pal = pal, values = round(y_quantidade,0), labFormat = function(type, cuts, p) {  # legenda para colorQuantile
        n = length(cuts)
        paste0(round(cuts[-n],0), " &ndash; ", round(cuts[-1],0))},
        title = opcoes[[input$var_covid]][["texto"]],
        labels = ~variavel,
        position = "bottomright")
      
  })
  
  #############
  # grafico_covid
  
  output$grafico_covid <- renderPlotly({
    
    var <- rlang::sym(input$var_covid)
    var2 <- rlang::sym(input$agrup_covid)
    pop_var <- rlang::sym(str_c("populacao_estimada_",input$agrup_covid))
    text_tooltip <- names(opcoes)[names(opcoes)!=input$var_covid]
    outra_var <- sym(text_tooltip[1])
    outra_var2 <- sym(text_tooltip[2])
    outra_var3 <- sym(text_tooltip[3])
    outra_var4 <- sym(text_tooltip[4])
    outra_var5 <- sym(text_tooltip[5])
    
    p <- dados_covid_rs %>%
      filter(regiao_covid %in% input$filtro_covid) %>%
      mutate(obitos = ifelse(evolucao == "OBITO", 1, 0),
             acompanhamento = ifelse(evolucao == "EM ACOMPANHAMENTO", 1, 0),
             recuperados = ifelse(evolucao == "CURA", 1, 0)) %>% 
      group_by(!!var2) %>%
      summarise(confirmados = n(), incidencia = n()*100000/as.numeric(first(!!pop_var)),obitos = sum(obitos, na.rm = T), 
                mortalidade = sum(obitos, na.rm = T)*100000/as.numeric(first(!!pop_var)), acompanhamento = sum(acompanhamento, na.rm = T), 
                recuperados = sum(recuperados, na.rm = T), populacao = first(!!pop_var)) %>%
      arrange(desc(!!var)) %>%
      slice_head(n = 25) %>%
      ggplot(aes(x = reorder(!!var2, !!var), y = !!var, text = paste(!!var2,paste0(input$var_covid," ",round(!!var,0)),paste0(text_tooltip[1]," ",round(!!outra_var,0)),paste0(text_tooltip[2]," ",round(!!outra_var2,0)),paste0(text_tooltip[3]," ",round(!!outra_var3,0)),paste0(text_tooltip[4]," ",round(!!outra_var4,0)),paste0(text_tooltip[5]," ",round(!!outra_var5,0)),paste0("populacao ",!!sym("populacao")),sep = "\n"))) +
      geom_col(fill = opcoes[[input$var_covid]][["cor"]]) +
      labs(x = input$agrup_covid, y = opcoes[[input$var_covid]][["texto"]]) +
      coord_flip()
  
    ggplotly(p, tooltip = c("text"))
    
  })
  
  #############
  # table_covid
  
  output$table_covid <- renderDataTable({
    
    var2 <- rlang::sym(input$agrup_covid)
    pop_var <- rlang::sym(str_c("populacao_estimada_",input$agrup_covid))
    
    dados_covid_rs %>%
      filter(regiao_covid %in% input$filtro_covid) %>%
      mutate(obitos = ifelse(evolucao == "OBITO", 1, 0),
             acompanhamento = ifelse(evolucao == "EM ACOMPANHAMENTO", 1, 0),
             recuperados = ifelse(evolucao == "CURA", 1, 0)) %>% 
      group_by(!!var2) %>%
      summarise(confirmados = n(), incidencia = n()*100000/as.numeric(first(!!pop_var)),obitos = sum(obitos, na.rm = T), 
                mortalidade = sum(obitos, na.rm = T)*100000/as.numeric(first(!!pop_var)), acompanhamento = sum(acompanhamento, na.rm = T), 
                recuperados = sum(recuperados, na.rm = T), populacao = first(!!pop_var)) %>%
      arrange(desc(confirmados)) %>%
      datatable(
        rownames = F,
        class = "compact",
        options = list(
          dom = "tfS", 
          ordering = T,
          scrollY = "460px",
          paging = FALSE
        )
      ) %>%
      formatStyle(input$agrup_covid, fontWeight = "bold", fontSize = "12px", backgroundColor ="#f0f0f0") %>%
      formatStyle("confirmados", color = opcoes[["confirmados"]][["cor"]], fontWeight = "bold", fontSize = "12px", backgroundColor ="#f0f0f0") %>%
      formatStyle("incidencia", color = opcoes[["incidencia"]][["cor"]], fontWeight = "bold", fontSize = "12px", backgroundColor ="#f0f0f0") %>%
      formatStyle("obitos", color = opcoes[["obitos"]][["cor"]], fontWeight = "bold", fontSize = "12px", backgroundColor ="#f0f0f0") %>%
      formatStyle("mortalidade", color = opcoes[["mortalidade"]][["cor"]], fontWeight = "bold", fontSize = "12px", backgroundColor ="#f0f0f0") %>%
      formatStyle("recuperados", color = opcoes[["recuperados"]][["cor"]], fontWeight = "bold", fontSize = "12px", backgroundColor ="#f0f0f0") %>%
      formatStyle("acompanhamento", color = opcoes[["acompanhamento"]][["cor"]], fontWeight = "bold", fontSize = "12px", backgroundColor ="#f0f0f0") %>%
      formatStyle("populacao", fontWeight = "bold", fontSize = "12px", backgroundColor ="#f0f0f0") %>%
      formatRound(c("incidencia","mortalidade"), digits = 2)
      
  })
  
  #############
  # ui_serie_covid
  
  output$ui_serie_covid <- renderUI({
    
    var <- rlang::sym(input$var_covid)
    var2 <- rlang::sym(input$agrup_covid)
    
    aux <- dados_covid_rs %>%
      filter(regiao_covid %in% input$filtro_covid) %>%
      as.data.frame()
    
    leveis <- levels(as.factor(aux[,input$agrup_covid]))
    
    if(input$agrup_covid == "municipio") {
      text2 <- " município ou deixe todos selecionados(default)"
    } else {
      text2 <- "a região covid ou deixe todas selecionadas(default)"
    }
    
    
    tabBox(id = "tab_covid",
           width = 12,
           title = NULL,
           tabPanel("Diário",
                    plotlyOutput("serie_covid_dia", height = 500)
           ),
           tabPanel("Semana Epidemiológica",
                    plotlyOutput("serie_covid_sem", height = 500)
           ),
           tabPanel("Filtro",
                    selectInput(
                      "filtro_serie_covid",
                      label = paste0("Selecione algum",text2),
                      choices = c("Todos selecionados",leveis),
                      selected = "Todos selecionados",
                      multiple = F
                    )
           )
    )
    
  })
  
  ############
  # serie_covid_dia
  
  output$serie_covid_dia <- renderPlotly({
    
    #input <- list(var_covid = "acompanhamento", agrup_covid = "municipio", filtro_covid = unique(dados_covid_rs$regiao_covid), filtro_serie_covid ="Todos selecionados")

    var <- rlang::sym(input$var_covid)
    var2 <- rlang::sym(input$agrup_covid)
    pop_var <- rlang::sym(str_c("populacao_estimada_",input$agrup_covid))
    
    if(input$filtro_serie_covid != "Todos selecionados") {
        pop <- dados_covid_rs %>%
          filter(!!var2 == input$filtro_serie_covid) %>%
          slice_head(n = 1) %>%
          select(!!pop_var) %>%
          as.numeric()
        
        aux <- dados_covid_rs %>%
          filter(!!var2 == input$filtro_serie_covid)
    } else {
      pop <- pop_regiao %>%
        filter(regiao_covid %in% input$filtro_covid)
      
      pop <- sum(pop$populacao_estimada_regiao_covid)
      
      aux <- dados_covid_rs
    }
    
    if(input$var_covid %in% c("confirmados","incidencia")) {
      aux <- aux %>%
        filter(regiao_covid %in% input$filtro_covid) %>%
        group_by(data_confirmacao) %>%
        summarise(confirmados = n(), incidencia = n()*100000/pop) %>%
        mutate(frequencia = !!var) %>%
        arrange(data_confirmacao)
      
      n_days <- max(aux$data_confirmacao)-min(aux$data_confirmacao)
      dias <- min(aux$data_confirmacao)+days(0:n_days)
    } else if(input$var_covid %in% c("obitos","mortalidade","recuperados")){
      aux <- aux %>%
        filter(regiao_covid %in% input$filtro_covid) %>%
        mutate(obitos = ifelse(evolucao == "OBITO", 1, 0),
               recuperados = ifelse(evolucao == "CURA", 1, 0)) %>% 
        group_by(data_evolucao) %>%
        summarise(obitos = sum(obitos, na.rm = T), mortalidade = sum(obitos, na.rm = T)*100000/pop,
                  recuperados = sum(recuperados, na.rm = T)) %>%
        filter(!is.na(data_evolucao)) %>%
        mutate(frequencia = !!var) %>%
        ungroup() %>%
        mutate(data_confirmacao = data_evolucao) %>%
        select(-data_evolucao) %>%
        arrange(data_confirmacao)
      
      n_days <- max(aux$data_confirmacao)-min(aux$data_confirmacao)
      dias <- min(aux$data_confirmacao)+days(0:n_days)
    } else {
      aux <- aux %>%
        filter(regiao_covid %in% input$filtro_covid) %>%
        mutate(obitos = ifelse(evolucao == "OBITO", 1, 0),
               acompanhamento = ifelse(is.na(evolucao), 0, 1),
               recuperados = ifelse(evolucao == "CURA", 1, 0))
      negativos <- aux %>%
        group_by(data_evolucao) %>%
        filter(!is.na(data_evolucao)) %>%
        summarise(negativos = sum(obitos,na.rm = T)+sum(recuperados, na.rm =T)) %>%
        mutate(data_confirmacao = data_evolucao) %>%
        select(-data_evolucao)
      
      acomp <- aux %>%
        group_by(data_sintomas) %>%
        summarise(acompanhamento = sum(acompanhamento, na.rm = T)) %>%
        mutate(frequencia = acompanhamento) %>%
        mutate(data_confirmacao = data_sintomas) %>%
        select(-data_sintomas)
      
      
      n_days <- max(negativos$data_confirmacao,acomp$data_confirmacao)-min(negativos$data_confirmacao,acomp$data_confirmacao)
      dias <- min(negativos$data_confirmacao,acomp$data_confirmacao)+days(0:n_days)
      
    }
      
    if(input$var_covid != "acompanhamento") {
      aux2 <- tibble(data_confirmacao = dias[!(dias %in% aux$data_confirmacao)],
                     frequencia = 0)
      
      aux <- bind_rows(aux,aux2) %>%
        arrange(data_confirmacao)
      
      aux$acumulado <- c(aux$frequencia[1],rep(0,n_days))
      
      label_x <- "Dia de confirmação"
      caption_x <- "*Dados referentes à data de 'confirmação', e não 'notificação', portanto dados antigos são frequentemente adicionados"
      y_caption <- 0.99
      
      for (i in 2:nrow(aux)) {
        aux$acumulado[i] <- aux$acumulado[i-1]+aux$frequencia[i]
      }
      
      ordem <- as.character(format(aux$data_confirmacao, "%d-%b"))
      
      aux$data_confirmacao <- as.character(format(aux$data_confirmacao, "%d-%b"))
      
      p <- ggplotly(ggplot(aux) +
                      geom_line(aes(x = data_confirmacao, y = acumulado, group = 1, color = "Acumulado"), linetype = 'dotted') +
                      geom_point(aes(x = data_confirmacao, y = acumulado, color = "Acumulado")) + 
                      geom_col(aes(x = data_confirmacao, y = frequencia, fill = "Frequência")) +
                      scale_x_discrete(limits = ordem) +
                      scale_color_manual(values = list("Acumulado" = opcoes[[input$var_covid]][["cor"]])) +
                      scale_fill_manual(values = list("Frequência" = opcoes[[input$var_covid]][["cor"]])) +
                      labs(x = label_x, y = opcoes[[input$var_covid]][["texto"]], colour = NULL, fill = NULL) +
                      theme(axis.text.x = element_text(angle=90,size=8, vjust = 0.5)) +
                      theme(plot.background = element_rect(fill = "transparent", color = NA), # bg of the plot
                            panel.grid.major = element_blank(),
                            legend.position=c(0.05, 0.95),
                            legend.key = element_blank())) %>%
        layout(legend = list(
          orientation = "v",
          x = 0.01,
          y = 0.95
        ),
        annotations = list(
          list(x = 0.001, y = y_caption, text = caption_x, 
               showarrow = F, xref='paper', yref='paper', 
               xanchor='left', yanchor='auto', xshift=0, yshift=0,
               font=list(size=10, color="gray"))
        )
        )
      
    } else {
      negativos2 <- tibble(data_confirmacao = dias[!(dias %in% negativos$data_confirmacao)],
                     negativos = 0)
      
      negativos <- bind_rows(negativos,negativos2) %>%
        arrange(data_confirmacao)
      
      acomp2 <- tibble(data_confirmacao = dias[!(dias %in% acomp$data_confirmacao)],
                           frequencia = 0)
      
      acomp <- bind_rows(acomp,acomp2) %>%
        arrange(data_confirmacao) %>%
        left_join(negativos, by = "data_confirmacao") %>%
        mutate(negativos = ifelse(is.na(negativos),0,negativos)) %>%
        mutate(frequencia = frequencia-negativos)
      
      acomp$acumulado <- c(acomp$frequencia[1],rep(0,n_days))
      
      label_x <- "Dia de início dos sintomas"
      caption_x <- "*Dados referentes à data de início dos sintomas, portanto dados mais antigos são frequentemente adicionados"
      y_caption <- 0.99
      
      for (i in 2:nrow(acomp)) {
        acomp$acumulado[i] <- acomp$acumulado[i-1]+acomp$frequencia[i]
      }
      
      aux <- acomp %>%
        mutate(acumulado = ifelse(acumulado < 0, 0, acumulado))
      
      ordem <- as.character(format(aux$data_confirmacao, "%d-%b"))
      
      aux$data_confirmacao <- as.character(format(aux$data_confirmacao, "%d-%b"))
      
      p <- ggplotly(ggplot(aux) +
                      geom_line(aes(x = data_confirmacao, y = acumulado, group = 1, color = "Em acompanhamento"), linetype = 'dotted') +
                      geom_point(aes(x = data_confirmacao, y = acumulado, color = "Em acompanhamento")) + 
                      geom_col(aes(x = data_confirmacao, y = frequencia, fill = "Frequência")) +
                      scale_x_discrete(limits = ordem) +
                      scale_color_manual(values = list("Em acompanhamento" = opcoes[[input$var_covid]][["cor"]])) +
                      scale_fill_manual(values = list("Frequência" = opcoes[[input$var_covid]][["cor"]])) +
                      labs(x = label_x, y = opcoes[[input$var_covid]][["texto"]], colour = NULL, fill = NULL) +
                      theme(axis.text.x = element_text(angle=90,size=8, vjust = 0.5)) +
                      theme(plot.background = element_rect(fill = "transparent", color = NA), # bg of the plot
                            panel.grid.major = element_blank(),
                            legend.position=c(0.05, 0.95),
                            legend.key = element_blank())) %>%
        layout(legend = list(
          orientation = "v",
          x = 0.01,
          y = 0.95
        ),
        annotations = list(
          list(x = 0.001, y = y_caption, text = caption_x, 
               showarrow = F, xref='paper', yref='paper', 
               xanchor='left', yanchor='auto', xshift=0, yshift=0,
               font=list(size=10, color="gray"))
        )
        )
    }
    
    
    
    for (i in 1:length(p$x$data)){
      if (!is.null(p$x$data[[i]]$name)){
        p$x$data[[i]]$name =  gsub("\\(","",str_split(p$x$data[[i]]$name,",")[[1]][1])
      }
    }
    
    p
  })
  
  
  ############
  # serie_covid_semana
  
  output$serie_covid_sem <- renderPlotly({
    
    #input <- list(var_covid = "acompanhamento", agrup_covid = "municipio", filtro_covid = unique(dados_covid_rs$regiao_covid), filtro_serie_covid ="Todos selecionados")
    
    var <- rlang::sym(input$var_covid)
    var2 <- rlang::sym(input$agrup_covid)
    pop_var <- rlang::sym(str_c("populacao_estimada_",input$agrup_covid))
    
    if(input$filtro_serie_covid != "Todos selecionados") {
      pop <- dados_covid_rs %>%
        filter(!!var2 == input$filtro_serie_covid) %>%
        slice_head(n = 1) %>%
        select(!!pop_var) %>%
        as.numeric()
      
      aux <- dados_covid_rs %>%
        filter(!!var2 == input$filtro_serie_covid)
    } else {
      pop <- pop_regiao %>%
        filter(regiao_covid %in% input$filtro_covid)
      
      pop <- sum(pop$populacao_estimada_regiao_covid)
      
      aux <- dados_covid_rs
    }
    
    if(input$var_covid %in% c("confirmados","incidencia")) {
      aux <- aux %>%
        filter(regiao_covid %in% input$filtro_covid) %>%
        group_by(semana_epidemiologica_confirmacao) %>%
        summarise(confirmados = n(), incidencia = n()*100000/pop) %>%
        mutate(frequencia = !!var) %>%
        arrange(semana_epidemiologica_confirmacao)
      
      n_weeks <- max(aux$semana_epidemiologica_confirmacao)-min(aux$semana_epidemiologica_confirmacao)
      weeks <- min(aux$semana_epidemiologica_confirmacao):(min(aux$semana_epidemiologica_confirmacao)+n_weeks)
    } else if(input$var_covid %in% c("obitos","mortalidade","recuperados")){
      aux <- aux %>%
        filter(regiao_covid %in% input$filtro_covid) %>%
        mutate(obitos = ifelse(evolucao == "OBITO", 1, 0),
               recuperados = ifelse(evolucao == "CURA", 1, 0)) %>% 
        group_by(semana_epidemiologica_evolucao) %>%
        summarise(obitos = sum(obitos, na.rm = T), mortalidade = sum(obitos, na.rm = T)*100000/pop,
                  recuperados = sum(recuperados, na.rm = T)) %>%
        filter(!is.na(semana_epidemiologica_evolucao)) %>%
        mutate(frequencia = !!var) %>%
        ungroup() %>%
        mutate(semana_epidemiologica_confirmacao = semana_epidemiologica_evolucao) %>%
        select(-semana_epidemiologica_evolucao) %>%
        arrange(semana_epidemiologica_confirmacao)
      
      n_weeks <- max(aux$semana_epidemiologica_confirmacao)-min(aux$semana_epidemiologica_confirmacao)
      weeks <- min(aux$semana_epidemiologica_confirmacao):(min(aux$semana_epidemiologica_confirmacao)+n_weeks)
    } else {
      
      aux <- aux %>%
        filter(regiao_covid %in% input$filtro_covid) %>%
        mutate(obitos = ifelse(evolucao == "OBITO", 1, 0),
               acompanhamento = ifelse(is.na(evolucao), 0, 1),
               recuperados = ifelse(evolucao == "CURA", 1, 0))
      negativos <- aux %>%
        group_by(semana_epidemiologica_evolucao) %>%
        filter(!is.na(semana_epidemiologica_evolucao)) %>%
        summarise(negativos = sum(obitos,na.rm = T)+sum(recuperados, na.rm =T)) %>%
        mutate(semana_epidemiologica_confirmacao = semana_epidemiologica_evolucao) %>%
        select(-semana_epidemiologica_evolucao)
      
      acomp <- aux %>%
        group_by(semana_epidemiologica_sintomas) %>%
        summarise(acompanhamento = sum(acompanhamento, na.rm = T)) %>%
        mutate(frequencia = acompanhamento) %>%
        mutate(semana_epidemiologica_confirmacao = semana_epidemiologica_sintomas) %>%
        select(-semana_epidemiologica_sintomas)
      
      
      n_weeks <- max(negativos$semana_epidemiologica_confirmacao,acomp$semana_epidemiologica_confirmacao)-min(negativos$semana_epidemiologica_confirmacao,acomp$semana_epidemiologica_confirmacao)
      weeks <- min(negativos$semana_epidemiologica_confirmacao,acomp$semana_epidemiologica_confirmacao):(min(negativos$semana_epidemiologica_confirmacao,acomp$semana_epidemiologica_confirmacao)+n_weeks)
      
    }
    
    if(input$var_covid != "acompanhamento") {
      
      aux2 <- tibble(semana_epidemiologica_confirmacao = weeks[!(weeks %in% aux$semana_epidemiologica_confirmacao)],
                     frequencia = 0)
      
      aux <- bind_rows(aux,aux2) %>%
        arrange(semana_epidemiologica_confirmacao)
      
      aux$acumulado <- c(aux$frequencia[1],rep(0,n_weeks))
      
      for (i in 2:nrow(aux)) {
        aux$acumulado[i] <- aux$acumulado[i-1]+aux$frequencia[i]
      }
      
      label_x <- "Semana epidemiológica de confirmação"
      caption_x <- "*Dados referentes à data de 'confirmação', e não 'notificação', portanto dados antigos são frequentemente adicionados"
      y_caption <- 0.99
      
      ordem <- as.character(aux$semana_epidemiologica_confirmacao)
      
      aux$semana_epidemiologica_confirmacao <- as.character(aux$semana_epidemiologica_confirmacao)
      
      p <- ggplotly(ggplot(aux) +
                      geom_line(aes(x = semana_epidemiologica_confirmacao, y = acumulado, group = 1, color = "Acumulado"), linetype = 'dotted') +
                      geom_point(aes(x = semana_epidemiologica_confirmacao, y = acumulado, color = "Acumulado")) + 
                      geom_col(aes(x = semana_epidemiologica_confirmacao, y = frequencia, fill = "Frequência")) +
                      scale_x_discrete(limits = ordem) +
                      scale_color_manual(values = list("Acumulado" = opcoes[[input$var_covid]][["cor"]])) +
                      scale_fill_manual(values = list("Frequência" = opcoes[[input$var_covid]][["cor"]])) +
                      labs(x = label_x, y = opcoes[[input$var_covid]][["texto"]], colour = NULL, fill = NULL) +
                      theme(axis.text.x = element_text(angle=90,size=8, vjust = 0.5)) +
                      theme(plot.background = element_rect(fill = "transparent", color = NA), # bg of the plot
                            panel.grid.major = element_blank(),
                            legend.position=c(0.05, 0.95),
                            legend.key = element_blank())) %>%
        layout(legend = list(
          orientation = "v",
          x = 0.01,
          y = 0.95
        ),
        annotations = list(
          list(x = 0.001, y = y_caption, text = caption_x, 
               showarrow = F, xref='paper', yref='paper', 
               xanchor='left', yanchor='auto', xshift=0, yshift=0,
               font=list(size=10, color="gray"))
        )
        )
      
    } else {
      
      negativos2 <- tibble(semana_epidemiologica_confirmacao = weeks[!(weeks %in% negativos$semana_epidemiologica_confirmacao)],
                           negativos = 0)
      
      negativos <- bind_rows(negativos,negativos2) %>%
        arrange(semana_epidemiologica_confirmacao)
      
      acomp2 <- tibble(semana_epidemiologica_confirmacao = weeks[!(weeks %in% acomp$semana_epidemiologica_confirmacao)],
                       frequencia = 0)
      
      acomp <- bind_rows(acomp,acomp2) %>%
        arrange(semana_epidemiologica_confirmacao) %>%
        left_join(negativos, by = "semana_epidemiologica_confirmacao") %>%
        mutate(negativos = ifelse(is.na(negativos),0,negativos)) %>%
        mutate(frequencia = frequencia-negativos)
      
      acomp$acumulado <- c(acomp$frequencia[1],rep(0,n_weeks))
      
      label_x <- "Semana epidemiológica de início dos sintomas"
      caption_x <- "*Dados referentes à data de início dos sintomas, portanto dados mais antigos são frequentemente adicionados"
      y_caption <- 0.99
      
      for (i in 2:nrow(acomp)) {
        acomp$acumulado[i] <- acomp$acumulado[i-1]+acomp$frequencia[i]
      }
      
      aux <- acomp %>%
        mutate(acumulado = ifelse(acumulado < 0, 0, acumulado))
      
      ordem <- as.character(aux$semana_epidemiologica_confirmacao)
      
      aux$semana_epidemiologica_confirmacao <- as.character(aux$semana_epidemiologica_confirmacao)
      
      p <- ggplotly(ggplot(aux) +
                      geom_line(aes(x = semana_epidemiologica_confirmacao, y = acumulado, group = 1, color = "Em acompanhamento"), linetype = 'dotted') +
                      geom_point(aes(x = semana_epidemiologica_confirmacao, y = acumulado, color = "Em acompanhamento")) + 
                      geom_col(aes(x = semana_epidemiologica_confirmacao, y = frequencia, fill = "Frequência")) +
                      scale_x_discrete(limits = ordem) +
                      scale_color_manual(values = list("Em acompanhamento" = opcoes[[input$var_covid]][["cor"]])) +
                      scale_fill_manual(values = list("Frequência" = opcoes[[input$var_covid]][["cor"]])) +
                      labs(x = label_x, y = opcoes[[input$var_covid]][["texto"]], colour = NULL, fill = NULL) +
                      theme(axis.text.x = element_text(angle=90,size=8, vjust = 0.5)) +
                      theme(plot.background = element_rect(fill = "transparent", color = NA), # bg of the plot
                            panel.grid.major = element_blank(),
                            legend.position=c(0.05, 0.95),
                            legend.key = element_blank())) %>%
        layout(legend = list(
          orientation = "v",
          x = 0.01,
          y = 0.95
        ),
        annotations = list(
          list(x = 0.001, y = y_caption, text = caption_x, 
               showarrow = F, xref='paper', yref='paper', 
               xanchor='left', yanchor='auto', xshift=0, yshift=0,
               font=list(size=10, color="gray"))
        )
        )
      
    }
    
    
    
    for (i in 1:length(p$x$data)){
      if (!is.null(p$x$data[[i]]$name)){
        p$x$data[[i]]$name =  gsub("\\(","",str_split(p$x$data[[i]]$name,",")[[1]][1])
      }
    }
    
    p
  })
  
  # ui_filtro_quadradinhos
  output$ui_filtro_quadradinhos <- renderUI({
    
    var <- rlang::sym(input$var_covid)
    var2 <- rlang::sym(input$agrup_covid)
    pop_var <- rlang::sym(str_c("populacao_estimada_",input$agrup_covid))
    
    aux <- dados_covid_rs %>%
      filter(regiao_covid %in% input$filtro_covid) %>%
      mutate(obitos = ifelse(evolucao == "OBITO", 1, 0),
             acompanhamento = ifelse(evolucao == "EM ACOMPANHAMENTO", 1, 0),
             recuperados = ifelse(evolucao == "CURA", 1, 0)) %>% 
      group_by(!!var2) %>%
      summarise(confirmados = n(), incidencia = n()*100000/as.numeric(first(!!pop_var)),obitos = sum(obitos, na.rm = T), 
                mortalidade = sum(obitos, na.rm = T)*100000/as.numeric(first(!!pop_var)), acompanhamento = sum(acompanhamento, na.rm = T), 
                recuperados = sum(recuperados, na.rm = T), populacao = first(!!pop_var)) %>%
      arrange(desc(!!var))
    
    aux <- as.data.frame(aux)
    
    if(input$var_covid %in% c("acompanhamento","recuperados")) {
      "Em Construção"
    } else {
      box(
        width = 12,
        selectInput(
          "filtro_quadradinhos",
          label = "Selecione os municípios de interesse(por default estão os 15 de maior quantidade da variável escolhida)",
          choices = aux[,input$agrup_covid],
          selected = aux[1:15,input$agrup_covid],
          multiple = T
        ),
        plotlyOutput("plot_quadradinhos", height = 650L)
      )
    }
    
  })
  
  # plot_quadradinhos
  output$plot_quadradinhos <- renderPlotly({
    
    #input <- list(var_covid = "obitos", agrup_covid = "municipio", filtro_covid = unique(dados_covid_rs$regiao_covid), filtro_quadradinhos = aux[1:15,input$agrup_covid])
    
    var <- rlang::sym(input$var_covid)
    var2 <- rlang::sym(input$agrup_covid)
    pop_var <- rlang::sym(str_c("populacao_estimada_",input$agrup_covid))
    
    if(input$var_covid %in% c("confirmados","incidencia")) {
      aux <- dados_covid_rs %>%
        filter(!!var2 %in% input$filtro_quadradinhos) %>%
        group_by(data_confirmacao, !!var2) %>%
        summarise(confirmados = n(), incidencia = n()*100000/as.numeric(first(!!pop_var))) %>%
        mutate(frequencia = !!var) %>%
        arrange(data_confirmacao)
    } else {
      aux <- dados_covid_rs %>%
        filter(!!var2 %in% input$filtro_quadradinhos) %>%
        mutate(obitos = ifelse(evolucao == "OBITO", 1, 0)) %>% 
        group_by(data_evolucao,!!var2) %>%
        summarise(obitos = sum(obitos, na.rm = T), mortalidade = sum(obitos, na.rm = T)*100000/as.numeric(first(!!pop_var))) %>%
        filter(!is.na(data_evolucao)) %>%
        mutate(frequencia = !!var) %>%
        ungroup() %>%
        mutate(data_confirmacao = data_evolucao) %>%
        select(-data_evolucao) %>%
        arrange(data_confirmacao)
    }
    data_minima <- aux %>%
      group_by(!!var2) %>%
      summarise(minimo = min(data_confirmacao)) %>%
      arrange(minimo) %>%
      select(minimo) %>%
      as.list()
    
    n_days <- max(aux$data_confirmacao)-data_minima$minimo
    soma_dias <- pmap(list(rep(0,length(n_days)),n_days), seq)
    
    aux <- as.data.frame(aux)
    
    dias <- list()
    dias_faltantes <- list()
    for(i in 1:length(n_days)) {
      dias[[i]] <- data_minima$minimo[[i]]+soma_dias[[i]]
      dias_faltantes[[i]] <- dias[[i]][!(dias[[i]] %in% aux[aux[,input$agrup_covid]==unique(aux[,input$agrup_covid])[[i]],"data_confirmacao"])]
    }
    
    
    if(input$agrup_covid=="municipio") {
      aux2 <- tibble(data_confirmacao = as_date(unlist(dias_faltantes)),
                     municipio = rep(unique(aux[,input$agrup_covid]),unlist(map(dias_faltantes,length))),
                     frequencia = 0)
    } else {
      aux2 <- tibble(data_confirmacao = as_date(unlist(dias_faltantes)),
                     regiao_covid = rep(unique(aux[,input$agrup_covid]),unlist(map(dias_faltantes,length))),
                     frequencia = 0)
    }
    
    aux <- bind_rows(aux,aux2) %>%
      arrange(data_confirmacao)
    
    aux$acumulado <- aux$frequencia
    
    for(i in unique(aux[,input$agrup_covid])) {
      indices <- which(aux[,input$agrup_covid]==i)
      for(j in indices[2:length(indices)]) {
        aux$acumulado[j] <- aux$acumulado[indices[which(indices==j)-1]] + aux$frequencia[j]
      }
    }
    
    p <- ggplot(aux, aes(x = data_confirmacao, y = reorder(!!var2,frequencia, FUN = sum), fill = acumulado, text = paste(!!var2,paste0(input$var_covid," ",round(acumulado,0)),sep = "\n"))) +
      geom_tile() +
      labs(y = input$agrup_covid) +
      scale_fill_gradientn(trans = "sqrt",name = opcoes[[input$var_covid]][["texto"]], colours = brewer.pal(9,opcoes[[input$var_covid]][["paleta"]])) +
      scale_x_date(date_breaks = "1 month", date_labels = "%b") +
      theme_tufte(base_family="Helvetica")
    
    ggplotly(p, tooltip = c("text","x"))
    
  })
  
  ###############################
  ###### secoond tabItem ########
  ###############################
  
  # caixas com numeros gerais
  
  # caixa de leitos totais
  output$box_tot <- renderValueBox({
    aux <- leitos_uti %>%
      group_by(cnes) %>%
      filter(data_atualizacao == max(data_atualizacao)) %>%
      filter(regiao_covid %in% input$filtro_leitos)
    
    valueBox(
      sum(aux$leitos_total),
      "Leitos totais",
      icon = icon("hospital"),
      color = "green" 
    )
  })
  # caixa de internados
  output$box_int <- renderValueBox({
    aux <- leitos_uti %>%
      group_by(cnes) %>%
      filter(data_atualizacao == max(data_atualizacao)) %>%
      filter(regiao_covid %in% input$filtro_leitos)
    
    valueBox(
      sum(aux$leitos_disponiveis),
      "Leitos disponíveis",
      icon = icon("procedures"),
      color = "blue" 
    )
  })
  # caixa de lotação
  output$box_lot <- renderValueBox({
    aux <- leitos_uti %>%
      group_by(cnes) %>%
      filter(data_atualizacao == max(data_atualizacao)) %>%
      filter(regiao_covid %in% input$filtro_leitos)

    valueBox(
      paste0(round(100*sum(aux$leitos_internacoes)/sum(aux$leitos_total),2),"%"),
      "Porcentagem de lotação dos leitos",
      icon = icon("hospital-user"),
      color = "purple", 
    )
  })
  # caixas de leitos covid
  output$box_cov <- renderValueBox({
    aux <- leitos_uti %>%
      group_by(cnes) %>%
      filter(data_atualizacao == max(data_atualizacao)) %>%
      filter(regiao_covid %in% input$filtro_leitos)
    
    valueBox(
      sum(aux$leitos_covid),
      "Leitos ocupados COVID-19",
      icon = icon("virus"),
      color = "maroon", 
    )
  })
  
  #####################
  # Mapa_leitos
  
  output$mapa_leitos <- renderLeaflet({
    
    var <- rlang::sym(input$var_leitos)
    
    if (input$agrup_leitos == "hospital") {
      if (input$var_leitos != "lotacao") {
        aux_mapa <- leitos_uti %>%
          filter(!is.na(!!var)) %>%
          filter(regiao_covid %in% input$filtro_leitos) %>%
          group_by(cnes, LATITUDE, LONGITUDE, hospital) %>%
          filter(data_atualizacao == max(data_atualizacao)) %>%
          summarise(var = sum(!!var)) %>%
          filter(var != 0)
      } else {
        aux_mapa <- leitos_uti %>%
          filter(!is.na(!!var)) %>%
          filter(regiao_covid %in% input$filtro_leitos) %>%
          group_by(cnes, LATITUDE, LONGITUDE, hospital) %>%
          filter(data_atualizacao == max(data_atualizacao)) %>%
          summarise(var = ifelse(sum(leitos_total)!=0,sum(leitos_internacoes)/sum(leitos_total),NA))
      }
      
    } else if (input$agrup_leitos == "municipio") {
      aux_mapa <- leitos_mapa_mun_rs %>%
        mutate(var = !!var) %>%
        filter(regiao_covid %in% input$filtro_leitos)
    } else {
      aux_mapa <- leitos_mapa_reg_rs %>%
        filter(regiao_covid %in% input$filtro_leitos) %>%
        mutate(var = !!var)
    }
    
    y_quantidade <- aux_mapa$var
    
    if (input$var_leitos == "leitos_total") {
      paleta <- "Greens"
      cor <- "#00a65a"
      texto <- "Total de leitos"
    } else if (input$var_leitos == "leitos_disponiveis") {
      paleta <- "Blues"
      cor <- "#0073b7"
      texto <- "Leitos disponíveis"
    } else if(input$var_leitos == "lotacao") {
      paleta <- "Purples"
      cor <- "#605ca8"
      texto <- "Lotação média"
    } else {
      paleta <- "RdPu"
      cor <- "#d81b60"
      texto <- "Leitos ocupados COVID-19"
    }
    
    if(input$agrup_leitos != "hospital") {
      
      variavel <- as.data.frame(aux_mapa)[,input$agrup_leitos]
      
      if(input$var_leitos != "lotacao") {
        
        y_quantidade <- replace_na(y_quantidade, 0) 
        
        # criando intervalo com uma função muito boa
        
        intervalos <- classInt::classIntervals(var = y_quantidade, n = 6, style = "fisher")
        
        intervalos[["brks"]][1:2] <- c(0,1)
        
        pal <- colorBin(palette=paleta, domain = y_quantidade, bins = intervalos[["brks"]])
        
        leaflet(aux_mapa) %>%
          addTiles(urlTemplate = "http://mt0.google.com/vt/lyrs=m&hl=en&x={x}&y={y}&z={z}&s=Ga", attribution = 'Google') %>%
          addPolygons(fillColor = ~pal(y_quantidade), 
                      weight = 1.5,
                      opacity = 0.7,
                      fillOpacity = 0.7,
                      color = "gray",
                      highlight = highlightOptions(
                        weight = 5,
                        color = "#666",
                        fillOpacity = 0.7,
                        bringToFront = TRUE),
                      label = sprintf("%s - %s", variavel, y_quantidade),
                      labelOptions = labelOptions(
                        style = list("font-weight" = "normal", padding = "6px 11px"),
                        textsize = "15px",
                        direction = "auto"))   %>%
          addLegend(pal = pal, values = y_quantidade, labFormat = function(type, cuts, p) {  # legenda para colorQuantile
            n = length(cuts)
            paste0(cuts[-n], " &ndash; ", cuts[-1])},
            title = texto,
            labels = ~variavel,
            position = "bottomright")
        
      } else {
        
        paleta <- RColorBrewer::brewer.pal(n=6,"Purples")
        paleta <- paleta[2:6]
        
        intervalos <- classInt::classIntervals(var = y_quantidade, n = 6, style = "fisher")
        
        pal <- colorBin(palette=paleta, domain = y_quantidade, bins = intervalos[["brks"]], na.color = "#ffffff")
        
        leaflet(aux_mapa) %>%
          addTiles(urlTemplate = "http://mt0.google.com/vt/lyrs=m&hl=en&x={x}&y={y}&z={z}&s=Ga", attribution = 'Google') %>%
          addPolygons(fillColor = ~pal(y_quantidade), 
                      weight = 1.5,
                      opacity = 0.7,
                      fillOpacity = 0.7,
                      color = "gray",
                      highlight = highlightOptions(
                        weight = 5,
                        color = "#666",
                        fillOpacity = 0.7,
                        bringToFront = TRUE),
                      label = sprintf("%s - %s", variavel, paste0(100*round(y_quantidade,4),"%")),
                      labelOptions = labelOptions(
                        style = list("font-weight" = "normal", padding = "6px 11px"),
                        textsize = "15px",
                        direction = "auto"))   %>%
          addLegend(pal = pal, values = paste0(100*round(y_quantidade,4),"%"), labFormat = function(type, cuts, p) {  # legenda para colorQuantile
            n = length(cuts)
            paste0(round(cuts[-n]*100,2),"%", " &ndash; ", round(cuts[-1]*100,2),"%")},
            title = texto,
            labels = ~variavel,
            position = "bottomright")
        
      }
      
      
      
    } else {
      
      if(input$var_leitos != "lotacao") {
        
        labs <- lapply(seq(nrow(aux_mapa)), function(i) {
          paste0(aux_mapa[i, "var"], " - ",texto, '</p>', 
                 " ",aux_mapa[i, "hospital"]) 
        })
        
        calculo_raio <- 3.5*aux_mapa$var^(1/2)
        
        leaflet(aux_mapa) %>%
          addTiles(urlTemplate = "http://mt0.google.com/vt/lyrs=m&hl=en&x={x}&y={y}&z={z}&s=Ga", attribution = 'Google') %>%
          addCircleMarkers(lng = aux_mapa$LONGITUDE, lat = aux_mapa$LATITUDE, radius = calculo_raio,
                           color = cor, fillOpacity = 0.5, label = lapply(labs, htmltools::HTML), 
                           labelOptions = labelOptions(interactive = T, textsize = "15px"))
        
      } else {
        
        labs <- lapply(seq(nrow(aux_mapa)), function(i) {
          paste0(100*round(aux_mapa[i, "var"],4),"%", " ",texto, '</p>', 
                 " ",aux_mapa[i, "hospital"]) 
        })
        
        calculo_raio <- (10*aux_mapa$var)^(2)/3
        
        leaflet(aux_mapa) %>%
          addTiles(urlTemplate = "http://mt0.google.com/vt/lyrs=m&hl=en&x={x}&y={y}&z={z}&s=Ga", attribution = 'Google') %>%
          addCircleMarkers(lng = aux_mapa$LONGITUDE, lat = aux_mapa$LATITUDE, radius = calculo_raio,
                           color = cor, fillOpacity = 0.5, label = lapply(labs, htmltools::HTML), 
                           labelOptions = labelOptions(interactive = T, textsize = "15px"))
      }
      
      
      
    }
  })
  
  #############
  # grafico_leitos
  
  output$grafico_leitos <- renderPlotly({
    
    var <- rlang::sym(input$var_leitos)
    var2 <- rlang::sym(input$agrup_leitos)
    
    if (input$var_leitos == "leitos_total") {
      cor <- "#00a65a"
      texto <- "Total de leitos"
    } else if (input$var_leitos == "leitos_disponiveis") {
      cor <- "#0073b7"
      texto <- "Leitos disponíveis"
    } else if(input$var_leitos == "lotacao") {
      cor <- "#605ca8"
      texto <- "Lotação média"
    } else {
      cor <- "#d81b60"
      texto <- "Leitos ocupados COVID-19"
    }
    
    if (input$agrup_leitos != "hospital") {
      if(input$var_leitos != "lotacao") {
        aux <- leitos_uti %>%
          filter(regiao_covid %in% input$filtro_leitos) %>%
          group_by(cnes) %>%
          filter(data_atualizacao == max(data_atualizacao)) %>%
          ungroup() %>%
          select(c(input$var_leitos,input$agrup_leitos))
        
        aux <- aux %>%
          group_by(!!var2) %>%
          summarise(var = sum(!!var)) %>%
          mutate_all(~ replace(., . == 0, NA)) %>%
          filter_all(all_vars(!is.na(.))) %>%
          arrange(desc(var))
      } else {
        aux <- leitos_uti %>%
          filter(regiao_covid %in% input$filtro_leitos) %>%
          group_by(cnes) %>%
          filter(data_atualizacao == max(data_atualizacao)) %>%
          ungroup() %>%
          select(c(leitos_total,leitos_internacoes,input$agrup_leitos))
        
        aux <- aux %>%
          group_by(!!var2) %>%
          summarise(var = ifelse(sum(leitos_total) == 0, NA, sum(leitos_internacoes)/sum(leitos_total))) %>%
          mutate_all(~ replace(., . == 0, NA)) %>%
          filter_all(all_vars(!is.na(.))) %>%
          arrange(desc(var))
      }
      
      
    } else {
      aux <- leitos_uti %>%
        filter(regiao_covid %in% input$filtro_leitos) %>%
        group_by(cnes) %>%
        filter(data_atualizacao == max(data_atualizacao)) %>%
        ungroup() %>%
        select(c(input$var_leitos,"hospital","cnes")) %>%
        mutate(var = !!var) %>%
        filter(!is.na(var)) %>%
        arrange(desc(var))
      
    }
    

    aux <- as.data.frame(aux)
    
    aux <- aux[1:25,]
    
    ordem <- aux[,input$agrup_leitos]
      
    p <- ggplot(aux, aes(x = !!var2, y = var)) +
      geom_col(fill = cor) +
      labs(x = input$agrup_leitos, y = input$var_leitos) +
      scale_x_discrete(limits = rev(ordem)) +
      coord_flip()
    
    if(input$var_leitos == "lotacao") {
      p <- p +
        scale_y_continuous(labels=percent)
    }
    
    ggplotly(p)
    
  })
  
  #############
  # table_leitos
  
  output$table_leitos <- renderDataTable({
    
    var <- rlang::sym(input$var_leitos)
    var2 <- rlang::sym(input$agrup_leitos)
    
    if (input$agrup_leitos != "hospital") {
      if(input$var_leitos != "lotacao") {
        aux <- leitos_uti %>%
          filter(regiao_covid %in% input$filtro_leitos) %>%
          group_by(cnes) %>%
          filter(data_atualizacao == max(data_atualizacao)) %>%
          ungroup() %>%
          select(c(input$var_leitos,input$agrup_leitos))
        
        aux <- aux %>%
          group_by(!!var2) %>%
          summarise(var = sum(!!var)) %>%
          mutate_all(~ replace(., . == 0, NA)) %>%
          filter_all(all_vars(!is.na(.))) %>%
          arrange(desc(var))
      } else {
        aux <- leitos_uti %>%
          filter(regiao_covid %in% input$filtro_leitos) %>%
          group_by(cnes) %>%
          filter(data_atualizacao == max(data_atualizacao)) %>%
          ungroup() %>%
          select(c(leitos_total,leitos_internacoes,input$agrup_leitos))
        
        aux <- aux %>%
          group_by(!!var2) %>%
          summarise(var = ifelse(sum(leitos_total) == 0, NA, sum(leitos_internacoes)/sum(leitos_total))) %>%
          mutate_all(~ replace(., . == 0, NA)) %>%
          filter_all(all_vars(!is.na(.))) %>%
          arrange(desc(var))
      }
      
      
    } else {
      aux <- leitos_uti %>%
        filter(regiao_covid %in% input$filtro_leitos) %>%
        group_by(cnes) %>%
        filter(data_atualizacao == max(data_atualizacao)) %>%
        ungroup() %>%
        select(c(input$var_leitos,"hospital","cnes")) %>%
        mutate(var = !!var) %>%
        filter(!is.na(var)) %>%
        arrange(desc(var))
      
    }
    
    
    if (input$var_leitos == "leitos_total") {
      cor <- "#00a65a"
      texto <- "Total de leitos"
    } else if (input$var_leitos == "leitos_disponiveis") {
      cor <- "#0073b7"
      texto <- "Leitos disponíveis"
    } else if(input$var_leitos == "lotacao") {
      cor <- "#605ca8"
      texto <- "Lotação média"
      aux$var <- paste0(round(aux$var,4)*100,"%")
    } else {
      cor <- "#d81b60"
      texto <- "Leitos ocupados COVID-19"
    }
    
    if(input$agrup_leitos == "municipio") {
      text2 <- "Município"
    } else if(input$agrup_leitos == "regiao_covid") {
      text2 <- "Mesoregião"
    } else {
      text2 <- "Hospital"
    }
    
    datatable(
      aux[,c(input$agrup_leitos,"var")], 
      rownames=F,
      class = "compact",
      colnames = c(text2,texto),
      options = list(
        dom = "tfS", 
        ordering = F,
        scrollY = "530px",
        paging = FALSE
      )
    ) %>%
      formatStyle(input$agrup_leitos,color = "#787878", fontSize = "12px", backgroundColor = "#f0f0f0") %>%
      formatStyle("var", color = cor, fontWeight = "bold",fontSize = "12px", backgroundColor = "#f0f0f0")
    
  })
  
  #############
  # ui_serie_leitos
  
  output$ui_serie_leitos <- renderUI({
    
    var2 <- rlang::sym(input$agrup_leitos)
    
    aux <- leitos_uti %>%
      filter(regiao_covid %in% input$filtro_leitos) %>%
      filter(!is.na(leitos_total) & leitos_total != 0) %>%
      as.data.frame()
    
    leveis <- levels(as.factor(aux[,input$agrup_leitos]))
    
    if(input$agrup_leitos == "municipio") {
      text2 <- " município ou deixe todos selecionados(default)"
    } else if(input$agrup_leitos == "regiao_covid") {
      text2 <- "a mesoregião ou deixe todas selecionadas(default)"
    } else {
      text2 <- " hospital ou deixe todos selecionados(default)"
    }
    
    aux_covid <- leitos_uti %>%
      filter(regiao_covid %in% input$filtro_leitos) %>%
      filter(!is.na(leitos_covid) & leitos_covid != 0) %>%
      as.data.frame()
    
    leveis_covid <- levels(as.factor(aux_covid[,input$agrup_leitos]))
    
    fluidRow(
      column(
        width = 6,
        tabBox(
          id = "tab_ocup",
          title = "Quantidade de Leitos de UTI OCUPADOS ao longo do tempo",
          width = 12,
          tabPanel("Diário",
                   plotlyOutput("serie_leitos_ocup_dia", height = 500)),
          tabPanel("Semana Epidemiológica",
                   plotlyOutput("serie_leitos_ocup_sem", height = 500)),
          tabPanel("Filtro",
                   selectInput(
                     "filtro_serie_leitos_ocup",
                     label = paste0("Selecione algum",text2),
                     choices = c("Todos selecionados",leveis),
                     selected = "Todos selecionados",
                     multiple = F
                   )) 
        )
      ),
      column(
        width = 6,
        tabBox(
          id = "tab_disp",
          title = "Quantidade de Leitos de UTI DISPONÍVEIS ao longo do tempo",
          width = 12,
          tabPanel("Diário",
                   plotlyOutput("serie_leitos_disp_dia", height = 500)),
          tabPanel("Semana Epidemiológica",
                   plotlyOutput("serie_leitos_disp_sem", height = 500)),
          tabPanel("Filtro",
                   selectInput(
                     "filtro_serie_leitos_disp",
                     label = paste0("Selecione algum",text2),
                     choices = c("Todos selecionados",leveis),
                     selected = "Todos selecionados",
                     multiple = F
                   ))
        )
      ),
      column(
        width = 9,
        tabBox(
          id = "tab_covid",
          title = "Quantidade de Leitos de UTI OCUPADOS com pacientes com covid-19 ao longo do tempo",
          width = 12,
          tabPanel("Diário",
                   plotlyOutput("serie_leitos_covid_dia", height = 500)),
          tabPanel("Semana Epidemiológica",
                   plotlyOutput("serie_leitos_covid_sem", height = 500)),
          tabPanel("Filtro",
                   selectInput(
                     "filtro_serie_leitos_covid",
                     label = paste0("Selecione algum",text2),
                     choices = c("Todos selecionados",leveis_covid),
                     selected = "Todos selecionados",
                     multiple = F
                   ))
        )
      ),
      column(
        width = 3,
        box(
          dataTableOutput("table_leitos", height = "580px"),
          width = 12
        )
      )
    )
    
  
    
    
  })
  
  
  
  #############
  # serie_leitos_ocup_dia
  
  output$serie_leitos_ocup_dia <- renderPlotly({
    
    var2 <- rlang::sym(input$agrup_leitos)
    
    aux <- leitos_uti
    
    if(input$filtro_serie_leitos_ocup != "Todos selecionados") {
      aux <- aux %>%
        filter(!!var2 == input$filtro_serie_leitos_ocup)
    }
    
    aux <- aux %>%
      filter(regiao_covid %in% input$filtro_leitos) %>%
      group_by(data_atualizacao) %>%
      summarise(`Leitos ocupados` = sum(leitos_internacoes, na.rm = T), `Leitos sem covid` = sum(leitos_internacoes, na.rm =T) - sum(leitos_covid, na.rm = T),
                `Total leitos` = sum(leitos_total, na.rm = T), lotacao = sum(leitos_internacoes, na.rm = T)/sum(leitos_total, na.rm = T)) %>%
      arrange(data_atualizacao)
    
    ordem <- as.character(format(unique(aux$data_atualizacao), "%d-%m"))
    
    aux$data_atualizacao <- as.character(format(aux$data_atualizacao, "%d-%m"))

    p <- ggplot(aux) +
      geom_col(aes(x = data_atualizacao, y = `Leitos ocupados`, label = lotacao), fill = "#605ca8") +
      geom_line(aes(x = data_atualizacao, y = `Total leitos`, group = 1), color = "#00a65a") +
      geom_point(aes(x = data_atualizacao, y = `Total leitos`), color = "#00a65a") +
      scale_x_discrete(limits = ordem) +
      theme(axis.text.x = element_text(angle=45,size=8, vjust = 0.5))

    ggplotly(p)
      
   
  })
  
  #############
  # serie_leitos_ocup_sem
  
  output$serie_leitos_ocup_sem <- renderPlotly({
    
    var2 <- rlang::sym(input$agrup_leitos)
    
    aux <- leitos_uti
    
    if(input$filtro_serie_leitos_ocup != "Todos selecionados") {
      aux <- aux %>%
        filter(!!var2 == input$filtro_serie_leitos_ocup)
    }
    
    aux <- aux %>%
      filter(regiao_covid %in% input$filtro_leitos) %>%
      group_by(data_atualizacao, semana_epidemiologica) %>%
      summarise(`Leitos ocupados` = sum(leitos_internacoes, na.rm = T), `Leitos sem covid` = sum(leitos_internacoes, na.rm =T) - sum(leitos_covid, na.rm = T),
                `Total leitos` = sum(leitos_total, na.rm = T), lotacao = sum(leitos_internacoes, na.rm = T)/sum(leitos_total, na.rm = T)) %>%
      group_by(semana_epidemiologica) %>%
      summarise(`Leitos ocupados` = mean(`Leitos ocupados`, na.rm = T), 
                `Total leitos` = mean(`Total leitos`, na.rm = T), lotacao = mean(`Leitos ocupados`, na.rm = T)/mean(`Total leitos`, na.rm = T)) %>%
      arrange(semana_epidemiologica)
    
    ordem <- as.character(unique(aux$semana_epidemiologica))
    
    aux$semana_epidemiologica <- as.character(aux$semana_epidemiologica)
    
    p <- ggplot(aux) +
      geom_col(aes(x = semana_epidemiologica, y = `Leitos ocupados`, label = lotacao), fill = "#605ca8") +
      geom_line(aes(x = semana_epidemiologica, y = `Total leitos`, group = 1), color = "#00a65a") +
      geom_point(aes(x = semana_epidemiologica, y = `Total leitos`), color = "#00a65a") +
      scale_x_discrete(limits = ordem) +
      labs(y = "Média semanal de leitos ocupados") +
      theme(axis.text.x = element_text(angle=45,size=8, vjust = 0.5))
    
    ggplotly(p)
    
  })
  
  #############
  # serie_leitos_disp_dia
  
  output$serie_leitos_disp_dia <- renderPlotly({
    
    var2 <- rlang::sym(input$agrup_leitos)
    
    aux <- leitos_uti
    
    if(input$filtro_serie_leitos_disp != "Todos selecionados") {
      aux <- aux %>%
        filter(!!var2 == input$filtro_serie_leitos_disp)
    }
    
    aux <- aux %>%
      filter(regiao_covid %in% input$filtro_leitos) %>%
      group_by(data_atualizacao) %>%
      summarise(`Leitos ocupados` = sum(leitos_internacoes, na.rm = T), `Leitos disponíveis` = sum(leitos_total-leitos_internacoes, na.rm =T),
                `Total leitos` = sum(leitos_total, na.rm = T), lotacao = sum(leitos_internacoes, na.rm = T)/sum(leitos_total, na.rm = T)) %>%
      arrange(data_atualizacao)
    
    ordem <- as.character(format(unique(aux$data_atualizacao), "%d-%m"))
    
    aux$data_atualizacao <- as.character(format(aux$data_atualizacao, "%d-%m"))

    p <- ggplot(aux) +
      geom_col(aes(x = data_atualizacao, y = `Leitos disponíveis`), fill = "#0073b7") +
      geom_line(aes(x = data_atualizacao, y = `Total leitos`, group = 1), color = "#00a65a") +
      geom_point(aes(x = data_atualizacao, y = `Total leitos`), color = "#00a65a") +
      scale_x_discrete(limits = ordem) +
      theme(axis.text.x = element_text(angle=45,size=8, vjust = 0.5))

    ggplotly(p)
    
    
  })
  
  #############
  # serie_leitos_disp_sem
  
  output$serie_leitos_disp_sem <- renderPlotly({
    
    var2 <- rlang::sym(input$agrup_leitos)
    
    aux <- leitos_uti
    
    if(input$filtro_serie_leitos_disp != "Todos selecionados") {
      aux <- aux %>%
        filter(!!var2 == input$filtro_serie_leitos_disp)
    }
    
    aux <- aux %>%
      filter(regiao_covid %in% input$filtro_leitos) %>%
      group_by(data_atualizacao, semana_epidemiologica) %>%
      summarise(`Leitos ocupados` = sum(leitos_internacoes, na.rm = T), `Leitos sem covid` = sum(leitos_internacoes, na.rm =T) - sum(leitos_covid, na.rm = T),
                `Total leitos` = sum(leitos_total, na.rm = T), lotacao = sum(leitos_internacoes, na.rm = T)/sum(leitos_total, na.rm = T)) %>%
      group_by(semana_epidemiologica) %>%
      summarise(`Leitos ocupados` = mean(`Leitos ocupados`, na.rm = T), `Leitos disponíveis` = mean(`Total leitos`, na.rm = T) - mean(`Leitos ocupados`, na.rm = T),
                `Total leitos` = mean(`Total leitos`, na.rm = T), lotacao = mean(`Leitos ocupados`, na.rm = T)/mean(`Total leitos`, na.rm = T)) %>%
      arrange(semana_epidemiologica)
    
    ordem <- as.character(unique(aux$semana_epidemiologica))
    
    aux$semana_epidemiologica <- as.character(aux$semana_epidemiologica)
    
    p <- ggplot(aux) +
      geom_col(aes(x = semana_epidemiologica, y = `Leitos disponíveis`), fill = "#0073b7") +
      geom_line(aes(x = semana_epidemiologica, y = `Total leitos`, group = 1), color = "#00a65a") +
      geom_point(aes(x = semana_epidemiologica, y = `Total leitos`), color = "#00a65a") +
      scale_x_discrete(limits = ordem) +
      labs(y = "Média semanal de leitos disponíveis") +
      theme(axis.text.x = element_text(angle=45,size=8, vjust = 0.5))
    
    ggplotly(p)
    
  })
  
  #############
  # serie_leitos_covid_dia
  
  output$serie_leitos_covid_dia <- renderPlotly({
    
    var2 <- rlang::sym(input$agrup_leitos)
    
    aux <- leitos_uti
    
    if(input$filtro_serie_leitos_covid != "Todos selecionados") {
      aux <- aux %>%
        filter(!!var2 == input$filtro_serie_leitos_covid)
    }
    
    aux <- aux %>%
      filter(regiao_covid %in% input$filtro_leitos) %>%
      group_by(data_atualizacao) %>%
      summarise(`Leitos ocupados` = sum(leitos_internacoes, na.rm = T), `Leitos com covid` = sum(leitos_covid, na.rm =T),
                `Total leitos` = sum(leitos_total, na.rm = T), lotacao = sum(leitos_internacoes, na.rm = T)/sum(leitos_total, na.rm = T)) %>%
      arrange(data_atualizacao)
    
    ordem <- as.character(format(unique(aux$data_atualizacao), "%d-%m"))
    
    aux$data_atualizacao <- as.character(format(aux$data_atualizacao, "%d-%m"))

    p <- ggplot(aux) +
      geom_col(aes(x = data_atualizacao, y = `Leitos com covid`), fill = "#d81b60") +
      scale_x_discrete(limits = ordem) +
      scale_y_continuous(limits = c(0,max(aux$`Leitos com covid`+20))) +
      theme(axis.text.x = element_text(angle=45,size=8, vjust = 0.5)) 

    ggplotly(p)
    
  })
  
  #############
  # serie_leitos_covid_sem
  
  output$serie_leitos_covid_sem <- renderPlotly({
    
    var2 <- rlang::sym(input$agrup_leitos)
    
    aux <- leitos_uti
    
    if(input$filtro_serie_leitos_covid != "Todos selecionados") {
      aux <- aux %>%
        filter(!!var2 == input$filtro_serie_leitos_covid)
    }
    
    aux <- aux %>%
      filter(regiao_covid %in% input$filtro_leitos) %>%
      group_by(data_atualizacao, semana_epidemiologica) %>%
      summarise(`Leitos ocupados` = sum(leitos_internacoes, na.rm = T), `Leitos sem covid` = sum(leitos_internacoes, na.rm =T) - sum(leitos_covid, na.rm = T),
                `Total leitos` = sum(leitos_total, na.rm = T), lotacao = sum(leitos_internacoes, na.rm = T)/sum(leitos_total, na.rm = T), `Leitos com covid` = sum(leitos_covid, na.rm = T)) %>%
      group_by(semana_epidemiologica) %>%
      summarise(`Leitos ocupados` = mean(`Leitos ocupados`, na.rm = T), `Leitos com covid` = mean(`Leitos com covid`, na.rm = T),
                `Total leitos` = mean(`Total leitos`, na.rm = T), lotacao = mean(`Leitos ocupados`, na.rm = T)/mean(`Total leitos`, na.rm = T)) %>%
      arrange(semana_epidemiologica)
    
    ordem <- as.character(unique(aux$semana_epidemiologica))
    
    aux$semana_epidemiologica <- as.character(aux$semana_epidemiologica)
    
    p <- ggplot(aux) +
      geom_col(aes(x = semana_epidemiologica, y = `Leitos com covid`), fill = "#d81b60") +
      scale_x_discrete(limits = ordem) +
      scale_y_continuous(name = "Média semanal de leitos com covid", limits = c(0,max(aux$`Leitos com covid`+20))) +
      theme(axis.text.x = element_text(angle=45,size=8, vjust = 0.5)) 
    
    ggplotly(p)
    
  })
  

  ###############################
  ######  third tabItem  ########
  ###############################
  
  output$dados_covid <- renderValueBox({
    
    valueBox(
      value = "COVID-19",
      subtitle = "Dados extraídos do Painel Coronavírus RS da Secretaria de Saúde do RS",
      icon = icon("viruses"),
      color = "aqua",
      href = "http://ti.saude.rs.gov.br/covid19/",
      width = 12
    )
    
  })
  
  output$dados_leitos <- renderValueBox({
    
    valueBox(
      value = "UTI - Adulto",
      subtitle = "Secretaria da Saúde do Rio Grande do Sul",
      icon = icon("hospital"),
      color = "aqua",
      href = "http://ti.saude.rs.gov.br/covid19/leitos/dashboard.php",
      width = 12
    )
    
  })
  
  output$dados_regioes_covid <- renderValueBox({
    
    valueBox(
      value = "Dados regiões COVID",
      subtitle = "Shapefiles gentilmente gentilmente fornecidos pela DEPLAN-SEPLAG",
      icon = icon("archive"),
      color = "aqua",
      href = "https://planejamento.rs.gov.br/estudos-deplan",
      width = 12
    )
    
  })
  
  output$licenca <- renderValueBox({
    
    valueBox("Licença:", 
             subtitle = "(CC BY-SA 4.0)", 
             icon = icon("creative-commons"), 
             color = "aqua", 
             width = 12,
             href = "https://creativecommons.org/licenses/by-sa/4.0/deed.en"
    )
    
  })
  
  output$covidMetrika <- renderValueBox({
    
    valueBox("Site covidMetrika", 
             subtitle = div("Aplicativo desenvolvido pelo grupo covidMetrika",br(),"Confira aqui nosso site para ver nossos outros projetos!"), 
             icon = icon("external-link-alt"), 
             color = "orange", 
             width = 12,
             href = "https://www.ufrgs.br/covidmetrika/"
    )
    
  })
  
  output$git_covidMetrika <- renderValueBox({
    
    valueBox("Repositório covidMetrika", 
             subtitle = div("Confira aqui nosso repositório no GitHub!",br(),"Contato: covidmetrika@gmail.com"), 
             icon = icon("github"), 
             color = "orange", 
             width = 12,
             href = "https://github.com/CovidMetrika/dashboard_rs"
    )
    
  })
  
  output$dashboard_poa <- renderValueBox({
    
    valueBox("Dashboard POA", 
             subtitle = div("Confira aqui o nosso outro painel de dados com foco em Porto Alegre, informando os casos de COVID-19 e a situação dos leitos de UTI"), 
             icon = icon("columns"), 
             color = "orange", 
             width = 12,
             href = "https://mhbarbian.shinyapps.io/covid19_poa/"
    )
    
  })
  
  output$dashboard_br <- renderValueBox({
    
    valueBox("Dashboard Brasil", 
             subtitle = div("Confira aqui o nosso outro painel de dados com foco no Brasil e seus estados"), 
             icon = icon("columns"), 
             color = "orange", 
             width = 12,
             href = "https://mhbarbian.shinyapps.io/CovidMetrika/"
    )
    
  })
  
}

shinyApp(ui, server)



