# myUI

options(OutDec = ",") # Muda de ponto para virgula nos decimais! 

header <- dashboardHeaderPlus(
  enable_rightsidebar = T,
  rightSidebarIcon = "gears",
  title = tagList(
    span(class = "logo-lg", "Painel COVID-19 RS"), 
    icon = icon("tachometer-alt")),
  titleWidth = 350
)

sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Leitos UTI - Adulto", tabName = "mapa_leitos_rs", icon = icon("procedures")),
    menuItem("COVID-19", tabName = "mapa_covid_rs", icon = icon("virus")),
    menuItem("Fonte de dados", tabName = "fonte", icon = icon("archive")),
    menuItem("CovidMetrika", tabName = "sobre", icon = icon("users"))
  ),
  width = 180
)

rightsidebar <- rightSidebar(
  width = 320,
  h3("Digite as regiões de interesse"),
  selectizeInput("filtro_covid",
                 label = NULL,
                 choices = levels(as.factor(dados_covid_rs$regiao_covid)),
                 selected = levels(as.factor(dados_covid_rs$regiao_covid)),
                 multiple = T,
                 width = "100%")
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
                    width = 5,
                    h1("COVID-19 Rio Grande do Sul"),
                    #h3("É possível escolher quais são as regiões do RS devem ser apresentadas, 
                    #o ícone no canto superior direito possibilita essa seleção")
                    #para isso
                    #    ícone no canto superior direito há um ícone em que ")
                    # h5(em(data_hora_atual))
                    h4("O ícone no canto superior direito seleciona quais 
                    as regiões covid do RS serão apresentadas")
                  ),
                  column(
                    tags$img(src = "logos.png", 
                             width = "100%"),
                    width = 7
                  )
                )
              ),
              # reescrevendo as cores default para as que eu quero nas boxes de óbitos cartório
              
              tags$style(".small-box.bg-lime { background-color: #404040 !important; color: #FFFFFF !important; }"),
              #bsModal("modal_incidencia", "Incidência", "box_inci", size = "small", "Número de casos confirmados de COVID-19 por 100 000 habitantes na população de determinado espaço geográfico"),
              #bsModal("modal_mortalidade", "Mortalidade", "box_mort", size = "small", "Número de óbitos confirmados por COVID-19 por 100 000 habitantes na população de determinado espaço geográfico"),
              #setShadow(id = "box_inci", class = "small-box"),
              #setShadow(id = "box_mort", class = "small-box"),
              fluidRow(
                column(
                  width = 4,
                  h3("Selecione a variável de interesse"),
                  radioButtons("var_covid",
                               label = NULL,
                               choices = list("Confirmados" = "confirmados","Óbitos" = "obitos",
                                              "Recuperados" = "recuperados", "Em acompanhamento" = "acompanhamento"),
                               selected = "confirmados",
                               inline = T)
                ),
                column(
                  width = 4,
                  h3("Selecione o tipo de agrupamento"),
                  radioButtons("agrup_covid",
                               label = NULL,
                               choices = list("Municípios" = "municipio", "Regiões COVID" = "regiao_covid"),
                               selected = "municipio",
                               inline = T),
                ),
                column(
                  width = 4,
                  h3("Escolha entre taxa por 100 mil habitantes ou número original"),
                  radioButtons("tipo_covid",
                               label = NULL,
                               choices = list("Número" = "", "Taxa" = "_taxa"),
                               selected = "",
                               inline = T),
                )
              ),
              fluidRow(
                valueBoxOutput("box_conf", width = 3),
                valueBoxOutput("box_obit", width = 3),
                valueBoxOutput("box_recu", width = 3),
                valueBoxOutput("box_acom", width = 3)
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
                  width = 6,
                  uiOutput("ui_faixa_etaria_covid")
                ),
                column(
                  width = 6,
                  uiOutput("ui_sexo_covid")
                ),
                column(
                  width = 12,
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
                               selected = "leitos_covid",
                               inline = T)
                ),
                column(
                  width = 4,
                  h3("Selecione o tipo de agrupamento"),
                  radioButtons("agrup_leitos",
                               label = NULL,
                               choices = list("Hospital" = "hospital", "Municípios" = "municipio", "Regiões COVID" = "regiao_covid"),
                               selected = "hospital",
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

ui <- dashboardPagePlus(enable_preloader = T, rightsidebar = rightsidebar, header = header, sidebar = sidebar, 
                        body = body, skin = "red")