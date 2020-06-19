library(shiny)
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
library(tidyverse)
library(scales)
library(shinydashboardPlus)
library(shinyEffects)
library(lubridate)
library(ggthemes)
library(shinyalert)
library(shinyBS)

# rodando o script de data_wrangling para obtenção dos dados

source("data_wrangling.R", encoding = "UTF-8")
source("my_ui.R", encoding = "UTF-8")
source("my_server.R", encoding = "UTF-8")

#data_hora_atual <- str_c("Última atualização em ",format(with_tz(httr::GET("http://www.google.com/")$date, "America/Sao_Paulo"), "%H:%M %d/%m/%Y"))

##############################################################################################
# Aplicativo
##############################################################################################

shinyApp(
  ui = ui, 
  server = server
)





