# testando novos plots



# pegando dados do script de manipulação
#source("data_wrangling.R", encoding = "UTF-8")


# plot waffle - em construção
plot_waffle <- function(){
  
  hospitalizados <- dados_covid_rs %>%
    group_by(hospitalizacao) %>%
    summarise(n = n()) %>%
    mutate(prop = n/sum(n))
  
  letalidade <- dados_covid_rs %>%
    group_by(evolucao) %>%
    summarise(n = n()) %>%
    mutate(prop = n/sum(n)) %>%
    filter(!is.na(evolucao))
  
  aux <- c(letalidade$n[letalidade$evolucao=="OBITO"],hospitalizados$n[hospitalizados$hospitalizacao=="Sim"],hospitalizados$n[hospitalizados$hospitalizacao=="Nao"])
  
  names(aux) <- c("OBI","hospi","sem hospi")
  
  waffle(aux/100, rows = 10)

  
  parts <- c(80, 30, 20, 10)
  waffle(parts, rows=8, use_glyph = "male")
  
  warningsparts <- data.frame(
    names = LETTERS[1:4],
    vals = c(80, 30, 20, 10)
  )
  
  waffle(parts, rows=8)

}

# plot do número de casos em: (acompanhamento, recuperados, óbitos) por dia
plot_evolucao <- function(){
  
  #input <- list(var_covid = "acompanhamento", agrup_covid = "municipio", tipo_covid = "_taxa",filtro_covid = unique(dados_covid_rs$regiao_covid), filtro_serie_covid ="Todos selecionados")
  
  pop <- pop_regiao %>%
    filter(regiao_covid %in% input$filtro_covid)
    
  pop <- sum(pop$populacao_estimada_regiao_covid)
    
  aux <- dados_covid_rs %>%
    filter(regiao_covid %in% input$filtro_covid) %>%
    mutate(obitos = ifelse(evolucao == "OBITO", 1, 0),
           acompanhamento = ifelse(is.na(evolucao), 0, 1),
           recuperados = ifelse(evolucao == "CURA", 1, 0))
  
  obi_rec <- aux %>%
    group_by(data_evolucao) %>%
    summarise(obitos = sum(obitos, na.rm = T), obitos_taxa = sum(obitos, na.rm = T)*100000/pop,
              recuperados = sum(recuperados, na.rm = T), recuperados_taxa = sum(recuperados, na.rm = T)*100000/pop) %>%
    filter(!is.na(data_evolucao)) %>%
    mutate(data_confirmacao = data_evolucao) %>%
    select(-data_evolucao)

  acomp <- aux %>%
    group_by(data_sintomas) %>%
    summarise(acompanhamento = sum(acompanhamento, na.rm = T), acompanhamento_taxa = sum(acompanhamento, na.rm = T)*100000/pop) %>%
    mutate(data_confirmacao = data_sintomas) %>%
    select(-data_sintomas)
  
  n_days <- max(obi_rec$data_confirmacao,acomp$data_confirmacao)-min(obi_rec$data_confirmacao,acomp$data_confirmacao)
  dias <- min(obi_rec$data_confirmacao,acomp$data_confirmacao)+days(0:n_days)
  
  aux2 <- tibble(data_confirmacao = dias[!(dias %in% obi_rec$data_confirmacao)],
                 obitos = 0,
                 obitos_taxa = 0,
                 recuperados = 0,
                 recuperados_taxa = 0)
  
  obi_rec <- bind_rows(obi_rec,aux2) %>%
    arrange(data_confirmacao)
  
  acomp2 <- tibble(data_confirmacao = dias[!(dias %in% acomp$data_confirmacao)],
                   acompanhamento = 0,
                   acompanhamento_taxa = 0)
  
  acomp <- bind_rows(acomp,acomp2) %>%
    arrange(data_confirmacao) %>%
    left_join(obi_rec, by = "data_confirmacao") %>%
    mutate(acompanhamento = acompanhamento-obitos-recuperados,
           acompanhamento_taxa = acompanhamento_taxa-obitos_taxa-recuperados_taxa) %>%
    mutate(across(where(is.numeric), cumsum))
  
  label_x <- "Data"
  caption_x <- paste("*Dados para casos em acompanhamento são referentes à data de início dos sintomas,","portanto dados mais antigos são frequentemente adicionados", sep = "\n")
  y_caption <- 0.99
  
  p <- ggplotly(ggplot(acomp, aes(text = paste(data_confirmacao,paste0("Em acompanhamento ",round(acompanhamento,0)),
                                               paste0("Óbitos ",round(obitos,0)),
                                               paste0("Recuperados ",round(recuperados,0)),
                                               paste0("Taxa em acompanhamento ",round(acompanhamento_taxa,2)),
                                               paste0("Taxa de óbitos ",round(obitos_taxa,2)),
                                               paste0("Taxa de recuperados ",round(recuperados_taxa,2)),sep = "\n"))) +
                  geom_line(aes(x = data_confirmacao, y = acompanhamento, group = 1, color = "Em acompanhamento")) +
                  geom_line(aes(x = data_confirmacao, y = recuperados, group = 1, color = "Recuperados")) +
                  geom_line(aes(x = data_confirmacao, y = obitos, group = 1, color = "Óbitos")) +
                  scale_x_date(date_breaks = "1 month", date_labels = "%b") +
                  scale_color_manual(values = list("Em acompanhamento" = opcoes[["acompanhamento"]][["cor"]],
                                                   "Recuperados" = opcoes[["recuperados"]][["cor"]],
                                                   "Óbitos" = opcoes[["obitos"]][["cor"]])) +
                  labs(x = label_x, y = "Casos", colour = NULL, fill = NULL) +
                  theme(plot.background = element_rect(fill = "transparent", color = NA), # bg of the plot
                        panel.grid.major = element_blank(),
                        legend.position=c(0.05, 0.95),
                        legend.key = element_blank()),
                tooltip = c("text")) %>%
    layout(legend = list(
      orientation = "v",
      x = 0.01,
      y = 0.85
    ),
    annotations = list(
      list(x = 0.001, y = y_caption, text = caption_x, 
           showarrow = F, xref='paper', yref='paper', 
           xanchor='left', yanchor='auto', xshift=0, yshift=0,
           font=list(size=10, color="gray"))
    )
    )

  for (i in 1:length(p$x$data)){
    if (!is.null(p$x$data[[i]]$name)){
      p$x$data[[i]]$name =  gsub("\\(","",str_split(p$x$data[[i]]$name,",")[[1]][1])
    }
  }
  
  return(p)
}




# plot



