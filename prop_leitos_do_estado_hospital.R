library(tidyverse)

# plot percentual hospital

source("data_wrangling.R", encoding = "UTF-8")

# hospitais no banco
unique(leitos_uti$hospital)

# hospital/is de interesse
hospital_escolhido <- c("Hospital De Clinicas")

aux <- leitos_uti %>%
  group_by(data_atualizacao) %>%
  mutate(soma_tot = sum(leitos_total, na.rm = T), soma_inter = sum(leitos_internacoes, na.rm = T), soma_covid = sum(leitos_covid, na.rm = T)) %>%
  group_by(data_atualizacao, hospital) %>%
  summarise(leitos_total_prop = sum(leitos_total, na.rm = T)/first(soma_tot), leitos_internacoes_prop = sum(leitos_internacoes, na.rm = T)/first(soma_inter), leitos_covid_prop = sum(leitos_covid, na.rm = T)/first(soma_covid)) %>%
  filter(hospital %in% hospital_escolhido)


p <- ggplot(aux) +
  geom_col(aes(x = data_atualizacao, y = leitos_covid_prop)) +
  scale_y_continuous(labels=percent) +
  scale_x_date(date_breaks = "1 week")

ggplotly(p)







