library(httr)     # Cominucacao com API's
library(jsonlite) # Manipualcao de .json
library(ggplot2)  # graficos
library(plotly)   # graficos interativos

dados <- 
  GET(url = "https://brasil.io/api/dataset/covid19/caso/data?is_last=True&place_type=state") %>% 
  content %>%
  toJSON %>%
  fromJSON

ultima_data <- dados[["results"]][["date"]][[1]] %>%
  as.Date %>%
  format(format="%d/%m/%Y")

dados <- data.frame(
  
  estado      = dados$results$state     %>% unlist,
  confirmados = dados$results$confirmed %>% unlist,
  mortes      = dados$results$deaths    %>% unlist
  
)

conf_est <- ggplot(dados,
                   aes(x = reorder(estado, confirmados),
                       y = confirmados)) + 
  geom_bar(stat = "identity", fill  = "#f25a5a", alpha = 0.5) +
  geom_line( aes(y = confirmados), color = "#f25a5a", size=1) +
  geom_point(aes(y = confirmados), color = "#f25a5a", size=2) +
  xlab("Estados") +
  coord_flip()

mort_est <- ggplot(dados,
                   aes(x = reorder(estado, mortes),
                       y = mortes)) + 
  geom_bar(stat = "identity", fill  = "#f28f5a", alpha = 0.5) +
  geom_line( aes(y = mortes), color = "#f28f5a", size = 1) +
  geom_point(aes(y = mortes), color = "#f28f5a", size = 2) +
  xlab("Estados") +
  coord_flip()
