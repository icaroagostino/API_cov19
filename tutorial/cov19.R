library(httr)     # Cominucacao com API's
library(jsonlite) # Manipualcao de .json
library(ggplot2)  # graficos
library(plotly)   # graficos interativos

cov_br_conf <- GET(url = "https://api.covid19api.com/country/brazil/status/confirmed",
                   encode = 'json') %>%
  content %>%
  toJSON %>%
  fromJSON

cov_br_rec <- GET(url = "https://api.covid19api.com/country/brazil/status/recovered",
                  encode = 'json') %>%
  content %>%
  toJSON %>%
  fromJSON

cov_br_dt <- GET(url = "https://api.covid19api.com/country/brazil/status/deaths",
                 encode = 'json') %>%
  content %>%
  toJSON %>%
  fromJSON

datas <- cov_br_conf$Date %>% unlist %>% as.Date()

cov_br <- data.frame(
  
  dia = datas,
  confirmados = cov_br_conf$Cases %>% unlist,
  mortes      = cov_br_dt$Cases   %>% unlist,
  recuperados = cov_br_rec$Cases  %>% unlist
  
)

cov_br <- cov_br[-1,]

graph_conf <- cov_br %>%
  ggplot(aes(x=dia)) +
  geom_area( aes(y = confirmados), fill  = "#f25a5a", alpha = 0.4) +
  geom_line( aes(y = confirmados), color = "#f25a5a", size=1) +
  geom_point(aes(y = confirmados), color = "#f25a5a", size=2)

graph_mort <- cov_br %>%
  ggplot(aes(x=dia)) +
  geom_area( aes(y = mortes), fill  = "#f28f5a", alpha = 0.4) +
  geom_line( aes(y = mortes), color = "#f28f5a", size = 1) +
  geom_point(aes(y = mortes), color = "#f28f5a", size = 2)

graph_rec <- cov_br %>%
  ggplot(aes(x=dia)) +
  geom_area( aes(y = recuperados), fill  = "#69b37b", alpha = 0.4) +
  geom_line( aes(y = recuperados), color = "#69b37b", size = 1) +
  geom_point(aes(y = recuperados), color = "#69b37b", size = 2)
