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

#----------------------

# Total acumulado de casos

datas <- cov_br_conf$Date %>% unlist %>% as.Date()

cov_br <- data.frame(
  
  dia = datas,
  confirmados = cov_br_conf$Cases %>% unlist,
  mortes      = cov_br_dt$Cases   %>% unlist,
  recuperados = cov_br_rec$Cases  %>% unlist
  
)

cov_br <- cov_br[36:NROW(cov_br),]

graph_conf <- cov_br %>%
  ggplot(aes(x = dia)) +
  geom_area( aes(y = confirmados), fill  = "#f25a5a", alpha = 0.4) +
  geom_line( aes(y = confirmados), color = "#f25a5a", size=1) +
  geom_point(aes(y = confirmados), color = "#f25a5a", size=2)

graph_mort <- cov_br %>%
  ggplot(aes(x = dia)) +
  geom_area( aes(y = mortes), fill  = "#f28f5a", alpha = 0.4) +
  geom_line( aes(y = mortes), color = "#f28f5a", size = 1) +
  geom_point(aes(y = mortes), color = "#f28f5a", size = 2)

graph_rec <- cov_br %>%
  ggplot(aes(x = dia)) +
  geom_area( aes(y = recuperados), fill  = "#69b37b", alpha = 0.4) +
  geom_line( aes(y = recuperados), color = "#69b37b", size = 1) +
  geom_point(aes(y = recuperados), color = "#69b37b", size = 2)

#----------------------

# Novos casos

new_cases <- function(col){
  
  a <- cov_br[[col]][1]
  
  for (i in 1:(NROW(cov_br)-1)){
    
    a <- c(a, cov_br[[col]][i+1] - cov_br[[col]][i])
    
  }
  
  return(a)
  
}

cov_br_nc <- data.frame(
  
  dia = cov_br$dia,
  confirmados = new_cases(2),
  mortes      = new_cases(3),
  recuperados = new_cases(4)
  
)

graph_conf_nc <- cov_br_nc %>%
  ggplot(aes(x = dia, y = confirmados)) +
  geom_bar(stat = "identity", fill  = "#f25a5a", alpha = 0.5) +
  geom_line( aes(y = confirmados), color = "#f25a5a", size=1) +
  geom_point(aes(y = confirmados), color = "#f25a5a", size=2)

graph_mort_nc <- cov_br_nc %>%
  ggplot(aes(x = dia, y = mortes)) +
  geom_bar(stat = "identity", fill  = "#f28f5a", alpha = 0.5) +
  geom_line( aes(y = mortes), color = "#f28f5a", size = 1) +
  geom_point(aes(y = mortes), color = "#f28f5a", size = 2)

graph_rec_nc <- cov_br_nc %>%
  ggplot(aes(x = dia, y = recuperados)) +
  geom_bar(stat = "identity", fill  = "#69b37b", alpha = 0.5) +
  geom_line( aes(y = recuperados), color = "#69b37b", size = 1) +
  geom_point(aes(y = recuperados), color = "#69b37b", size = 2)

#----------------------

# Último dia dos dados

cov_ld <- data.frame(
  
  row.names = c("Último dia", "Total acumulado"),
  
  `Casos confirmados` = c(cov_br_nc[NROW(cov_br_nc),][[2]],
                        cov_br[NROW(cov_br),][[2]]),
  
  `Mortes confirmadas` = c(cov_br_nc[NROW(cov_br_nc),][[3]],
                         cov_br[NROW(cov_br),][[3]]),
  
  Recuperados = c(cov_br_nc[NROW(cov_br_nc),][[4]],
                  cov_br[NROW(cov_br),][[4]])
  
)
