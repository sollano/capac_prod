library(tidyverse)
library(forestr)
tab_coef <- data.frame(talhao = c(1:20, 24,25,27,28,30,31,33,35,36,37), 
                        rbind(data.frame(b0 = rep(23, 20), b1 = rep(0.03, 20), b2 = rep(1.3, 20) ), 
                              data.frame(b0 = rep(23, 10), b1 = rep(0.03, 10), b2 = rep(1, 10) )  )  )


dados2 <- full_join(dados, tab_coef)

dados %>%
  group_by(talhao) %>% 
  nest() %>% 
  mutate(Reg = map(data, ~safe_nls( Hd1 ~ b0 * (1 - exp(1)^( -b1 * Id )  )^b2, ., 
                                    start = c( b0=23, b1=0.03, b2 = 1.3  ) )[[1]] ),
         Coefs = map(Reg, pos_tidy)
  ) %>% unnest(Coefs, .drop = T) %>% select(-X1) -> ouseraquenao

dados %>%
  group_by(talhao) %>% 
  nest() %>% 
  mutate(Reg = map(data, ~safe_nls( Hd1 ~ b0 * (1 - exp(1)^( -b1 * Id )  )^b2, ., 
                                    start = c( b0=23, b1=0.03, b2 = 1.3  ) )[[1]] ),
         Coefs = map(Reg, pos_tidy)
  ) %>% unnest(Coefs, .drop = T) %>% select(-X1)

dados2 %>%
  group_by(talhao) %>% 
  nest() %>% 
  mutate(Reg = map(data, ~safe_nls( Hd1 ~ b0 * (1 - exp(1)^( -b1 * Id )  )^b2, ., 
                                    start = c( b0=mean(.$b0), b1=mean(.$b1), b2 = mean(.$b2)  ) )[[1]] ),
         Coefs = map(Reg, pos_tidy)
  )  %>% 
  unnest(Coefs, .drop = T) %>% 
  select(-X1)%>% 
  as.data.frame -> seraquefoi


# ####

.groups <- "talhao"

betas <- names(tab_coef)[!names(tab_coef) %in% .groups]



dados2 %>%
  group_by(talhao) %>% 
  nest() %>% 
  mutate(Reg = map(data, ~safe_nls( Hd1 ~ b0 * (1 - exp(1)^( -b1 * Id )  )^b2, ., 
                                    start = c( "b0"=mean(.$b0), "b1"=mean(.$b1), "b2" = mean(.$b2)  ) )[[1]] ),
         Coefs = map(Reg, pos_tidy)
  )  %>% 
  unnest(Coefs, .drop = T) %>% 
  select(-X1)%>% 
  as.data.frame 



dados2 %>%
  group_by(talhao) %>% 
  nest() %>% 
  mutate(Reg = map(data, ~safe_nls( Hd1 ~ b0 * (1 - exp(1)^( -b1 * Id )  )^b2, ., 
                                    start = c( betas[1]=mean(.$b0), betas[2]=mean(.$b1), betas[3] = mean(.$b2)  ) )[[1]] ),
         Coefs = map(Reg, pos_tidy)
  )  %>% 
  unnest(Coefs, .drop = T) %>% 
  select(-X1)%>% 
  as.data.frame 
