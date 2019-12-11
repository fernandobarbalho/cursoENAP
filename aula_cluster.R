library(cluster)
library(purrr)
set.seed(1)

#gera um formato não tidy para avaliar a evolução anual como parâmetro do cluster
adm_spread<-
  rec_adm_rfb_mun %>% 
  filter(UF_sigla != "-",
         valor_arrecadado_administradas >0) %>%
  spread(ano, valor_arrecadado_administradas)

model<- pam(x = adm_spread[,3:15], k = 2)
model$silinfo$avg.width
model<- pam(x = adm_spread[,3:15], k = 3)
model$silinfo$avg.width
model<- pam(x = adm_spread[,3:15], k = 4)
model$silinfo$avg.width


#gera um formato não tidy para avaliar a evolução anual como parâmetro do cluster
itr_spread<-
  itr_municipio %>% 
  filter(ano>2000) %>%
  spread(ano, valor_arrecadado)

#busca identificar qual o número ideal de grupos
sil_width <- map_dbl(2:10,  function(k){
  model<- pam(x = itr_spread[,4:21], k = k)
  model$silinfo$avg.width
})

silhueta<- sil_width

#O número ideal de grupos é 2
model<- pam(x = itr_spread[,4:21], k = 2)

#cria uma tabela 
itr_cluster<-
  itr_spread%>%
  mutate(cluster = model$clustering) 


itr_cluster %>%
  gather(key = "ano", value = "valor_arrecadado", -c(1:3,22)) %>%
  ggplot(aes(x= ano, y= valor_arrecadado )) +
  geom_line(aes(group = municipio,  color= factor(cluster))) +
  scale_y_log10()

#Informa o número de elementos por agrupamento formado

itr_cluster %>%  
  group_by(cluster) %>%
  summarise(
    quantidade= n()
  ) %>%
  ggplot()  +
  geom_col(aes(x=factor(cluster), y= quantidade, fill= factor(cluster)))  +
  coord_flip()




df_join<-
itr_municipio %>%
  inner_join(rec_adm_rfb_mun %>%
               filter(valor_arrecadado_administradas > 0,
                      UF_sigla != "-")
  )

df_join<-
df_join%>%
  mutate(valor_arrecadado=log(valor_arrecadado),
         valor_arrecadado_administradas = log(valor_arrecadado_administradas))

sil_width <- map_dbl(2:5,  function(k){
  model<- pam(x = df_join[,5:6], k = k)
  model$silinfo$avg.width
})

pam(x = df_join[,5:6], k = 3)
