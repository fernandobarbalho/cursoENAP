library(cluster)
library(purrr)
set.seed(1)

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
  


