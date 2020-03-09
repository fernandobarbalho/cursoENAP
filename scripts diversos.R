library(dplyr)
library(ggplot2)
library(rtweet)



#Gráfico do ranking dos 10 países que mais visitam o Brasil
chegadas_2018%>%
  #mutate(cod_mes= factor(`cod mes`))%>%
  group_by(País)%>%
  summarise(
    total=(sum(Chegadas)/1000)
  ) %>%
  arrange(desc(total))%>%
  top_n(10)%>%
  ungroup()%>%
  mutate(País = reorder(País, total)) %>%
  ggplot()+
  geom_col(aes(x=País, y=total), fill= "#72ADDA")+
  coord_flip() +
  theme_light() +
  labs(
    title = "Visitantes estrangeiros em 2018 (em milhares)"
  ) +
  xlab(NULL) +
  ylab(NULL)

#Gráfico do ranking dos estados por número de visitantes
chegadas_2018%>%
  group_by(UF)%>%
  summarise(
    total=(sum(Chegadas)/1000)
  ) %>%
  arrange(desc(total))%>%
  ungroup()%>%
  mutate(UF = reorder(UF, total)) %>%
  ggplot()+
  geom_col(aes(x=UF, y=total), fill= "#72ADDA")+
  coord_flip() +
  theme_light() +
  labs(
    title = "Visitantes estrangeiros em 2018 por UF(em milhares)"
  ) +
  xlab(NULL) +
  ylab(NULL)


#Gráfico da série temporal dos meses  
chegadas_2018%>%
  mutate(cod_mes= factor(`cod mes`))%>%
  group_by(cod_mes, Continente) %>%
  summarise(
    total=(sum(Chegadas)/1000)
  ) %>%
  ggplot()+
  geom_col(aes(x= cod_mes, y=total, fill= Continente))+
  theme_light() +
  labs(
    title = "Visitantes estrangeiros em 2018 por mês(em milhares)"
  ) +
  xlab(NULL) +
  ylab(NULL)


library(tidyr)
library(cluster)
#Análise de clusters dos países
prep_model<- chegadas_2018%>%
  group_by(País) %>%
  summarise(
    total = sum(Chegadas)
  ) 


model<- pam(x = prep_model[,2], k = 3)

# Optou-se por 3 clusters
chegadas_cluster<-
  prep_model%>%
  mutate(cluster = model$clustering) 

chegadas_cluster %>%
  ggplot(aes(x=factor(cluster), y= total/1000))+
  geom_jitter(aes(color= factor(cluster))) 
#geom_boxplot()


#Total de entradas por cluster
chegadas_cluster%>%
  group_by(cluster) %>%
  summarise(
    total = sum(total)
  )%>%
  ggplot()+
  geom_col(aes(x=cluster, y =total, fill= factor(cluster)))
  

#Número de países por cluster
chegadas_cluster%>%
  ggplot()+
  geom_bar(aes(x=cluster, fill= factor(cluster)))


#Gráfico só do cluster 2
chegadas_cluster%>%
  filter(cluster==2)%>%
  mutate(País = reorder(País, total)) %>%
  ggplot()+
  geom_col(aes(x=País, y =total))+
  coord_flip()


#Gráfico com os top 10 do cluster 1
chegadas_cluster%>%
  filter(cluster==1)%>%
  arrange(desc(total))%>%
  select(País, total) %>%
  top_n(10) %>%
  mutate(País = reorder(País, total)) %>%
  ggplot()+
  geom_col(aes(x=País, y =total))+
  coord_flip()



chegadas_2018%>%
  #filter(País=="Argentina") %>%
  group_by(UF, Via)%>%
  summarise(
    total=sum(Chegadas)
  ) %>%
  ungroup()%>%
  mutate(UF=reorder(UF, total))%>%
  ggplot()+
  geom_col(aes(x=UF,y=total, fill= Via))+
  coord_flip()

  

df <- data.frame(x = c(10, 4, 1, 6, 3, 1, 1))
df %>% top_n(2)

rtweet::post_message("rtweet::post_message()")

rtweet::post_tweet(status= "rtweet::post_tweet('status= text')")


post_tweet(status ="test")

post_tweet(status=" first in a thread ")
