library(cluster)
library(purrr)
set.seed(1)


library(readxl)
rec_adm_rfb_mun <- read_excel("arrecadacao-da-receita-administrada-pela-rfb-por-municipio-2005-a-2017.xlsx", 
                              skip = 5)

rec_adm_rfb_mun <-
  rec_adm_rfb_mun %>%
  gather(key = "ano", value = "valor_arrecadado_administradas", -(1:2))

names(rec_adm_rfb_mun)[1]<-"municipio"
names(rec_adm_rfb_mun)[2]<-"UF_sigla"

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

adm_cluster<-
  adm_spread%>%
  mutate(cluster = model$clustering) 

save(list = "adm_cluster", file= "adm_cluster.RData")

adm_cluster %>%
  gather(key = "ano", value = "valor_arrecadado", -c(1:2,16)) %>%
  ggplot(aes(x= ano, y=  valor_arrecadado/10^6 )) +
  scale_color_viridis(discrete=TRUE) +
  geom_line(aes(group = municipio)) +
  theme_light() +
  theme(
    axis.text.x =  element_text(angle = 90, hjust = 1),
    panel.grid = element_blank()
  )+
labs(
  y = "Valor arrecadado em R$ milhões",
  x= NULL
)+
  scale_y_continuous(labels=function(x) format(x, big.mark = ".", scientific = FALSE))
  #scale_y_log10(labels=function(x) format(x, big.mark = ".", decimal.mark= ",", scientific = FALSE))


adm_cluster %>%
  gather(key = "ano", value = "valor_arrecadado", -c(1:2,16)) %>%
  ggplot(aes(x= ano, y= valor_arrecadado/10^6 )) +
  scale_color_viridis(discrete=TRUE) +
  geom_line(aes(group = municipio,  color= factor(cluster))) +
  theme_light() +
  theme(
    axis.text.x =  element_text(angle = 90, hjust = 1),
    panel.grid = element_blank()
  )+
  labs(
    y = "Valor arrecadado em R$ milhões",
    x= NULL,
    color = "Grupos"
  )+
  scale_y_continuous(labels=function(x) format(x, big.mark = ".", scientific = FALSE))+
  scale_y_log10(labels=function(x) format(x, big.mark = ".", scientific = FALSE))

adm_cluster %>%  
  group_by(cluster) %>%
  summarise(
    quantidade= n()
  ) %>%
  ggplot()  +
  geom_col(aes(x=factor(cluster), y= quantidade, fill= factor(cluster)))  +
  scale_fill_viridis(discrete=TRUE) +
  theme_light() +
  theme(
    
    panel.grid = element_blank()
  ) +
  labs(
    y = "Quantidade de municípios",
    x=  NULL,
    fill = "Grupos"
  )+
  coord_flip()

adm_cluster %>%
  gather(key = "ano", value = "valor_arrecadado", -c(1:2,16)) %>%
  group_by(cluster) %>%
  summarise(
    total= sum(valor_arrecadado, na.rm = TRUE)
  ) %>%
  ggplot()  +
  geom_col(aes(x=factor(cluster), y= total/10^6, fill= factor(cluster)))  +
  scale_fill_viridis(discrete=TRUE) +
  theme_light() +
  theme(
    
    panel.grid = element_blank()
  )+
  labs(
    y = "Total arrecadado (valores em R$ milhões)",
    x=  NULL,
    fill = "Grupos"
  )+
  scale_y_continuous(labels=function(x) format(x, big.mark = ".", scientific = FALSE)) +
  coord_flip()

adm_cluster %>%
  gather(key = "ano", value = "valor_arrecadado", -c(1:2,16)) %>%
  filter(cluster == 4) %>%
  ggplot(aes(x= ano, y= valor_arrecadado/10^6 )) +
  scale_color_viridis(discrete=TRUE) +
  geom_line(aes(group = municipio,  color= municipio)) +
  theme_light() +
  theme(
    axis.text.x =  element_text(angle = 90, hjust = 1),
    panel.grid = element_blank()
  )+
  labs(
    y = "Total arrecadado (valores em R$ milhões)",
    x=  NULL,
    color = "Municípios Grupo 4"
  )+
  scale_y_continuous(labels=function(x) format(x, big.mark = ".", scientific = FALSE))

adm_cluster %>%
  gather(key = "ano", value = "valor_arrecadado", -c(1:2,16)) %>%
  filter(cluster == 3) %>%
  ggplot(aes(x= ano, y= valor_arrecadado/10^6 )) +
  scale_color_viridis(discrete=TRUE) +
  geom_line(aes(group = municipio,  color= municipio)) +
  theme_light() +
  theme(
    axis.text.x =  element_text(angle = 90, hjust = 1),
    panel.grid = element_blank()
  )+
  labs(
    y = "Total arrecadado (valores em R$ milhões)",
    x=  NULL,
    color = "Municípios Grupo 3"
  )+
  scale_y_continuous(labels=function(x) format(x, big.mark = ".", scientific = FALSE))


adm_cluster %>%
  gather(key = "ano", value = "valor_arrecadado", -c(1:2,16)) %>%
  filter(cluster == 3) %>%
  ggplot(aes(x= ano, y= valor_arrecadado/10^6 )) +
  scale_color_viridis(discrete=TRUE) +
  geom_line(aes(group = municipio,  color= municipio)) +
  theme_light() +
  theme(
    strip.text =  element_blank(),
    axis.text.x =  element_text(angle = 90, hjust = 1),
    panel.grid = element_blank()
  )+
  labs(
    y = "Total arrecadado (valores em R$ milhões)",
    x=  NULL,
    color = "Municípios Grupo 3"
  )+
  scale_y_continuous(labels=function(x) format(x, big.mark = ".", scientific = FALSE))+
  facet_grid(municipio~.,  space = "free_y")


adm_cluster %>%
  gather(key = "ano", value = "valor_arrecadado", -c(1:2,16)) %>%
  filter(cluster == 3) %>%
  ggplot(aes(x= ano, y= valor_arrecadado/10^6 )) +
  scale_color_viridis(discrete=TRUE) +
  geom_line(aes(group = municipio,  color= municipio)) +
  theme_light() +
  theme(
    axis.text.x =  element_text(angle = 90, hjust = 1),
    panel.grid = element_blank()
  )+
  labs(
    y = "Total arrecadado (valores em R$ milhões)",
    x=  NULL,
    color = "Municípios Grupo 3"
  )+
  scale_y_continuous(labels=function(x) format(x, big.mark = ".", scientific = FALSE))



adm_cluster %>%
  gather(key = "ano", value = "valor_arrecadado", -c(1:2,16)) %>%
  filter(cluster == 2) %>%
  group_by(municipio) %>%
  summarise(
    total =  sum(valor_arrecadado, na.rm = TRUE)
  ) %>%
  arrange(total) %>%
  top_n(10) %>%
  mutate(municipio = reorder(municipio, total)) %>%
  ggplot()  +
  geom_col(aes(x=municipio, y= total/10^6), fill = "#31698E")  +
  scale_fill_viridis(discrete=TRUE) +
  theme_light() +
  theme(
    
    panel.grid = element_blank()
  )+
  labs(
    y = "Total arrecadado (valores em R$ milhões)",
    x=  NULL,
    fill = "Município"
  )+
  scale_y_continuous(labels=function(x) format(x, big.mark = ".", scientific = FALSE)) +
  coord_flip()

adm_cluster %>%
  gather(key = "ano", value = "valor_arrecadado", -c(1:2,16)) %>%
  filter(cluster == 2) %>%
  group_by(municipio) %>%
  summarise(
    total =  sum(valor_arrecadado, na.rm = TRUE)
  ) %>%
  arrange(total) %>%
  top_n(-10) %>% 
  mutate(municipio = reorder(municipio, total)) %>%
  ggplot()  +
  geom_col(aes(x=municipio, y= total/10^6), fill = "#31698E")  +
  
  scale_fill_viridis(discrete=TRUE) +
  theme_light() +
  theme(
    
    panel.grid = element_blank()
  )+
  labs(
    y = "Total arrecadado (valores em R$ milhões)",
    x=  NULL,
    fill = "Município"
  )+
  scale_y_continuous(labels=function(x) format(x, big.mark = ".", scientific = FALSE)) +
  coord_flip()


adm_cluster %>%
  gather(key = "ano", value = "valor_arrecadado", -c(1:2,16)) %>%
  filter(cluster == 1) %>%
  ggplot() +
  geom_boxplot(aes(x= UF_sigla, y= valor_arrecadado/10^6), color= "#440053")+
  theme_light() +
  theme(
    
    panel.grid = element_blank()
  )+
  labs(
    y = "Total arrecadado (valores em R$ milhões)",
    x=  NULL
  )+
  scale_y_log10(labels=function(x) format(x, big.mark = ".", decimal.mark = ",", scientific = FALSE)) 
  

