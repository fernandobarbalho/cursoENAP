
load("enap_dados.Rdata")

library(ggplot2)
#Visualização de dados pode apoiar na sumarização da informação e até mesmo ajudar a verificar
#inconsistência de dados. Uma das formas de se verificar isso é através de gráficos que mostrem
#a distribuição dos dados em torno de uma variável.

#Num primeiro exemplo vamos visualizar como se distribui os dados ao longo dos anos.
#Primeiro vamos ver essa distribuição para os dados de itr

library(ggplot2)
#A bilbioteca ggplot2 trabalha com uma gramática de gráficos para gerar as figuras.
#A explicação dessa gramática virá pelo exemplo
itr_municipio %>% #indicação da fonte dos dados
  filter(ano>2000)%>%
  ggplot( #ggplot é a função que desenha o gráfico.  
    aes( #Na função aes indicamos os elementos que compõem um gráfico
      x=UF_sigla, #elemento x, normalmente eixo horizontal
      y=valor_arrecadado) #elemento y, normalmente eixo veritical
  )+
  geom_violin() + 
  scale_y_log10()
  #aqui indicamos a figura geométrica que fará a representação do gráfico.
#no caso, trata-se de um boxplot +
  
  
  itr_municipio %>% #indicação da fonte dos dados
    filter(ano>2000)%>%
    group_by(UF,municipio) %>%
    summarise(
      valor_arrecadado = sum(valor_arrecadado)
    ) %>%
    ungroup() %>%
    arrange(desc(valor_arrecadado)) %>%
    top_n(20) %>%
    mutate(municipio = reorder(municipio, valor_arrecadado)) %>%
    ggplot(aes(x=municipio, y= valor_arrecadado, fill= UF)) +
    geom_col() +
    coord_flip()
  
  itr_municipio %>% #indicação da fonte dos dados
    filter(ano>2000)%>%
    group_by(UF) %>%
    summarise(
      valor_arrecadado = sum(valor_arrecadado)
    ) %>%
    ungroup() %>%
    arrange(desc(valor_arrecadado)) %>%
    top_n(20) %>%
    mutate(UF = reorder(UF, valor_arrecadado)) %>%
    ggplot(aes(x=UF, y= valor_arrecadado, fill= UF)) +
    geom_col() +
    coord_flip()
    
    
  