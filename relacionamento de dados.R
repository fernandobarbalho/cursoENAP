library(readxl)
rec_adm_rfb_mun <- read_excel("arrecadacao-da-receita-administrada-pela-rfb-por-municipio-2005-a-2017.xlsx", 
                                                                                     skip = 5)
names(rec_adm_rfb_mun)

#alterando o formato do dado
rec_adm_rfb_mun <-
rec_adm_rfb_mun %>%
  gather(key = "ano", value = "valor_arrecadado_administradas", -(1:2))


#As tabelas se relacionam a partir de chaves que são variáveis comuns, com mesma semãntica.
#Nessa prática vamos relacionar os dados de arrecadação de TIR com receitas administradas
#a ideia é saber por município quanto se arrecada por cada um desses tipos de receita

#De forma a facilitar o uso do R na montagem das relações entre tabelas, as variáveis
#chaves devem ter rigorosamente o mesmo nome, inclusive no que diz respeito ao uso de 
#letras maiúsculas e minúsculas
names(rec_adm_rfb_mun)
names(itr_municipio)

#Para essas duas tabelas, as chaves são formadas pela combinação de ano, estado e nome de 
#município. Essas informações estão representadas de maneira distintas em cada uma das
#tabelas.
#Em ambas tabelas nós temos a variável ano
#Na tabela rec_adm_rfb_num nós temos as variáveis MUNICIPIOS e UF
#Na tabela itr_municipios nós temas as variáveis UF, UF_sigla e municipio

#A variável UF tem exatamente a mesma nomenclatura em ambas tabelas, porém a semântica é
#diferente entre elas. Numa nós temos a sigla da UF e na outra nós temos o nome completo da
#UF. Além disso nós temos na tabela itr_municipio a variável UF_sigla, que contém as siglas
#dos Estados

#Já em relação ao nome do municípios, os nomes das variáveis são diferentes inclusive no uso
#de letras maiúsculas e minúsculas. 

#Faz necessário então corrigir os problemas apontados acima

names(rec_adm_rfb_mun)[1]<-"municipio"
names(rec_adm_rfb_mun)[2]<-"UF_sigla"

names(rec_adm_rfb_mun)

#Normalmente os relacionamentos entre tabelas são feitos a partir de códigos.
#Os municípios brasileiros, por exemplo, tem seus códigos geridos pelo IBGE
#As tabelas que estamos trabalhando não tem esse código. Portanto para saber se um
#municipio está nas duas tabelas, é necessários que os seus respectivos nomes estão
#escritos exatamente da mesma forma em ambas tabelas.

#Os códigos abaixo fazem a transformação dos textos dos nomes dos municipios

rec_adm_rfb_mun$municipio<- toupper(rec_adm_rfb_mun$municipio)#coloca tudo em maiúsculo
itr_municipio$municipio<- toupper(itr_municipio$municipio)#coloca tudo em maiúsculo

library(abjutils)

rec_adm_rfb_mun$municipio <- rm_accent(rec_adm_rfb_mun$municipio)#retira acentos
itr_municipio$municipio <- rm_accent(itr_municipio$municipio)#retira acentos


#Agora já é possível fazer o relacionamento entre as duas tabelas.
#Nesse primeiro caso vamos criar uma nova tabela que traga todos as informações sobre as
#receitas arrecadadas dos municipios que estão na tabela itr_municipio e também na tabela
#rec_adm_rfb_mun

itr_municipio %>% 
  inner_join(rec_adm_rfb_mun) #todos os elementos que estão nas duas tabelas

#Nesse segundo caso, vamos fazer um relacionamento que traga todas as linhas de itr_municipio
#que estejam ou não também presentes em rec_adm_rfb_mun

itr_municipio %>% 
  left_join(rec_adm_rfb_mun) #todos os elementos que estão na primeira tabela 






