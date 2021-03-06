#script para importação e tratamento de dados de ITR por município
#Essa linha e a linha de cima são comentários. Todo comentário começa com # 
#(cont) e não são reconhecidos pelo R

#O arquivo que vamos trabalhar está disponível no link indicado abaixo
#http://receita.economia.gov.br/dados/receitadata/arrecadacao/copy_of_arrecadacao-das-receitas-administradas-pela-rfb-por-municipio/arrecadacao-do-itr-por-municipio/arrecadacao-do-itr-por-municipio-e-uf-2000-a-2018.xlsx

#O comando abaixo atribui o endereço do arquivo para a variável endereco
endereco<-"http://receita.economia.gov.br/dados/receitadata/arrecadacao/copy_of_arrecadacao-das-receitas-administradas-pela-rfb-por-municipio/arrecadacao-do-itr-por-municipio/arrecadacao-do-itr-por-municipio-e-uf-2000-a-2018.xlsx"

#A linha abaixo faz o download do arquivo para o diretório corrente 

download.file(url = endereco, #url do arquivo de dadi abertique vai ser baixado
              destfile = "tir_municipio.xlsx", #nome do arquivo destino
              mode = "wb") #indica ao R que trata-se de arquivo binário

#Biblioteca para manipulação de planilha xlsx
library(readxl)
#comando para ler arquivo excel
tir_municipio <- read_excel("tir_municipio.xlsx",#nome do arquivo a ser lido 
                            skip = 8)#Desconsidera as 8 primeiras linhas

#nomes das variaveis
names(tir_municipio)

#alterando o nome da primeira coluna
names(tir_municipio)[1]<-"UF"
#alterando o nome da segunda coluna
names(tir_municipio)[2]<-"municipio"

names(tir_municipio)

unique(tir_municipio$UF)
