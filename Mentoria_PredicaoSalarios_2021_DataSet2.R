# Mini-Projeto 3 - Planejamento de Carreira para Pessoas
# A Análise Estatística em Avaliações Anuais de Desempenho foi reutilizada para basear o salário em relação a educação.

# Neste Mini-Projeto nosso objetivo é aplicar análise estatística em dados de salário em relação a educação em busca de provável correlação entre nível educacional e salário.

# Diretório de trabalho
#getwd()
#setwd("~/Dropbox/DSA/AnaliseEstatisticaI/Modulo09/Mini-Projeto3")

# Pacotes
library(ggplot2)
library(dplyr)
library(GGally)
library(pscl)
library(ROCR)


########## Carregando os Dados ########## 

# Carregando os dados que foram salvos ao final do script da Parte 1.
dados_aval = read.csv("C:/Users/Sheila Dias/Downloads/NYC_Jobs3.csv", sep = ',', stringsAsFactors = TRUE)

# Removemos a primeira coluna por ser índice
df_aval <- dados_aval

# Tipos de dados
str(df_aval)

# Sumário
summary(df_aval)

# Visualiza os dados
View(df_aval)

view(df_aval_saida_positiv)
?view #data$Recruitment.Contact <- NULL

install.packages("dplyr")

#df_aval1 <- data$Recruitment.Contact <- NULL

#view(df_aval)

#foram tratados dados, retiradas letras do data set de níveis: para não termos nível 0, acrescentamos 1 aos níveis.
#os cargos gerenciais, foram retiradas a letra M de Manager e os cargos de diretoria que estavam com Y, colocamos 6 para virar 7.

#mtcars[ ,c("Level")]
#na.omit(df_aval["Salary.Range.From"])

#df_aval <- na.omit(df_aval["Salary.Range.To", "Salary.Range.From"])
#names(df_aval)

#df_aval <- na.omit(df_aval["Level","Career.Level"])



#df_aval <- mtcars[ ,c("Level")] # seleciona quatro colunas

# Nomes das colunas
names(df_aval)


########## Correlação ########## 

# Vamos analisar a Correlação entre algumas das variáveis numéricas
ggpairs(df_aval, columns = c("Career.Level",
                             "Salary.Range.From",
                             "Salary.Range.To",
                             "Level"))

# Para interpretar as correlações, analise o gráfico observando linhas e colunas 
# (o conceito de correlação foi abordado na aula 5):

# A primeira visualisação de correlação mostrava que existia um vale entre os níveis de carreira. Precisa passar alguma
#ferramenta de limpeza ou de transformação antes de rodar o GGpairs novamente.

# Ao rodar o NA omit para excluir os NAs havia nas em todas as colunas. precisa primeiro excluir algumas colunas.
#Para selecionar as quatro colunas usar o comando MTCARS:
mtcars[,c("Level")]

names(df_aval)

#Utilizamos o excel para limpeza dos NAs e para tratamento de alguns dados missings e com textos misturados, para
#excluir colunas em branco e tambem perdemos 14 linhas que ainda continha dados com dízimas periódicas.
# Os dados com dízimas periódicas foram removidos automaticamente pela biblioteca stat_density do R as demais 12 foram removidas
#pela biblioteca ggally_statistic (data, mapping, na.rm, geom_point).


names(df_aval) # Mas o mais importante: quando um funcionário estava insatisfeito, ele deixou a empresa.


########## Teste t Para Confirmar a Hipótese ########## 

# Vamos realizar um teste t com nível de confiança de 95% e ver se ele rejeita corretamente 
# a hipótese nula de que a amostra vem da mesma distribuição que a população de funcionários. 
#Aplicando NA omits para excluir as linhas que contem NA:

#df_aval <-na.omit(df_aval)

#ggpairs(df_aval, columns = c("Career.Level",                              "Salary.Range.From",                             "Salary.Range.To",
                # "Level"))
# Primeiro, vamos analisar o salário médio das carreiras
names(df_aval)
salarioinicial_media <- mean(df_aval$Salary.Range.From)
salarioinicial_media

salariomaximo_media <- mean(df_aval$Salary.Range.To)
salariomaximo_media


# Para isso, criamos um subset somente com variável target igual a 1.
df_aval_saida_positiv <- subset(df_aval, Salary.Range.From > salariomaximo_media)
view(df_aval_saida_positiv)

# E então calculamos a satisfação média desse grupo
satisfacao_media_func_saiu_empresa <-mean(df_aval_saida_positiv$nivel_satisfacao)
satisfacao_media_func_saiu_empresa

# Os resultados são coerentes, pois cerca de 44% de satisfaçãoo média para quem saiu da empresa 
# faz sentido. Vamos ao teste t.

# Teste t de uma amostra 
?t.test
t.test(df_aval_saida_positiv$nivel_satisfacao, mu = satisfacao_media)

# Valor-p < 0.05 indica que há evidência estatística para rejeitarmos a hipótese nula.
# A hipótese nula é que a amostra vem da mesma distribuição que a população de funcionários.

# Teste t de duas amostra s
t.test(df_aval_saida_positiv$nivel_satisfacao, df_aval$nivel_satisfacao)

# Valor-p < 0.05, rejeitamos a hipótese nula.


########## Construção do Modelo ########## 

# Criaremos um modelo para prever se o funcionário vai ou não deixar a empresa.

# Variável target: saida
# Variáveis preditoras: nivel_satisfacao, ultima_avaliacao, media_mensal_horas, salario e num_projetos

# Dimensões do dataset original
dim(df_aval)

# Como vamos prever a saída, essa será nossa variável alvo
# Vamos convertê-la para o tipo fator
df_aval$saida <- factor(df_aval$saida)

# Vamos checar as proporções das classes
table(df_aval$saida)

# Temos 0 como a classe negativa e 1 como a classe positiva.
# Sendo assim, 12185 funcionÃ¡rios nÃ£o deixaram a empresa, enquanto 3813 deixaram a empresa.

# Divisão dos dados em treino e teste
dados_treino <- df_aval[1:12000,]
dados_teste <- df_aval[12001:15998,]

# Dimensões
dim(dados_treino)
dim(dados_teste)

# Criação do modelo
# Usaremos a função glm() para um modelo de regressão logística binária
names(df_aval)
?glm
modelo_func_v1 <- glm(saida ~ nivel_satisfacao + 
                        ultima_avaliacao + 
                        media_mensal_horas + 
                        salario + 
                        num_projetos,
                   data = dados_treino,
                   binomial())

# Sumário do modelo
summary(modelo_func_v1)

# Os 3 asteriscos ao lado de cada variável e o valor-p baixo indicam que todas as variáveis preditoras
# são relevantes para prever a variável de saída.

# Previsões com o modelo 
previsoes_v1 <- predict(modelo_func_v1, newdata = dados_teste, type = 'response')
previsoes_v1

# Definindo os limites das previsões das classes
# Como as previsões estão no formato de probabilidade, vamos definir um limite e gerar a previsão# de classe final. Se a probabilidade for maior que 0.55, classificamos como positivo,
# caso contrário como negativo.
previsoes_v1 <- ifelse(previsoes_v1 > 0.55, 1, 0)
previsoes_v1

# Vamos avaliar o modelo
erro_modelo_v1 <- mean(previsoes_v1 != dados_teste$saida, na.rm = T)
print(paste('Acurácia do Modelo', 1 - erro_modelo_v1))

# Nosso modelo é capaz de prever com aproximadamente 79% de precisão.

# Vamos criar o plot da Curva ROC e calcular a mÃ©trica AUC.
p <- predict(modelo_func_v1, newdata = dados_teste, type="response")
pr <- prediction(p, dados_teste$saida)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf)
auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
auc

# As métricas não estão ruins, vamos tentar melhorar. Criaremos a segunda versÃ£o do modelo, 
#dessa vez mudando a estratÃ©gia de divisão dos dados de treino e de teste.

# Definindo o tamanho da divisão dos dados para 80/20
data_size <- ceiling(0.8 * nrow(df_aval))

# Divisão em treino e teste
set.seed(100)
indice <- sample(seq_len(nrow(df_aval)), size = data_size)
dados_treino <- df_aval[indice, ]
dados_teste <- df_aval[-indice, ]

# Dimensões
dim(dados_treino)
dim(dados_teste)

# Inclusão de variáveis
names(df_aval)
modelo_func_v2 <- glm(saida ~ nivel_satisfacao + 
                        ultima_avaliacao + 
                        num_projetos + 
                        media_mensal_horas + 
                        tempo_empresa + 
                        acidente_trabalho + 
                        promocao_5_anos + 
                        salario,
                      data = dados_treino,
                      binomial())

# Sumário do modelo
summary(modelo_func_v2)

# previsões com o modelo
previsoes_v2 <- predict(modelo_func_v2, newdata = dados_teste, type = 'response')
previsoes_v2 <- ifelse(previsoes_v2 > 0.55, 1, 0)

# Avaliação do modelo
erro_modelo_v2 <- mean(previsoes_v2 != dados_teste$saida, na.rm = T)
print(paste('Acurácia do Modelo', 1 - erro_modelo_v2))

# Tivemos uma pequena redução na acurácia. Mas e o AUC?

p <- predict(modelo_func_v2, newdata = dados_teste, type = "response")
pr <- prediction(p, dados_teste$saida)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf)
auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
auc

# Nosso modelo tem agora uma performance global melhor, ou seja, um modelo mais 
# estável e generalizável.

# Você pode agora usar o modelo com dados de novos funcionários e então prever se
# eles deixarão a empresa ou não.


# Conclusão

# Para resumir nossa análise sobre porque os funcionários deixam a empresa, dentre todos os fatores que contribuem
# o preditor mais forte é o nível de satisfaçao do funcionário.

# Em geral, os funcionários saem quando estão com excesso de trabalho (mais de 250 horas de média mensal de trabalho) 
# ou com trabalho insuficiente (menos de 150 de média mensal de trabalho).

# Empregados com avaliações baixas ou muito altas provavelmente sairão da empresa.

# Empregados com salários baixos ou médios deixam a empresa.

# Empregados que tiveram menos (3 ou menos) ou mais (6 ou mais) projetos provavelmente deixarão a empresa.







