################################################################################
############################ ÁRVORE DE REGRESSÃO ###############################
################################################################################
#Pacotes necessários
library(rpart.plot)
library(rpart)
library(readxl)
# Importando os dados
dados <- read_excel("Dados_experimentais.xlsx", sheet = "Arv_Reg")

dados2 <- dados |> 
  dplyr::select(-1,)


# Calculando parâmetros da Arvore de Regressão
fit <- rpart(PRO~., data = dados2,
             method = "anova",
             parms = list(split = "gini"),# ou "information"
             control = rpart.control(
               cp = 0.022,
               minsplit = 1, # quant minima de linhas em cada nó
               minbucket = 1, # quant minima de linhas em cada no terminal
               maxdepth = 3)) #quant maxima de nos


#contribuição das variáveis
barplot(fit$variable.importance)
fit$cptable
plotcp(fit)

# Executa a poda da arvore
fit2 <- prune.rpart(fit, cp=0.088)
plotcp(fit2)

# plotar a arvore

rpart.plot(fit,
           type = 0,
           extra = 101,
           box.palette = c("yellow","green"),
           branch.lty=2,
           shadow.col = "black",
           digits= 3,
           roundint = TRUE,
           fallen.leaves = TRUE,
           under = TRUE,
           nn=FALSE,
           cex = 1.3)

# Estatísticas de precisão do modelo
y_predito <- predict(fit, newdata = dados2)
y_original <- dados2$PRO
n <- length(y_original)


library(caret)
teste <- data.frame(obs = dados2$PRO, pred=y_predito)
caret::defaultSummary(teste)

# RMSE <- Raiz do erro quadrático médio 
# Rsquared <- Coeficiente de determinação
# MAE <- Erro absoluto médio



