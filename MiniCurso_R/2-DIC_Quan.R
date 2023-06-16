################################################################################
################### DELINEAMENTO INTEIRAMENTE CASUALIZADO ######################
######################### TRTATAMENTO QUANTITATIVO #############################
################################################################################

# IMPORTANDO OS DADOS EXPERIMENTAIS
library(readxl)
dados <- read_excel("Dados_experimentais.xlsx", sheet = "DIC_Quan")

# INSTALANDO e CARREGANDO PACOTES NECESSÁRIOS
library(ExpDes.pt)
library(ggplot2)
library(dplyr)
library(metan)
library(ggpmisc)

#ANALISES ESTATISTICAS
attach(dados)

ResAP <- dic(trat = DOSE, resp = AP, quali = FALSE, sigT = 0.05)


################################################################################
###################### GRÁFICOS DE REGRESSÃO ###################################
################################################################################
# 1. Regressão Polinomial###

# 1.1 Regressão Linear           y = a + bx
# 1.1 Regressão Quadrática       y = a + bx + cx^2
# 1.3 Regressão Cúbica           y = a + bx + cx^2 + cx^3

reg1 <- ggplot(dados, aes(x = DOSE, y = AP)) +
  geom_point(size = 2.5, pch = 20, col ="blue", fill = "cornflowerblue")+
  geom_smooth(method = "lm", formula = y ~ poly (x, degree = 2, raw = TRUE))+ # valor é igual ao grau da regressão
  ggpmisc::stat_poly_eq(aes(label =  paste(..eq.label.., ..rr.label..,
                                           sep = "~~~~")),
                        label.x = 0.9, label.y = 0.9, # Ajuste da equação no grafico
                        formula = y ~ poly (x, degree =2, raw = TRUE))+
  theme(
    axis.line = element_line(size = 0.5, color = "#222222"),
    text = element_text(family="serif", size = 13),
    axis.text.y = element_text(size=12, color = "black"),
    axis.text.x = element_text(size = 13, angle = 0, vjust = 0.4, color="black"),
    axis.ticks = element_line(colour = 'black'),
    axis.ticks.length = unit(.25, "cm"),
    axis.ticks.x = element_line(colour = "black"),
    axis.ticks.y = element_line(colour = "black"),
    plot.title = element_text(hjust = 0.45, vjust=2.12, 
                              colour = "black", size = 13, family = "serif"))

reg1

metan::arrange_ggplot(reg1, reg2, reg3, reg4)

### MÁXIMA EFICIÊNCIA TÉCNICA (MET) - REGRESSÃO QUADRÁTICA
#  y = a + bx + cx^3
#  MET = -b / 2c
MET <- -1.04/(2*-0.012)
MET

# INSERIR MET NO GRAFICO <- geom_text(x = 90, y = 80, label = "MET = 43.33", size = 3)+
