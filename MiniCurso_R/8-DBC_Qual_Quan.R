################################################################################
#################### DELINEAMENTO BLOCOS CASUALIZADOS ##########################
################# TRTATAMENTOS QUALITATIVO X QUANTITATIVO ######################
################################################################################

# IMPORTANDO OS DADOS EXPERIMENTAIS
library(readxl)
dados <- read_excel("Dados_experimentais.xlsx", sheet = "DBC_Qual_Quan")

# INSTALANDO e CARREGANDO PACOTES NECESSÁRIOS
library(ExpDes.pt)
library(ggplot2)
library(dplyr)
library(metan)
library(ggpmisc)

#ANALISES ESTATISTICAS
attach(dados)

ResAP <- fat2.dbc(fator1 = GEN,
                  fator2 = DOSE,
                  bloco = BLOCO,
                  resp = AP, 
                  quali = c(TRUE, FALSE),
                  mcomp = "tukey",
                  fac.names = c("Genotipos", "Doses"),
                  sigT = 0.05)

shapiro.test(ResAP$residuos)
hist(ResAP$residuos)
################################################################################
###################### GRÁFICOS DE REGRESSÃO ###################################
################################################################################
# 1. Regressão Polinomial###

# 1.1 Regressão Linear           y = a + bx
# 1.1 Regressão Quadrática       y = a + bx + cx^2
# 1.3 Regressão Cúbica           y = a + bx + cx^2 + cx^3

# 2. Interação significativa -> Um modelo de regressão para cada genótipo.
# 3. Interação não significativa -> Apenas um modelo de regressão com efeitos princ de DOSE

X_TORQUE <- dados |> 
  select(GEN, DOSE, AP) |> 
  filter(GEN == "TORQUE") |> 
  mean_by(DOSE)

X_ZEUS <- dados |> 
  select(GEN, DOSE, AP) |> 
  filter(GEN == "ZEUS") |> 
  mean_by(DOSE) # sem esta função os dados são plotados por repetições

X_VORAZ <- dados |> 
  select(GEN, DOSE, AP) |> 
  filter(GEN == "VORAZ") |> 
  mean_by(DOSE)


reg1 <- ggplot(dados, aes(x = DOSE, y = AP)) +
  geom_point(size = 2.5, pch = 20, col ="black", fill = "cornflowerblue")+
  geom_smooth(method = "lm", formula = y ~ poly (x, degree = 1, raw = TRUE))+ # valor é igual ao grau da regressão
  ggpmisc::stat_poly_eq(aes(label =  paste(..eq.label.., ..rr.label..,
                                           sep = "~~~~")),
                        label.x = 0.9, label.y = 0.9, # Ajuste da equação no grafico
                        formula = y ~ poly (x, degree =1, raw = TRUE))+
  labs(title = "BMX Zeus")+
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

arrange_ggplot(reg1, reg2)
###################################################################################





