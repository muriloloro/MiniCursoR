################################################################################
################### DELINEAMENTO INTEIRAMENTE CASUALIZADO ######################
################## TRTATAMENTOS QUALITATIVO X QUALITATIVO ######################
################################################################################

# IMPORTANDO OS DADOS EXPERIMENTAIS
library(readxl)
dados <- read_excel("Dados_experimentais.xlsx", sheet = "DIC_Qual_Qual")

# INSTALANDO e CARREGANDO PACOTES NECESSÁRIOS
library(ExpDes.pt)
library(ggplot2)
library(dplyr)
library(metan)

#ANALISES ESTATISTICAS
attach(dados)

boxplot(data = dados, AP ~ GEN)
boxplot(data = dados, AP ~ INOC)
boxplot(data = dados, AP ~ GEN+INOC)

ResAP <- fat2.dic(fator1 = GEN, fator2 = INOC, resp = AP, 
                   quali = c(TRUE, TRUE), mcomp = "tukey",
                   fac.names = c("Genotipos", "Inoculantes"),
                   sigT = 0.05, sigF = 0.05)

################################################################################
############################## GRÁFICOS ########################################
################################################################################

#Grafico 1
dados |> 
  ggplot()+
  geom_col(aes(x = GEN, y = AP), fill = "darkred")

#Grafico 2
mean_AP <- mean(dados$AP)

bar1 <- ggplot(dados, aes(x = GEN, y = AP, fill= INOC)) +
  labs(x=NULL,y="Altura da planta (cm)")+
  stat_summary(fun = mean,
               geom = "bar",
               col = "black",
               width = 0.8,
               position = position_dodge()) + 
  stat_summary(fun.data = mean_se,
               geom = "errorbar",
               width = 0.2,
               position = position_dodge(0.8))+
  geom_hline(yintercept = mean_AP, linetype = "dashed")+
  theme(
    panel.background = element_blank(),
    axis.line = element_line(size = 0.5, color = "#222222"),
    text = element_text(family="serif", size = 13),
    axis.text.y = element_text( size=12, color = "black"),
    axis.text.x = element_text(size = 13, angle = 45, vjust = 0.4, color="black"),
    axis.ticks = element_line(colour = 'black'),
    axis.ticks.length = unit(.25, "cm"),
    axis.ticks.x = element_line(colour = "black"),
    axis.ticks.y = element_line(colour = "black"),
    plot.title = element_text(hjust = 0.45, vjust=2.12,
                              colour = "black", size = 13, family = "serif"))

bar1
