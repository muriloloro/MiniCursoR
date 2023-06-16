################################################################################
####################### DELINEAMENTO BLOCOS CASUALIZADOS #######################
################# TRTATAMENTOS QUANTITATIVOS X QUANTITATIVOS ###################
################################################################################

# IMPORTANDO OS DADOS EXPERIMENTAIS
library(readxl)
dados <- read_excel("Dados_experimentais.xlsx", sheet = "DBC_Quan_Quan")

# INSTALANDO e CARREGANDO PACOTES NECESSÁRIOS
library(ExpDes.pt)
library(ggplot2)
library(dplyr)
library(metan)
library(ggpmisc)

#ANALISES ESTATISTICAS
attach(dados)
srmod <- resp_surf(dados,
                   factor1 = POTA,
                   factor2 = NITRO,
                   rep = BLOCO,
                   resp = AP)

P1 <-  plot(srmod)
P2 = plot(srmod, cut = 9,
          colorkey = list(space = "top", width = 1),
          xlab = "Dose de Nitrogênio (Kg/ha)",
          ylab= "Dose de Potássio (Kg/ha)")


