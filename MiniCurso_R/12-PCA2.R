################################################################################
###################### ANÁLISE DE COMPONENTES PRINCIPAIS #######################
################################################################################
#Pacotes necessários
library(metan)
library(stats)
library(factoextra)
library(cowplot)
library(readxl)

# Importando os dados
dados <- read_excel("Dados_experimentais.xlsx", sheet = "PCA2")


# 1 - Selecionando variáveis
res.pca1 <- dados |> 
  dplyr::select(SFM, SFF, FMC, FFC, AP, PROD, RSFM, RSFF, RFMC, RFFC, SSFM, SSFF, SFMC, SFFC)

# 2 - Padronizando variaveis
res.pca <- prcomp(res.pca1, scale. = TRUE)

# 3- Extrair os autovalores
eig.val <- get_eigenvalue(res.pca)

# 4 - Escolha do número de componentes principais
m1 <- fviz_eig(res.pca,
               addlabels = TRUE,
               barfill = "orange",
               xlab = "Componentes Principais",
               ylab = "Variância explicada (%)",
               title = "",
               barcolor = "black")+
  theme_bw()+
  theme_classic()+
  theme(
    panel.grid = element_blank(),
    axis.text = element_text(family = "serif", colour = "black", size = 12),
    axis.title.x = element_text(size = 14, family = "serif"),
    axis.title.y = element_text(size = 14, family = "serif"),
    legend.text = element_text(size = 14, family = "serif"))

m1

# Componentes principais

m2 <- fviz_pca_biplot(res.pca, 
                      # indivíduos
                      geom.ind = "point",
                      fill.ind = dados$DATA, col.ind = "black",
                      pointshape = 21, pointsize = 4,
                      palette = c("red", "blue", "green", "yellow", "black"),
                      addEllipses = FALSE,
                      repel = TRUE,
                      title = "",
                      # variáveis
                      alpha.var ="contrib",
                      gradient.cols = c("red", "blue", "green", "yellow", "black"),
                      legend.title = list(fill = "Datas",
                                          alpha = "Contribuição"))+
  theme_bw()+
  theme_classic()+
  theme(
    panel.grid = element_blank(),
    axis.text = element_text(family = "serif", colour = "black", size = 14),
    axis.title.x = element_text(size = 14, family = "serif"),
    axis.title.y = element_text(size = 14, family = "serif"),
    legend.text = element_text(size = 14, family = "serif"))

m2

# Salvando Figuras
ggsave(plot = Fig, "PCA2.tiff",
       units = "in",
       width = 9,
       height = 6,
       dpi = 300,
       compression = "lzw")





