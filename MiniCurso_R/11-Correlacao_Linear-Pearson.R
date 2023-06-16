################################################################################
################### ANÁLISE DE CORRELAÇÃO LINEAR DE PEARSON ####################
################################################################################
#Pacotes necessários
library(metan)
library(dplyr)
library(readxl)

# Importando os dados
dados <- read_excel("Dados_experimentais.xlsx", sheet = "Cor")


# Gráfico de dispersão
dados |> 
  dplyr::select(PRO, MCG, AP, DE, CE, ME, MSPA) |> 
  pairs( main = "Grafico de dispersão", pch = 21, 
         bg = "blue")



# Estatísticas descritivas
desc <- data.frame(dados |> 
             desc_stat())

# Grafico com estatisticas
mean_cv <- mean(desc$cv)

desc |>
  ggplot()+
  geom_col(aes(x= variable, y= cv, fill= "red"))+
  geom_hline(yintercept = mean_cv, linetype = "dashed")+
  labs(title = "Coeficiente de variação (%)")
  
  
# Gráfico de análise de correlação linear de Pearson
dados |> 
  dplyr::select(PRO, MCG, AP, DE, CE, ME, MSPA) |> 
  metan::corr_plot(decimal.mark = ",",
                   minsize = 5,
                   maxsize = 5,
                   axis.labels = FALSE,
                   size.point = 1.5,
                   pan.spacing = 0.1,
                   resolution = 500,
                   show.labels.in = "show",
                   prob = 0.05,
                   digits = 2)+
  theme(
    strip.text = element_text(size=14, family = "serif"))

# Salvando Figuras
ggsave(plot = Fig, "Correl.tiff",
       units = "in",
       width = 9,
       height = 6,
       dpi = 300,
       compression = "lzw")





