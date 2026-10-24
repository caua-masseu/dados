# Função para verificar e carregar pacotes
verifica_pacote <- function(pacote) {
  if (!requireNamespace(pacote, quietly = TRUE)) {
    install.packages(pacote)
  }
  library(pacote, character.only = TRUE)
}

# Verificando e carregando os pacotes
verifica_pacote("readr")
verifica_pacote("mclust")
verifica_pacote("tidyverse")


dados <- read.csv("https://raw.githubusercontent.com/caua-masseu/dados/refs/heads/main/2.12_Health_systems.csv") #Estar conectado na internet
data <- dados[,c(3:9)]

colnames(data) <- c("Country", "Curr_Exp", "Gov_Exp", "Out_Exp",
                    "Exp_Per_Capita", "Exp_Per_Capita_PPP","Ext_Health_Exp")

df <- data[,c(-1, -6)] %>% drop_na()        
df$Exp_Per_Capita  <- log(df$Exp_Per_Capita)      #Aplicando transformacao nas variáveis
df$Ext_Health_Exp  <- df$Ext_Health_Exp ^(1/2)


#Analise Exploratoria
pairs(df)

hist(df$Curr_Exp,  main = "", ylab = "Frequência", xlab = "Curr_Exp")
hist(df$Gov_Exp, main = "", ylab = "Frequência", xlab = "Gov_Exp")
hist(df$Out_Exp, main = "", ylab = "Frequência", xlab = "Out_Exp")
hist(df$Exp_Per_Capita, main = "", ylab = "Frequência", xlab = "Exp_Per_Capita")
hist(df$Ext_Health_Exp, main = "", ylab = "Frequência", xlab = "Ext_Health_Exp")


#Modelo de mistura gaussiana  e a selecao de modelo
model.BIC <- mclustBIC(df)

plot(model.BIC, xlab = "Número de Clusteres", legendArgs = list(cex = 0.7))

model <- Mclust(df, 2, modelNames = "VVE")        #Estimando o modelo
plot(model, what = "classification")

#Medindo a incerteza
plot(sort(model$uncertainty), pch = 15, lty = 5, 
     xlab = "Observações em ordem crescente da medida de incerteza", ylab = "Incerteza", cex = 0.5)


#Comparando os clusteres
df %>% mutate(Cluster = factor(model$classification)) %>%
  pivot_longer(-Cluster, names_to = "Variavéis", values_to = "Valores") %>%
  ggplot(aes(x = Variavéis, y = Valores, fill = Cluster)) +
  geom_boxplot() + theme_classic() + coord_flip() +
  theme(legend.position = "bottom")

