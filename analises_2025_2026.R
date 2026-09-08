#------------------------------------------------------------------------------#

# instalar pacote
#library('readxl')

# Carregar pacotes
library(readxl)

#------------------------------------------------------------------------------#

# Ler planilha

dados <- read_excel("dados_geral_2025_2026.xlsx")

head(dados)      # primeiras linhas
summary(dados)   # resumo estatístico
str(dados)       # estrutura das variáveis

#------------------------------------------------------------------------------#

# 1. Preparar os dados

dados$data <- as.Date(dados$data, format = "%d/%m/%Y")
dados$especie <- as.factor(dados$especie)
dados$Mês <- as.numeric(dados$Mês)
dados$turno <- as.factor(dados$turno)
dados$revis <- as.factor(dados$revis)

str(dados)

#------------------------------------------------------------------------------#

# 2. Teste de normalidade (Shapiro-Wilk)

library(dplyr)

shapiro.test(dados$abundancia)

boxplot(dados$abundancia)

hist(dados$abundancia)

#------------------------------------------------------------------------------#

names(dados)

# 7. Ajuste um modelo Binomial Negativa

library(glmmTMB)

m_nb_aditivo <- glmmTMB(abundancia ~ especie + Mês + turno + revis + (1 | voo), 
                data = dados, 
                family = nbinom2)

summary(m_nb_aditivo)

m_nb_interacao <- glmmTMB(abundancia ~ revis*especie + revis*Mês + revis*turno + (1 | voo), 
                        data = dados, 
                        family = nbinom2)

summary(m_nb_interacao)

#------------------------------------------------------------------------------#

# 10. Visualizar as distribuições com boxplots e histogramas

boxplot(Contagem ~ Monitoramento, data = dados)
boxplot(Contagem ~ Espécie, data = dados)

#------------------------------------------------------------------------------#

# 10. Extrair as médias ajustadas (em escala original, não log) 

library(emmeans)
library(ggplot2)

# 11. Médias ajustadas (em escala original, não log)
emm <- emmeans(m_nb, ~ Espécie * Monitoramento, type = "response")
emm

# 12. Converter para data.frame para plotagem
emm_df <- as.data.frame(emm)

# 13. Gráfico com ggplot2

ggplot(emm_df, aes(x = Espécie, y = response, fill = Monitoramento)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL), 
                width = 0.2, position = position_dodge(0.9)) +
  labs(y = "Contagem esperada", 
       x = "Espécie", 
       fill = "Monitoramento",
       title = "Médias ajustadas do modelo Binomial Negativa") +
  theme_minimal()

#------------------------------------------------------------------------------#

dados_of <- subset(dados, Espécie == "o_flavescens")
dados_aa <- subset(dados, Espécie == "a_australis")

#------------------------------------------------------------------------------#

library(tidyr)

dados_of_wide <- pivot_wider(
  dados_of,
  id_cols = Data,
  names_from = Monitoramento,
  values_from = Contagem
)

wilcox.test(
  dados_of_wide$visual,
  dados_of_wide$vant,
  paired = TRUE
)

#------------------------------------------------------------------------------#

dados_aa_wide <- pivot_wider(
  dados_aa,
  id_cols = Data,
  names_from = Monitoramento,
  values_from = Contagem
)

wilcox.test(
  dados_aa_wide$visual,
  dados_aa_wide$vant,
  paired = TRUE
)

#------------------------------------------------------------------------------#

library(ggplot2)

# Exemplo para a_australis
dados_aa <- subset(dados, Espécie == "a_australis")

ggplot(dados_aa,
       aes(x = Monitoramento,
           y = Contagem,
           group = Data)) +
  geom_line(alpha = 0.6) +
  geom_point(size = 3) +
  theme_classic() +
  labs(x = "Monitoramento",
       y = "Contagem",
       title = "Arctocephalus australis")

#------------------------------------------------------------------------------#

ggplot(dados_of,
       aes(x = Monitoramento,
           y = Contagem,
           group = Data)) +
  geom_line(alpha = 0.6) +
  geom_point(size = 3) +
  theme_classic() +
  labs(x = "Monitoramento",
       y = "Contagem",
       title = "Otaria flavescens")

#------------------------------------------------------------------------------#

dados_wide <- pivot_wider(
  dados,
  id_cols = c(Data, Espécie),
  names_from = Monitoramento,
  values_from = Contagem
)

wilcox.test(
  dados_wide$visual,
  dados_wide$vant,
  paired = TRUE,
  exact = FALSE
)