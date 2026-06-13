#------------------------------------------------------------------------------#

# Carregar pacotes

library(readxl)

#------------------------------------------------------------------------------#

# Ler planilha

dados <- read_excel("dados_nema.xlsx")

head(dados)      # primeiras linhas
summary(dados)   # resumo estatístico
str(dados)       # estrutura das variáveis

#------------------------------------------------------------------------------#

# 1. Preparar os dados

dados$Data <- as.Date(dados$Data, format = "%d/%m/%Y")
dados$Espécie <- as.factor(dados$Espécie)
dados$Monitoramento <- as.factor(dados$Monitoramento)
dados$Contagem <- as.numeric(dados$Contagem)

str(dados)

#------------------------------------------------------------------------------#

# 2. Teste de normalidade (Shapiro-Wilk)

library(dplyr)

dados %>%
  group_by(Espécie, Monitoramento) %>%
  summarise(
    p.value = shapiro.test(Contagem)$p.value
  )

#------------------------------------------------------------------------------#

# 3. Verificar a homogeneidade das variâncias com o teste de Levene
  
library(car)

leveneTest(
  Contagem ~ interaction(Espécie, Monitoramento),
  data = dados
)

#------------------------------------------------------------------------------#

# 4. Visualizar as distribuições com boxplots e histogramas

boxplot(Contagem ~ Espécie * Monitoramento, data = dados)

hist(dados$Contagem)

#------------------------------------------------------------------------------#

# 5. Ajuste um modelo Poisson

library(lme4)

m_pois <- glmer(Contagem ~ Espécie * Monitoramento + (1 | Data), 
                data = dados, 
                family = poisson)

summary(m_pois)

# 6. Teste de superdispersão

overdisp_fun <- function(model) {
  rdf <- df.residual(model)
  rp <- residuals(model, type = "pearson")
  Pearson.chisq <- sum(rp^2)
  prat <- Pearson.chisq / rdf
  pval <- pchisq(Pearson.chisq, df = rdf, lower.tail = FALSE)
  c(chisq = Pearson.chisq, ratio = prat, rdf = rdf, p = pval)
}

overdisp_fun(m_pois)

# Se o 'ratio' for >> 1, há superdispersão

#------------------------------------------------------------------------------#

# 7. Ajuste um modelo Binomial Negativa

library(glmmTMB)

m_nb <- glmmTMB(Contagem ~ Espécie * Monitoramento + (1 | Data), 
                data = dados, 
                family = nbinom2)

summary(m_nb)

# 8. Teste de AKAIKE

AIC(m_pois, m_nb)

# 9. Teste de superdispersão

overdisp_fun(m_nb)

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
