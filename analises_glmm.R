#==============================================================================
# MLGMs binomial negativa com sazonalidade cíclica (seno e cosseno)
#==============================================================================

library(readxl)
library(glmmTMB)
library(DHARMa)
library(emmeans)
library(performance)

#------------------------------------------------------------------------------#
# 1. Ler planilha
#------------------------------------------------------------------------------#

dados <- read_excel("dados_geral_2025_2026_sem mirounga.xlsx")

#------------------------------------------------------------------------------#
# 2. Preparar os dados
#------------------------------------------------------------------------------#

dados$data <- as.Date(dados$data, format = "%d/%m/%Y")
dados$especie <- as.factor(dados$especie)
dados$Mês <- as.numeric(dados$Mês)
dados$turno <- as.factor(dados$turno)
dados$revis <- as.factor(dados$revis)
dados$voo <- as.factor(dados$voo)

#------------------------------------------------------------------------------#
# 3. Variáveis cíclicas para sazonalidade anual
#------------------------------------------------------------------------------#

dados$mes_sin <- sin(2*pi*dados$Mês/12)
dados$mes_cos <- cos(2*pi*dados$Mês/12)

#------------------------------------------------------------------------------#
# 4. Modelo aditivo
#------------------------------------------------------------------------------#

m_nb_aditivo <- glmmTMB(
  abundancia ~ especie + mes_sin + mes_cos + turno + revis +
    (1 | voo),
  data = dados,
  family = nbinom2
)

summary(m_nb_aditivo)

#------------------------------------------------------------------------------#
# 5. Modelo com interações
#------------------------------------------------------------------------------#

m_nb_interacao <- glmmTMB(
  abundancia ~
    revis * especie +
    revis * (mes_sin + mes_cos) +
    revis * turno +
    especie * (mes_sin + mes_cos) +
    especie * turno +
    (1 | voo),
  data = dados,
  family = nbinom2
)

summary(m_nb_interacao)

#------------------------------------------------------------------------------#
# 6. Comparação entre modelos
#------------------------------------------------------------------------------#

anova(m_nb_aditivo,
      m_nb_interacao,
      test = "Chisq")

#------------------------------------------------------------------------------#
# 7. Pseudo-R²
#------------------------------------------------------------------------------#

r2_nakagawa(m_nb_aditivo)
r2_nakagawa(m_nb_interacao)

#------------------------------------------------------------------------------#
# 8. Diagnóstico dos modelos
#------------------------------------------------------------------------------#

res_aditivo <- simulateResiduals(m_nb_aditivo)
plot(res_aditivo)
testDispersion(res_aditivo)
testZeroInflation(res_aditivo)

res_interacao <- simulateResiduals(m_nb_interacao)
plot(res_interacao)
testDispersion(res_interacao)
testZeroInflation(res_interacao)

#------------------------------------------------------------------------------#
# 9. Contrastes marginais com ajuste de Bonferroni
#------------------------------------------------------------------------------#

# Comparação entre espécies dentro de cada REVIS
emm_especie_revis <- emmeans(
  m_nb_interacao,
  ~ especie | revis,
  type = "response"
)

pairs(
  emm_especie_revis,
  adjust = "bonferroni"
)

# Comparação entre REVIS dentro de cada espécie
emm_revis_especie <- emmeans(
  m_nb_interacao,
  ~ revis | especie,
  type = "response"
)

pairs(
  emm_revis_especie,
  adjust = "bonferroni"
)

# Comparação entre turnos dentro de cada REVIS
emm_turno_revis <- emmeans(
  m_nb_interacao,
  ~ turno | revis,
  type = "response"
)

pairs(
  emm_turno_revis,
  adjust = "bonferroni"
)

# Comparação entre REVIS dentro de cada turno
emm_revis_turno <- emmeans(
  m_nb_interacao,
  ~ revis | turno,
  type = "response"
)

pairs(
  emm_revis_turno,
  adjust = "bonferroni"
)

#------------------------------------------------------------------------------#
# 10. Médias marginais para figuras e tabelas
#------------------------------------------------------------------------------#

emmeans(m_nb_interacao, ~ especie | revis, type = "response")
emmeans(m_nb_interacao, ~ revis | especie, type = "response")
emmeans(m_nb_interacao, ~ turno | revis, type = "response")
emmeans(m_nb_interacao, ~ revis | turno, type = "response")
