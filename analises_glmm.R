#==============================================================================
# MLGMs binomial negativa com sazonalidade cíclica (seno e cosseno)
#==============================================================================

library(readxl)
library(glmmTMB)
library(DHARMa)
library(emmeans)
library(performance)
library(dplyr)
library(ggplot2)

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

# 10.1. Contrastes entre espécies dentro de cada REVIS

emm_especie_revis <- emmeans(
  m_nb_interacao,
  ~ especie | revis,
  type = "response"
)

emm_especie_revis

pairs(
  emm_especie_revis,
  adjust = "bonferroni"
)

# 10.2. Contrastes entre REVIS dentro de cada espécie

emm_revis_especie <- emmeans(
  m_nb_interacao,
  ~ revis | especie,
  type = "response"
)

emm_revis_especie

pairs(
  emm_revis_especie,
  adjust = "bonferroni"
)

# 10.3. Contrastes entre turnos dentro de cada espécie

emm_turno_especie <- emmeans(
  m_nb_interacao,
  ~ turno | especie,
  type = "response"
)

emm_turno_especie

pairs(
  emm_turno_especie,
  adjust = "bonferroni"
)

# 10.4. Contrastes entre espécies dentro de cada turno

emm_especie_turno <- emmeans(
  m_nb_interacao,
  ~ especie | turno,
  type = "response"
)

pairs(
  emm_especie_turno,
  adjust = "bonferroni"
)

# 10.5. Médias marginais para gráficos de sazonalidade

novo <- expand.grid(
  Mês = 1:12,
  especie = levels(dados$especie),
  revis = levels(dados$revis),
  turno = "Diurno"
)

novo$mes_sin <- sin(2*pi*novo$Mês/12)
novo$mes_cos <- cos(2*pi*novo$Mês/12)

pred <- predict(
  m_nb_interacao,
  newdata = novo,
  type = "response",
  se.fit = TRUE,
  re.form = NA
)

novo$pred <- pred$fit
novo$se <- pred$se.fit

novo$IC_inf <- exp(log(novo$pred) - 1.96*novo$se/novo$pred)
novo$IC_sup <- exp(log(novo$pred) + 1.96*novo$se/novo$pred)

head(novo)

ggplot(novo,
       aes(x = Mês,
           y = pred,
           color = especie)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2) +
  facet_wrap(~ revis) +
  theme_bw() +
  labs(
    x = "Mês",
    y = "Abundância prevista",
    color = "Espécie"
  )

novo$grupo <- interaction(novo$revis,
                          novo$especie,
                          sep = " - ")

ggplot(novo,
       aes(Mês,
           pred,
           color = grupo)) +
  geom_line(linewidth = 1.2) +
  geom_point() +
  theme_bw() +
  labs(
    x = "Mês",
    y = "Abundância prevista",
    color = "Grupo"
  )




# Grade de predição
novo <- expand.grid(
  Mês = 1:12,
  especie = levels(dados$especie),
  revis = levels(dados$revis),
  turno = "Diurno"
)

novo$mes_sin <- sin(2*pi*novo$Mês/12)
novo$mes_cos <- cos(2*pi*novo$Mês/12)

# Predições na escala do link
pred <- predict(
  m_nb_interacao,
  newdata = novo,
  type = "link",
  se.fit = TRUE,
  re.form = NA
)

novo$eta <- pred$fit
novo$SE_eta <- pred$se.fit

# IC95% na escala do link
novo$eta_LCL <- novo$eta - 1.96 * novo$SE_eta
novo$eta_UCL <- novo$eta + 1.96 * novo$SE_eta

# Retornar para a escala da abundância
novo$pred <- exp(novo$eta)
novo$IC_inf <- exp(novo$eta_LCL)
novo$IC_sup <- exp(novo$eta_UCL)

head(novo)




# Renomear áreas para a figura
novo$revis_label <- factor(
  novo$revis,
  levels = c("IL", "ML"),
  labels = c(
    "Wildlife Refuge Ilha dos Lobos",
    "Wildlife Refuge Molhe Leste"
  )
)

ggplot(
  novo,
  aes(
    x = Mês,
    y = pred,
    color = especie,
    fill = especie
  )
) +
  geom_ribbon(
    aes(
      ymin = IC_inf,
      ymax = IC_sup
    ),
    alpha = 0.20,
    colour = NA
  ) +
  geom_line(linewidth = 1.5) +
  facet_wrap(~ revis_label, nrow = 1) +
  
  scale_color_manual(
    values = c(
      "a_australis" = "grey50",
      "o_flavescens" = "black"
    ),
    labels = c(
      expression(italic("Arctocephalus australis")),
      expression(italic("Otaria flavescens"))
    )
  ) +
  
  scale_fill_manual(
    values = c(
      "a_australis" = "grey50",
      "o_flavescens" = "black"
    ),
    labels = c(
      expression(italic("Arctocephalus australis")),
      expression(italic("Otaria flavescens"))
    )
  ) +
  
  scale_x_continuous(
    breaks = 1:12,
    labels = c(
      "Jan", "Feb", "Mar", "Apr",
      "May", "Jun", "Jul", "Aug",
      "Sep", "Oct", "Nov", "Dec"
    )
  ) +
  
  labs(
    x = "Month",
    y = "Predicted abundance",
    color = "Species",
    fill = "Species"
  ) +
  
  theme_classic(base_size = 16) +
  
  theme(
    axis.title = element_text(size = 18),
    axis.text = element_text(size = 14),
    legend.title = element_text(size = 16),
    legend.text = element_text(size = 14),
    strip.text = element_text(
      size = 16,
      face = "bold"
    ),
    legend.position = "right"
  )

ggsave(
  "Figure_seasonality.png",
  width = 12,
  height = 6,
  dpi = 600
)
