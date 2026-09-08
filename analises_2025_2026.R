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

residuos_adit <- residuals(m_nb_aditivo, type = "pearson")

shapiro.test(residuos_adit)

hist(residuos_adit)

m_nb_interacao <- glmmTMB(abundancia ~ revis*especie + revis*Mês + revis*turno + (1 | voo), 
                        data = dados, 
                        family = nbinom2)

summary(m_nb_interacao)

residuos_int <- residuals(m_nb_interacao, type = "pearson")

shapiro.test(residuos_int)

hist(residuos_int)

#------------------------------------------------------------------------------#

library(ggplot2)
library(dplyr)

# ============================================================
# CALCULAR ABUNDÂNCIA MÉDIA E DESVIO PADRÃO POR REVIS E TURNO
# ============================================================

dados_grafico <- dados %>%
  group_by(revis, turno) %>%
  summarise(
    abundancia_media = mean(abundancia, na.rm = TRUE),
    desvio_padrao = sd(abundancia, na.rm = TRUE),
    .groups = "drop"
  )

# ============================================================
# GRÁFICO
# ============================================================

abund_revis_turno <- ggplot(
  dados_grafico,
  aes(
    x = revis,
    y = abundancia_media,
    fill = turno
  )
) +
  
  # Barras
  geom_col(
    position = position_dodge(width = 0.8),
    width = 0.7,
    color = "black"
  ) +
  
  # Barras de desvio padrão — somente superior
  geom_errorbar(
    aes(
      ymin = abundancia_media,
      ymax = abundancia_media + desvio_padrao
    ),
    position = position_dodge(width = 0.8),
    width = 0.2,
    color = "black"
  ) +
  
  # Cores dos períodos
  scale_fill_manual(
    values = c(
      "Diurno" = "grey60",
      "Noturno" = "black"
    ),
    labels = c(
      "Diurno" = "Daytime",
      "Noturno" = "Nighttime"
    )
  ) +
  
  # Eixo X com nomes completos
  scale_x_discrete(
    labels = c(
      "IL" = "Ilha dos Lobos",
      "ML" = "Molhe Leste"
    )
  ) +
  
  # Eixo Y de 10 em 10, iniciando no zero
  scale_y_continuous(
    breaks = seq(
      0,
      max(
        dados_grafico$abundancia_media +
          dados_grafico$desvio_padrao,
        na.rm = TRUE
      ) + 10,
      by = 10
    ),
    expand = c(0, 0)
  ) +
  
  # Títulos e legendas
  labs(
    x = NULL,
    y = "Number of individuals",
    fill = "Periods"
  ) +
  
  # Tema
  theme_classic() +
  
  theme(
    # Fonte geral
    text = element_text(
      family = "Times New Roman",
      size = 12
    ),
    
    # Título do eixo Y em negrito
    axis.title.y = element_text(
      family = "Times New Roman",
      size = 12,
      face = "bold"
    ),
    
    # Título do eixo X removido
    axis.title.x = element_blank(),
    
    # Textos dos eixos
    axis.text.x = element_text(
      family = "Times New Roman",
      size = 12
    ),
    
    axis.text.y = element_text(
      family = "Times New Roman",
      size = 12
    ),
    
    # Título da legenda
    legend.title = element_text(
      family = "Times New Roman",
      size = 12
    ),
    
    # Texto da legenda
    legend.text = element_text(
      family = "Times New Roman",
      size = 12
    ),
    
    # Legenda na parte superior
    legend.position = "top"
  )

abund_revis_turno

# ============================================================
# FILTRAR DADOS DIURNOS
# ============================================================

dados_diurno <- dados %>%
  filter(turno == "Diurno")

# ============================================================
# CALCULAR ABUNDÂNCIA MÉDIA E DESVIO PADRÃO
# POR REVIS E ESPÉCIE
# ============================================================

dados_grafico_diurno <- dados_diurno %>%
  group_by(revis, especie) %>%
  summarise(
    abundancia_media = mean(abundancia, na.rm = TRUE),
    desvio_padrao = sd(abundancia, na.rm = TRUE),
    .groups = "drop"
  )

# ============================================================
# GRÁFICO
# ============================================================

abund_revis_especie_diurno <- ggplot(
  dados_grafico_diurno,
  aes(
    x = revis,
    y = abundancia_media,
    fill = especie
  )
) +
  
  # Barras
  geom_col(
    position = position_dodge(width = 0.8),
    width = 0.7,
    color = "black"
  ) +
  
  # Desvio padrão — somente superior
  geom_errorbar(
    aes(
      ymin = abundancia_media,
      ymax = abundancia_media + desvio_padrao
    ),
    position = position_dodge(width = 0.8),
    width = 0.2,
    color = "black"
  ) +
  
  # Cores e nomes das espécies
  scale_fill_manual(
    values = c(
      "o_flavescens" = "black",
      "a_australis" = "grey60"
    ),
    labels = c(
      "o_flavescens" = expression(italic("Otaria flavescens")),
      "a_australis" = expression(italic("Arctocephalus australis"))
    )
  ) +
  
  # Eixo X com nomes completos
  scale_x_discrete(
    labels = c(
      "IL" = "Ilha dos Lobos",
      "ML" = "Molhe Leste"
    )
  ) +
  
  # Eixo Y de 10 em 10, iniciando no zero
  scale_y_continuous(
    breaks = seq(
      0,
      max(
        dados_grafico_diurno$abundancia_media +
          dados_grafico_diurno$desvio_padrao,
        na.rm = TRUE
      ) + 10,
      by = 10
    ),
    expand = c(0, 0)
  ) +
  
  # Títulos e legendas
  labs(
    x = NULL,
    y = "Number of individuals",
    fill = "Species"
  ) +
  
  # Tema
  theme_classic() +
  
  theme(
    # Fonte geral
    text = element_text(
      family = "Times New Roman",
      size = 12
    ),
    
    # Título do eixo Y em negrito
    axis.title.y = element_text(
      family = "Times New Roman",
      size = 12,
      face = "bold"
    ),
    
    # Título do eixo X removido
    axis.title.x = element_blank(),
    
    # Textos dos eixos
    axis.text.x = element_text(
      family = "Times New Roman",
      size = 12
    ),
    
    axis.text.y = element_text(
      family = "Times New Roman",
      size = 12
    ),
    
    # Título da legenda
    legend.title = element_text(
      family = "Times New Roman",
      size = 12
    ),
    
    # Texto da legenda
    legend.text = element_text(
      family = "Times New Roman",
      size = 12
    ),
    
    # Legenda na parte superior
    legend.position = "top"
  )

# Exibir gráfico
abund_revis_especie_diurno

# ============================================================
# FILTRAR DADOS NOTURNOS
# ============================================================

dados_noturno <- dados %>%
  filter(turno == "Noturno")

# ============================================================
# CALCULAR ABUNDÂNCIA MÉDIA E DESVIO PADRÃO
# POR REVIS E ESPÉCIE
# ============================================================

dados_grafico_noturno <- dados_noturno %>%
  group_by(revis, especie) %>%
  summarise(
    abundancia_media = mean(abundancia, na.rm = TRUE),
    desvio_padrao = sd(abundancia, na.rm = TRUE),
    .groups = "drop"
  )

# ============================================================
# GRÁFICO
# ============================================================

abund_revis_especie_noturno <- ggplot(
  dados_grafico_noturno,
  aes(
    x = revis,
    y = abundancia_media,
    fill = especie
  )
) +
  
  # Barras
  geom_col(
    position = position_dodge(width = 0.8),
    width = 0.7,
    color = "black"
  ) +
  
  # Desvio padrão — somente superior
  geom_errorbar(
    aes(
      ymin = abundancia_media,
      ymax = abundancia_media + desvio_padrao
    ),
    position = position_dodge(width = 0.8),
    width = 0.2,
    color = "black"
  ) +
  
  # Cores das espécies
  scale_fill_manual(
    values = c(
      "o_flavescens" = "black",
      "a_australis" = "grey60"
    )
  ) +
  
  # Eixo X com nomes completos
  scale_x_discrete(
    labels = c(
      "IL" = "Ilha dos Lobos",
      "ML" = "Molhe Leste"
    )
  ) +
  
  # Eixo Y de 10 em 10, iniciando no zero
  scale_y_continuous(
    breaks = seq(
      0,
      max(
        dados_grafico_noturno$abundancia_media +
          dados_grafico_noturno$desvio_padrao,
        na.rm = TRUE
      ) + 10,
      by = 10
    ),
    expand = c(0, 0)
  ) +
  
  # Títulos dos eixos
  labs(
    x = NULL,
    y = "Number of individuals"
  ) +
  
  # Tema
  theme_classic() +
  
  theme(
    # Fonte geral
    text = element_text(
      family = "Times New Roman",
      size = 12
    ),
    
    # Título do eixo Y em negrito
    axis.title.y = element_text(
      family = "Times New Roman",
      size = 12,
      face = "bold"
    ),
    
    # Título do eixo X removido
    axis.title.x = element_blank(),
    
    # Textos dos eixos
    axis.text.x = element_text(
      family = "Times New Roman",
      size = 12
    ),
    
    axis.text.y = element_text(
      family = "Times New Roman",
      size = 12
    ),
    
    # EXCLUIR LEGENDA
    legend.position = "none"
  )

# Exibir gráfico
abund_revis_especie_noturno

# ============================================================
# INSTALAR O PACOTE, CASO AINDA NÃO TENHA
# ============================================================

# install.packages("patchwork")

library(patchwork)

# ============================================================
# MONTAR PAINEL
# ============================================================

painel_abundancia <-
  abund_revis_turno +
  (
    abund_revis_especie_diurno /
      abund_revis_especie_noturno
  ) +
  plot_layout(
    widths = c(1, 1)
  )

# ============================================================
# EXIBIR PAINEL
# ============================================================

painel_abundancia

ggsave(
  filename = "painel_abundancia.png",
  plot = painel_abundancia,
  width = 12,
  height = 8,
  units = "in",
  dpi = 600,
  bg = "white"
)
