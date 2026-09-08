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
# SÉRIE TEMPORAL MENSAL
# Abril_2025 a Agosto_2026
# ============================================================

library(dplyr)
library(tidyr)
library(ggplot2)

# ============================================================
# SÉRIE TEMPORAL POR REVIS
# MÉDIA ± DESVIO PADRÃO
# Abril/2025 a Agosto/2026
# ============================================================

library(dplyr)
library(tidyr)
library(ggplot2)

# ============================================================
# SÉRIE TEMPORAL POR REVIS
# MÉDIA + INTERVALO DE CONFIANÇA DE 95%
# Abril/2025 a Agosto/2026
# ============================================================

library(dplyr)
library(tidyr)
library(ggplot2)


# ============================================================
# 1. DEFINIR A ORDEM COMPLETA DOS MESES
# ============================================================

ordem_meses <- c(
  "Abril_2025",
  "Maio_2025",
  "Junho_2025",
  "Julho_2025",
  "Agosto_2025",
  "Setembro_2025",
  "Outubro_2025",
  "Novembro_2025",
  "Dezembro_2025",
  "Janeiro_2026",
  "Fevereiro_2026",
  "Março_2026",
  "Abril_2026",
  "Maio_2026",
  "Junho_2026",
  "Julho_2026",
  "Agosto_2026"
)


# ============================================================
# 2. NOMES DOS MESES EM PORTUGUÊS
# ============================================================

meses_pt <- c(
  "Janeiro",
  "Fevereiro",
  "Março",
  "Abril",
  "Maio",
  "Junho",
  "Julho",
  "Agosto",
  "Setembro",
  "Outubro",
  "Novembro",
  "Dezembro"
)


# ============================================================
# 3. CRIAR MÊS_ANO A PARTIR DA COLUNA "data"
# ============================================================

dados_linha <- dados %>%
  mutate(
    
    # Garantir que a coluna data seja Date
    data = as.Date(data),
    
    # Extrair mês
    mes_num = as.integer(format(data, "%m")),
    
    # Extrair ano
    ano_num = as.integer(format(data, "%Y")),
    
    # Criar Mês_Ano
    mes_ano = paste0(
      meses_pt[mes_num],
      "_",
      ano_num
    )
  )


# ============================================================
# 4. FILTRAR O PERÍODO DE INTERESSE
# ============================================================

dados_linha <- dados_linha %>%
  filter(
    mes_ano %in% ordem_meses
  )


# ============================================================
# 5. TRANSFORMAR MÊS_ANO EM FATOR ORDENADO
# ============================================================

dados_linha <- dados_linha %>%
  mutate(
    mes_ano = factor(
      mes_ano,
      levels = ordem_meses
    )
  )


# ============================================================
# 6. CONFERIR OS REVIS EXISTENTES
# ============================================================

print(unique(dados_linha$revis))


# ============================================================
# 7. CALCULAR:
#    - N
#    - MÉDIA
#    - DESVIO PADRÃO
#    - ERRO PADRÃO
#    - IC 95%
# ============================================================

dados_mensal <- dados_linha %>%
  group_by(
    revis,
    mes_ano
  ) %>%
  summarise(
    
    # Número de observações
    n = sum(!is.na(abundancia)),
    
    # Média
    media = mean(
      abundancia,
      na.rm = TRUE
    ),
    
    # Desvio padrão
    dp = sd(
      abundancia,
      na.rm = TRUE
    ),
    
    .groups = "drop"
  ) %>%
  
  mutate(
    
    # --------------------------------------------------------
    # Erro padrão da média
    # --------------------------------------------------------
    
    erro_padrao = ifelse(
      n > 1,
      dp / sqrt(n),
      NA_real_
    ),
    
    # --------------------------------------------------------
    # Valor crítico da distribuição t
    # --------------------------------------------------------
    
    t_critico = ifelse(
      n > 1,
      qt(
        0.975,
        df = n - 1
      ),
      NA_real_
    ),
    
    # --------------------------------------------------------
    # Limite inferior do IC 95%
    # --------------------------------------------------------
    
    IC_inferior = ifelse(
      n > 1,
      media - t_critico * erro_padrao,
      media
    ),
    
    # --------------------------------------------------------
    # Limite superior do IC 95%
    # --------------------------------------------------------
    
    IC_superior = ifelse(
      n > 1,
      media + t_critico * erro_padrao,
      media
    )
  )


# ============================================================
# 8. GARANTIR QUE TODOS OS MESES APAREÇAM
#    PARA CADA REVIS
# ============================================================

dados_mensal <- dados_mensal %>%
  complete(
    revis,
    mes_ano = factor(
      ordem_meses,
      levels = ordem_meses
    )
  )


# ============================================================
# 9. CONFERIR OS DADOS
# ============================================================

print(dados_mensal)


# ============================================================
# 10. GRÁFICO
#
# DUAS LINHAS:
#    - uma para cada REVIS
#
# FAIXAS SOMBREADAS:
#    - IC 95%
#
# LINHAS:
#    - média mensal
# ============================================================
# ============================================================
# CRIAR ÍNDICE NUMÉRICO DO TEMPO
# ============================================================

dados_mensal <- dados_mensal %>%
  mutate(
    tempo = as.numeric(
      factor(
        mes_ano,
        levels = ordem_meses
      )
    )
  )


# ============================================================
# 1. GRÁFICO COM LINHAS E IC 95% SUAVIZADOS
# ============================================================

grafico_linha <- ggplot(
  dados_mensal,
  aes(
    x = tempo,
    group = revis
  )
) +
  
  # ==========================================================
# INTERVALO DE CONFIANÇA 95% SUAVIZADO
# ==========================================================

geom_smooth(
  aes(
    y = media,
    color = revis,
    fill = revis
  ),
  method = "loess",
  formula = y ~ x,
  span = 0.75,
  se = TRUE,
  alpha = 0.20,
  linewidth = 1.2,
  na.rm = TRUE
) +
  
  # ==========================================================
# LINHA CENTRAL DA TENDÊNCIA SUAVIZADA
# ==========================================================

geom_smooth(
  aes(
    y = media,
    color = revis
  ),
  method = "loess",
  formula = y ~ x,
  span = 0.75,
  se = FALSE,
  linewidth = 1.4,
  na.rm = TRUE
) +
  
  # ==========================================================
# EIXO X
# ==========================================================

scale_x_continuous(
  breaks = 1:length(ordem_meses),
  labels = ordem_meses,
  expand = expansion(mult = c(0.02, 0.02))
) +
  
  # ==========================================================
# RÓTULOS
# ==========================================================

labs(
  x = "Mês",
  y = "Abundância média",
  color = "REVIS",
  fill = "REVIS"
) +
  
  # ==========================================================
# TEMA
# ==========================================================

theme_classic() +
  
  theme(
    
    axis.text.x = element_text(
      angle = 45,
      hjust = 1,
      vjust = 1,
      size = 10
    ),
    
    axis.text.y = element_text(
      size = 10
    ),
    
    axis.title.x = element_text(
      size = 12
    ),
    
    axis.title.y = element_text(
      size = 12
    ),
    
    legend.title = element_text(
      size = 11
    ),
    
    legend.text = element_text(
      size = 10
    ),
    
    plot.title = element_blank()
  )


# ============================================================
# 2. MOSTRAR O GRÁFICO
# ============================================================

print(grafico_linha)


# ============================================================
# 3. SALVAR
# ============================================================

ggsave(
  filename = "serie_temporal_REVIS_LOESS_IC95.png",
  plot = grafico_linha,
  width = 16,
  height = 8,
  dpi = 300
)



