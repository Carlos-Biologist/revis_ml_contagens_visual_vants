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
library(performance)

m_nb_aditivo <- glmmTMB(abundancia ~ especie + Mês + turno + revis + (1 | voo), 
                data = dados, 
                family = nbinom2)

summary(m_nb_aditivo)

r2_nakagawa(m_nb_aditivo)

# ============================================================
# MODELO NULO
# ============================================================

m_nb_nulo <- glmmTMB(
  abundancia ~ 1 + (1 | voo),
  data = dados,
  family = nbinom2
)


# ============================================================
# VERIFICAR O MODELO NULO
# ============================================================

summary(m_nb_nulo)


# ============================================================
# CALCULAR R² USANDO O MODELO NULO
# ============================================================

library(performance)

r2_nb <- r2_nakagawa(
  m_nb_aditivo,
  null_model = m_nb_nulo
)

r2_nb



residuos_adit <- residuals(m_nb_aditivo, type = "pearson")

shapiro.test(residuos_adit)

hist(residuos_adit)

m_nb_interacao <- glmmTMB(abundancia ~ revis*especie + revis*Mês + revis*turno + (1 | voo), 
                        data = dados, 
                        family = nbinom2)

summary(m_nb_interacao)

r2_nakagawa(m_nb_interacao)

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

###############################################################################
###############################################################################

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
# NOMES DOS MESES EM INGLÊS PARA O EIXO X
# ============================================================

ordem_meses_ingles <- c(
  "April_2025",
  "May_2025",
  "June_2025",
  "July_2025",
  "August_2025",
  "September_2025",
  "October_2025",
  "November_2025",
  "December_2025",
  "January_2026",
  "February_2026",
  "March_2026",
  "April_2026",
  "May_2026",
  "June_2026",
  "July_2026",
  "August_2026"
)


# ============================================================
# PADRONIZAR OS NOMES DOS REVIS
#
# IL -> Ilha dos Lobos
# ML -> Molhe Leste
# ============================================================

dados_mensal <- dados_mensal %>%
  mutate(
    Wildlife_Refuge = case_when(
      
      grepl(
        "^IL$|Ilha dos Lobos|REVIS IL",
        revis,
        ignore.case = TRUE
      ) ~ "Ilha dos Lobos",
      
      grepl(
        "^ML$|Molhe Leste|REVIS ML",
        revis,
        ignore.case = TRUE
      ) ~ "Molhe Leste",
      
      TRUE ~ as.character(revis)
    )
  )


# ============================================================
# DEFINIR A ORDEM DA LEGENDA
# ============================================================

dados_mensal <- dados_mensal %>%
  mutate(
    Wildlife_Refuge = factor(
      Wildlife_Refuge,
      levels = c(
        "Ilha dos Lobos",
        "Molhe Leste"
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
    group = Wildlife_Refuge
  )
) +
  
  # ==========================================================
# INTERVALO DE CONFIANÇA 95% SUAVIZADO
# ==========================================================

geom_smooth(
  aes(
    y = media,
    color = Wildlife_Refuge,
    fill = Wildlife_Refuge
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
    color = Wildlife_Refuge
  ),
  method = "loess",
  formula = y ~ x,
  span = 0.75,
  se = FALSE,
  linewidth = 1.4,
  na.rm = TRUE
) +
  
  # ==========================================================
# CORES
#
# Ilha dos Lobos = preto
# Molhe Leste    = cinza
# ==========================================================

scale_color_manual(
  values = c(
    "Ilha dos Lobos" = "black",
    "Molhe Leste" = "grey50"
  )
) +
  
  scale_fill_manual(
    values = c(
      "Ilha dos Lobos" = "black",
      "Molhe Leste" = "grey50"
    )
  ) +
  
  # ==========================================================
# EIXO X
# ==========================================================

scale_x_continuous(
  breaks = 1:length(ordem_meses),
  labels = ordem_meses_ingles,
  expand = expansion(
    mult = c(0.02, 0.02)
  )
) +
  
  # ==========================================================
# RÓTULOS EM INGLÊS
# ==========================================================

labs(
  x = "Month/Year",
  y = "Number of indivíduals",
  color = "Wildlife Refuge",
  fill = "Wildlife Refuge"
) +
  
  # ==========================================================
# TEMA
# ==========================================================

theme_classic(
  base_family = "Times New Roman",
  base_size = 12
) +
  
  theme(
    
    # --------------------------------------------------------
    # EIXO X
    # --------------------------------------------------------
    
    axis.text.x = element_text(
      angle = 45,
      hjust = 1,
      vjust = 1,
      size = 12,
      family = "Times New Roman"
    ),
    
    # --------------------------------------------------------
    # EIXO Y
    # --------------------------------------------------------
    
    axis.text.y = element_text(
      size = 12,
      family = "Times New Roman"
    ),
    
    # --------------------------------------------------------
    # TÍTULO DO EIXO X
    # --------------------------------------------------------
    
    axis.title.x = element_text(
      size = 12,
      family = "Times New Roman",
      face = "plain"
    ),
    
    # --------------------------------------------------------
    # TÍTULO DO EIXO Y EM NEGRITO
    # --------------------------------------------------------
    
    axis.title.y = element_text(
      size = 12,
      family = "Times New Roman",
      face = "bold"
    ),
    
    # --------------------------------------------------------
    # LEGENDA
    # --------------------------------------------------------
    
    legend.position = "top",
    
    legend.title = element_text(
      size = 12,
      family = "Times New Roman"
    ),
    
    legend.text = element_text(
      size = 12,
      family = "Times New Roman"
    ),
    
    # Espaçamento da legenda
    legend.spacing.x = unit(
      0.5,
      "cm"
    ),
    
    # --------------------------------------------------------
    # SEM TÍTULO
    # --------------------------------------------------------
    
    plot.title = element_blank()
  )


# ============================================================
# 2. MOSTRAR O GRÁFICO
# ============================================================

print(grafico_linha)


# ============================================================
# 3. SALVAR O GRÁFICO
# ============================================================

ggsave(
  filename = "serie_temporal_REVIS.png",
  plot = grafico_linha,
  width = 16,
  height = 8,
  dpi = 300
)

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
    
    data = as.Date(data),
    
    mes_num = as.integer(
      format(data, "%m")
    ),
    
    ano_num = as.integer(
      format(data, "%Y")
    ),
    
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
# 6. CONFERIR OS REVIS E AS ESPÉCIES
# ============================================================

print(unique(dados_linha$revis))

print(unique(dados_linha$especie))


# ============================================================
# 7. CALCULAR:
#    - N
#    - MÉDIA
#    - DESVIO PADRÃO
#    - ERRO PADRÃO
#    - IC 95%
#
#    AGORA POR:
#    - REVIS
#    - ESPÉCIE
#    - MÊS
# ============================================================

dados_mensal <- dados_linha %>%
  group_by(
    revis,
    especie,
    mes_ano
  ) %>%
  summarise(
    
    # Número de observações
    n = sum(
      !is.na(abundancia)
    ),
    
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
#    PARA CADA REVIS E CADA ESPÉCIE
# ============================================================

dados_mensal <- dados_mensal %>%
  complete(
    revis,
    especie,
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
# 10. CRIAR ÍNDICE NUMÉRICO DO TEMPO
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
# 11. NOMES DOS MESES EM INGLÊS
# ============================================================

ordem_meses_ingles <- c(
  "April_2025",
  "May_2025",
  "June_2025",
  "July_2025",
  "August_2025",
  "September_2025",
  "October_2025",
  "November_2025",
  "December_2025",
  "January_2026",
  "February_2026",
  "March_2026",
  "April_2026",
  "May_2026",
  "June_2026",
  "July_2026",
  "August_2026"
)


# ============================================================
# 12. PADRONIZAR OS NOMES DOS REVIS
#
# IL -> Ilha dos Lobos
# ML -> Molhe Leste
# ============================================================

dados_mensal <- dados_mensal %>%
  mutate(
    
    Wildlife_Refuge = case_when(
      
      grepl(
        "^IL$|Ilha dos Lobos|REVIS IL",
        revis,
        ignore.case = TRUE
      ) ~ "Ilha dos Lobos",
      
      grepl(
        "^ML$|Molhe Leste|REVIS ML",
        revis,
        ignore.case = TRUE
      ) ~ "Molhe Leste",
      
      TRUE ~ as.character(revis)
    )
  )


# ============================================================
# 13. DEFINIR A ORDEM DA LEGENDA
# ============================================================

dados_mensal <- dados_mensal %>%
  mutate(
    Wildlife_Refuge = factor(
      Wildlife_Refuge,
      levels = c(
        "Ilha dos Lobos",
        "Molhe Leste"
      )
    )
  )

# ============================================================
# 14. PADRONIZAR OS NOMES DAS ESPÉCIES
# ============================================================

dados_mensal <- dados_mensal %>%
  mutate(
    especie = case_when(
      
      especie == "o_flavescens" ~ "Otaria flavescens",
      
      especie == "a_australis" ~ "Arctocephalus australis",
      
      TRUE ~ as.character(especie)
    )
  )


# ============================================================
# 15. DEFINIR A ORDEM DAS ESPÉCIES
# ============================================================

dados_mensal <- dados_mensal %>%
  mutate(
    especie = factor(
      especie,
      levels = c(
        "Otaria flavescens",
        "Arctocephalus australis"
      )
    )
  )


# ============================================================
# 16. GRÁFICO
#
# UM PAINEL PARA CADA ESPÉCIE
# DUAS COLUNAS E UMA LINHA
#
# EIXO Y PADRONIZADO:
# -30 ATÉ 100
# ============================================================

grafico_linha <- ggplot(
  dados_mensal,
  aes(
    x = tempo,
    group = Wildlife_Refuge
  )
) +
  
  # ==========================================================
# INTERVALO DE CONFIANÇA 95% SUAVIZADO
# ==========================================================

geom_smooth(
  aes(
    y = media,
    color = Wildlife_Refuge,
    fill = Wildlife_Refuge
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
    color = Wildlife_Refuge
  ),
  method = "loess",
  formula = y ~ x,
  span = 0.75,
  se = FALSE,
  linewidth = 1.4,
  na.rm = TRUE
) +
  
  # ==========================================================
# CORES
# ==========================================================

scale_color_manual(
  values = c(
    "Ilha dos Lobos" = "black",
    "Molhe Leste" = "grey50"
  )
) +
  
  scale_fill_manual(
    values = c(
      "Ilha dos Lobos" = "black",
      "Molhe Leste" = "grey50"
    )
  ) +
  
  # ==========================================================
# EIXO X
# ==========================================================

scale_x_continuous(
  breaks = 1:length(ordem_meses),
  labels = ordem_meses_ingles,
  expand = expansion(
    mult = c(0.02, 0.02)
  )
) +
  
  # ==========================================================
# EIXO Y
#
# MESMA ESCALA PARA AS DUAS ESPÉCIES
# ==========================================================

scale_y_continuous(
  limits = c(-30, 100),
  breaks = seq(
    -30,
    100,
    by = 10
  )
) +
  
  # ==========================================================
# RÓTULOS
# ==========================================================

labs(
  x = "Month/Year",
  y = "Number of individuals",
  color = "Wildlife Refuge",
  fill = "Wildlife Refuge"
) +
  
  # ==========================================================
# FACET
#
# DUAS COLUNAS E UMA LINHA
# ==========================================================

facet_wrap(
  ~ especie,
  ncol = 2
) +
  
  # ==========================================================
# TEMA
# ==========================================================

theme_classic(
  base_family = "Times New Roman",
  base_size = 12
) +
  
theme(
  
  # --------------------------------------------------------
  # EIXO X
  # --------------------------------------------------------
  
  axis.text.x = element_text(
    angle = 45,
    hjust = 1,
    vjust = 1,
    size = 12,
    family = "Times New Roman"
  ),
  
  # --------------------------------------------------------
  # EIXO Y
  # --------------------------------------------------------
  
  axis.text.y = element_text(
    size = 12,
    family = "Times New Roman"
  ),
  
  # --------------------------------------------------------
  # TÍTULOS DOS EIXOS
  # --------------------------------------------------------
  
  axis.title.x = element_text(
    size = 12,
    family = "Times New Roman",
    face = "plain"
  ),
  
  axis.title.y = element_text(
    size = 12,
    family = "Times New Roman",
    face = "bold"
  ),
  
  # --------------------------------------------------------
  # LEGENDA
  # --------------------------------------------------------
  
  legend.position = "top",
  
  legend.title = element_text(
    size = 12,
    family = "Times New Roman"
  ),
  
  legend.text = element_text(
    size = 12,
    family = "Times New Roman"
  ),
  
  legend.spacing.x = unit(
    0.5,
    "cm"
  ),
  
  # --------------------------------------------------------
  # NOMES DAS ESPÉCIES
  # --------------------------------------------------------
  
  strip.text = element_text(
    size = 12,
    family = "Times New Roman",
    face = "bold.italic"
  ),
  
  plot.title = element_blank()
)

# ============================================================
# 17. MOSTRAR O GRÁFICO
# ============================================================

print(grafico_linha)


# ============================================================
# 18. SALVAR O GRÁFICO
# ============================================================

ggsave(
  filename = "serie_temporal_REVIS_por_especie.png",
  plot = grafico_linha,
  width = 16,
  height = 8,
  dpi = 300
)

################################################################################
################################################################################

# Ler planilha

dados_simul <- read_excel("dados_geral_2025_2026_simultaneo.xlsx")

head(dados_simul)      # primeiras linhas
summary(dados_simul)   # resumo estatístico
str(dados_simul)       # estrutura das variáveis

#------------------------------------------------------------------------------#

# 1. Preparar os dados

dados_simul$data <- as.Date(dados_simul$data, format = "%d/%m/%Y")
dados_simul$especie <- as.factor(dados_simul$especie)
dados_simul$Mês <- as.numeric(dados_simul$Mês)
dados_simul$turno <- as.factor(dados_simul$turno)
dados_simul$revis <- as.factor(dados_simul$revis)
dados_simul$simult <- as.numeric(dados_simul$simult)

str(dados_simul)

#------------------------------------------------------------------------------#

# 2. Teste de normalidade (Shapiro-Wilk)

library(dplyr)

shapiro.test(dados_simul$abundancia)

boxplot(dados_simul$abundancia)

hist(dados_simul$abundancia)

#------------------------------------------------------------------------------#

names(dados_simul)

# 7. Ajuste um modelo Binomial Negativa

library(glmmTMB)

m_nb_aditivo_simul <- glmmTMB(abundancia ~ especie + Mês + turno + revis + (1 | voo), 
                        data = dados_simul, 
                        family = nbinom2)

summary(m_nb_aditivo_simul)

residuos_adit_sim <- residuals(m_nb_aditivo_simul, type = "pearson")

shapiro.test(residuos_adit_sim)

hist(residuos_adit_sim)

m_nb_interacao_simul <- glmmTMB(abundancia ~ revis*especie + revis*Mês + revis*turno + (1 | voo), 
                          data = dados_simul, 
                          family = nbinom2)

summary(m_nb_interacao_simul)

residuos_int_simul <- residuals(m_nb_interacao_simul, type = "pearson")

shapiro.test(residuos_int_simul)

hist(residuos_int_simul)

################################################################################
################################################################################

# ============================================================
# GRÁFICO DE COLUNAS EMPILHADAS
# Otaria flavescens
#
# Cada "simult" = uma coluna
# IL = preto
# ML = cinza
#
# Eixo X:
# May/Daytime/2025
# Jun/Daytime/2025
# Jul/Daytime/2025
# Aug/Daytime/2025
# Aug/Nighttime/2025
# ...
#
# O número no topo = total IL + ML
# ============================================================

library(readxl)
library(dplyr)
library(ggplot2)

# ============================================================
# FILTRAR Otaria flavescens
# ============================================================

dados_flavescens <- dados_simul %>%
  filter(
    especie == "o_flavescens",
    revis %in% c("IL", "ML"),
    !is.na(simult),
    !is.na(abundancia)
  ) %>%
  mutate(
    simult = as.integer(simult)
  )

# ============================================================
# CRIAR NOMES DOS MESES, TURNOS E ANO
# ============================================================

dados_flavescens <- dados_flavescens %>%
  mutate(
    
    mes_nome = case_when(
      Mês == 1  ~ "Jan",
      Mês == 2  ~ "Feb",
      Mês == 3  ~ "Mar",
      Mês == 4  ~ "Apr",
      Mês == 5  ~ "May",
      Mês == 6  ~ "Jun",
      Mês == 7  ~ "Jul",
      Mês == 8  ~ "Aug",
      Mês == 9  ~ "Sep",
      Mês == 10 ~ "Oct",
      Mês == 11 ~ "Nov",
      Mês == 12 ~ "Dec",
      TRUE ~ NA_character_
    ),
    
    turno_nome = case_when(
      turno %in% c("Diurno", "diurno", "Daytime") ~ "Daytime",
      turno %in% c("Noturno", "noturno", "Nighttime") ~ "Nighttime",
      TRUE ~ as.character(turno)
    ),
    
    ano_nome = as.character(ano),
    
    rotulo_simult = paste(
      mes_nome,
      turno_nome,
      ano_nome,
      sep = "/"
    )
  )

# ============================================================
# CRIAR TABELA DE RÓTULOS
# ============================================================

ordem_simult <- dados_flavescens %>%
  arrange(simult) %>%
  group_by(simult) %>%
  summarise(
    rotulo_simult = first(rotulo_simult),
    .groups = "drop"
  ) %>%
  arrange(simult)


# ============================================================
# AGREGAR ABUNDÂNCIA POR SIMULT E REVIS
# ============================================================

dados_flavescens <- dados_flavescens %>%
  group_by(simult, revis) %>%
  summarise(
    abundancia = sum(abundancia, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    revis = factor(
      revis,
      levels = c("IL", "ML")
    )
  ) %>%
  arrange(simult, revis)


# ============================================================
# CALCULAR TOTAL IL + ML
# ============================================================

totais_simult <- dados_flavescens %>%
  group_by(simult) %>%
  summarise(
    total = sum(abundancia, na.rm = TRUE),
    .groups = "drop"
  )

# ============================================================
# GRÁFICO
#
# IMPORTANTE:
# O eixo X usa SIMULT como fator.
# Os rótulos são fornecidos separadamente pelo scale_x_discrete().
# ============================================================

grafico_flavescens <- ggplot(
  dados_flavescens,
  aes(
    x = factor(simult),
    y = abundancia,
    fill = revis
  )
) +
  
  # ----------------------------------------------------------
# COLUNAS EMPILHADAS
# ----------------------------------------------------------

geom_col(
  width = 0.75,
  color = "black",
  linewidth = 0.4
) +
  
  # ----------------------------------------------------------
# TOTAL NO TOPO DE CADA COLUNA
# ----------------------------------------------------------

geom_text(
  data = totais_simult,
  aes(
    x = factor(simult),
    y = total,
    label = total
  ),
  inherit.aes = FALSE,
  vjust = -0.4,
  size = 4,
  family = "Times New Roman"
) +
  
  # ----------------------------------------------------------
# RÓTULOS DO EIXO X
# ----------------------------------------------------------

scale_x_discrete(
  breaks = ordem_simult$simult,
  labels = ordem_simult$rotulo_simult
) +
  
  # ----------------------------------------------------------
# CORES
# ----------------------------------------------------------

scale_fill_manual(
  values = c(
    "IL" = "black",
    "ML" = "grey60"
  ),
  labels = c(
    "IL" = "Ilha dos Lobos",
    "ML" = "Molhe Leste"
  )
) +
  
  # ----------------------------------------------------------
# EIXO Y
# ----------------------------------------------------------

scale_y_continuous(
  expand = expansion(
    mult = c(0, 0.10)
  )
) +
  
  # ----------------------------------------------------------
# RÓTULOS DOS EIXOS E TÍTULO
# ----------------------------------------------------------

labs(
  title = "Otaria flavescens",
  x = NULL,
  y = "Number of individuals",
  fill = "Wildlife Refuge"
) +
  
  # ----------------------------------------------------------
# TEMA
# ----------------------------------------------------------

theme_classic(
  base_family = "Times New Roman",
  base_size = 12
) +
  
  theme(
    
    # --------------------------------------------------------
    # NOME DA ESPÉCIE
    # --------------------------------------------------------
    
    plot.title = element_text(
      family = "Times New Roman",
      size = 14,
      face = "bold.italic",
      hjust = 0.5,
      margin = margin(
        b = 12
      )
    ),
    
    # --------------------------------------------------------
    # EIXO X
    # --------------------------------------------------------
    
    axis.text.x = element_text(
      angle = 45,
      hjust = 1,
      vjust = 1,
      size = 11,
      family = "Times New Roman"
    ),
    
    # --------------------------------------------------------
    # EIXO Y
    # --------------------------------------------------------
    
    axis.text.y = element_text(
      size = 12,
      family = "Times New Roman"
    ),
    
    axis.title.y = element_text(
      size = 12,
      family = "Times New Roman",
      face = "bold"
    ),
    
    # --------------------------------------------------------
    # LEGENDA
    # --------------------------------------------------------
    
    legend.title = element_text(
      size = 12,
      family = "Times New Roman"
    ),
    
    legend.text = element_text(
      size = 11,
      family = "Times New Roman"
    )
  )


# ============================================================
# MOSTRAR GRÁFICO
# ============================================================

grafico_flavescens


# ============================================================
# SALVAR
# ============================================================

ggsave(
  filename = "Otaria_flavescens_simult_empilhado.png",
  plot = grafico_flavescens,
  width = 14,
  height = 7,
  dpi = 300
)

# ============================================================
# GRÁFICO DE COLUNAS EMPILHADAS
# Arctocephalus australis
#
# Cada "simult" = uma coluna
# IL = preto
# ML = cinza
#
# Eixo X:
# May/Daytime/2025
# Jun/Daytime/2025
# Jul/Daytime/2025
# Aug/Daytime/2025
# Aug/Nighttime/2025
# ...
#
# O número no topo = total IL + ML
# ============================================================

library(readxl)
library(dplyr)
library(ggplot2)


# ============================================================
# FILTRAR Arctocephalus australis
# ============================================================

dados_australis <- dados_simul %>%
  filter(
    especie == "a_australis",
    revis %in% c("IL", "ML"),
    !is.na(simult),
    !is.na(abundancia)
  ) %>%
  mutate(
    simult = as.integer(simult)
  )


# ============================================================
# CRIAR NOMES DOS MESES, TURNOS E ANO
# ============================================================

dados_australis <- dados_australis %>%
  mutate(
    
    mes_nome = case_when(
      Mês == 1  ~ "Jan",
      Mês == 2  ~ "Feb",
      Mês == 3  ~ "Mar",
      Mês == 4  ~ "Apr",
      Mês == 5  ~ "May",
      Mês == 6  ~ "Jun",
      Mês == 7  ~ "Jul",
      Mês == 8  ~ "Aug",
      Mês == 9  ~ "Sep",
      Mês == 10 ~ "Oct",
      Mês == 11 ~ "Nov",
      Mês == 12 ~ "Dec",
      TRUE ~ NA_character_
    ),
    
    turno_nome = case_when(
      turno %in% c("Diurno", "diurno", "Daytime") ~ "Daytime",
      turno %in% c("Noturno", "noturno", "Nighttime") ~ "Nighttime",
      TRUE ~ as.character(turno)
    ),
    
    ano_nome = as.character(ano),
    
    rotulo_simult = paste(
      mes_nome,
      turno_nome,
      ano_nome,
      sep = "/"
    )
  )


# ============================================================
# CRIAR TABELA DE RÓTULOS
# ============================================================

ordem_simult <- dados_australis %>%
  arrange(simult) %>%
  group_by(simult) %>%
  summarise(
    rotulo_simult = first(rotulo_simult),
    .groups = "drop"
  ) %>%
  arrange(simult)


# ============================================================
# AGREGAR ABUNDÂNCIA POR SIMULT E REVIS
# ============================================================

dados_australis <- dados_australis %>%
  group_by(simult, revis) %>%
  summarise(
    abundancia = sum(abundancia, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    revis = factor(
      revis,
      levels = c("IL", "ML")
    )
  ) %>%
  arrange(simult, revis)


# ============================================================
# CALCULAR TOTAL IL + ML
# ============================================================

totais_simult <- dados_australis %>%
  group_by(simult) %>%
  summarise(
    total = sum(abundancia, na.rm = TRUE),
    .groups = "drop"
  )


# ============================================================
# GRÁFICO
#
# O eixo X usa SIMULT como fator.
# Os rótulos são fornecidos separadamente pelo scale_x_discrete().
# ============================================================

grafico_australis <- ggplot(
  dados_australis,
  aes(
    x = factor(simult),
    y = abundancia,
    fill = revis
  )
) +
  
  # ----------------------------------------------------------
# COLUNAS EMPILHADAS
# ----------------------------------------------------------

geom_col(
  width = 0.75,
  color = "black",
  linewidth = 0.4
) +
  
  # ----------------------------------------------------------
# TOTAL NO TOPO DE CADA COLUNA
# ----------------------------------------------------------

geom_text(
  data = totais_simult,
  aes(
    x = factor(simult),
    y = total,
    label = total
  ),
  inherit.aes = FALSE,
  vjust = -0.4,
  size = 4,
  family = "Times New Roman"
) +
  
  # ----------------------------------------------------------
# RÓTULOS DO EIXO X
# ----------------------------------------------------------

scale_x_discrete(
  breaks = ordem_simult$simult,
  labels = ordem_simult$rotulo_simult
) +
  
  # ----------------------------------------------------------
# CORES
# ----------------------------------------------------------

scale_fill_manual(
  values = c(
    "IL" = "black",
    "ML" = "grey60"
  ),
  labels = c(
    "IL" = "Ilha dos Lobos",
    "ML" = "Molhe Leste"
  )
) +
  
  # ----------------------------------------------------------
# EIXO Y
# ----------------------------------------------------------

scale_y_continuous(
  expand = expansion(
    mult = c(0, 0.10)
  )
) +
  
  # ----------------------------------------------------------
# RÓTULOS DOS EIXOS E TÍTULO
# ----------------------------------------------------------

labs(
  title = "Arctocephalus australis",
  x = NULL,
  y = "Number of individuals",
  fill = "Wildlife Refuge"
) +
  
  # ----------------------------------------------------------
# TEMA
# ----------------------------------------------------------

theme_classic(
  base_family = "Times New Roman",
  base_size = 12
) +
  
  theme(
    
    # --------------------------------------------------------
    # NOME DA ESPÉCIE
    # --------------------------------------------------------
    
    plot.title = element_text(
      family = "Times New Roman",
      size = 14,
      face = "bold.italic",
      hjust = 0.5,
      margin = margin(
        b = 12
      )
    ),
    
    # --------------------------------------------------------
    # EIXO X
    # --------------------------------------------------------
    
    axis.text.x = element_text(
      angle = 45,
      hjust = 1,
      vjust = 1,
      size = 11,
      family = "Times New Roman"
    ),
    
    # --------------------------------------------------------
    # EIXO Y
    # --------------------------------------------------------
    
    axis.text.y = element_text(
      size = 12,
      family = "Times New Roman"
    ),
    
    axis.title.y = element_text(
      size = 12,
      family = "Times New Roman",
      face = "bold"
    ),
    
    # --------------------------------------------------------
    # LEGENDA
    # --------------------------------------------------------
    
    legend.title = element_text(
      size = 12,
      family = "Times New Roman"
    ),
    
    legend.text = element_text(
      size = 11,
      family = "Times New Roman"
    )
  )


# ============================================================
# MOSTRAR GRÁFICO
# ============================================================

grafico_australis


# ============================================================
# SALVAR
# ============================================================

ggsave(
  filename = "Arctocephalus_australis_simult_empilhado.png",
  plot = grafico_australis,
  width = 14,
  height = 7,
  dpi = 300
)
