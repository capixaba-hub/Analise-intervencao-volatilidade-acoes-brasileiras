# 1. Extração de Dados e Retornos
tickers <- c("PETR4.SA", "VALE3.SA", "ITUB4.SA")
getSymbols(tickers, src = "yahoo", from = "2007-01-01", to = "2026-10-04")



ret_petr <- na.omit(diff(log(Cl(PETR4.SA))))
ret_vale <- na.omit(diff(log(Cl(VALE3.SA))))
ret_itub <- na.omit(diff(log(Cl(ITUB4.SA))))






# --- PACOTES NECESSÁRIOS ---
library(Kendall)
library(moments)
library(lmtest)
library(tseries)
library(FinTS)

# --- FUNÇÃO PARA CALCULAR ESTATÍSTICAS DESCRITIVAS COMPLETAS ---
calcular_descritivas <- function(serie, nome_ativo) {
  s <- na.omit(as.numeric(serie))
  
  # Garantir que os retornos estão em escala percentual (r_t * 100)
  if (max(abs(s), na.rm = TRUE) < 1) {
    s <- s * 100
  }
  
  jb_test <- jarque.bera.test(s)
  
  data.frame(
    Ativo          = nome_ativo,
    N              = length(s),
    Media          = round(mean(s), 4),
    Desvio_Padrao  = round(sd(s), 4),
    Minimo         = round(min(s), 4),
    Maximo         = round(max(s), 4),
    Assimetria     = round(skewness(s), 4),
    Curtose        = round(kurtosis(s), 4), # Curtose bruta (Normal = 3)
    JB_Estatistica = round(unname(jb_test$statistic), 2),
    JB_pvalor      = format.pval(unname(jb_test$p.value), eps = 0.001, digits = 3)
  )
}

# --- EXECUÇÃO PARA OS TRÊS ATIVOS ---
# Utilizando as variáveis de retorno do seu ambiente: ret_petr, ret_vale, ret_itub
res_petr <- calcular_descritivas(ret_petr, "PETR4")
res_vale <- calcular_descritivas(ret_vale, "VALE3")
res_itub <- calcular_descritivas(ret_itub, "ITUB4")

# Consolidação da tabela no R
tabela_descritiva_final <- rbind(res_petr, res_vale, res_itub)

# Exibir no console
print(tabela_descritiva_final)








estatisticas <- function(x) {
  c(
    Média = mean(x, na.rm = TRUE),
    Mediana = median(x, na.rm = TRUE),
    DP = sd(x, na.rm = TRUE),
    Mínimo = min(x, na.rm = TRUE),
    Máximo = max(x, na.rm = TRUE),
    Assimetria = moments::skewness(x, na.rm = TRUE),
    Curtose = moments::kurtosis(x, na.rm = TRUE),
    Jarque_Bera = jarque.bera.test(na.omit(x))$statistic,
    p_valor_JB = jarque.bera.test(na.omit(x))$p.value
  )
}

resultados <- rbind(
  PETR4 = estatisticas(ret_petr),
  VALE3 = estatisticas(ret_vale),
  ITUB4 = estatisticas(ret_itub)
)

resultados


# Unifica as três séries de retornos alinhando pelas datas do índice
retornos_xts <- merge(ret_petr, ret_vale, ret_itub)
colnames(retornos_xts) <- c("PETR4", "VALE3", "ITUB4")

# Cria o data frame mantendo a coluna de datas explícita
df_retornos <- data.frame(
  Data = index(retornos_xts),
  coredata(retornos_xts)
)

# Caso precise das datas como um vetor simples isolado:
datas <- index(ret_petr)
length(ret_vale)

# 2. Ajuste ARIMA (Exemplo prático para você substituir pela sua ordem e restrições)
# Substitua o vetor 'order' e 'fixed' de acordo com a sua análise FAC/FACP.
# O transform.pars = FALSE é obrigatório ao usar 'fixed' com zeros/NAs.

acf(ret_petr, lag.max = 36, ylim = c(-0.2,0.2))
pacf(ret_petr, lag.max = 36, ylim = c(-0.2,0.2))
Box.test(ret_petr, lag = 36, type = "Ljung-Box")

# -- PETROBRAS --
arima_petr <- arima(ret_petr, 
                    order = c(7, 0, 0), 
                    fixed = c(0, 0, 0, 0, NA,NA,NA,0), 
                    transform.pars = FALSE)

coeftest(arima_petr)


acf(residuals(arima_petr), lag.max = 36, ylim = c(-0.2,0.2))
pacf(residuals(arima_petr), lag.max = 36, ylim = c(-0.2,0.2))
Box.test(residuals(arima_petr), lag = 36, type = "Ljung-Box")
ArchTest(residuals(arima_petr), lags = 36)
# -- VALE --

acf(ret_vale, lag.max = 36, ylim = c(-0.2,0.2))
pacf(ret_vale, lag.max = 36, ylim = c(-0.2,0.2))
Box.test(ret_vale, lag = 36, type = "Ljung-Box")

# 1. Extrair o dia da semana das datas (1 = Segunda, ..., 5 = Sexta)
# Usamos %u para garantir o formato numérico independente do idioma do seu sistema
dias_semana <- as.factor(format(index(ret_vale), "%u"))

# 2. Teste preliminar via Regressão Linear (OLS)
# Isso permite verificar rapidamente se algum dia da semana tem média estatisticamente diferente
teste_sazonalidade <- lm(ret_vale ~ dias_semana)
summary(teste_sazonalidade) # nao apresenta sazonalidade deterministica


# Carregue o pacote tseries para os testes de raiz unitária
library(tseries)

# ==============================================================================
# 1. TESTE DE TENDÊNCIA ESTOCÁSTICA (Estacionariedade)
# ==============================================================================
# O Teste Augmented Dickey-Fuller (ADF) verifica a presença de raiz unitária.
# H0: A série possui raiz unitária (tem tendência estocástica / não é estacionária).
# H1: A série é estacionária.

cat("\n--- Teste ADF (Tendência Estocástica) ---\n")
adf_teste <- adf.test(na.omit(ret_vale))
print(adf_teste)


# ==============================================================================
# 2. TESTE DE TENDÊNCIA DETERMINÍSTICA (Regressão Linear contra o Tempo)
# ==============================================================================
# Verificamos se há um "crescimento" ou "decaimento" linear na série de retornos
# ao longo do tempo através de um modelo simples de Mínimos Quadrados Ordinários (OLS).
# H0: O coeficiente de tempo é igual a zero (Não há tendência determinística).
# H1: O coeficiente de tempo é diferente de zero (Há tendência determinística).

# Criação de um índice de tempo t = 1, 2, ..., T
tempo <- 1:length(ret_vale)

# Ajuste da regressão
teste_tendencia_lin <- lm(ret_vale ~ tempo)

cat("\n--- Regressão OLS (Tendência Determinística) ---\n")
summary(teste_tendencia_lin)

cat("\n--- Teste de Mann-Kendall ---\n")
mk_teste <- MannKendall(as.numeric(na.omit(ret_vale)))
print(mk_teste) # Nao ha tendencia 


arima_vale <- arima(ret_vale, 
                    order = c(19, 0, 1), 
                    fixed = c(0, NA, NA,NA,0,NA,0,0,0,NA,0,0,0,0,0,0,0,0,NA,0,0), 
                    transform.pars = FALSE)
coeftest(arima_vale)

acf(residuals(arima_vale), lag.max = 36, ylim = c(-0.1,0.1))
pacf(residuals(arima_vale), lag.max = 36, ylim = c(-0.2,0.2))
Box.test(residuals(arima_vale), lag = 36, type = "Ljung-Box")
ArchTest(residuals(arima_vale), lags = 36)



# -- ITAÚ --
acf(ret_itub, lag.max = 36, ylim = c(-0.1,0.1))
pacf(ret_itub, lag.max = 36, ylim = c(-0.2,0.2))
Box.test(ret_itub, lag = 36, type = "Ljung-Box")

arima_itub <- arima(ret_itub, 
                    order = c(6, 0, 0), 
                    fixed = c(0, 0, NA, 0,0,NA,0), 
                    transform.pars = FALSE)

coeftest(arima_itub)

acf(residuals(arima_itub), lag.max = 36, ylim = c(-0.1,0.1))
pacf(residuals(arima_itub), lag.max = 36, ylim = c(-0.2,0.2))
Box.test(residuals(arima_itub), lag = 36, type = "Ljung-Box")
ArchTest(residuals(arima_itub), lags = 36)


# 3. Função de Diagnóstico de Resíduos
diagnostico_residuos <- function(modelo_arima, nome_ativo, def_lag = 20) {
  residuos <- residuals(modelo_arima)
  
  # Teste para Ruído Branco (Autocorrelação Linear)
  lb_linear <- Box.test(residuos, lag = def_lag, type = "Ljung-Box")
  
  # Teste para Efeito ARCH (Heterocedasticidade Condicional)
  lb_quadrado <- Box.test(residuos^2, lag = def_lag, type = "Ljung-Box")
  
  cat("\n=========================================\n")
  cat("Ativo:", nome_ativo, "\n")
  cat("-----------------------------------------\n")
  cat("Ljung-Box (Resíduos - Ruído Branco):\n")
  cat("Estatística:", round(lb_linear$statistic, 4), "| p-valor:", round(lb_linear$p.value, 4), "\n\n")
  
  cat("Ljung-Box (Resíduos ao Quadrado - Efeito ARCH):\n")
  cat("Estatística:", round(lb_quadrado$statistic, 4), "| p-valor:", round(lb_quadrado$p.value, 4), "\n")
  cat("=========================================\n")
}

# 4. Executando os Testes
diagnostico_residuos(arima_petr, "PETR4")
diagnostico_residuos(arima_vale, "VALE3")
diagnostico_residuos(arima_itub, "ITUB4")


library(rugarch)

# Lembre-se de substituir o 'armaOrder' de cada especificação pelas ordens (p, q) 
# que você validou na etapa do diagnóstico do ARIMA. 
# A distribuição 'sstd' (Skew-Student-t) acomoda a assimetria e curtose dos retornos.

# ==============================================================================
# 1. Ajuste APARCH(1,1) para PETROBRAS (PETR4)
# ==============================================================================
spec_petr <- ugarchspec(
  variance.model = list(model = "apARCH", garchOrder = c(1, 1)),
  mean.model = list(armaOrder = c(0, 0), include.mean = TRUE), # Substitua pela sua ordem
  distribution.model = "sstd" 
)

aparch_petr <- ugarchfit(spec = spec_petr, data = residuals(arima_petr), solver = "hybrid")

cat("\n--- Resumo APARCH(1,1) - PETR4 ---\n")
show(aparch_petr) # Use 'show' para imprimir o sumário completo no console

plot(aparch_petr, which = 12)

# ==============================================================================
# 2. Ajuste APARCH(1,1) para VALE (VALE3)
# ==============================================================================
# Nota: Se você confirmou sazonalidade com as dummies, inclua o argumento:
# external.regressors = matriz_dummies_dias dentro do mean.model
spec_vale <- ugarchspec(
  variance.model = list(model = "apARCH", garchOrder = c(1, 1)),
  mean.model = list(armaOrder = c(0, 0), include.mean = TRUE), # Substitua pela sua ordem
  distribution.model = "sstd"
)

aparch_vale <- ugarchfit(spec = spec_vale, data = residuals(arima_vale), solver = "hybrid")

cat("\n--- Resumo APARCH(1,1) - VALE3 ---\n")
show(aparch_vale)

plot(aparch_vale, which = 9)


# ==============================================================================
# 3. Ajuste APARCH(1,1) para ITAÚ (ITUB4)
# ==============================================================================
spec_itub <- ugarchspec(
  variance.model = list(model = "apARCH", garchOrder = c(1, 1)),
  mean.model = list(armaOrder = c(0, 0), include.mean = TRUE), # Substitua pela sua ordem
  distribution.model = "sstd"
)

aparch_itub <- ugarchfit(spec = spec_itub, data = residuals(arima_itub), solver = "hybrid")

cat("\n--- Resumo APARCH(1,1) - ITUB4 ---\n")
show(aparch_itub)

plot(aparch_itub, which = 3)




library(ggplot2)
library(rugarch)
library(ggplot2)

# Função para extrair a volatilidade e gerar o gráfico com as duas linhas de referência
plotar_volatilidade_completo <- function(fit_aparch, serie_xts, nome_ativo) {
  
  # Extração da volatilidade condicional (sigma_t)
  vol <- as.numeric(sigma(fit_aparch))
  datas <- as.Date(index(serie_xts))
  
  # Média da volatilidade condicional
  media_vol <- mean(vol, na.rm = TRUE)
  
  # Limiar dos 5% superiores (Percentil 95%)
  cauda_5_vol <- quantile(vol, probs = 0.975, na.rm = TRUE)
  
  df_plot <- data.frame(
    Data = datas,
    Volatilidade = vol
  )
  
  p <- ggplot(df_plot, aes(x = Data, y = Volatilidade)) +
    geom_line(color = "steelblue", linewidth = 0.5) +
    
    # Linha vermelha contínua para a Média
    geom_hline(aes(yintercept = media_vol, color = "Média"), 
               linetype = "solid", linewidth = 0.8) +
    
    # Linha para a cauda dos 5% mais altos (Percentil 95)
    geom_hline(aes(yintercept = cauda_5_vol, color = "Cauda 5% (P95)"), 
               linetype = "dashed", linewidth = 0.8) +
    
    scale_color_manual(
      name = "Linhas de Corte",
      values = c("Média" = "red", "Cauda 5% (P95)" = "darkorange")
    ) +
    
    # CORREÇÃO DO EIXO X: Força que o gráfico mostre o limite total até 2026
    scale_x_date(
      date_breaks = "2 years",
      date_labels = "%Y",
      limits = c(min(datas), max(datas)),
      expand = expansion(mult = c(0.01, 0.02))
    ) +
    
    labs(
      title = paste("Volatilidade Condicional APARCH(1,1) -", nome_ativo),
      subtitle = paste0("Média: ", round(media_vol, 4), 
                        " | Limiar 5% Extremos: ", round(cauda_5_vol, 4)),
      x = "Data",
      y = "Volatilidade Condicional (\u03c3_t)"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(face = "bold", size = 13),
      plot.subtitle = element_text(color = "gray30"),
      legend.position = "bottom",
      axis.text.x = element_text(angle = 45, hjust = 1)
    )
  
  return(p)
}

# ==============================================================================
# Gerando e exibindo os 3 gráficos individualmente
# ==============================================================================

grafico_petr <- plotar_volatilidade_completo(aparch_petr, ret_petr, "PETR4")
grafico_vale <- plotar_volatilidade_completo(aparch_vale, ret_vale, "VALE3")
grafico_itub <- plotar_volatilidade_completo(aparch_itub, ret_itub, "ITUB4")

# Exibir no RStudio
print(grafico_petr)
print(grafico_vale)
print(grafico_itub)

# ==============================================================================
# FUNÇÃO PARA EXTRAIR OS PICOS EXTREMOS E QUANTIFICAR A ELEVAÇÃO EM RELAÇÃO À MÉDIA
# ==============================================================================

estudo_picos_extremos <- function(fit_aparch, serie_xts, nome_ativo, prob_limiar = 0.975, jan_dias = 5) {
  
  # 1. Obtenção da volatilidade e datas
  vol <- as.numeric(sigma(fit_aparch))
  datas <- index(serie_xts)
  
  # 2. Média incondicional e Limiar P97.5
  media_vol <- mean(vol, na.rm = TRUE)
  limiar_975 <- quantile(vol, probs = prob_limiar, na.rm = TRUE)
  
  # 3. Índices dos pontos na cauda de 2.5%
  idx_extremos <- which(vol >= limiar_975)
  
  # 4. Agrupamento por Janela Temporal (Isolamento de clusters de eventos)
  grupos <- cumsum(c(1, diff(idx_extremos) > jan_dias))
  
  # Extração do índice do pico máximo em cada grupo
  picos_idx <- tapply(idx_extremos, grupos, function(indices) {
    indices[which.max(vol[indices])]
  })
  
  datas_picos <- datas[picos_idx]
  vol_picos <- vol[picos_idx]
  
  # 5. Cálculo dos desvios em relação à média
  aumento_abs <- vol_picos - media_vol
  contrib_perc <- ((vol_picos - media_vol) / media_vol) * 100
  
  # Tabela de resultados ordenada pelo impacto máximo
  df_picos <- data.frame(
    Evento = paste0("Evento_", seq_along(picos_idx)),
    Data_Pico = datas_picos,
    Vol_Media = round(media_vol, 4),
    Vol_Pico = round(vol_picos, 4),
    Aumento_Absoluto = round(aumento_abs, 4),
    Contrib_Percentual = round(contrib_perc, 2)
  )
  
  df_picos <- df_picos[order(-df_picos$Vol_Pico), ]
  rownames(df_picos) <- NULL
  
  return(list(
    Tabela_Picos = df_picos,
    Datas_Picos = datas_picos,
    Vol_Picos = vol_picos,
    Media_Vol = media_vol,
    Limiar = limiar_975
  ))
}

# ==============================================================================
# FUNÇÃO PARA PLOTAR A TRAJETÓRIA E OS ELEVADORES DE VOLATILIDADE (BASE R)
# ==============================================================================

plotar_estudo_picos_base <- function(fit_aparch, serie_xts, obj_estudo, nome_ativo) {
  
  vol <- as.numeric(sigma(fit_aparch))
  datas <- index(serie_xts)
  
  # Plot da volatilidade condicional
  plot(datas, vol, type = "l", col = "gray60", lwd = 1,
       main = paste("Análise de Picos e Contribuição em Relação à Média -", nome_ativo),
       xlab = "Data", ylab = expression(sigma[t]),
       ylim = c(min(vol), max(vol) * 1.1))
  
  # Linha da Média Incondicional (Vermelho)
  abline(h = obj_estudo$Media_Vol, col = "red", lty = 1, lwd = 2)
  
  # Linha do Limiar P97.5 (Laranja tracejada)
  abline(h = obj_estudo$Limiar, col = "darkorange", lty = 2, lwd = 2)
  
  # Segmentos que conectam a Média até o Topo de cada Pico Extremo
  segments(x0 = obj_estudo$Datas_Picos, y0 = obj_estudo$Media_Vol,
           x1 = obj_estudo$Datas_Picos, y1 = obj_estudo$Vol_Picos,
           col = "red", lwd = 1.8)
  
  # Marcadores de topo dos picos
  points(obj_estudo$Datas_Picos, obj_estudo$Vol_Picos, col = "red", pch = 19, cex = 0.8)
  
  legend("topleft", 
         legend = c("Volatilidade", "Média Incondicional", "Limiar 2.5% (P97.5)", "Elevação (Média -> Pico)"),
         col = c("gray60", "red", "darkorange", "red"),
         lty = c(1, 1, 2, 1),
         pch = c(NA, NA, NA, 19),
         bg = "white", cex = 0.8)
}

# ==============================================================================
# EXECUÇÃO DO ESTUDO PARA AS 3 SÉRIES
# ==============================================================================

estudo_petr <- estudo_picos_extremos(aparch_petr, ret_petr, "PETR4")
estudo_vale <- estudo_picos_extremos(aparch_vale, ret_vale, "VALE3")
estudo_itub <- estudo_picos_extremos(aparch_itub, ret_itub, "ITUB4")

# Exibindo os 10 picos mais graves de cada empresa e sua contribuição % em relação à média:

cat("\n--- Top Picos e Contribuição sobre a Média - PETR4 ---\n")
print(head(estudo_petr$Tabela_Picos, 10))

cat("\n--- Top Picos e Contribuição sobre a Média - VALE3 ---\n")
print(head(estudo_vale$Tabela_Picos, 10))

cat("\n--- Top Picos e Contribuição sobre a Média - ITUB4 ---\n")
print(head(estudo_itub$Tabela_Picos, 10))



# ==============================================================================
# 1. DEFINIÇÃO DAS DATAS SELECIONADAS POR ATIVO
# ==============================================================================

datas_sel_petr <- as.Date(c("2020-03-17", "2008-10-29", "2018-06-04", "2014-10-28", "2016-03-18"))
datas_sel_vale <- as.Date(c("2020-03-17", "2008-10-29", "2016-03-09", "2015-05-11"))
datas_sel_itub <- as.Date(c("2008-10-27", "2020-04-02", "2021-10-05"))

# ==============================================================================
# 2. FUNÇÃO DE PLOTAGEM COM LEGENDA À DIREITA E VETORES DE IMPULSO SELECIONADOS
# ==============================================================================

plotar_picos_selecionados <- function(fit_aparch, serie_xts, datas_selecionadas, nome_ativo) {
  
  vol <- as.numeric(sigma(fit_aparch))
  datas <- index(serie_xts)
  media_vol <- mean(vol, na.rm = TRUE)
  
  # Mapear os índices e valores de volatilidade das datas selecionadas
  idx_sel <- match(datas_selecionadas, datas)
  vol_sel <- vol[idx_sel]
  
  # Limiar de 2.5% (P97.5) global da série para referência
  limiar_975 <- quantile(vol, probs = 0.975, na.rm = TRUE)
  
  # Plot da série de volatilidade (expandindo ylim em 20% para a legenda não cobrir picos)
  plot(datas, vol, type = "l", col = "gray60", lwd = 1,
       main = paste("Volatilidade APARCH e Impulsos Selecionados -", nome_ativo),
       xlab = "Data", ylab = expression(sigma[t]),
       ylim = c(min(vol), max(vol) * 1.20))
  
  # Linhas de Média e Limiar
  abline(h = media_vol, col = "red", lty = 1, lwd = 1.8)
  abline(h = limiar_975, col = "darkorange", lty = 2, lwd = 1.5)
  
  # Vetores de elevação (Média -> Pico) APENAS para os eventos selecionados
  segments(x0 = datas_selecionadas, y0 = media_vol,
           x1 = datas_selecionadas, y1 = vol_sel,
           col = "red", lwd = 2.2)
  
  # Pontos nos topos selecionados
  points(datas_selecionadas, vol_sel, col = "red", pch = 19, cex = 1.1)
  
  # Legenda posicionada no canto superior direito sem obstrução
  legend("topright", 
         legend = c("Volatilidade", "Média Incondicional", "Limiar 2.5% (P97.5)", "Impulso Selecionado"),
         col = c("gray60", "red", "darkorange", "red"),
         lty = c(1, 1, 2, 1),
         pch = c(NA, NA, NA, 19),
         bg = "white", cex = 0.75, box.col = "gray80")
  
  # Retorna tabela simplificada dos eventos plotados
  data.frame(
    Data = datas_selecionadas,
    Vol_Media = round(media_vol, 4),
    Vol_Pico = round(vol_sel, 4),
    Aumento_Absoluto = round(vol_sel - media_vol, 4),
    Contrib_Percentual = round(((vol_sel - media_vol) / media_vol) * 100, 2)
  )
}

# ==============================================================================
# GERAÇÃO DOS 3 GRÁFICOS
# ==============================================================================

par(mfrow = c(3, 1), mar = c(3, 4, 3, 2))

tab_petr_sel <- plotar_picos_selecionados(aparch_petr, ret_petr, datas_sel_petr, "PETR4")
tab_vale_sel <- plotar_picos_selecionados(aparch_vale, ret_vale, datas_sel_vale, "VALE3")
tab_itub_sel <- plotar_picos_selecionados(aparch_itub, ret_itub, datas_sel_itub, "ITUB4")

par(mfrow = c(1, 1))




# Índices posicionais dos picos selecionados
indices_petr <- c(3273, 452, 2832, 1936, 2276)
indices_vale <- c(3273, 452, 2269, 2065)
indices_itub <- c(450, 3285, 3658)



# ---------------------------------------------------------
# 1. Resíduos do ARIMA
# ---------------------------------------------------------

res_petr <- residuals(arima_petr)


# ---------------------------------------------------------
# 2. Pontos de intervenção
# ---------------------------------------------------------

indices_petr <- c(452, 1936, 2276, 2832, 3273)


# ---------------------------------------------------------
# 3. Definir a janela de cada intervenção
# ---------------------------------------------------------

antes_petr <- c(40, 30, 50, 30, 20)

depois_petr <- c(0, 0, 0, 0, 0)


# ---------------------------------------------------------
# 4. Matriz de intervenções
# ---------------------------------------------------------

n <- length(res_petr)

intervencoes_petr <- matrix(
  0,
  nrow = n,
  ncol = length(indices_petr)
)

colnames(intervencoes_petr) <- paste0(
  "I", seq_along(indices_petr)
)


# ---------------------------------------------------------
# 5. Criar as janelas individualmente
# ---------------------------------------------------------

for (j in seq_along(indices_petr)) {
  
  inicio <- max(
    1,
    indices_petr[j] - antes_petr[j]
  )
  
  fim <- min(
    n,
    indices_petr[j] + depois_petr[j]
  )
  
  intervencoes_petr[inicio:fim, j] <- 1
}


# ---------------------------------------------------------
# 6. Visualizar as janelas utilizadas
# ---------------------------------------------------------

data.frame(
  intervencao = colnames(intervencoes_petr),
  ponto = indices_petr,
  antes = antes_petr,
  depois = depois_petr,
  inicio = pmax(1, indices_petr - antes_petr),
  fim = pmin(n, indices_petr + depois_petr)
)


spec_petr <- ugarchspec(
  variance.model = list(
    model = "apARCH",
    garchOrder = c(1, 1),
    external.regressors = intervencoes_petr
  ),
  mean.model = list(
    armaOrder = c(0, 0),
    include.mean = TRUE
  ),
  distribution.model = "sstd"
)

aparch_petr <- ugarchfit(
  spec = spec_petr,
  data = res_petr,
  solver = "hybrid"
)

show(aparch_petr)






# ---------------------------------------------------------
# Volatilidade condicional + períodos das intervenções
# ---------------------------------------------------------

# Volatilidade condicional estimada pelo APARCH
vol_petr <- sigma(aparch_petr)

# Datas correspondentes
datas_petr <- index(ret_petr)

# Caso ret_petr não tenha índice de datas:
# datas_petr <- dados$DATE

# Limites das intervenções
inicio_interv <- pmax(
  1,
  indices_petr - antes_petr
)

fim_interv <- pmin(
  length(vol_petr),
  indices_petr + depois_petr
)


# ---------------------------------------------------------
# Gráfico
# ---------------------------------------------------------

plot(
  datas_petr,
  vol_petr,
  type = "l",
  lwd = 1.5,
  xlab = "Data",
  ylab = "Volatilidade condicional",
  main = "Volatilidade condicional da PETR com intervenções"
)


# ---------------------------------------------------------
# Marcar as janelas de intervenção
# ---------------------------------------------------------

for (j in seq_along(indices_petr)) {
  
  # Faixa da intervenção
  rect(
    xleft = datas_petr[inicio_interv[j]],
    xright = datas_petr[fim_interv[j]],
    ybottom = par("usr")[3],
    ytop = par("usr")[4],
    col = adjustcolor("gray", alpha.f = 0.25),
    border = NA
  )
  
  # Linha no ponto central da intervenção
  abline(
    v = datas_petr[indices_petr[j]],
    lty = 2,
    lwd = 1.5
  )
  
  # Identificação da intervenção
  text(
    x = datas_petr[indices_petr[j]],
    y = par("usr")[4],
    labels = paste0("I", j),
    pos = 1,
    cex = 0.8
  )
}


# ---------------------------------------------------------
# Redesenhar a volatilidade por cima das faixas
# ---------------------------------------------------------

lines(
  datas_petr,
  vol_petr,
  lwd = 1.5
)


################################
##############################
#################################
###################################

# ---------------------------------------------------------
# AJUSTE VALE COM INTERVENÇÃO
# ---------------------------------------------------------

# Resíduos do ARIMA
res_vale <- residuals(arima_vale)


# ---------------------------------------------------------
# 1. Pontos de intervenção
# ---------------------------------------------------------

indices_vale <- c(452, 2269, 2065, 3273)


# ---------------------------------------------------------
# 2. Definir a janela de cada intervenção
# ---------------------------------------------------------

antes_vale <- c(50, 50, 40, 30)

depois_vale <- c(0, 0, 0, 0)


# ---------------------------------------------------------
# 3. Matriz de intervenções
# ---------------------------------------------------------

n <- length(res_vale)

intervencoes_vale <- matrix(
  0,
  nrow = n,
  ncol = length(indices_vale)
)

colnames(intervencoes_vale) <- paste0(
  "I", seq_along(indices_vale)
)


# ---------------------------------------------------------
# 4. Criar as janelas
# ---------------------------------------------------------

for (j in seq_along(indices_vale)) {
  
  inicio <- max(
    1,
    indices_vale[j] - antes_vale[j]
  )
  
  fim <- min(
    n,
    indices_vale[j] + depois_vale[j]
  )
  
  intervencoes_vale[inicio:fim, j] <- 1
}


# ---------------------------------------------------------
# 5. APARCH com intervenções
# ---------------------------------------------------------

spec_vale <- ugarchspec(
  variance.model = list(
    model = "apARCH",
    garchOrder = c(1, 1),
    external.regressors = intervencoes_vale
  ),
  mean.model = list(
    armaOrder = c(0, 0),
    include.mean = TRUE
  ),
  distribution.model = "sstd"
)


# ---------------------------------------------------------
# 6. Estimação
# ---------------------------------------------------------

aparch_vale <- ugarchfit(
  spec = spec_vale,
  data = res_vale,
  solver = "hybrid"
)

show(aparch_vale)



# ---------------------------------------------------------
# Volatilidade condicional + intervenções
# ---------------------------------------------------------

vol_vale <- sigma(aparch_vale)

datas_vale <- index(ret_vale)

inicio_interv <- pmax(
  1,
  indices_vale - antes_vale
)

fim_interv <- pmin(
  length(vol_vale),
  indices_vale + depois_vale
)


plot(
  datas_vale,
  vol_vale,
  type = "l",
  lwd = 1.5,
  xlab = "Data",
  ylab = "Volatilidade condicional",
  main = "Volatilidade condicional da VALE com intervenções"
)


for (j in seq_along(indices_vale)) {
  
  rect(
    xleft = datas_vale[inicio_interv[j]],
    xright = datas_vale[fim_interv[j]],
    ybottom = par("usr")[3],
    ytop = par("usr")[4],
    col = adjustcolor("gray", alpha.f = 0.25),
    border = NA
  )
  
  abline(
    v = datas_vale[indices_vale[j]],
    lty = 2,
    lwd = 1.5
  )
  
  text(
    x = datas_vale[indices_vale[j]],
    y = par("usr")[4],
    labels = paste0("I", j),
    pos = 1,
    cex = 0.8
  )
}

lines(
  datas_vale,
  vol_vale,
  lwd = 1.5
)











# ---------------------------------------------------------
# AJUSTE ITUB COM INTERVENÇÃO
# ---------------------------------------------------------

# Resíduos do ARIMA
res_itub <- residuals(arima_itub)


# ---------------------------------------------------------
# 1. Pontos de intervenção
# ---------------------------------------------------------

indices_itub <- c(450, 3285, 3658)


# ---------------------------------------------------------
# 2. Definir a janela de cada intervenção
# ---------------------------------------------------------

antes_itub <- c(25, 25, 25)

depois_itub <- c(0, 10, 10)


# ---------------------------------------------------------
# 3. Matriz de intervenções
# ---------------------------------------------------------

n <- length(res_itub)

intervencoes_itub <- matrix(
  0,
  nrow = n,
  ncol = length(indices_itub)
)

colnames(intervencoes_itub) <- paste0(
  "I", seq_along(indices_itub)
)


# ---------------------------------------------------------
# 4. Criar as janelas
# ---------------------------------------------------------

for (j in seq_along(indices_itub)) {
  
  inicio <- max(
    1,
    indices_itub[j] - antes_itub[j]
  )
  
  fim <- min(
    n,
    indices_itub[j] + depois_itub[j]
  )
  
  intervencoes_itub[inicio:fim, j] <- 1
}


# ---------------------------------------------------------
# 5. APARCH com intervenções
# ---------------------------------------------------------

spec_itub <- ugarchspec(
  variance.model = list(
    model = "apARCH",
    garchOrder = c(1, 1),
    external.regressors = intervencoes_itub
  ),
  mean.model = list(
    armaOrder = c(0, 0),
    include.mean = TRUE
  ),
  distribution.model = "sstd"
)


# ---------------------------------------------------------
# 6. Estimação
# ---------------------------------------------------------

aparch_itub <- ugarchfit(
  spec = spec_itub,
  data = res_itub,
  solver = "hybrid"
)

show(aparch_itub)




# ---------------------------------------------------------
# Volatilidade condicional + intervenções
# ---------------------------------------------------------

vol_itub <- sigma(aparch_itub)

datas_itub <- index(ret_itub)

inicio_interv <- pmax(
  1,
  indices_itub - antes_itub
)

fim_interv <- pmin(
  length(vol_itub),
  indices_itub + depois_itub
)


plot(
  datas_itub,
  vol_itub,
  type = "l",
  lwd = 1.5,
  xlab = "Data",
  ylab = "Volatilidade condicional",
  main = "Volatilidade condicional da ITUB com intervenções"
)


for (j in seq_along(indices_itub)) {
  
  rect(
    xleft = datas_itub[inicio_interv[j]],
    xright = datas_itub[fim_interv[j]],
    ybottom = par("usr")[3],
    ytop = par("usr")[4],
    col = adjustcolor("gray", alpha.f = 0.25),
    border = NA
  )
  
  abline(
    v = datas_itub[indices_itub[j]],
    lty = 2,
    lwd = 1.5
  )
  
  text(
    x = datas_itub[indices_itub[j]],
    y = par("usr")[4],
    labels = paste0("I", j),
    pos = 1,
    cex = 0.8
  )
}

lines(
  datas_itub,
  vol_itub,
  lwd = 1.5
)




###############
# Residuos 
###############

library(rugarch)
library(FinTS)    # Para teste ARCH-LM alternativo
library(tseries)  # Para testes de normalidade/estacionariedade

# Consideração: 'fit_aparch' é o objeto retornado pela função ugarchfit()

# ==============================================================================
# CÓDIGO R: TESTES DE DIAGNÓSTICO COM 36 LAGS (PETR4, VALE3, ITUB4)
# ==============================================================================
library(rugarch)
library(FinTS)

# Função para extrair os 3 testes específicos fixados em 36 defasagens
testes_diagnostico_36 <- function(fit_obj, nome_ativo) {
  # Extração dos resíduos padronizados (tilde_epsilon)
  res_std <- residuals(fit_obj, standardize = TRUE)
  
  # 1. Ljung-Box nos Resíduos Padronizados (Média)
  lb_res <- Box.test(res_std, lag = 36, type = "Ljung-Box")
  
  # 2. Ljung-Box nos Resíduos Quadrados (Variância)
  lb_sq <- Box.test(res_std^2, lag = 36, type = "Ljung-Box")
  
  # 3. Teste ARCH-LM de Engle
  arch_lm <- FinTS::ArchTest(res_std, lags = 36)
  
  # Tabela consolidada para o ativo
  data.frame(
    Ativo = nome_ativo,
    Teste = c("Ljung-Box (Resíduos)", "Ljung-Box (Resíduos²)", "ARCH-LM"),
    Lag = 36,
    Estatistica = c(lb_res$statistic, lb_sq$statistic, arch_lm$statistic),
    p_valor = c(lb_res$p.value, lb_sq$p.value, arch_lm$p.value)
  )
}

# Aplicação para cada modelo ajustado (substitua os nomes dos seus objetos fit):
res_petr <- testes_diagnostico_36(aparch_petr, "PETR4")
res_vale <- testes_diagnostico_36(aparch_vale, "VALE3")
res_itub <- testes_diagnostico_36(aparch_itub, "ITUB4")

# Consolidação do quadro de resultados
tabela_diagnosticos <- rbind(res_petr, res_vale, res_itub)
print(tabela_diagnosticos)
