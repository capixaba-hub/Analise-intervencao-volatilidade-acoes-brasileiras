# Análise de retornos e volatilidade de ações brasileiras com modelos ARIMA e APARCH

## 1. Descrição do projeto

Este projeto analisa o comportamento dos retornos e da volatilidade das ações da Petrobras (PETR4), Vale (VALE3) e Itaú Unibanco (ITUB4), utilizando séries históricas de preços obtidas na plataforma Yahoo Finance.

As análises abrangem estatísticas descritivas, testes de propriedades das séries temporais, ajuste de modelos ARIMA para os retornos e modelos APARCH para a volatilidade condicional. Também são investigados episódios de volatilidade extrema e os efeitos de intervenções em períodos selecionados.

## 2. Objetivos

- Caracterizar estatisticamente os retornos das ações analisadas.
- Investigar a estacionariedade, a tendência e a autocorrelação das séries.
- Ajustar modelos ARIMA para representar a dinâmica dos retornos.
- Modelar a volatilidade condicional por meio de modelos APARCH.
- Avaliar a adequação dos modelos com testes de diagnóstico dos resíduos.
- Identificar picos extremos de volatilidade e quantificar sua elevação em relação à volatilidade média.
- Investigar intervenções em períodos selecionados e comparar o comportamento da volatilidade entre os ativos.

## 3. Dados

Os preços históricos são obtidos automaticamente por meio do Yahoo Finance, considerando os seguintes códigos:

| Ativo | Código |
|---|---|
| Petrobras | `PETR4.SA` |
| Vale | `VALE3.SA` |
| Itaú Unibanco | `ITUB4.SA` |

O código utiliza dados de 1º de janeiro de 2007 a 4 de outubro de 2026, conforme os argumentos definidos na função de extração.

Os retornos são calculados como diferenças dos logaritmos dos preços de fechamento:

\[
r_t=\log(P_t)-\log(P_{t-1}),
\]

em que \(P_t\) representa o preço de fechamento no instante \(t\).

## 4. Metodologia

### 4.1. Estatísticas descritivas

São calculadas medidas de posição e dispersão, valores mínimo e máximo, assimetria e curtose. O teste de Jarque–Bera é utilizado para avaliar a hipótese de normalidade dos retornos.

### 4.2. Análise das séries temporais

A função de autocorrelação (FAC), a função de autocorrelação parcial (FACP) e o teste de Ljung–Box são utilizados para investigar a dependência temporal dos retornos.

Para a série da Vale, o código também inclui o teste de Dickey–Fuller aumentado (ADF), uma regressão linear contra o tempo, o teste de Mann–Kendall e uma análise preliminar de sazonalidade semanal.

### 4.3. Modelos ARIMA

São ajustados modelos ARIMA com ordens e restrições de parâmetros definidas individualmente para cada ativo. Os resíduos são examinados para verificar a presença de autocorrelação e de efeitos ARCH.

### 4.4. Modelos APARCH

A volatilidade condicional é modelada por meio de especificações APARCH(1,1), com distribuição *Skewed Student-t* (`sstd`). Essa distribuição permite acomodar assimetria e caudas mais pesadas que as da distribuição normal.

Os modelos são estimados sobre os resíduos dos modelos ARIMA, utilizando o procedimento de otimização híbrido disponível no pacote `rugarch`.

### 4.5. Diagnóstico dos modelos

Após o ajuste dos modelos APARCH, são aplicados testes de Ljung–Box aos resíduos padronizados e aos resíduos padronizados ao quadrado, além do teste ARCH-LM. O objetivo é investigar a autocorrelação residual e a permanência de heterocedasticidade condicional.

### 4.6. Identificação de picos de volatilidade

A volatilidade condicional estimada é utilizada para identificar observações extremas. O código adota o percentil 97,5% como limiar, correspondente aos 2,5% superiores da distribuição empírica da volatilidade estimada.

Para cada pico selecionado, são calculados:

- volatilidade média estimada;
- volatilidade no pico;
- aumento absoluto em relação à média;
- aumento percentual em relação à média.

Também são produzidos gráficos da trajetória da volatilidade, com linhas de referência para a média e o limiar de extremos.

### 4.7. Modelos com intervenções

São construídas variáveis indicadoras para janelas temporais selecionadas individualmente para PETR4, VALE3 e ITUB4. Essas variáveis são incorporadas às equações de variância dos modelos APARCH por meio do argumento `external.regressors`.

Os gráficos correspondentes apresentam a trajetória da volatilidade condicional e as janelas de intervenção utilizadas na modelagem.

## 5. Pacotes necessários

O código utiliza funções dos seguintes pacotes do R:

- `quantmod`: obtenção de dados financeiros e manipulação de séries de preços;
- `xts`: manipulação de séries temporais indexadas por datas;
- `Kendall`: teste de Mann–Kendall;
- `moments`: cálculo de assimetria e curtose;
- `lmtest`: testes e inferência para modelos estatísticos;
- `tseries`: teste ADF e teste de Jarque–Bera;
- `FinTS`: teste ARCH-LM;
- `rugarch`: especificação e estimação de modelos APARCH;
- `ggplot2`: construção de gráficos.

Os pacotes devem estar instalados antes da execução. O código também utiliza funções do R base, como `arima()`, `acf()`, `pacf()`, `Box.test()` e `lm()`.

## 6. Como executar

1. Instale as dependências necessárias.
2. Abra o script no R ou no RStudio.
3. Execute os comandos na ordem apresentada, começando pela obtenção dos dados.
4. Verifique se a conexão com o Yahoo Finance está disponível.
5. Execute os ajustes ARIMA, os testes de diagnóstico e os modelos APARCH.
6. Gere as tabelas e os gráficos de volatilidade.
7. Execute as etapas de identificação dos picos e de modelagem das intervenções.

Os modelos ARIMA possuem ordens e restrições de parâmetros específicas para cada ativo. Portanto, alterações nos dados ou no período amostral podem exigir a revisão dessas especificações.

## 7. Observações para reprodução

- Os preços são obtidos diretamente do Yahoo Finance, podendo sofrer alterações decorrentes de ajustes ou revisões na fonte.
- Os resultados dependem do período amostral, da disponibilidade dos dados e das especificações adotadas.
- As datas de intervenção e os índices posicionais definidos no script devem ser conferidos em relação às séries efetivamente obtidas.
- O código contém etapas exploratórias e ajustes sucessivos dos modelos. Para reproduzir os resultados finais, deve-se identificar e executar as especificações correspondentes à análise de interesse.
- A reprodução integral requer as dependências instaladas e acesso à fonte de dados.

## 8. Reprodutibilidade e uso

O código é disponibilizado para permitir a inspeção dos procedimentos estatísticos e facilitar a reprodução das análises. Para citar ou reutilizar o material, recomenda-se informar a autoria, a versão do código e a data de acesso ao repositório.

## 9. Fonte dos dados

Yahoo Finance. Dados históricos de preços das ações PETR4.SA, VALE3.SA e ITUB4.SA. Disponível em: <https://finance.yahoo.com/>.

---

**Nota:** este README descreve os procedimentos identificados no script fornecido. Ele não implica que todos os modelos ou testes tenham sido executados com sucesso nem que os resultados tenham sido validados independentemente.
