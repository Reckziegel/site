---
title: 'Baixando dados financeiros com YahooTickers'
author: Bernardo Reckziegel
date: '2021-11-25'
slug: []
categories:
  - R package
tags:
  - R
  - tidydata
meta_img: images/tickers.png
description: Baixe e modele dados de diferentes bolsas mundiais de maneira rápida e eficiente
---

Meu primeiro pacote - `YahooTickers`- recentemente completou 3 anos. Por esse motivo, o post inaugural será sobre ele. 

Quando comecei a usar o R, não demorou muito para que me desse conta que estava sempre repetindo as mesmas tarefas. Assim, decidi sistematizar o acesso ao API do [YahooFinance](https://finance.yahoo.com/), o que resultou na construção do `YahooTickers`. 

O API do Yahoo não é perfeito, mas é simples e funciona razoavelmente bem. Se você programa com frequência e não tem acesso a Bloomberg, Economatica ou Refinitiv é possível que esse pacote interesse a você.

Para baixar o`YahooTickers` no RStudio você deve utilizar o seguinte comando no console: 


```r
# install.packages("devtools")
# devtools::install_github("Reckziegel/YahooTickers")
```

<!-- > OBS: O download de pacotes que não estão no [CRAN](https://cran.r-project.org/) pode ser realizado como o auxílio  do `devtools`.  -->

Demonstrarei suas principais funcionalidades usando algumas ações do índice Dow Jones.  


```r
# bibliotecas necessárias
library(YahooTickers)
library(forecast)
library(ggplot2)
```

Primeiro, selecione um índice (em nosso caso `dow`):


```r
ticks <- get_tickers(dow)
ticks <- ticks |> 
  dplyr::filter(tickers != "DOW") # essa ação possui histórico relativamente curto
                                  # Chama-se DOW Inc., não confundir com 
                                  # o índice Dow Jones.
ticks
```

```
## # A tibble: 29 x 1
##    tickers
##    <chr>  
##  1 MMM    
##  2 AXP    
##  3 AMGN   
##  4 AAPL   
##  5 BA     
##  6 CAT    
##  7 CVX    
##  8 CSCO   
##  9 KO     
## 10 GS     
## # ... with 19 more rows
```

<!-- Perceba que no comando acima não é necessario usar `strings` "com aspas" na função `get_tickers()`, pois toda estrutura é construída sob o conceito de `tidy eval`, o mecanismo de _data masking_ que turbina o [tidyverse](https://www.tidyverse.org/).  -->

O próximo passo é fazer o download das ações que compôem esse índice de referência com `get_stocks()`:


```r
stocks <- ticks |> 
  dplyr::slice(1:12) |> # selecionando as 12 primeiras ações
                        # para tornar a análise mais rápida
  get_stocks(periodicity = "monthly")
stocks
```

```
## # A tibble: 1,728 x 8
##    date       tickers  open  high   low close    volume adjusted
##    <date>     <fct>   <dbl> <dbl> <dbl> <dbl>     <dbl>    <dbl>
##  1 2010-01-01 MMM      83.1  85.2  79.1  80.5  75208100     58.1
##  2 2010-02-01 MMM      80.8  81.8  77.2  80.2  75020400     57.8
##  3 2010-03-01 MMM      80.6  84.5  80.3  83.6  91066100     60.7
##  4 2010-04-01 MMM      83.9  90.2  82.7  88.7  96407000     64.4
##  5 2010-05-01 MMM      89.2  90.5  69.0  79.3 109573600     57.6
##  6 2010-06-01 MMM      78.7  83    72.7  79.0 114407500     57.7
##  7 2010-07-01 MMM      79.1  87.5  77.0  85.5  89556700     62.5
##  8 2010-08-01 MMM      86.8  88.4  78.4  78.6  74721100     57.4
##  9 2010-09-01 MMM      79.5  88    79.3  86.7  64059700     63.8
## 10 2010-10-01 MMM      87.4  91.5  83.8  84.2  82038100     61.9
## # ... with 1,718 more rows
```

Os dados já vem no formato `tidy` (formato-longo), de modo a potencializar a iteratividade com o `ggplot2` e o todo o ecossistema do `tidyverse`. Veja:


```r
stocks |> 
  ggplot(aes(x = date, y = adjusted, color = tickers)) + 
  geom_line() + 
  facet_wrap(~tickers, scales = "free_y") + 
  scale_y_log10() + 
  labs(title    = "Ações do Índice Dow Jones", 
       subtitle = "2010-2021")
```

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-5-1.png" width="672" style="display: block; margin: auto;" />

Caso haja interesse em levar a adiante a modelagem econométrica, o usuário tem a opção de utilizar `get_returns()` e `get_models()` em sequência:


```r
models <- stocks |> 
  get_returns(.group   = tickers, 
              .type    = arithmetic, 
              .omit_na = TRUE, 
              adjusted) |> 
  get_models(.group      = tickers, 
             .col        = adjusted, 
             .initial    = 60, 
             .assess     = 1, 
             .cumulative = FALSE, 
             .fun        = Arima, 
             c(1, 0, 0)) # modelo auto-regressivo
models
```

```
## # A tibble: 1,992 x 17
##    date       tickers data   term  estimate model.desc  sigma logLik   AIC   BIC
##    <date>     <fct>   <list> <fct>    <dbl> <fct>       <dbl>  <dbl> <dbl> <dbl>
##  1 2015-02-01 MMM     <tibb~ ar1    -0.217  ARIMA(1,0~ 0.0489   97.0 -188. -182.
##  2 2015-02-01 MMM     <tibb~ inte~   0.0152 ARIMA(1,0~ 0.0489   97.0 -188. -182.
##  3 2015-03-01 MMM     <tibb~ ar1    -0.218  ARIMA(1,0~ 0.0489   96.9 -188. -182.
##  4 2015-03-01 MMM     <tibb~ inte~   0.0156 ARIMA(1,0~ 0.0489   96.9 -188. -182.
##  5 2015-04-01 MMM     <tibb~ ar1    -0.236  ARIMA(1,0~ 0.0487   97.2 -188. -182.
##  6 2015-04-01 MMM     <tibb~ inte~   0.0147 ARIMA(1,0~ 0.0487   97.2 -188. -182.
##  7 2015-05-01 MMM     <tibb~ ar1    -0.202  ARIMA(1,0~ 0.0495   96.2 -186. -180.
##  8 2015-05-01 MMM     <tibb~ inte~   0.0133 ARIMA(1,0~ 0.0495   96.2 -186. -180.
##  9 2015-06-01 MMM     <tibb~ ar1    -0.209  ARIMA(1,0~ 0.0469   99.5 -193. -187.
## 10 2015-06-01 MMM     <tibb~ inte~   0.0149 ARIMA(1,0~ 0.0469   99.5 -193. -187.
## # ... with 1,982 more rows, and 7 more variables: ME <dbl>, RMSE <dbl>,
## #   MAE <dbl>, MPE <dbl>, MAPE <dbl>, MASE <dbl>, ACF1 <dbl>
```

Seguindo esse workflow, as principais métricas _in-sample_ do modelo escolhido ficam acessíveis imediatamente e, assim como no gráfico anterior, o `ggplot2` cai como uma luva:


```r
models |> 
  dplyr::filter(term == "ar1") |> 
  ggplot(aes(x = date, y = estimate, color = tickers)) + 
  geom_smooth(span = 0.10) + 
  facet_wrap(~tickers, scales = "free_y") + 
  labs(title    = "Coeficiente Autoregressivo de Ordem 1", 
       subtitle = "Estimação para dados mensais em uma janela móvel de 60 períodos")
```

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-7-1.png" width="672" style="display: block; margin: auto;" />

Para analisar o comportamento de outras métricas, o processo é semelhante. Por exemplo, o ponto de ótimo da máxima verossimilhança em cada instante do tempo pode ser visto com o comando:


```r
models |> 
  ggplot(aes(x = date, y = logLik, color = tickers)) + 
  geom_line() + 
  facet_wrap(~tickers, scales = "free_y") + 
  labs(title    = "Evolução a Máxima Verossimilhança", 
       subtitle = "Estimação para dados mensais em uma janela móvel de 60 períodos")
```

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-8-1.png" width="672" />

Em alguns casos, as métricas _in sample_ não são tão relevantes. Nessas ocasiões, é necessário analisar a qualidade do modelo de outras formas.  Se o objetivo da análise for previsão, `Yahootickers` oferece a dobradinha `get_forecast()` e `get_metrics()` para avaliação dos erros _pseudo-fora da amostra_:   


```r
metrics <- models |> 
  get_forecasts() |> 
  get_metrics(.group    = tickers, 
              .truth    = adjusted, 
              .forecast = point_forecast)
metrics
```

```
## # A tibble: 12 x 6
##    tickers     mse   rmse    mae  mape    mase
##    <fct>     <dbl>  <dbl>  <dbl> <dbl>   <dbl>
##  1 MMM     0.00300 0.0548 0.0411  121.  0.0853
##  2 AXP     0.00499 0.0706 0.0467  Inf   0.223 
##  3 AMGN    0.00472 0.0687 0.0530  130.  0.0924
##  4 AAPL    0.00665 0.0816 0.0680  378.  0.198 
##  5 BA      0.0121  0.110  0.0755  Inf   0.142 
##  6 CAT     0.00557 0.0746 0.0607  Inf   0.127 
##  7 CVX     0.00588 0.0767 0.0543  171.  0.240 
##  8 CSCO    0.00465 0.0682 0.0529  Inf   1.97  
##  9 KO      0.00214 0.0463 0.0347  Inf   2.57  
## 10 GS      0.00679 0.0824 0.0634  Inf   0.168 
## 11 HD      0.00363 0.0602 0.0470  Inf  16.3   
## 12 HON     0.00311 0.0557 0.0397  615.  0.355
```

Perceba que, mais uma vez, a utilização do `ggplot2` é imediata:


```r
metrics |> 
  dplyr::mutate(tickers = forcats::fct_reorder(tickers, mse)) |> 
  ggplot(aes(x = tickers, y = mse, fill = tickers)) + 
  geom_col() + 
  labs(title    = "Erro Quadrático Médio", 
       subtitle = "Estimação para dados mensais em uma janela móvel de 60 períodos")
```

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-10-1.png" width="672" />

Enfim, o pacote possui uma estrutura bem delineada que funciona da seguinte forma:


```r
### -- YahooTickers Workflow -- ### 
# get_tickers() |> 
#   get_stocks() |> 
#   get_models() |> 
#   get_forecasts() |> 
#   get_metrics()
```

Obviamente, ele não foi desenvolvido para análises muito complexas, __a principal função é coletar dados e fazer a "coisa acontecer" rápido__, sem `for loops` e de maneira que as pessoas entendam o que está acontecendo (_human readable_).

Atualmente, o pacote suporta até 26 bolsas mundiais. Mas... como nem tudo é perfeito, a detecção de quais ações compôem quais índices é um processo relativamente complicado, que envolve _web scrapping_ de sites públicos, em constante transformação. 

Faço meu melhor para manter aquilo que já foi produzido sempre funcionando e foi assim que essa bibiliteca sobreviveu ao longo dos últimos 3 anos. 

Por fim, se você chegou até aqui, talvez tenha interesse em olhar a página de referência, onde encontrará documentação detalhada das funções de utilizadas nesse post: https://reckziegel.github.io/YahooTickers/. 

Por hoje é isso e _happy-modeling_!
