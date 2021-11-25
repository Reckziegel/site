---
title: 'Baixando dados financeiros com "YahooTickers"'
author: Bernardo Reckziegel
date: '2021-11-25'
slug: []
categories:
  - R package
tags:
  - R
  - tidydata
meta_img: images/tickers.png
description: Baixe dados de diferentes bolsas mundiais de maneira rápida e eficiente
---

Hoje o meu primeiro pacote - `YahooTickers`- completa 3 anos. Nada mais justo que o post inaugural desse blog seja dedicado a ele. 

Essa biblioteca foi contruida porque me dei conta que perdia muito tempo repetindo sempre as 
mesmas tarefas. Assim, resolvi sistematizar o acesso ao API do YahooFinance. Esse API não 
é perfeito, mas é de fácil acesso e funciona razoavelmente bem na maioria das vezes. 

Se você não tem acesso a Bloomberg, Economatica ou Refinitiv é possível que esse pacote
interesse a você.


Para baixar a biblioteca no seu RStudio você deve utilizar o seguinte comando no console: 


```r
# install.packages("devtools")
# devtools::install_github("Reckziegel/YahooTickers")
```

> OBS: O download de pacotes que não estão no [CRAN](https://cran.r-project.org/) pode ser realizado como o auxílio  do `devtools`. 

Para demonstrar algumas das principais funcionalidades do `YahooTickers` utilizarei 
como exemplo o índice Dow Jones:[^1].  


```r
# bibliotecas necessárias
library(YahooTickers)
library(forecast)
library(ggplot2)
```

Inicie selecionando os tickers do índice de referência:


```r
ticks <- get_tickers(dow)
ticks
```

```
## # A tibble: 30 x 1
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
## 10 DOW    
## # ... with 20 more rows
```

O próximo passo é baixarmos a ações que compôem o índice DJI:


```r
stocks <- get_stocks(tickers = dplyr::slice_head(ticks, n = 2), periodicity = "monthly")
stocks
```

```
## # A tibble: 288 x 8
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
## # ... with 278 more rows
```

Os dados já vem no formato `tidy` (longo), de modo a potencializar a iteratividade com o `ggplot2` e o todo o ecossistema do `tidyverse`:


```r
stocks |> 
  ggplot(aes(x = date, y = adjusted, color = tickers)) + 
  geom_line() + 
  facet_wrap(~tickers, scales = "free_y") + 
  scale_y_log10()
```

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-5-1.png" width="672" />

Caso haja interesse em levar a adiante a modelagem econométrica, o usuário pode utilizar `get_returns()` e `get_models()` em sequência:


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
             c(1, 0, 0))
models
```

```
## # A tibble: 332 x 17
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
## # ... with 322 more rows, and 7 more variables: ME <dbl>, RMSE <dbl>,
## #   MAE <dbl>, MPE <dbl>, MAPE <dbl>, MASE <dbl>, ACF1 <dbl>
```

Seguindo esse workflow, o acesso das principais métricas _in-sample_ do modelo escolhido fica extremamente fácil e, assim como no gráfico de preços, a iteração com `ggplot2` é imediata:


```r
models |> 
  dplyr::filter(term == "ar1") |> 
  ggplot(aes(x = date, y = estimate, color = tickers)) + 
  geom_smooth(span = 0.10) + 
  labs(title    = "Coeficiente Autoregressivo de Ordem 1", 
       subtitle = "Estimação para uma janela móvel de 60 períodos")
```

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-7-1.png" width="672" />

É possível levar a análise ainda mais adiante analisando o a acurácia referente ao modelo adotado para cada ação individualmente. Essa operação deve ser realizada com `get_forcasts()` e `get_metrics()` em sequência: 


```r
models |> 
  get_forecasts() |> 
  get_metrics(.group = tickers, .truth = adjusted, .forecast = point_forecast)
```

```
## # A tibble: 1 x 5
##       mse   rmse    mae  mape  mase
##     <dbl>  <dbl>  <dbl> <dbl> <dbl>
## 1 0.00400 0.0632 0.0439   Inf 0.161
```



[^1]: Esse índice possui apenas 30 ações e com isso os downlaods são mais rápidos. 
Na prática, poderia ser qualquer índice que apareça aqui: https://github.com/Reckziegel/YahooTickers.
