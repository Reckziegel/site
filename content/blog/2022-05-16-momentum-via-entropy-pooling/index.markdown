---
title: Momentum Entropy-Pooling
author: Bernardo Reckziegel
date: '2022-05-17'
slug: []
categories:
  - R
  - views
tags:
  - entropy-pooling
  - ffp
  - momentum
  - factor-investing
meta_img: images/image.png
description: Description for the page
---

Nos últimos posts mostrei algumas funcionalidades básicas do pacote `ffp`. Em particular, como portfolio e risk-managers podem utilizar essa biblioteca para adicionar opiniões nos mercados.

Hoje avanço e mostro como entropy-pooling pode ser útil também para construção de estratégias de _factor-investing_ e _smart-beta_.

Geralmente, os "fatores" são construídos com base em portfolios dollar-neutro, no qual assume-se que as ações que estão mais próximas de possuirem uma característica "x" devem ser compradas e aquelas que mais se afastam dessa característica devem ser vendidas. No caso da estratégia de momentum, geralmente se compra as ações aque mais subiram nos últimos 12 meses e se vende aquelas que mais caíram. 

Não vou entrar no mérito se a estratégia é "boa" ou não. Meu objetivo é apenas mostrar como entropy-pooling pode ser utilizado para contrução de portfolios eficientes com um _tilt_ ao fator de momentum. Dito de outra forma: estou "vendendo" a técnica (entropy-pooling), não a estratégia (momentum).

Trabalho com a base de dados `br_stock_indices.xlsx` que você pode baixar no endereço https://github.com/Reckziegel/site/tree/master/data.


```r
library(tidyverse)
library(lubridate)
library(readxl)
library(rsample)
library(ffp)

indices <- read_excel(path = "/Meu Drive/site/data/br_stock_indices.xlsx", 
                      col_types = c("date", rep("numeric", 6))) |> 
  mutate(date = as_date(date))  
returns <- indices |> 
  modify_if(.p = is.numeric, .f = ~ log(.x / dplyr::lag(.x))) |> # invariance
  na.omit()
returns
```

```
## # A tibble: 855 x 7
##    date            IDIV     IBOV    IEEX    IFNC     IMAT     INDX
##    <date>         <dbl>    <dbl>   <dbl>   <dbl>    <dbl>    <dbl>
##  1 2006-01-06  0.0189    0.0586   0.0424  0.102   0.0372   0.0320 
##  2 2006-01-13  0.0222    0.0118   0.0255  0.0164  0.00665 -0.00232
##  3 2006-01-20  0.0169    0.0220   0.0633  0.0438  0.0130   0.00799
##  4 2006-01-27  0.0367    0.0303   0.0123  0.0322  0.0645   0.0460 
##  5 2006-02-03 -0.00171  -0.0149  -0.0349  0.0136 -0.00152 -0.00231
##  6 2006-02-10  0.0233   -0.00771  0.0465  0.0192 -0.0170  -0.00308
##  7 2006-02-17  0.0493    0.0384   0.0421  0.0317  0.0288   0.0349 
##  8 2006-02-24 -0.000517  0.00491  0.0391 -0.0187  0.0247   0.00872
##  9 2006-03-03  0.0262    0.0162   0.0355  0.0216  0.0242   0.0252 
## 10 2006-03-10 -0.0501   -0.0617  -0.0469 -0.0561 -0.0507  -0.0265 
## # ... with 845 more rows
```

Selecionei alguns índices bastante conhecidos e disponíveis por um período de tempo relativamente longo para o Brasil. Não há _chery-picking_ na escolha do dataset no sentido de favorecer os retornos da estratégia, que apresenta uma performance consistente ao longo dos últimos 200 anos (https://www.aqr.com/Insights/Research/Journal-Article/Fact-Fiction-and-Momentum-Investing).




```r
optimin <- returns |> 
  rolling_origin(initial = 52 * 16, assess = 1, cumulative = TRUE)
optimin <- optimin |> 
  mutate(optimin, date = get_assessment_date(optimin))

optimin <- optimin |>
  dplyr::mutate(.analysis   = map(.x = splits, .f = analysis),
                .assessment = map(.x = splits, .f = assessment))
optimin <- optimin |>
  dplyr::mutate(.moments = map(.x = .analysis, .f = ~ momentum_moments(.x = .x, .period = 52)))
optimin <- optimin |>
  dplyr::mutate(.weights = map(.x = .moments, .f = ~ optimal_portfolio(sigma = .x$sigma, mu = .x$mu, .wmin = 0, .wmax = 1)))
optimin <- optimin |>
  dplyr::mutate(ret = map2_dbl(.x = .weights, .y = .assessment, .f = ~ as.matrix(.y[ , -1]) %*% .x))
optimin
```

```
## # Rolling origin forecast resampling 
## # A tibble: 23 x 8
##    splits          id      date       .analysis .assessment      .moments    
##    <list>          <chr>   <date>     <list>    <list>           <list>      
##  1 <split [832/1]> Slice01 2021-12-17 <tibble>  <tibble [1 x 7]> <named list>
##  2 <split [833/1]> Slice02 2021-12-24 <tibble>  <tibble [1 x 7]> <named list>
##  3 <split [834/1]> Slice03 2021-12-31 <tibble>  <tibble [1 x 7]> <named list>
##  4 <split [835/1]> Slice04 2022-01-07 <tibble>  <tibble [1 x 7]> <named list>
##  5 <split [836/1]> Slice05 2022-01-14 <tibble>  <tibble [1 x 7]> <named list>
##  6 <split [837/1]> Slice06 2022-01-21 <tibble>  <tibble [1 x 7]> <named list>
##  7 <split [838/1]> Slice07 2022-01-28 <tibble>  <tibble [1 x 7]> <named list>
##  8 <split [839/1]> Slice08 2022-02-04 <tibble>  <tibble [1 x 7]> <named list>
##  9 <split [840/1]> Slice09 2022-02-11 <tibble>  <tibble [1 x 7]> <named list>
## 10 <split [841/1]> Slice10 2022-02-18 <tibble>  <tibble [1 x 7]> <named list>
## # ... with 13 more rows, and 2 more variables: .weights <list>, ret <dbl>
```



```r
benchmark <- select(returns, date, IBOV)

optimin |>
  left_join(benchmark, by = "date") |>
  select(date, ret, IBOV) |>
  na.omit() |>
  mutate(Momentum = ret - (1.04 ^ (1 / 52) - 1)) |>
  select(date, Momentum, IBOV) |>
  mutate_if(is.numeric, ~ cumprod(1 + .x)) |>
  pivot_longer(cols = -date) |>
  ggplot(aes(x = date, y = value, color = name)) +
  geom_line() + 
  scale_y_log10() + 
  scale_color_viridis_d(end = 0.75, option = "C") + 
  labs(title = "Fator Momemtum contruído via Entropy-Pooling", 
       subtitle = "Portfolios long-only com 'tilt' na performance de 52 semanas", 
       x = NULL, y = NULL, color = NULL) + 
  theme(legend.position = "bottom")
```

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-4-1.png" width="672" />

Antes de plotar a estratégia contra o ibovespa adiciono uma taxa anual de `\(4\%\)`, que considero um valor bastante elevado e capaz de emular não somente os custos fixos, como potencial bonus e custos transacionais.


