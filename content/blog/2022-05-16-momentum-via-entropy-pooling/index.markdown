---
title: Momentum Entropy-Pooling
author: Bernardo Reckziegel
date: '2022-05-18'
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

Hoje mostro como entropy-pooling pode ser útil para construção de estratégias de _factor-investing_ e _smart-beta_.

Foco minha atenção no fator de [momentum](https://www.investopedia.com/terms/m/momentum.asp) por dois motivos: 

1) A performance desse fator no Brasil e no [exterior](https://www.aqr.com/Insights/Research/Journal-Article/Value-and-Momentum-Everywhere) é estrelar;
2) É mais fácil de construir do que os demais fatores.  

O segundo motivo é especialmente importante para um texto curto como esse. 

Trabalho com a base de dados `br_stock_indices.xlsx` que você pode baixar no endereço https://github.com/Reckziegel/site/tree/master/data.


```r
library(tidyverse) # Dispensa introdução
library(lubridate) # Manipulação de datas
library(readxl)    # Leitura de arquivos xmlx
library(rsample)   # Rolling-Windonws no mundo do tidyverse
library(quadprog)  # Otimização Quadrátiva
library(ffp)       # Probabilidades Flexíveis

indices <- read_excel(path = "/Meu Drive/site/data/br_stock_indices.xlsx", 
                      col_types = c("date", rep("numeric", 6))) |> 
  mutate(date = as_date(date))  
returns <- indices |> 
  # invariance
  modify_if(.p = is.numeric, .f = ~ log(.x / lag(.x))) |> 
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

Esse índices são bastante conhecidos e com um histórico relativamente longo para o Brasil. Reforço que não há _chery-picking_, no sentido de favorecer os resultados da estratégia. Escolhi esses índices porque queria trabalhar com ativos que tivessem pelo menos 15 anos de história. No mais, acho que os fatos falam por si ([Fact, Fiction and Momentum Investing](https://www.aqr.com/Insights/Research/Journal-Article/Fact-Fiction-and-Momentum-Investing)). 

O portfolio de momentum geralmente é construído como um portfolio _dollar-neutral_, `\(100\%\)` investido, no qual a performance passada determina quais ativos entram na ponta comprada e/ou vendida. 

Aqui, aplico uma mudança no modo de construção do fator. Ao invés de comprar/vender os ativos que estão acima/abaixo de um determinado percentil, "rankeio" as ações de de melhor para pior performance e utilizo entropy-pooling para construir um vetor de probabilidades que acomoda o "rankiamento" e ao mesmo tempo distorce ao mínimo o vetor de probabilidades _equal-weigthed_ original. Com base no vetor de probabilidades posterior, estimo os momentos _condicionais_ - `\(\mu\)` e `\(\sigma\)` - que resultam da relação de ordenação imposta por mim. 

Ou seja, a cada ponto do tempo soluciono numericamente o sistema:

$$ \sum_{i=1}^I x_i(ln(x_i) - ln(p_i)) $$
`\(s.t.\)`

$$ \sum_{i=1}^I \hat{p_i} (Opinion_{i, 1} - Opinion_{i, 2}) \leq 0 $$ 
$$ \sum_{i=1}^I \hat{p_i} (Opinion_{i, 2} - Opinion_{i, 3}) \leq 0 $$ 

$$ ... $$ 

$$ \sum_{i=1}^I \hat{p_i} (Opinion_{i, j-1} - Opinion_{i, v}) \leq 0 $$ 
No qual a solução, `\(p^*\)`, 




```r
optimin <- returns |> 
  rolling_origin(initial = 845, assess = 1, cumulative = TRUE)
optimin <- optimin |> 
  mutate(optimin, date = get_assessment_date(optimin))

optimin <- optimin |>
  mutate(.analysis   = map(.x = splits, .f = analysis),
         .assessment = map(.x = splits, .f = assessment))
optimin <- optimin |>
  mutate(.moments = map(.x = .analysis, .f = ~ momentum_moments(.x = .x, .period = 52)))
optimin <- optimin |>
  mutate(.weights = map(.x = .moments, .f = ~ optimal_portfolio(sigma = .x$sigma, mu = .x$mu, .wmin = 0, .wmax = 1)))
optimin <- optimin |>
  mutate(ret = map2_dbl(.x = .weights, .y = .assessment, .f = ~ as.matrix(.y[ , -1]) %*% .x))
optimin
```

```
## # Rolling origin forecast resampling 
## # A tibble: 10 x 8
##    splits          id      date       .analysis .assessment      .moments    
##    <list>          <chr>   <date>     <list>    <list>           <list>      
##  1 <split [845/1]> Slice01 2022-03-18 <tibble>  <tibble [1 x 7]> <named list>
##  2 <split [846/1]> Slice02 2022-03-25 <tibble>  <tibble [1 x 7]> <named list>
##  3 <split [847/1]> Slice03 2022-04-01 <tibble>  <tibble [1 x 7]> <named list>
##  4 <split [848/1]> Slice04 2022-04-08 <tibble>  <tibble [1 x 7]> <named list>
##  5 <split [849/1]> Slice05 2022-04-15 <tibble>  <tibble [1 x 7]> <named list>
##  6 <split [850/1]> Slice06 2022-04-22 <tibble>  <tibble [1 x 7]> <named list>
##  7 <split [851/1]> Slice07 2022-04-29 <tibble>  <tibble [1 x 7]> <named list>
##  8 <split [852/1]> Slice08 2022-05-06 <tibble>  <tibble [1 x 7]> <named list>
##  9 <split [853/1]> Slice09 2022-05-13 <tibble>  <tibble [1 x 7]> <named list>
## 10 <split [854/1]> Slice10 2022-05-20 <tibble>  <tibble [1 x 7]> <named list>
## # ... with 2 more variables: .weights <list>, ret <dbl>
```



```r
benchmark <- select(returns, date, IBOV)

optimin |>
  left_join(benchmark, by = "date") |>
  select(date, ret, IBOV) |>
  mutate(Momentum = ret - (1.01 ^ (1 / 52) - 1)) |>
  select(date, Momentum, IBOV) |>
  mutate_if(is.numeric, ~ cumprod(1 + .x)) |>
  pivot_longer(cols = -date) |>
  ggplot(aes(x = date, y = value, color = name)) +
  geom_line() + 
  scale_y_log10() + 
  scale_color_viridis_d(end = 0.75, option = "C") + 
  labs(title = "Momentum Entropy-Pooling", 
       subtitle = "Portfolio long-only com 'tilt' em momentum (52 semanas)", 
       x = NULL, y = NULL, color = NULL) + 
  theme(legend.position = "bottom")
```

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-4-1.png" width="672" />

Antes de plotar a estratégia contra o ibovespa adiciono uma taxa anual de `\(4\%\)`, que considero um valor bastante elevado e capaz de emular não somente os custos fixos, como potencial bonus e custos transacionais.


