---
title: Taxa de Juros (Real) Neutra com base no Focus
author: Bernardo Reckziegel
date: '2021-11-29'
slug: []
categories:
  - R
  - Fixed-Income
tags:
  - focus
  - interest-rates
  - rbcb
meta_img: images/image.png
description: Description for the page
---

O post de hoje é simples. O objetivo é _aproximar_ o resultado apresentado pelo BC no box [Proxy da taxa de juros estrutural implícita nas expectativas da pesquisa Focus](https://www.bcb.gov.br/content/ri/relatorioinflacao/201912/ri201912b9p.pdf) (RTI de 2019).

Abaixo os pacotes utilizados:


```r
# required packages
# install.packages(devtools)
# devtools::install_github('wilsonfreitas/rbcb')
library(rbcb)
library(tidyverse)
```

> Como o `rbcb` não está no [CRAN](https://cran.r-project.org/) você deve usar o pacote `devtools` realizar o download diretamente do github.

Começamos coletando as expectativas para o IPCA 12 meses à frente e também a SELIC projetada pelos players TOP5 do relatório Focus: 


```r
inflation <- rbcb::get_annual_market_expectations(indic = "IPCA")
selic     <- rbcb::get_top5s_annual_market_expectations(indic = "Selic")
```

Ambas as séries contêm o mesmo período de referência. Com isso, é possível extraír a previsão (3 anos à frente) dos participantes de mercado para cada variável: 


```r
inflation_filtered <- inflation |> 
    mutate(reference_date = as.numeric(reference_date),
           n_ahead = reference_date - lubridate::year(date)) |> 
    filter(n_ahead == 3) |> 
    select(date, ipca_median = median)

selic_filtered <- selic |> 
    mutate(reference_date = as.numeric(reference_date),
           n_ahead = reference_date - lubridate::year(date)) |> 
    filter(n_ahead == 3) |> 
    select(date, selic_median = median)
```

Em seguida, junta-se ambas pela data e consolida-se a mediana das expectativas para taxa de juros real _em cada ponto do tempo_:


```r
real_rate_expectations <- left_join(selic_filtered, inflation_filtered, by = "date") |> 
    mutate(real_rate_median = selic_median - ipca_median) |> 
  
    group_by(date) |> 
    summarise(median = median(real_rate_median)) |> 
    ungroup()
  
real_rate_expectations
```

```
## # A tibble: 5,032 x 2
##    date       median
##    <date>      <dbl>
##  1 2001-11-06   NA  
##  2 2001-11-07   NA  
##  3 2001-11-08   NA  
##  4 2001-11-09    7.5
##  5 2001-11-12    7.5
##  6 2001-11-13    7.5
##  7 2001-11-14    7.5
##  8 2001-11-16    7.5
##  9 2001-11-19    7.5
## 10 2001-11-20    7.5
## # ... with 5,022 more rows
```

Agora é só correr para o abraço com o `ggplot2`:


```r
real_rate_expectations |> 
    ggplot(aes(x = date, y = median)) +
    geom_line() +
    labs(title    = "Taxa de Juros (Real) Estrutural",
         subtitle = "Informações Implícitas no Relatório Focus",
         x        = NULL, 
         y        = NULL) +
    scale_y_continuous(labels = scales::percent_format(scale = 1, accuracy = 1)) +
    theme(panel.background = element_rect(fill = "#efe4ce"),
          plot.background  = element_rect(fill = "#efe4ce"))
```

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-5-1.png" width="672" />

As últimas informações disponíveis apontam para uma taxa de juros real de médio-prazo em torno de `\(3.50\%\)`. Adicionando-se a inflação esperada para 2022 (atualmente em volta de `\(5\%\)`) chega-se a uma SELIC neutra de `\(8.50\%\)`. 

Eu disse que era simples!
