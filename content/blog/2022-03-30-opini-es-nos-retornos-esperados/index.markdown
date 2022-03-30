---
title: Opiniões nos retornos esperados
author: Bernardo Reckziege
date: '2022-03-30'
slug: []
categories:
  - R
  - views
tags:
  - entropy-pooling
  - bayesian inference
  - ffp
meta_img: images/image.png
description: Description for the page
---

Dando continuidade ao post anterior, hoje mostro como utilizar entropy-pooling para imputar opiniões nos retornos esperados. 

Utilizo o dataset `EuStockMarkets` que acompanha o `R` para facilitar a reprodução:


```r
library(tidyverse)

# Invariance
x <- diff(log(EuStockMarkets))

# Plot
as_tibble(x) |> 
  mutate(id = 1:nrow(x)) |> 
  pivot_longer(cols = -id) |> 
  ggplot(aes(x = id, y = value, color = name)) + 
  geom_line(show.legend = FALSE) +
  scale_color_viridis_d(option = "E", end = 0.75) + 
  facet_wrap(~name) + 
  labs(x = NULL, y = NULL)
```

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-1-1.png" width="672" />

***

Digamos que o time de análise acredite que os retornos do índice `FTSE` serão `\(20\%\)` superiores a média histórica e que não haja opinião formada em relação aos demais ativos. 

No pacote [ffp](https://reckziegel.github.io/FFP/index.html) as opiniões são contruídas com a família de funções `view_on_*()`:


```r
library(ffp) 

# Returns 20% higher than average
expectations <- mean(x[ , "FTSE"]) * 1.2

# ffp views constructor
views <- view_on_mean(x = as.matrix(x[ , "FTSE"]), mean = expectations)
views
```

```
## # ffp view
## Type:  View On Mean
## Aeq :  Dim 1 x 1859 
## beq :  Dim 1 x 1
```

O output da função `view_on_mean()` sempre retorna uma lista com dois elementos[^1]: `Aeq` e `beq`[^2]. Essas são as matrizes `\(H\)` e `\(h\)` das equações do [post anterior](https://www.bernardo.codes/blog/2022-03-28-opini-es-uma-breve-introdu-o/).

Com as restrições em mãos, o problema da entropia mínima relativa (EMR) pode ser resolvido com `entropy_pooling`:


```r
# Prior probabilities 
prior <- rep(1 / nrow(x), nrow(x)) 

ep <- entropy_pooling(p = prior, Aeq = views$Aeq, beq = views$beq, solver = "nlminb")
ep
```

```
## <ffp[1859]>
## 0.0005425631 0.0005340012 0.000544236 0.0005418246 0.0005322989 ... 0.0005451271
```
O vetor `ep` contém as probabilidades que satisfazem a visão do time de análise e que distorcem ao mínimo o vetor de probabilidades uniforme, que chamamos de `prior`. 

Abaixo a vizualização dessa distorção: 


```r
autoplot(ep) + 
  scale_color_viridis_c(option = "E", end = 0.75)
```

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-4-1.png" width="672" />

É por meio do vetor `ep` que é possível computar as estatísticas de locação e dispersão _condicionais_, aquelas que absorvem as opiniões subjetivas do time de análise. Esse cálculo é facilmente implementado com a função `ffp_moments`: 


```r
ep_moments <- ffp_moments(x = x, p = ep)
ep_moments
```

```
## $mu
##          DAX          SMI          CAC         FTSE 
## 0.0007233428 0.0008764380 0.0005145713 0.0005183822 
## 
## $sigma
##               DAX          SMI          CAC         FTSE
## DAX  1.056934e-04 6.656913e-05 8.311516e-05 5.217276e-05
## SMI  6.656913e-05 8.515083e-05 6.251254e-05 4.282438e-05
## CAC  8.311516e-05 6.251254e-05 1.214742e-04 5.677744e-05
## FTSE 5.217276e-05 4.282438e-05 5.677744e-05 6.341068e-05
```

> __Atenção__: Não confunda o resultado da função `ffp_moments()` - momentos condicionais - com aquele que você obteria utilizando as funções `colMeans()` e `cov()` - momentos incondicionais.

<!-- Esses novos momentos podem  então, finalmente ser utlizados em um otimizador, no estilo média-variância, risk-parity, etc. Obviamente, os resultados do otimizador, serão tão bons quanto as opiniões (trash in, trash out). -->

Abaixo a divergência entre esses métodos:


```r
round(ep_moments[["mu"]] / colMeans(x) - 1, 2)
```

```
##  DAX  SMI  CAC FTSE 
## 0.11 0.07 0.18 0.20
```

Para o `FTSE` a divergência bate exatamente com a expectativa do time de análise. Todavia, os outros ativos também acabaram sendo afetados. A expectativa de retorno do `DAX` aumentou `\(9\%\)`, `SMI`, `\(7\%\)` e `CAC`, `\(17\%\)`. Se o time de análise se sentir confortável com esses resultados, jogue o output de `ep_moments` para um otimizador e bola pra frente. Se houver restrições, a análise precisa ser refeita. 

Por exemplo, vamos refazer as opiniões assumindo que o retorno do `FTSE` será `\(20\%\)` superior a média e o restante dos ativos manterão os retornos em linha com a média histórica de cada série:


```r
expectations <- colMeans(x)
expectations[["FTSE"]] <- expectations[["FTSE"]] * 1.2

views      <- view_on_mean(x = as.matrix(x), mean = expectations)
ep         <- entropy_pooling(p = prior, Aeq = views$Aeq, beq = views$beq, solver = "nlminb")
ep_moments <- ffp_moments(x = x, p = ep)

round(ep_moments[["mu"]] / colMeans(x) - 1, 2)
```

```
##  DAX  SMI  CAC FTSE 
##  0.0  0.0  0.0  0.2
```

Agora as divergências sumiram e a razão entre as médias condicionais e incodicionais afetam apenas o `FTSE`.







[^1]: Dado que construímos a opinião em apenas `\(1\)` ativo, o vetor `views` contém apenas uma linha em `Aeq` e um elementro em `beq`.  
[^2]: `Aeq` e `beq` de "__eq__ uality", pois esses elementos estão presentes em uma restrição que deve ser satisfeita com igualdade.
