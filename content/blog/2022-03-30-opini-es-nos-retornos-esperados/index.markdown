---
title: Opiniões nos retornos esperados
author: Bernardo Reckziegel
date: '2022-03-31'
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
# Invariance
x <- diff(log(EuStockMarkets))

head(x)
```

```
##               DAX          SMI          CAC         FTSE
## [1,] -0.009326550  0.006178360 -0.012658756  0.006770286
## [2,] -0.004422175 -0.005880448 -0.018740638 -0.004889587
## [3,]  0.009003794  0.003271184 -0.005779182  0.009027020
## [4,] -0.001778217  0.001483372  0.008743353  0.005771847
## [5,] -0.004676712 -0.008933417 -0.005120160 -0.007230164
## [6,]  0.012427042  0.006737244  0.011714353  0.008517217
```

***

> Para reproduzir os scripts abaixo você precisará instalar o pacote `ffp` com o comando `install.packages("ffp")`.

***

Digamos que o time de gestão acredite que os retornos do índice `FTSE` serão `\(20\%\)` superiores a média histórica e que não haja opinião formada em relação aos demais ativos. 

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

O output da função `view_on_mean()` sempre retorna uma lista com dois elementos: `Aeq` e `beq`[^1] que entram como restrições lineares no problema da entropia mínima relativa, como demonstrado abaixo:

$$ argmin \sum_{j=1}^J x_j(ln(x_j) - ln(p_j)) $$
$$ s.t. Hx = h $$  

Os elementos `Aeq` e `beq` correspondem as matrizes `\(H\)` e `\(h\)`, respectivamente.

Essa otimização é solucionada com a função `entropy_pooling`:


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

O vetor `ep` contém as probabilidades que satisfazem a visão do time de gestão e que distorcem ao mínimo o vetor de probabilidades uniforme, que chamamos de `prior`. 

Abaixo a visualização dessa distorção: 


```r
library(ggplot2)

autoplot(ep) + 
  scale_color_viridis_c(option = "E", end = 0.75) + 
  labs(title    = "Probabilidades Posteriores", 
       subtitle = "Opiniões nos Retornos Esperados", 
       x        = NULL, 
       y        = NULL)
```

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-4-1.png" width="672" />

É por meio do vetor `ep` que é possível computar as estatísticas de locação e dispersão _condicionais_, aquelas que absorvem as opiniões subjetivas do gestor. 

Esse cálculo é facilmente implementado com a função `ffp_moments`: 


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

__Atenção__: Não confunda o resultado da função `ffp_moments()`, que calcula os momentos _condicionais_, com aquele que você obteria utilizando as funções `colMeans()` e `cov()`, que computam os momentos _incondicionais_.

<!-- Esses novos momentos podem  então, finalmente ser utlizados em um otimizador, no estilo média-variância, risk-parity, etc. Obviamente, os resultados do otimizador, serão tão bons quanto as opiniões (trash in, trash out). -->

Abaixo a divergência entre esses dois métodos (expressa em variação percentual):


```r
round(ep_moments[["mu"]] / colMeans(x) - 1, 2)
```

```
##  DAX  SMI  CAC FTSE 
## 0.11 0.07 0.18 0.20
```

Para o índice inglês (`FTSE`) a divergência bate exatamente com as expectativas. Entretanto, os outros ativos também foram afetados. A expectativa de retorno para o `DAX` aumentou em `\(11\%\)`, `SMI` em `\(7\%\)` e `CAC` em `\(18\%\)`. 

Se o time de análise se sentir confortável com esses resultados, o output de `ep_moments` está pronto para ir para um otimizador (mean-variance, risk-parity, etc). Se houver restrições, a análise precisa ser refeita. 

Por exemplo, vamos refazer as opiniões assumindo que o retorno do `FTSE` será `\(20\%\)` superior a média enquanto o restante dos ativos terão retornos idênticos a suas médias amostrais: 


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

Agora as divergências sumiram e apenas o retorno do índice `FTSE` é afetado pelas opiniões.

No próximo post mostrarei como adicionar _views_ nas volatilidades.

[^1]: `Aeq` e `beq` de "__eq__ uality", pois esses elementos estão presentes em uma restrição que deve ser satisfeita com igualdade.
