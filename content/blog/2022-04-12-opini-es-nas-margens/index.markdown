---
title: Opiniões nas Margens
author: Bernardo Reckziegel
date: '2022-04-12'
slug: []
categories:
  - R
  - views
tags:
  - ffp
  - entropy-pooling
  - bayesian inference
meta_img: images/image.png
description: Description for the page
---

Dando sequência aos posts anteriores, hoje mostro como construir opiniões nas distribuições marginais. 

Mais uma vez utilizo o dataset `EuStockMarkets` -  que acompanha a instalação do `R` - para facilitar a reprodução dos resultados: 


```r
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

Vamos assumir que o time de econometria (após algumas linhas de código e meia dúzia de derivadas) conclua que a distribuição Student-t assimétrica é a que melhor se adapta aos dados em questão. 

Para conduzir o processo de estimação utilizo o pacote [cma](https://reckziegel.github.io/CMA/)[^1]. Essa biblioteca não está no [CRAN](https://cran.r-project.org/) - _for now_ -  e para instala-la você deverá rodar o comando `devtools::install_github("Reckziegel/CMA")` no console. 


```r
# devtools::install_github("Reckziegel/CMA")
library(cma)

t_model <- fit_t(x, symmetric = FALSE)
t_model
```

```
## # Margins Estimation
## Converged:       TRUE
## Dimension:       4
## AIC:            -52711.16
## Log-Likelihood:  26374.58
## Model:           Asymmetric Student-t
```

O pacote `cma` fornece algumas funções interessantes para geração de cenários. Utilizo `generate_margins` para criar um painel gigante com as mesmas propriedades estatísticas presentes em `t_model`:   


```r
t_margins <- generate_margins(model = t_model, n = 1000000)
t_margins
```

```
## # New Margins
## marginal: << tbl 1000000 x 4 >>
```

O output de `t_margins` é uma lista - `tibble` - que contém `\(1.000.000\)` linhas e `\(4\)` colunas. Veja:


```r
t_margins$marginal
```

```
## # A tibble: 1,000,000 x 4
##          DAX       SMI        CAC      FTSE
##        <dbl>     <dbl>      <dbl>     <dbl>
##  1  0.00913   0.0116    0.0111     0.00989 
##  2 -0.00778  -0.00494  -0.00361    0.00246 
##  3  0.0180   -0.0220   -0.0287    -0.0389  
##  4  0.00387  -0.00466   0.00318    0.000725
##  5  0.00308   0.00809  -0.00654   -0.000936
##  6 -0.00271  -0.00829  -0.0158    -0.00528 
##  7 -0.000529 -0.00719  -0.0410    -0.00288 
##  8 -0.00675  -0.00560  -0.00478   -0.00608 
##  9  0.0123    0.000881  0.00904    0.00448 
## 10 -0.00184  -0.00695  -0.0000675 -0.00389 
## # ... with 999,990 more rows
```

Como já comentei outras vezes, o pacote `ffp` disponibiliza a família de funções `view_on_*` para construção de opiniões. Hoje utilizo `view_on_marginal_distribution` para impor _views_ com base no painel recém gerado:


```r
library(ffp)

prior_from_simulation <- rep(1 / 1000000, 1000000)

views <- view_on_marginal_distribution(
  x     = x, 
  simul = t_margins$marginal, 
  p     = prior_from_simulation
)
views
```

```
## # ffp view
## Type:  View On Marginal Distribution
## Aeq :  Dim 16 x 1859 
## beq :  Dim 16 x 1
```

As listas `Aeq` e `beq` contidas no objeto `views` têm `\(16\)` linhas, ou seja, há `\(16\)` restrições ativas nessa opinião. Por quê tantas restrições? Porque a função `view_on_marginal_distribution` foi desenhada para que a média, variância, assimetria e curtose sejam idênticas ao painel simulado, `t_margins`. Temos, portanto, `\(4\)` restrições ativas para cada ativo, totalizando `\(16\)` restrições. 

Do ponto de vista matemático, o problema que queremos resolver é:

$$ min \sum_{i=1}^I x_i(ln(x_i) - ln(p_i)) $$
`\(s.t.\)`

$$ \sum_{i=1}^I \hat{p_i} O_{i,k}  =  \sum_{i=1}^I p_i \hat{O}_{i,k} $$
$$ \sum_{i=1}^I \hat{p_i} (O_{i,k})^2  =  \sum_{i=1}^I p_i (\hat{O}_{i,k})^2 $$
$$ \sum_{i=1}^I \hat{p_i} (O_{i,k})^3  =  \sum_{i=1}^I p_i (\hat{O}_{i,k})^3 $$

$$ \sum_{i=1}^I \hat{p_i} (O_{i,k})^4  =  \sum_{i=1}^I p_i (\hat{O}_{i,k})^4 $$

Esse sistema é resolvido com a função `entropy_pooling`:


```r
library(ggplot2)

prior_from_data <- rep(1 / nrow(x), nrow(x))

ep <- entropy_pooling(
  p      = prior_from_data, 
  Aeq    = views$Aeq, 
  beq    = views$beq, 
  solver = "nlminb"
)
ep
```

```
## <ffp[1859]>
## 0.0005389113 0.0005589966 0.0005357949 0.0005429065 0.000539703 ... 0.0005054094
```

O objeto `ep` contém as probabilidades que distorcem ao mínimo o vetor de probabilidades _equal-weighted_ e que ao mesmo tempo atendem as restrições desejadas.

O método `autoplot` do pacote `ggplot2` está disponível para objetos da classe `ffp`: 


```r
autoplot(ep) + 
  scale_color_viridis_c(option = "C", end = 0.75) + 
  labs(title    = "Distribuição de Probabilidades Posteriores", 
       subtitle = "Opinião nas Margens: distribuição t assimétrica", 
       x        = NULL, 
       y        = NULL)
```

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-7-1.png" width="672" />

Com base nessas probabilidades é possível computar as estimativas de locação e dispersão _condicionais_:


```r
cond_moments <- ffp_moments(x = x, p = ep)
cond_moments
```

```
## $mu
##          DAX          SMI          CAC         FTSE 
## 0.0006605320 0.0008186369 0.0004567979 0.0004432016 
## 
## $sigma
##               DAX          SMI          CAC         FTSE
## DAX  1.001914e-04 6.207432e-05 8.003929e-05 5.070221e-05
## SMI  6.207432e-05 8.076265e-05 5.982477e-05 4.159521e-05
## CAC  8.003929e-05 5.982477e-05 1.214189e-04 5.668368e-05
## FTSE 5.070221e-05 4.159521e-05 5.668368e-05 6.389930e-05
```

Observe que o momentos _condicionais_ batem exatamente com os momentos do painel simulado:  


```r
# Location Matches
round(cond_moments$mu / colMeans(t_margins$marginal) - 1, 2)
```

```
##  DAX  SMI  CAC FTSE 
##    0    0    0    0
```

```r
# Dispersion Matches
round(sqrt(diag(cond_moments$sigma)) / apply(t_margins$marginal, 2, sd) - 1, 2) 
```

```
##  DAX  SMI  CAC FTSE 
##    0    0    0    0
```

Ou seja, a opinião dos econometristas foi, de fato, levada em consideração no processo de otimização.

Agora vamos continuar com a brincadeira. Digamos que os economistas da gestora estejam pessimistas e acreditem que os retornos de todos os ativos terão um desempenho `\(10\%\)` abaixo da média histórica. Digamos também que os econometristas estejam preocupadas com um possível aumento da volatilidade e achem prudente reduzir o número de graus de liberdade do painel simulado de `\(\nu = 4.2\)`[^2] para `\(\nu = 2\)`: 


```r
# Return 10% below average
t_model$mu <- t_model$mu * 0.9

# degrees of freedom will be 2
t_model$chi <- 2.0
```

Para incorporar essas informações no modelo, as probabilidades precisam ser reestimadas:


```r
new_margins <- generate_margins(model = t_model, n = 1000000)
views <- view_on_marginal_distribution(
  x     = x,
  simul = new_margins$marginal,
  p     = prior_from_simulation
)
ep <- entropy_pooling(
  p      = prior_from_data,
  Aeq    = views$Aeq,
  beq    = views$beq,
  solver = "nlminb"
)
cond_moments <- ffp_moments(x = x, p = ep)
```

E veja que mais uma vez as opiniões são satisfeitas:


```r
# Location Matches
round(cond_moments$mu / colMeans(new_margins$marginal) - 1, 2)
```

```
##  DAX  SMI  CAC FTSE 
##    0    0    0    0
```

```r
# Dispersion Matches
round(sqrt(diag(cond_moments$sigma)) / apply(new_margins$marginal, 2, sd) - 1, 2)
```

```
##  DAX  SMI  CAC FTSE 
##    0    0    0    0
```

<!-- ![](https://media.giphy.com/media/ujUdrdpX7Ok5W/giphy.gif) -->

Modelar as margens como um todo adiciona um novo layer de flexibilidade ao processo de gestão de risco e construção de carteiras. A incerteza é tratada de maneira _prospectiva_, não olhando o retrovisor.

Contudo, há outros elementos em uma distribuição multivariada no qual podemos ter opiniões. No próximo post mostro uma fonte assossiação "invisível" pode ser relevante para modermos eventos de pânico. Até lá...


[^1]: Essa biblioteca será fundamental quando falarmos de copulas. 
[^2]: Você pode acessar essa informação com o comando `t_model$chi` no console.
