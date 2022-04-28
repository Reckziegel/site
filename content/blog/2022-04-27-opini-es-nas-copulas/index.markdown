---
title: Opiniões nas Copulas
author: Bernardo Reckziegel
date: '2022-04-28'
slug: []
categories:
  - R
  - views
tags:
  - bayesian inference
  - entropy-pooling
  - ffp
meta_img: images/image.png
description: Description for the page
---

Toda a distribuição multivariada é formada pela combinação de dois elementos:

> Distribuição Multivariada = Margens + Copulas

As margens carregam as informações _puramente_ índividuais (exclusivas de cada ativo) e as copulas levam as informações _puramente_ conjuntas (de dependência entre as variáveis).

Estamos mais acostumados a trabalhar com as margens, mas copulas talvez sejam uma fonte ainda mais rica de informação. É, portanto, natural _tentar_ estimar as copulas, assim como fazemos com as distribuições marginais.


```r
x <- matrix(diff(log(EuStockMarkets)), ncol = 4)
colnames(x) <- colnames(EuStockMarkets)
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

No gráfico abaixo coloco o índice `SMI` no eixo x e o `DAX` no eixo y. Perceba que é a combinação das margens que permite que façamos inferência sobre a associação linear dessas variáveis:

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-2-1.png" width="672" />

Ou seja, são as informações _exclusivamente_ individuais - as "margens" - que ditam a estrutura de correlação dos ativos financeiros.

<!-- Já a copula está dentro do intervalo `\([0, 1]\)` e pode ser vista como uma medida de _score_ não-linear, pois não há como passar uma única reta entre cada ponto presente no gráfico.  -->

Com as copulas é outra história. Pense nas copulas como a informação que sobra uma vez que tenhamos "limpado" a estrutura de correlação linear dos ativos. Ou seja, pegamos uma base de dados qualquer, "jogamos fora" a estrutura de correlação e o que sobrar chamamos de copula.

Abaixo a copula empírica dos índices `SMI` e `DAX`:

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-3-1.png" width="672" />

Perceba que essa copula, em particular, apresenta dois pontos de aglomeração nos extremos: quando o `SMI` cai muito, o `DAX` também cai muito; quando `SMI` sobe muito, o `DAX` também sobe muito. Ou seja, essa copula está nos revelando que em momentos de euforia e pânico os índices `SMI` e `DAX` andam juntos! 

Obviamente, nem todas as copulas são iguais. Abaixo mostro quatro tipos de copulas bastante conhecidas. Elas fazem parte do que chamamos de [copulas arquimedianas](https://en.wikipedia.org/wiki/Copula_(probability_theory).

<img src="images/arq_copulas.png" alt="" width="90%" height="60%"/>

A copula de Clayton geralmente é utilizada para modelar eventos de pânico, porque a maioria dos pontos se aglomeram à esquerda e para baixo (quando x cai, y também cai). Já a copula de Gumbel é utilizada para modelar eventos de euforia, pois a maioria dos pontos se concentram à direita e pra cima (quando x sobe, y também sobe).

Não há uma única forma de "decompor" as distribuições entre margens e copulas. Aqui, sigo o approach das _probabilidades flexíveis_ e manipulo esses elementos com o algoritmo [CMA](https://papers.ssrn.com/sol3/papers.cfm?abstract_id=1752702), que oferece uma receita de dois passos para "separar" e "combinar" distribuições multivariadas. 

O pacote `cma` não está no CRAN, então você deverá baixá-lo com o comando: `devtools::install_github("Reckziegel/CMA")` no console:


```r
# devtools::install_github("Reckziegel/CMA")
library(cma)

sep <- cma_separation(x = x)
sep
```

```
## # CMA Decomposition
## marginal: << tbl 1859 x 4 >>
## cdf     : << tbl 1859 x 4 >>
## copula  : << tbl 1859 x 4 >>
```

O objeto `sep` mantém cada elemento da distribuição guardado em uma lista distinta. Para objetos da classe `cma_separation` o pacote `cma` disponibiliza a família de funções `fit_copula_*()`. 

Como comentei antes, a copula de clayton é um candidado natural para modelagem de eventos de pânico. Dessa forma, escolho essa copula para conduzir a análise: 


```r
set.seed(1)

clayton_fit <- fit_copula_clayton(copula = sep)
clayton_fit
```

```
## # New Copula
## Conveged:       0
## Dimension:      4
## Log-Likelihood: 1615.755
## Model:          claytonCopula
```

Na copula de clayton, apenas um parâmetro de dependência precisa ser estimado:


```r
clayton_fit$estimate
```

```
## [1] 1.066038
```

Quando maior for o parâmetro `\(\alpha\)`, mais aglomerados os dados ficam na calda esquerda da distribuição. Veja:

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-7-1.png" width="672" />

Ou seja: podemos estimar a sensibilidade dos portfolios _ex-ante_ colocando as opiniões nas correlações - como fiz [aqui](https://www.bernardo.codes/blog/2022-04-06-opini-o-nas-correla-es/) - ou modelando as copulas diretamente. O segundo approach, sem dúvidas, é mais robusto e elegante.
 
Para precificar um cenário de "estresse" _ex-ante_, adiciono uma perturbação no parâmetro `\(\alpha\)`. Em particular, uso `\(\alpha = 5\)` e com essa nova estimativa simulo uma copula com `\(1.000.000\)` de linhas que seja compatível com essa especificação apresentada em `clayton_fit`. 

Esse processo é realizado com a função `generate_copulas` do pacote `cma`:


```r
# "twick" the alpha parameter
clayton_fit$estimate <- 5

# generate new scenarios
r_clayton <- generate_copulas(model = clayton_fit, n = 1000000)
```




```r
library(ffp)

prior_for_simul <- rep(1 / 1000000, 1000000)
views_on_cop <- view_on_copula(x = sep$copula, simul = r_clayton, p = prior_for_simul)
views_on_cop
```

```
## # ffp view
## Type:  View On Copula
## Aeq :  Dim 18 x 1859 
## beq :  Dim 18 x 1
```


```r
views_on_cor <- view_on_correlation(x = x, cor = cor(x))

views <- bind_views(views_on_cop, views_on_cor)
views
```

```
## # ffp view
## Type:  Multiple Views
## Aeq :  Dim 28 x 1859 
## beq :  Dim 28 x 1 
## A :  Dim 0 x 1 
## b :  Dim 0 x 1
```


```r
prior_from_data <- rep(1 / nrow(x), nrow(x))

ep <- entropy_pooling(p = prior_from_data, Aeq = views_on_cop$Aeq, beq = views_on_cop$beq, solver = "nloptr")
ep
```

```
## <ffp[1859]>
## 9.152781e-08 0.000864402 2.200265e-05 9.930021e-05 0.0009714989 ... 0.0008784395
```



```r
library(ggplot2)

autoplot(ep) + 
  scale_color_viridis_c(option = "C", end = 0.75) + 
  labs(title    = "Distribuição de Probabilidades Posteriores", 
       subtitle = "Opinião nas Copulas e nas Correlações", 
       x        = NULL, 
       y        = NULL)
```

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-12-1.png" width="672" />



```r
cond_mom <- ffp_moments(x = x, p = ep)
cond_mom
```

```
## $mu
##          DAX          SMI          CAC         FTSE 
## 0.0006191963 0.0007747545 0.0003525335 0.0003278094 
## 
## $sigma
##               DAX          SMI          CAC         FTSE
## DAX  1.215343e-04 9.297787e-05 1.087080e-04 7.416213e-05
## SMI  9.297787e-05 9.373274e-05 9.326959e-05 6.440196e-05
## CAC  1.087080e-04 9.326959e-05 1.278958e-04 7.747041e-05
## FTSE 7.416213e-05 6.440196e-05 7.747041e-05 6.460837e-05
```


```r
library(dplyr)

prior <- empirical_stats(x = x, p = as_ffp(prior_from_data)) |> 
  mutate(type = "Prior")
posterior <- empirical_stats(x = x, p = ep) |> 
  mutate(type = "Posterior")

bind_rows(prior, posterior) |> 
  ggplot(aes(x = name, y = value, color = type, fill = type)) +
  geom_col(position = "dodge") +
  facet_wrap(~stat, scales = "free") +
  scale_fill_viridis_d(end = 0.75, option = "C") + 
  scale_color_viridis_d(end = 0.75, option = "C") + 
  labs(
    title = "Análise de Sensibilidade Ex-Ante via Entropy-Pooling", 
    subtitle = "Opinião nas Copulas e nas Correlações",
    x = NULL, y = NULL, color = NULL, fill = NULL) 
```

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-14-1.png" width="672" />

Todas as estatísticas apontam para as direções esperadas: sob regime de "stress" os retornos posteriores são mais baixos, a volatilidade é mais alta, a assimetria é ainda mais negativa, a cauda da distribuição é mais larga, e o VaR e Expected Shortfall são mais devastadores.



