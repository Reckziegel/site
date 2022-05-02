---
title: Opiniões nas Copulas
author: Bernardo Reckziegel
date: '2022-05-02'
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

As margens carregam as informações *puramente* índividuais (exclusivas de cada ativo) e as copulas levam as informações *puramente* conjuntas (de dependência entre as variáveis)[^1].

Estamos mais acostumados a trabalhar com as margens, mas as copulas também são uma fonte riquíssima de informação. É, portanto, natural *tentar* estimar as copulas, assim como fazemos com as distribuições marginais.

Como de praxe, análise de hoje é conduzida com o dataset `EuStockMarkets`, que vem com a instalação do `R`:

``` r
# invariance
x <- matrix(diff(log(EuStockMarkets)), ncol = 4)
colnames(x) <- colnames(EuStockMarkets)
head(x)
```

    ##               DAX          SMI          CAC         FTSE
    ## [1,] -0.009326550  0.006178360 -0.012658756  0.006770286
    ## [2,] -0.004422175 -0.005880448 -0.018740638 -0.004889587
    ## [3,]  0.009003794  0.003271184 -0.005779182  0.009027020
    ## [4,] -0.001778217  0.001483372  0.008743353  0.005771847
    ## [5,] -0.004676712 -0.008933417 -0.005120160 -0.007230164
    ## [6,]  0.012427042  0.006737244  0.011714353  0.008517217

No gráfico abaixo coloco o índice `SMI` no eixo x e o `DAX` no eixo y. Perceba que é a combinação das distribuições marginais que nos permite fazer inferência sobre a associação linear dessas variáveis:

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-2-1.png" width="672" />

Ou seja, são as informações *puramente* individuais - as “margens” - que ditam a estrutura de correlação dos ativos financeiros.

<!-- Já a copula está dentro do intervalo `\([0, 1]\)` e pode ser vista como uma medida de _score_ não-linear, pois não há como passar uma única reta entre cada ponto presente no gráfico.  -->

Já as copulas possuem uma conexão de dependência mais profunda:

> Copula = Distribuição Multivariada - Margens

A copula é a informação que sobra uma vez que tenhamos “limpado” a informação das margens de um determinado ativo. Dito de outra forma: pegue um dataset qualquer, “jogue fora” a estrutura de correlação linear e o que sobrar você chame de copula.

Abaixo a copula empírica dos índices `SMI` e `DAX`:

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-3-1.png" width="672" />

As copulas são normalizadas dentro do cubo unitário `\([0,1]\)`. Perceba que essa copula, em particular, apresenta dois pontos de aglomeração nos extremos: quando o `SMI` cai muito, o `DAX` também cai muito; quando `SMI` sobe muito, o `DAX` também sobe muito. Ou seja, essa copula está nos revelando que em momentos de euforia e pânico os índices `SMI` e `DAX` andam juntos!

Obviamente, nem todas as copulas são iguais. Abaixo mostro quatro tipos de copulas bastante conhecidas, que fazem parte do mundo [arquimediano](https://en.wikipedia.org/wiki/Copula_(probability_theory)#Archimedean_copulas).

<img src="images/arq_copulas.png" alt="" width="90%" height="60%"/>

<hr/>

A copula de Clayton geralmente é utilizada para modelar eventos de pânico, porque muitos pontos se aglomeram à esquerda na parte inferior (quando x cai, y também cai). Já a copula de Gumbel é utilizada para modelar eventos de euforia, pois muitos pontos se concentram à direita na parte superior (quando x sobe, y também sobe).

Na prática, não há uma única forma de “decompor” as distribuições entre margens e copulas. Aqui, sigo o approach das *probabilidades flexíveis* e manipulo esses elementos com o algoritmo [CMA](https://papers.ssrn.com/sol3/papers.cfm?abstract_id=1752702), que oferece uma receita de dois passos para “separar” e “combinar” distribuições multivariadas.

O pacote `cma` não está no CRAN, então para reproduzir os códigos abaixo você deve rodar o comando: `devtools::install_github("Reckziegel/CMA")` no console:

``` r
# devtools::install_github("Reckziegel/CMA")
library(cma)

sep <- cma_separation(x = x)
sep
```

    ## # CMA Decomposition
    ## marginal: << tbl 1859 x 4 >>
    ## cdf     : << tbl 1859 x 4 >>
    ## copula  : << tbl 1859 x 4 >>

<!-- O objeto `sep` mantém cada elemento da distribuição guardado em uma lista distinta. -->

Para objetos da classe `cma_separation` o pacote `cma` disponibiliza a família de funções `fit_copula_*()`. Como comentei, a copula de clayton é um candidado natural para modelar eventos de estresse:

``` r
clayton_fit <- fit_copula_clayton(copula = sep, method = "ml")
clayton_fit
```

    ## # New Copula
    ## Conveged:       0
    ## Dimension:      4
    ## Log-Likelihood: 1615.755
    ## Model:          claytonCopula

Perceba que nessa copula apenas um parâmetro precisa ser estimado:

``` r
clayton_fit$estimate
```

    ## [1] 1.066038

Quando maior for o parâmetro `\(\alpha\)`, mais aglomerados os dados ficam a esquerda da distribuição.

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-8-1.png" width="672" />

Moral da história: podemos realizar uma análise de estresse *ex-ante* colocando as opiniões nas correlações - como fiz [aqui](https://www.bernardo.codes/blog/2022-04-06-opini-o-nas-correla-es/) - ou modelando as copulas diretamente. Eu acho o segundo approach mais elegante.

Para precificar um cenário de “sell-off” *ex-ante*, adiciono uma perturbação no parâmetro `\(\alpha\)`. Em particular, uso `\(\alpha = 5\)` e com essa nova estimativa simulo um painel amplo com `\(1.000.000\)` de linhas e `\(4\)` colunas que contêm as nesnas propriedades estatísticas do objeto `clayton_fit`.

Esse processo é realizado com a função `generate_copulas` do pacote `cma`:

``` r
# lock environment
set.seed(2)

# "twick" the alpha parameter
clayton_fit$estimate <- 5

# generate new scenarios
simul_clayton <- generate_copulas(model = clayton_fit, n = 1000000)
```

Para acomodar opiniões o pacote `ffp` disponibiliza a família de funções `view_on_*()`. Para o caso em questão, usa-se `view_on_copula`:

``` r
library(ffp)

prior_for_simul <- rep(1 / 1000000, 1000000)

views_on_cop <- view_on_copula(
  x     = sep$copula, 
  simul = simul_clayton, 
  p     = prior_for_simul
)
views_on_cop
```

    ## # ffp view
    ## Type:  View On Copula
    ## Aeq :  Dim 34 x 1859 
    ## beq :  Dim 34 x 1

Formalmente, busca-se minimizar a expressão:

$$ min \sum_{i=1}^I x_i(ln(x_i) - ln(p_i)) $$
`\(s.t.\)`
$$ \sum_{i=1}^I \hat{p_i} U_{j,k}U_{j,l}  =  \sum_{i=1}^I p_i \hat{U}_{j,k}\hat{U}_{j,l} $$
$$ \sum_{i=1}^I \hat{p_i} U_{j,k}U_{j,l}U_{j,i}  =  \sum_{i=1}^I p_i \hat{U}_{j,k}\hat{U}_{j,l}\hat{U}_{j,i} $$
$$ \sum_{i=1}^I \hat{p_i}  U_{j,k} = 0.5 $$
$$ \sum_{i=1}^I \hat{p_i}  U_{j,k}^2 = 0.33 $$

No qual `\(p_i \hat{U}_{j,k}\hat{U}_{j,l}\)` e `\(p_i \hat{U}_{j,k}\hat{U}_{j,l}\hat{U}_{j,i}\)` atuam como restrições nos momentos cruzados e `\(0.5\)` e `\(0.33\)` condicionam os dois primeiros momentos da distribuição uniforme.

Esse sistema é solucionado com a função `entropy_pooling`:

``` r
prior_from_data <- rep(1 / nrow(x), nrow(x))

ep <- entropy_pooling(
  p      = prior_from_data, 
  Aeq    = views_on_cop$Aeq, 
  beq    = views_on_cop$beq, 
  solver = "nloptr"
)
ep
```

    ## <ffp[1859]>
    ## 2.675292e-12 0.0002722608 6.850905e-06 5.248376e-05 0.001002795 ... 0.0005913308

O vetor de probabilidades `ep` é aquele que consegue atender as opiniões do econometrista distorcendo ao mínimo as probabilidades uniformes:

``` r
library(ggplot2)

autoplot(ep) + 
  scale_color_viridis_c(option = "C", end = 0.75) + 
  labs(title    = "Distribuição de Probabilidades Posteriores", 
       subtitle = "Opinião/Perturbação na Copula", 
       x        = NULL, 
       y        = NULL)
```

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-12-1.png" width="672" />

Dessas probabilidades, deriva-se os momentos *condicionais* que são o principal insumo para construção de uma **fronteira eficiente bayesiana**:

``` r
ffp_moments(x = x, p = ep)
```

    ## $mu
    ##          DAX          SMI          CAC         FTSE 
    ## 0.0005338149 0.0004894317 0.0004274987 0.0004413356 
    ## 
    ## $sigma
    ##               DAX          SMI          CAC         FTSE
    ## DAX  1.194373e-04 9.354658e-05 1.070850e-04 7.536304e-05
    ## SMI  9.354658e-05 9.929703e-05 9.467923e-05 6.690249e-05
    ## CAC  1.070850e-04 9.467923e-05 1.314587e-04 8.035356e-05
    ## FTSE 7.536304e-05 6.690249e-05 8.035356e-05 7.180174e-05

Outra maneira simples de analisar o impacto das opiniões é combinando o output da função `empirical_stats` com o `ggplot2`:

``` r
library(dplyr)

prior <- empirical_stats(x = x, p = as_ffp(prior_from_data)) |> 
  mutate(type = "Prior")
posterior <- empirical_stats(x = x, p = ep) |> 
  mutate(type = "Posterior")

bind_rows(prior, posterior) |> 
  # plot
  ggplot(aes(x = name, y = value, color = type, fill = type)) +
  geom_col(position = "dodge") +
  facet_wrap(~stat, scales = "free") +
  scale_fill_viridis_d(end = 0.75, option = "C") + 
  scale_color_viridis_d(end = 0.75, option = "C") + 
  labs(title = "Análise de 'Estresse' ex-ante via Entropy-Pooling", 
       subtitle = "Perturbação na Copula de Clayton",
       x = NULL, y = NULL, color = NULL, fill = NULL) 
```

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-14-1.png" width="672" />

O impacto da perturbação nas copulas vai na direção esperada: sob regime de “stress” os retornos são menores, as volatilidades mais elevadas, as margens mais assimétricas, as caudas mais largas e, por fim, o VaR e Expected Shortfall também são maiores.

Obviamente, manipulando os objetos `prior` e `posterior` é possível chegar aos números exatos. Abaixo um exemplo de como esse cálculo pode ser feito para o Value-at-Risk (VaR) ao nível de `\(99\%\)`:

``` r
library(tidyr)

bind_rows(prior, posterior) |> 
  filter(stat == "VaR") |> 
  select(name, value, type) |> 
  pivot_wider(names_from = "type", values_from = "value") |> 
  transmute(
    Ativo = name, 
    `VaR: Diferença Anualizada` = paste0(round(100 * sqrt(252) * (Posterior - Prior), 2), "%")
  )
```

    ## # A tibble: 4 x 2
    ##   Ativo `VaR: Diferença Anualizada`
    ##   <chr> <chr>                      
    ## 1 DAX   5.12%                      
    ## 2 SMI   7.01%                      
    ## 3 CAC   0.02%                      
    ## 4 FTSE  2.28%

Por hoje é isso e no próximo post falarei sobre como modelar eventos de pânico com [mixtures](https://en.wikipedia.org/wiki/Mixture_distribution).

[^1]: [A New Breed of Copulas for Risk and Portfolio Management](https://papers.ssrn.com/sol3/papers.cfm?abstract_id=1752702)
