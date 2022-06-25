---
title: What if...
author: Bernardo Reckziegel
date: '2022-06-26'
slug: []
categories:
  - R
  - backtest
tags:
  - entropy-pooling
  - cma
  - copula
  - MCMC
meta_img: images/image.png
description: Description for the page
---

No [post anterior](https://www.bernardo.codes/blog/2022-05-16-momentum-via-entropy-pooling/) mostrei como construir um portfolio bayesiano com *tilt* no fator de *momentum*, cuja a performance pode ser vista no gráfico abaixo:

![](images/ep_momentum_pnl_evolution.png)

Embora a estratégia supere o Ibovespa dentro do período **simulado**, não é possível saber - apenas olhando para o gráfico - se o "excesso de retorno" é fruto de habilidade ou sorte. De fato, *backtesting* é um tema é extremamente espinhoso e a chance de haver *overfitting* não pode ser subestimada[^1].

[^1]: Pseudo-Mathematics and Financial Charlatanism: The Effects of Backtest Overfitting on Out-of-Sample Performance: <https://papers.ssrn.com/sol3/papers.cfm?abstract_id=2308659>

Para ajudar a mitigar o problema causado pelo *data-mining,* alguns testes estatísticos interessantes foram desenvolvidos:

-   [A Reality Check for Data Snooping](https://www.ssc.wisc.edu/~bhansen/718/White2000.pdf)
-   [A Test for Superior Predictive Ability](https://papers.ssrn.com/sol3/papers.cfm?abstract_id=264569)
-   [Perfrmance Hypotesis Testing with the Sharpe and Treynor Measures](https://www.bing.com/search?q=Performance+hypotesis+testing+with+sharpe+and+treynor&cvid=1b500489108b4a61b514d7fb7e2666f0&aqs=edge..69i57.13552j0j1&pglt=675&FORM=ANNTA1&PC=W091/)

Aqui utilizo o approach [Copula-Marginal](https://papers.ssrn.com/sol3/papers.cfm?abstract_id=1752702). Assim como no post [Opiniões nas Copulas](https://www.bernardo.codes/blog/2022-04-27-opini-es-nas-copulas/), decomponho a distribuição multivariada do "mercado" entre margens e copulas e via simulação de Monte-Carlo projeto caminhos alternativos para as margens, enquanto as copulas são mantidas constantes.

O setup inicial é similar ao do [post anterior](https://www.bernardo.codes/blog/2022-05-16-momentum-via-entropy-pooling/):


```r
library(tidyverse) # Dispensa introdução
library(furrr)     # Paralell computing 
library(lubridate) # Manipulação de datas
library(readxl)    # Leitura de arquivos xmlx
library(curl)      # Leitura de arquivos xmlx
library(rsample)   # Rolling-windows no tidyverse
library(quadprog)  # Otimização Quadrática
library(cma)       # Copula-Marginal Algorithm
library(ffp)       # Probabilidades Flexíveis

# Connect  
url <- "https://github.com/Reckziegel/site/raw/master/data/br_stock_indices.xlsx"
destfile <- "br_stock_indices.xlsx"
curl_download(url, destfile)

# Import
indices <- read_excel(path = destfile, 
                      col_types = c("date", rep("numeric", 6))) |> 
  mutate(date = as_date(date))  

# Invariance quest
returns <- indices |> 
  mutate(across(where(is.numeric), ~ log(.x / lag(.x)))) |> 
  na.omit()
returns
```

```
## # A tibble: 850 x 7
##    date            IDIV     IBOV    IEEX    IFNC     IMAT     INDX
##    <date>         <dbl>    <dbl>   <dbl>   <dbl>    <dbl>    <dbl>
##  1 2006-02-10  0.0233   -0.00771  0.0465  0.0192 -0.0170  -0.00308
##  2 2006-02-17  0.0493    0.0384   0.0421  0.0317  0.0288   0.0349 
##  3 2006-02-24 -0.000517  0.00491  0.0391 -0.0187  0.0247   0.00872
##  4 2006-03-03  0.0262    0.0162   0.0355  0.0216  0.0242   0.0252 
##  5 2006-03-10 -0.0501   -0.0617  -0.0469 -0.0561 -0.0507  -0.0265 
##  6 2006-03-17  0.0258    0.0309   0.0446  0.0599  0.0323   0.0227 
##  7 2006-03-24 -0.0112   -0.0125  -0.0186 -0.0287 -0.00646 -0.0185 
##  8 2006-03-31  0.0373    0.00990 -0.0495 -0.0487  0.0381   0.0246 
##  9 2006-04-07  0.0256    0.0254   0.0120  0.0301  0.0166   0.0295 
## 10 2006-04-14 -0.0209   -0.0219  -0.0310 -0.0581 -0.00750 -0.0132 
## # ... with 840 more rows
```

Os cálculos para reprodução desse post são computacionalmente intensos e mais de `\(590.000\)` portfolios são estimados. Para diminuir o tempo de processamento utilizo todos os núcleos disponíveis no meu note:


```r
# Replicability
set.seed(123) 
# Verifique o número de `workers` no seu pc
plan(multisession, workers = 8) 
```

> Para detectar o número de core's no seu computador utilize a função `detectCores` do pacote `parallel`.

Começo com a construção de uma estrutura de *rolling-window*, no qual "treinamento" (`.analysis`) e "avaliação" (`.assessment`) são mantidos separados para fins de estimação e previsão:


```r
get_assessment_date <- function(x) {
  map(.x = x$splits, .f = ~ assessment(.x)) |> 
    map(.f = keep, .p = is.Date) |> 
    reduce(bind_rows) |> 
    pull(date)
}

rolling_structure <- returns |> 
  rolling_origin(initial = 52 * 5, cumulative = FALSE)
rolling_structure <- rolling_structure |>
  mutate(.date = get_assessment_date(rolling_structure),
         # map in parallel
         .analysis   = future_map(.x = splits, .f = analysis), 
         .assessment = future_map(.x = splits, .f = assessment)) |> 
  select(-splits, -id)
rolling_structure
```

```
## # A tibble: 590 x 3
##    .date      .analysis          .assessment     
##    <date>     <list>             <list>          
##  1 2011-02-04 <tibble [260 x 7]> <tibble [1 x 7]>
##  2 2011-02-11 <tibble [260 x 7]> <tibble [1 x 7]>
##  3 2011-02-18 <tibble [260 x 7]> <tibble [1 x 7]>
##  4 2011-02-25 <tibble [260 x 7]> <tibble [1 x 7]>
##  5 2011-03-04 <tibble [260 x 7]> <tibble [1 x 7]>
##  6 2011-03-11 <tibble [260 x 7]> <tibble [1 x 7]>
##  7 2011-03-18 <tibble [260 x 7]> <tibble [1 x 7]>
##  8 2011-03-25 <tibble [260 x 7]> <tibble [1 x 7]>
##  9 2011-04-01 <tibble [260 x 7]> <tibble [1 x 7]>
## 10 2011-04-08 <tibble [260 x 7]> <tibble [1 x 7]>
## # ... with 580 more rows
```

<!-- Até ai não há muita novidade, apenas segui o roteiro do último post.  -->

A seguir, construo a simulação de Monte-Carlo por meio de um processo de `\(4\)` etapas:

-   1\. Estimação da distribuição marginal do "mercado";

-   2\. Decomposiçao das séries de "treinamento" entre margens e copulas;

-   3\. Simulação de novos cenários de "treinamento" compatíveis com a distribuição marginal selecionada na etapa 1;

-   4\. Combinação das margens simuladas com as copulas empíricas.

Esse passo-a-passo é implementado em cada ponto do tempo para compor cenários alternativos para o "mercado", aqui representado pelo objeto `returns`. Sob esses novos cenários, a estratégia *Momentum Entropy-Pooling* é reestimada, de modo que centenas de caminhos compatíveis com a distribuição escolhida na etapa 1 sejam levados em consideração na hora de avaliar o histórico da estratégia.

##### Etapa 1

Assumo que o "mercado" seja representado pela distribuição [Normal Inverse-Gaussian](https://en.wikipedia.org/wiki/Normal-inverse_Gaussian_distribution) (NIG). Nesse estágio qualquer distribuição poderia ser utilizada, mas a [família generalizada hiperbólica](https://en.wikipedia.org/wiki/Generalised_hyperbolic_distribution) geralmente apresenta um bom *fit* do ponto de vista empírico:


```r
# Etapa 1
rolling_structure <- rolling_structure |> 
  # map in parallel
  mutate(.distribution = future_map(.x = .analysis, .f = fit_nig))
rolling_structure
```

```
## # A tibble: 590 x 4
##    .date      .analysis          .assessment      .distribution   
##    <date>     <list>             <list>           <list>          
##  1 2011-02-04 <tibble [260 x 7]> <tibble [1 x 7]> <list<dbl> [21]>
##  2 2011-02-11 <tibble [260 x 7]> <tibble [1 x 7]> <list<dbl> [21]>
##  3 2011-02-18 <tibble [260 x 7]> <tibble [1 x 7]> <list<dbl> [21]>
##  4 2011-02-25 <tibble [260 x 7]> <tibble [1 x 7]> <list<dbl> [21]>
##  5 2011-03-04 <tibble [260 x 7]> <tibble [1 x 7]> <list<dbl> [21]>
##  6 2011-03-11 <tibble [260 x 7]> <tibble [1 x 7]> <list<dbl> [21]>
##  7 2011-03-18 <tibble [260 x 7]> <tibble [1 x 7]> <list<dbl> [21]>
##  8 2011-03-25 <tibble [260 x 7]> <tibble [1 x 7]> <list<dbl> [21]>
##  9 2011-04-01 <tibble [260 x 7]> <tibble [1 x 7]> <list<dbl> [21]>
## 10 2011-04-08 <tibble [260 x 7]> <tibble [1 x 7]> <list<dbl> [21]>
## # ... with 580 more rows
```

Cada linha da coluna `.distribution` possui uma NIG com diferentes parâmetros:


```r
rolling_structure$.distribution[[1]]
```

```
## # Margins Estimation
## Converged:       TRUE
## Dimension:       6
## AIC:            -8179.117
## Log-Likelihood:  4123.558
## Model:           Asymmetric Normal Inverse Gaussian
```


```r
rolling_structure$.distribution[[2]]
```

```
## # Margins Estimation
## Converged:       TRUE
## Dimension:       6
## AIC:            -8176.888
## Log-Likelihood:  4122.444
## Model:           Asymmetric Normal Inverse Gaussian
```

<!--# A estimação é conduzida em cada ponto do tempo para evitar forward-looking bias. -->

##### Etapa 2

Na sequência, a distribuição multivariada do mercado é decomposta entre entre margens vs. copulas com o auxílio da função `cma_separation` do pacote `CMA`:


```r
# Etapa 2
rolling_structure <- rolling_structure |>
  # map in parallel
  mutate(.separation = future_map(.x = .analysis, .f = cma_separation))
rolling_structure
```

```
## # A tibble: 590 x 5
##    .date      .analysis          .assessment      .distribution    .separation
##    <date>     <list>             <list>           <list>           <list>     
##  1 2011-02-04 <tibble [260 x 7]> <tibble [1 x 7]> <list<dbl> [21]> <list<dbl>>
##  2 2011-02-11 <tibble [260 x 7]> <tibble [1 x 7]> <list<dbl> [21]> <list<dbl>>
##  3 2011-02-18 <tibble [260 x 7]> <tibble [1 x 7]> <list<dbl> [21]> <list<dbl>>
##  4 2011-02-25 <tibble [260 x 7]> <tibble [1 x 7]> <list<dbl> [21]> <list<dbl>>
##  5 2011-03-04 <tibble [260 x 7]> <tibble [1 x 7]> <list<dbl> [21]> <list<dbl>>
##  6 2011-03-11 <tibble [260 x 7]> <tibble [1 x 7]> <list<dbl> [21]> <list<dbl>>
##  7 2011-03-18 <tibble [260 x 7]> <tibble [1 x 7]> <list<dbl> [21]> <list<dbl>>
##  8 2011-03-25 <tibble [260 x 7]> <tibble [1 x 7]> <list<dbl> [21]> <list<dbl>>
##  9 2011-04-01 <tibble [260 x 7]> <tibble [1 x 7]> <list<dbl> [21]> <list<dbl>>
## 10 2011-04-08 <tibble [260 x 7]> <tibble [1 x 7]> <list<dbl> [21]> <list<dbl>>
## # ... with 580 more rows
```

Cada linha da coluna em `.separation` possui `\(3\)` elementos: `marginal`, `cdf` e `copula`.


```r
rolling_structure$.separation[[1]]
```

```
## # CMA Decomposition
## marginal: << tbl 260 x 6 >>
## cdf     : << tbl 260 x 6 >>
## copula  : << tbl 260 x 6 >>
```

##### Etapa 3

Para implementar as etapas 3 e 4, "empilho" o objeto `rolling_structure` `\(1.000\)` vezes (o número de simulações que pretendo conduzir):


```r
tidy_tbl <- tibble(.simulation = 1:1000)
tidy_tbl <- tidy_tbl |> 
  mutate(.rolling_windows = list(rolling_structure)) |> 
  unnest(.rolling_windows)
```

Para cada um desses `\(1.000\)` objetos, gero uma distribuição marginal com propriedades estatísticas similiares aquelas presentes na coluna `.distribution`. Esse processo é executado em cada ponto do tempo para um total de `\(590.000\)` distribuições marginais:


```r
# Etapa 3
tidy_tbl <- tidy_tbl |> 
  mutate(.marginals = map(
    .x = .distribution, 
    .f = ~ generate_margins(model = .x, n = 52 * 5) |> chuck("marginal")
    )
  )
```

Cada linha da coluna `.marginals` possui uma simulação distinta. A primeira simulação é acessada com o comando `rolling_structure$.marginals[[1]]` a segunda com `rolling_structure$.marginals[[2]]` e assim por diante...

##### Etapa 4

A última etapa no processo de simulação envolve a combinação das novas margens (`.marginals`) com as copulas empíricas, um trabalho para a função `cma_combination` do pacote `CMA`:


```r
# Etapa 4
tidy_tbl <- tidy_tbl |> 
  mutate(.combination = map2(
    .x = .marginals, 
    .y = .separation, 
    .f = ~ cma_combination(x = .x, cdf = .y$cdf, copula = .y$copula)
    )
  )
```

Assim, encerra-se o processo de construção do painel simulado pelo approach Monte-Carlo Copula-Marginal (MCCM).

#### Execução da Estratégia

A cada ponto do tempo, calculo a performance acumulada dos ativos que compõem o "mercado" (com base nas últimas 52 semanas) e atribuio a cada um uma classificação ordinal: o melhor ativo é o número `\(1\)`, o segundo melhor é o número `\(2\)`, etc.

Com base nessa classificação, imponho uma opinião sistemática na estrutura de mercado: assumo que os ativos que performaram bem no passado (mais bem "rankeados") continuarão a apresentar uma performance superior aos demais ativos no futuro.

Essa visão de mundo é adicionada via [Entropy-Pooling](https://papers.ssrn.com/sol3/papers.cfm?abstract_id=1213325), uma técnica bayesiana mais geral do que [Black-Litterman](https://en.wikipedia.org/wiki/Black%E2%80%93Litterman_model). A solução desse problema permite computar os momentos condicionais, `\(\mu_{posterior}\)` e `\(\sigma_{posterior}\)`:


```r
momentum_moments <- function(.x, .period = 52) {

  # Order assets by `.period` performance
  .order <- .x |>
    select(where(is.numeric)) |>
    summarise_all(~prod(1 + tail(.x, n = .period)) - 1) |>
    as.double() |>
    order()

  # Construct the Views
  prior <- rep(1 / nrow(.x), nrow(.x))
  views <- view_on_rank(x = .x, rank = .order)

  # Solve the Relative Entropy Problem
  ep <- try(entropy_pooling(p = prior, A = views$A, b = views$b, solver = "nloptr"))

  # If optimization fails (not common) use the prior
  if (class(ep)[[1]] == "try-error") {
    ep <- prior
  }

  # Compute the conditional moments
  ffp_moments(x = .x, p = ep)

}
```

Na prática, isso significa rodar o seguinte comando:


```r
tidy_tbl <- tidy_tbl |> 
  # map in parallel
  mutate(.moments = future_map(.x = .combination, .f = momentum_moments))
```

Os momentos *condicionais* calculados acima (`.moments`) são usados para estimação de um portfolio de média-variância *long-only* que, por construção, possui um *tilt* no fator de momentum.


```r
optimal_portfolio <- function(sigma, mu, .wmin = 0, .wmax = 0.4) {

  num_assets <- ncol(sigma)

  # Full investment constraint
  Aeq  <- matrix(1, 1, num_assets)
  beq  <- 1

  # Maximum/Minimum asset weight constraint
  A <- rbind(-diag(num_assets), diag(num_assets))
  b <- c(-rep(.wmax, num_assets), rep(.wmin, num_assets))

  Amat <- rbind(Aeq, A)
  bvec <- c(beq, b)

  weights <- solve.QP(
    Dmat = 2 * sigma, 
    dvec = mu, 
    Amat = t(Amat), 
    bvec = bvec, 
    meq  = length(beq)
    )$solution
  
  matrix(weights)

}
```

Durante a otimização, adiciono a restrição `\(w_{max} = 0.40\)` para manter ao menos `\(3\)` ativos na carteira em cada ponto do tempo:


```r
tidy_tbl <- tidy_tbl |> 
  mutate(.weights = map(
    .x = .moments, 
    .f = ~ optimal_portfolio(sigma = .x$sigma, mu = .x$mu)
    )
  )
```

Por fim, calculo o retorno bruto da estratégia com a interação dos pesos ótimos com os retornos realizados *out-of-sample*:


```r
tidy_tbl <- tidy_tbl |> 
  mutate(.gross_return = map2_dbl(
    .x = .weights, 
    .y = .assessment, 
    .f = ~ as.matrix(.y[, -1]) %*% .x
    )
  )
```

O resultado - líquido de custos operacionais[^2] - em relação ao Ibovespa pode ser visto com o comando:

[^2]: Custo total de `\(1,50\%\)` ao ano.


```r
# Ibovespa returns
benchmark <- returns |>
  select(date, IBOV) |> 
  rename(.date = "date") 

tidy_tbl |> 
  
  # merge simulations with IBOV
  left_join(benchmark, by = ".date") |> 
  
  # aggregate by simulation
  group_by(.simulation) |> 
  
  # compute the cumulative returns
  mutate(.net_returns     = .gross_return - (1.015 ^ (1 / 52) - 1),  
         .net_performance = cumprod(1 + .net_returns) * 100, 
         IBOV             = cumprod(1 + IBOV) * 100) |> 
  
  # Plot
  ggplot(aes(x = .date, y = .net_performance, color = .simulation, group = .simulation)) + 
  geom_line(alpha = 0.5, color = "#F89441FF") + 
  geom_line(aes(x = .date, y = IBOV), color = "#0D0887FF") + 
  scale_y_log10() + 
  labs(
    title    = "Momentum Entropy-Pooling vs. Ibovespa", 
    subtitle = "Simulação via Monte-Carlo Copula-Marginal (MCCM) para 1.000 cenários", 
    x        = NULL, 
    y        = NULL)
```

<img src="images/ep_momentum_simul.png" alt="" width="95%"/>

Os topos e fundos de todos os cenários simulados da estratégia batem com os topos e fundos do Ibovespa. Isso não é coincidência: a verdadeira fonte de dependência do "mercado" não foi descartada: graças a separação Copula-Marginal, *as copulas foram mantidas intactadas*.

A diferença de performance entre os portfólios simulados vêm inteiramente da aleatoriedade contida nas margens, que afetam o curso da estratégia por meio de diferentes pontos de ótimo:

`$$\Delta \ margens|_{copula} \rightarrow \Delta \ alocação \ ótima|_{copula} \rightarrow \Delta \ caminhos \ potenciais |_{copula}$$`

O approach MCCM fornece uma maneira robusta de se testar como uma estratégia teria performando sob a influência de choques idiossincráticos que não alteram a estrutura de dependência do mercado. Obviamente, assumir que a codependência não se altera é uma hipótese relativamente forte. Felizmente, também é possível adicionar perturbações nas copulas, mas ai é papo para outro post.

> Rodando essa simulação me dei conta de como as funções do pacote `CMA` estão levando mais tempo e alocando mais memória do que que deveriam. As operações de "decomposição" e "combinação" não são complexas e acredito que seja possível acelerar os cálculos. Nas próximas semanas vou me debruçar sobre o código original para entregar uma nova versão mais leve e mais rápida desse pacote.
