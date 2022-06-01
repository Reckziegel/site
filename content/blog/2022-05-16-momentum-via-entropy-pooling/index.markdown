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

Nos últimos posts mostrei algumas funcionalidades básicas do pacote [ffp](https://reckziegel.github.io/FFP/). Em particular, como portfolio e risk-managers podem utilizar essa biblioteca para adicionar opiniões nos mercados.

Hoje mostro como `ffp` pode ser útil para construção de estratégias de _factor-investing_ e _smart-beta_.

Foco minha atenção no fator de [momentum](https://www.investopedia.com/terms/m/momentum.asp) por dois motivos: 

1) A performance desse fator no [Brasil](https://nefin.com.br/data/risk_factors.html) e no [exterior](https://www.aqr.com/Insights/Research/Journal-Article/Value-and-Momentum-Everywhere) é estrelar;
2) É de fácil construção.  

O segundo motivo é especialmente importante para um texto curto como esse. 

O script abaixo carrega os pacotes utilizados e cria a conexão com a base de dados que permitirá replicar os resultados desse post: 


```r
library(tidyverse) # Dispensa introdução
library(lubridate) # Manipulação de datas
library(readxl)    # Leitura de arquivos xmlx
library(curl)      # Leitura de arquivos xmlx
library(rsample)   # Rolling-windows no tidyverse
library(quadprog)  # Otimização Quadrática
library(corrr)     # Correlation Networks 
library(viridisLite) # Viridis palette 
library(ffp)       # Probabilidades Flexíveis

# Access the dataset  
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

Para implementar a estratégia, seleciono os índices da B3 com pelo menos `\(15\)` anos de história. Os dados estão em periodicidade semanal e compreendem o período de 10/02/2010 até 20/05/2022. 

A estrutura de correlação histórica mostra que os índices `IMAT`, `INFC` e `IEEX` são os principais diversificadores em relação ao `IBOV`:


```r
returns |> 
  select(where(is.numeric)) |>  
  correlate() |> 
  network_plot(colours = plasma(n = 10))
```

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-2-1.png" width="672" />

Já a próximidade dos índices `INDX`e `IDIV` denuncia o _tilt_ do Ibovespa em direção ao fator de _value_, como mostrei no [post anterior](https://www.bernardo.codes/blog/2022-05-10-opini-es-nas-regress-es/).

Enfim, voltando aos fatores... Para construir o fator de momentum geralmente utiliza-se um portfolio _dollar-neutral_, `\(100\%\)` investido, no qual a performance passada determina quais ativos entram na ponta comprada e/ou vendida da carteira. O ponto de corte é algum percentil: por exemplo, compra-se/vende-se os ativos que tiveram uma performance relativa acima/abaixo de um determinado ponto de corte. 

Nesse post, ao invés de comprar/vender os ativos que estão acima/abaixo de um determinado percentil, utilizo entropy-pooling para construir um portfolio de média-variância com um _tilt_ em momentum. Para isso, ordeno as ações da melhor para pior performance e computo o vetor de probabilidades _posterior_ que acomoda essa "ordenação" e ao mesmo tempo oferece a menor distorção possível em relação ao vetor de probabilidades original, _equal-weigthed_. 

O processo de estimação é todo conduzido via [tidyverse](https://www.tidyverse.org/). O passo inicial envolve a construção de estruturas em formato de "rolling-window" com auxílio da função `rolling_origin`:




```r
get_assessment_date <- function(x) {
  map(.x = x$splits, .f = ~ assessment(.x)) |> 
    map(.f = keep, .p = is.Date) |> 
    reduce(bind_rows) |> 
    pull(date)
}

optimin <- returns |> 
  rolling_origin(initial = 52 * 5, assess = 1, cumulative = TRUE)
optimin <- optimin |> 
  mutate(optimin, 
         .date       = get_assessment_date(optimin),  
         .analysis   = map(.x = splits, .f = analysis),
         .assessment = map(.x = splits, .f = assessment)) |> 
  select(-splits, -id)
```

O objeto `optimin` é dividido entre "treinamento" - `.analysis` - e "avaliação" -`.assessment` - `\(1\)` passo a frente. 

Para resolver o problema da entropia-mínima relativa (EMR) o sistema de equações abaixo precisa ser resolvido a cada ponto do tempo: 

$$ \sum_{i=1}^I x_i(ln(x_i) - ln(p_i)) $$
`\(s.t.\)`
$$ \sum_{i=1}^I \hat{p_i} (Opinion_{i, 1} - Opinion_{i, 2}) \leq 0 $$ 
$$ \sum_{i=1}^I \hat{p_i} (Opinion_{i, 2} - Opinion_{i, 3}) \leq 0 $$ 
$$ ... $$ 
$$ \sum_{i=1}^I \hat{p_i} (Opinion_{i, j-1} - Opinion_{i, v}) \leq 0 $$ 

A solução numérica retorna o vetor de probabilidades _posterior_, `\(\hat{p}^*\)`, que permite estimar os parâmetros `\(\mu^*_{conditional}\)` e `\(\sigma^*_{conditional}\)`, principais insumos do modelo de média-variância. 

A função `momentum_moments` definida abaixo resume esse processo:


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

  # If optimization fails, use the prior
  if (class(ep)[[1]] == "try-error") {
    ep <- prior
  }

  # Compute the conditional moments
  ffp_moments(x = .x, p = ep)

}
```

O objeto `optimin` está no formato [tidy](https://r4ds.had.co.nz/tidy-data.html) o que torna mais fácil aplicar a função `momentum_moments` em série para cada elemento contido na coluna `.analysis`[^1]:


```r
# It may take a few minutes to run... Be patient!
optimin <- optimin |>
  mutate(.moments = map(
    .x = .analysis, 
    .f = ~ momentum_moments(.x = .x, .period = 52))
  )
optimin
```

Dentro de cada elemento da coluna `.moments` há uma lista com as estimativas `\(\hat{\mu}^*_{posterior}\)` e `\(\hat{\sigma}^*_{posterior}\)`. 

> Experimente rodar o comando `optimin$.moments[[1]]` no console.

Essas estimativas de média e variância _condicionais_ são o principal insumo para construção de um __portfolio eficiente bayesiano__, que é resolvido via otimização quadrática: 


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

Novamente, o fato do objeto `optimin` estar no formato `tidy` torna a otimização recursiva fácil e _human-readable_:


```r
optimin <- optimin |>
  mutate(.weights = map(
    .x = .moments, 
    .f = ~ optimal_portfolio(sigma = .x$sigma, mu = .x$mu)
    )
  )
```

No qual o peso máximo para cada ativo na carteira foi limitado em `\(40\%\)` para garantir que ao menos `\(3\)` ativos estejam na carteira a cada ponto do tempo. 

> Altere os parâmetros `.wmax` e `.xmin` se você quiser!

Dentro de cada elemento dentro da coluna em `.weights` há um vetor de alocação ótimo. Por exemplo, `optimin$.weights[[1]]` acessa o primeiro vetor, `optimin$.weights[[2]]` o segundo vetor, e assim por diante. 

O retorno bruto da estratégia é acessado com a interação dos elementos em `.weights` com `.assessment`:


```r
optimin <- optimin |>
  mutate(ret = map2_dbl(
    .x = .weights, 
    .y = .assessment, 
    .f = ~ as.matrix(.y[ , -1]) %*% .x)
  )
optimin
```



```r
benchmark <- select(returns, date, IBOV) |> 
  rename(.date = "date")

optimin |>
  left_join(benchmark, by = ".date") |>
  select(.date, ret, IBOV) |>
  mutate(Momentum = ret - (1.015 ^ (1 / 52) - 1)) |>
  select(.date, Momentum, IBOV) |>
  mutate_if(is.numeric, ~ cumprod(1 + .x) * 100) |>
  pivot_longer(cols = -.date) |>
  ggplot(aes(x = .date, y = value, color = name)) +
  geom_line() + 
  scale_y_log10() + 
  scale_color_viridis_d(end = 0.75, option = "C") + 
  labs(title = "Momentum Entropy-Pooling", 
       subtitle = "Portfolio long-only com 'tilt' em momentum", 
       x = NULL, y = NULL, color = NULL) + 
  theme(legend.position = "bottom")
```

<img src="images/ep_momentum_pnl_evolution.png" alt="" width="95%"/>

O gráfico acima mostra a evolução dessa estratégia contra o ibovespa. Adiciono o custo de `\(1,5\%\)` ao ano, que considero alto para uma estratégia "de fator" capaz de ganhar escala. A titulo de comparação, a Black Rock cobra `\(0,3\%\)` a.a. pelo seu [ETF de momentum](https://www.blackrock.com/pt/profissionais/products/270051/ishares-msci-world-momentum-factor-ucits-etf) e o Itaú cobra `\(0,5\%\)` a.a. sobre seus [ETF's de renda variável](https://www.itnow.com.br/). Outros custos relacionados a execução da estratégia acho que podem ser acomodados com `\(1,0\%\)` ao ano.

Óbvio, há outras questões envolvidas. Aumentar o universo de ativos disponíveis faz a fronteira eficiente se deslocar para a esquerda e para cima, expandindo as possibilidades de investimento e o retorno da estratégia. Mas esse efeito tem um limite: o erro de estimação não é neutro as mudanças no conjunto dos ativos e aumenta com a redução dos graus de liberdade. No geral, acho que a expansão da fronteira domina os erros de estimação, pelo menos para datasets pequenos (entre `\(15-30\)` ativos). Se a estratégia for aplicada em índices/fatores, uma dimensão baixa pode ser capaz de mitigar o parte significativa do risco idiossincrático. 

A frequência do rebalanceamento também é relevante. No exercício acima, o rebalanceamente é realizado em cada ponto do tempo. Para esse tipo de estratégia o ideal seria trabalhar com dados de maior latência e rabalancear a carteira com menor frequência. Essa mudança contribuiria para diminuir a incerteza que permeia os parâmetros da matrix de covariância _posteior_ de maneira significativa, além de reduzir o custo computacional.

Ainda tem o _turnover_: a estratégia de momentum gira mais do que a estratégia de _value_. Por outro lado, dividendos e juros sobre capital próprio (típicos de _value_) geralmente possuem uma maior tributação do que ganhos de capital (momentum). Em um ambiente global, é bem possível que esses efeitos se anulem, embora não possa cravar com certeza.

Enfim, nem tudo é um moranguinho. No próximo post mostro uma maneira mais sofisticada de se realizar _backtests_. 

Até lá...
 
[^1]: A função `map` no `R` têm uma função muito simular a função `map` no `Python`. 


