---
title: Momentum Entropy-Pooling
author: Bernardo Reckziegel
date: '2022-06-06'
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

O script abaixo carrega os pacotes utilizados e cria a conexão com a base de dados que permitirá replicar os resultados desse post[^1]: 


```r
library(tidyverse) # Dispensa introdução
library(lubridate) # Manipulação de datas
library(readxl)    # Leitura de arquivos xmlx
library(curl)      # Leitura de arquivos xmlx
library(rsample)   # Rolling-windows no tidyverse
library(quadprog)  # Otimização Quadrática
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

Geralmente, o fator de _momentum_ é construído como um portfolio _dollar-neutral_, `\(100\%\)` investido, no qual a performance passada determina quais ativos entram na ponta comprada e/ou vendida da carteira. O ponto de corte é algum percentil: por exemplo, ordena-se ativos da melhor para pior performance e compra-se aqueles até o `\(33º\)` percentil e vende-se os ativos do `\(67º\)` percentil para baixo, condicional a alguma restrição de liquidez.

Nesse post, ao invés de comprar/vender os ativos que estão acima/abaixo de um determinado percentil, utilizo [entropy-pooling](https://papers.ssrn.com/sol3/papers.cfm?abstract_id=1213325) para construir um portfolio de média-variância com um _tilt_ em _momentum_. Para isso, ordeno as ações da melhor para pior performance e computo o vetor de probabilidades _posterior_ que acomoda essa "ordenação" e oferece a menor distorção possível em relação ao vetor de probabilidades original, _equal-weigthed_[^2]. 

O processo de estimação é todo conduzido dentro do ecossistema do [tidyverse](https://www.tidyverse.org/). O passo inicial envolve a construção de uma estrutura no formato de _rolling-window_, no qual os dados são divididos entre "treinamento" - `.analysis` - e "avaliação" - `.assessment` - `\(1\)` passo a frente:




```r
get_assessment_date <- function(x) {
  map(.x = x$splits, .f = ~ assessment(.x)) |> 
    map(.f = keep, .p = is.Date) |> 
    reduce(bind_rows) |> 
    pull(date)
}

optimin <- returns |> 
  rolling_origin(initial = 52 * 5, assess = 1, cumulative = FALSE)
optimin <- optimin |> 
  mutate(optimin, 
         .date       = get_assessment_date(optimin),  
         .analysis   = map(.x = splits, .f = analysis),
         .assessment = map(.x = splits, .f = assessment)) |> 
  select(-splits, -id)
```

Para resolver o problema da Entropia-Mínima Relativa (EMR) o sistema de equações abaixo precisa ser solucionado em cada ponto do tempo: 

$$ \sum_{i=1}^I x_i(ln(x_i) - ln(p_i)) $$
`\(s.t.\)`
$$ \sum_{i=1}^I \hat{p_i} (Opinion_{i, 1} - Opinion_{i, 2}) \leq 0 $$ 
$$ \sum_{i=1}^I \hat{p_i} (Opinion_{i, 2} - Opinion_{i, 3}) \leq 0 $$ 
$$ ... $$ 
$$ \sum_{i=1}^I \hat{p_i} (Opinion_{i, j-1} - Opinion_{i, 5}) \leq 0 $$ 

O resultado dessa otimização é o vetor de probabilidades _posterior_ - `\(\hat{p}^*\)` - que permite estimar os parâmetros `\(\mu^*_{posterior}\)` e `\(\sigma^*_{posterior}\)`. A função `momentum_moments` implementa essa etapa:


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

Como o objeto `optimin` está no formato [tidy](https://r4ds.had.co.nz/tidy-data.html), a função `momentum_moments` pode ser aplicada em série de maneira bastante suscinta[^3]:


```r
# It may take a few minutes to run... Be patient!
optimin <- optimin |>
  mutate(.moments = map(
    .x = .analysis, 
    .f = ~ momentum_moments(.x = .x, .period = 52))
  )
```

Dentro de cada elemento da coluna `.moments` há uma lista com as estimativas `\(\hat{\mu}^*_{posterior}\)` e `\(\hat{\sigma}^*_{posterior}\)`[^4]. Essas estimativas _condicionais_ são o principal insumo para construção de um __portfolio eficiente bayesiano__, que é solucionado via otimização quadrática: 


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

Novamente, o fato do objeto `optimin` estar no formato [tidy](https://r4ds.had.co.nz/tidy-data.html) torna a otimização recursiva fácil e _human-readable_:


```r
optimin <- optimin |>
  mutate(.weights = map(
    .x = .moments, 
    .f = ~ optimal_portfolio(sigma = .x$sigma, mu = .x$mu, .wmin = 0, .wmax = 0.4)
    )
  )
```

Limito o peso máximo para cada ativo em `\(40\%\)` para garantir que ao menos `\(3\)` ativos estejam na carteira a cada ponto do tempo (altere os parâmetros `.wmax` e `.xmin` se você quiser).

Dentro de cada elemento na coluna `.weights` há um vetor de alocação ótimo. Por exemplo, `optimin$.weights[[1]]` acessa o primeiro vetor, `optimin$.weights[[2]]` o segundo vetor, e assim por diante. 

O retorno bruto da estratégia é computado com a interação dos elementos em `.weights` e `.assessment`:


```r
optimin <- optimin |>
  mutate(gross_return = map2_dbl(
    .x = .weights, 
    .y = .assessment, 
    .f = ~ as.matrix(.y[ , -1]) %*% .x)
  )
```

A última etapa passa pelo cálculo dos retornos acumulados do Ibovespa e da estratégia proposta:


```r
benchmark <- returns |> 
  select(date, IBOV) |> 
  rename(.date = "date")

# Joint strategy with Ibovespa
optimin |>
  left_join(benchmark, by = ".date") |>
  select(.date, gross_return, IBOV) |>
  
  # Apply 1.5% cost 
  mutate(`Momentum-EP` = gross_return - (1.015 ^ (1 / 52) - 1)) |>
  select(.date, `Momentum-EP`, IBOV) |>
  
  # Compound
  mutate_if(is.numeric, ~ cumprod(1 + .x) * 100) |>
  
  # Tidy
  pivot_longer(cols = -.date) |>
  
  # Plot
  ggplot(aes(x = .date, y = value, color = name)) +
  geom_line() + 
  scale_y_log10() + 
  scale_color_viridis_d(end = 0.75, option = "C") + 
  labs(title = "Momentum Entropy-Pooling", 
       subtitle = "Portfolio bayesiano long-only com 'tilt' em momentum", 
       x = NULL, y = NULL, color = NULL) + 
  theme(legend.position = "bottom")
```

<img src="images/ep_momentum_pnl_evolution.png" alt="" width="95%"/>

Adiciono o custo de `\(1,5\%\)` ao ano, que considero alto para uma estratégia capaz de ganhar escala. A titulo de comparação, a Black Rock cobra `\(0,3\%\)` a.a. pelo seu [ETF de momentum](https://www.blackrock.com/pt/profissionais/products/270051/ishares-msci-world-momentum-factor-ucits-etf) e o Itaú cobra `\(0,5\%\)` a.a. sobre seus [ETF's de renda variável](https://www.itnow.com.br/). Acredito que os demais custos relacionados a execução da estratégia possam ser acomodados com `\(1,0\%\)` ao ano.

Óbvio, há outras questões envolvidas: aumentar o universo de ativos disponíveis faz a fronteira eficiente se deslocar para a esquerda e para cima, expandindo as possibilidades de investimento e o retorno da estratégia. Mas esse efeito tem um limite: o erro de estimação não é neutro as mudanças na dimensão do "mercado" (aqui representado pelo objeto `returns`). No geral, acho que a expansão da fronteira domina os erros de estimação, pelo menos para datasets pequenos (entre `\(15-25\)` ativos). 

A frequência do rebalanceamento também é relevante. No exercício acima, o rebalanceamente é realizado em cada ponto do tempo. Mas para esse tipo de estratégia o ideal seria trabalhar com dados de maior latência e rabalancear a carteira com menor frequência (1x por mês, por exemplo). Essa mudança contribuiria para melhorar a estimação da matriz de covariância de maneira significativa, além de reduzir o custo computacional.

Ainda tem o _turnover_[^5]: a estratégia de _momentum_ gira mais do que a estratégia de _value_. Para mercados de baixa dimensão (como aquele apresentado aqui) esse não parece ser um problema. Mas, se o "mercado" for expandido, é muito provável que essa métrica tenha que ser analisada com mais atenção por meio de simulações. 


```r
turnover <- optimin |> 
  mutate(.turnover = map2_dbl(.x = .weights, .y = lag(.weights), .f = ~ sum(abs(.x - .y))))

turnover |> 
  ggplot(aes(x = .date, y = .turnover, color = .turnover)) + 
  geom_line(show.legend = FALSE) +  
  scale_y_continuous(labels = scales::percent_format()) + 
  scale_color_viridis_c(option = "C", end = 0.75) + 
  labs(title = "Momentum Entropy-pooling", 
       subtitle = "Turnover em um mercado de dimensão reduzida", 
       x = NULL, y = NULL)
```

<img src="images/ep_momentum_turnover.png" alt="" width="95%" height="80%"/>

<!-- Por outro lado, em um ambiente global, os dividendos (típicos de _value_) possuem uma maior tributação do que ganhos de capital (_momentum_). Talvez esses efeitos se anulem, não sei.  -->

Para terminar, acredito que o viés para esse tipo de estratégia seja para cima. O modelo de média-variância e o [CAPM](https://en.wikipedia.org/wiki/Capital_asset_pricing_model) ainda são mal compreendidos no Brasil, o que significa que tem pouca gente olhando para o que acontece em torno do ponto de ótimo. No mínimo, as estratégias que buscam a "optimalidade" ("allocation") oferecem alguma diversificação sobre a hegemonia das estratégias de _value_ ("positioning") dominantes no mercado brasileiro, mas ai já é papo para outro post.

Por hoje é isso e semana que vem mostro uma maneira robusta para avaliar cenários do tipo _"What if..."_. 

[^1]: O dataset `returns` contém os índices da B3 com pelo menos `\(15\)` anos de história. Os dados estão em periodicidade semanal e compreendem o período de 10/02/2010 até 20/05/2022. 
[^2]: Uma aplicação mais sofisticada do modelo apresentado aqui pode ser visto no paper [Quantitative Portfolio Construction and Systematic Trading Strategies using Factor Entropy Pooling](https://risk.edhec.edu/sites/risk/files/edhec-working-paper-quantitative-portfolio-construction_1398417370465_0.pdf).
[^3]: A função `map` no `R` têm um papel muito similar a função `map` no `Python`, mas no `R` utilizamos a programação funcional com mais frequência. :sunglasses: 
[^4]: Experimente rodar o comando `optimin$.moments[[1]]` no console.
[^5]: Defino o _turnover_ como a soma da diferença absoluta entre as posições do período vigente vs. as posições do período imediatamente anterior:
$$ turnover = \sum_{i=1}^{I} | w_t - w_{t-1} | $$
