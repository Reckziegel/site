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

1) A performance desse fator no [Brasil](https://nefin.com.br/data/risk_factors.html) e no [exterior](https://www.aqr.com/Insights/Research/Journal-Article/Value-and-Momentum-Everywhere) é estrelar;
2) É de fácil construção.  

O segundo motivo é especialmente importante para um texto curto como esse. 

Trabalho com a base de dados `br_stock_indices.xlsx` que você pode encontrar no endereço https://github.com/Reckziegel/site/tree/master/data. 


```r
library(tidyverse) # Dispensa introdução
library(lubridate) # Manipulação de datas
library(readxl)    # Leitura de arquivos xmlx
library(rsample)   # Rolling-windows no tidyverse
library(quadprog)  # Otimização Quadrática
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

<!-- Esse índices são bastante conhecidos e com um histórico relativamente longo para o Brasil. Reforço que não há _chery-picking_, no sentido de escolher os ativos que favorecem os resultados da estratégia. Os índices foram escolhidos porque queria trabalhar com ativos que tivessem pelo menos 15 anos de história. SObre a performance histórica, acho que os fatos falam por si ([Fact, Fiction and Momentum Investing](https://www.aqr.com/Insights/Research/Journal-Article/Fact-Fiction-and-Momentum-Investing)).  -->

Selecionei todos os índices da B3 que tinham ao menos `\(15\)` anos de história. Esse foi o único filtro utilizado e não houve _chery-picking_ no sentido de favorecer a estratégia proposta. Como comento mais abaixo, acho que uma base de dados restrita e composta de ativos com alta correlação entre si, provavelmente exerce o efeito oposto: diminui, ao invés de aumentar, o set de oportunidades. 


```r
cor(returns[ , 2:6])
```

```
##           IDIV      IBOV      IEEX      IFNC      IMAT
## IDIV 1.0000000 0.9130121 0.8138845 0.8414326 0.6924251
## IBOV 0.9130121 1.0000000 0.7333250 0.8865941 0.7800530
## IEEX 0.8138845 0.7333250 1.0000000 0.6802823 0.4603051
## IFNC 0.8414326 0.8865941 0.6802823 1.0000000 0.5826127
## IMAT 0.6924251 0.7800530 0.4603051 0.5826127 1.0000000
```

Voltando aos fatores. Geralmente, o fator de momentum geralmente é construído como um portfolio _dollar-neutral_, `\(100\%\)` investido, no qual a performance passada determina quais ativos entram na ponta comprada e/ou vendida da carteira. O ponto de corte é algum percentil: por exemplo, compra-se os ativos do `\(20º\)` percentil para cima e vende-se aqueles dos `\(80º\)` para baixo.

Nesse post aplico uma mudança no modo como esse fator é construído. Ao invés de comprar/vender os ativos que estão acima/abaixo de um determinado percentil, ordeno as ações de melhor para pior performance e utilizo entropy-pooling para obter o vetor de probabilidades posterior que acomoda a "ordenação" e ao mesmo tempo oferece a menor distorção possível em relação ao vetor de probabilidades original, _equal-weigthed_. 

Ou seja, a cada ponto do tempo o sistema de equações é solucionado:

$$ \sum_{i=1}^I x_i(ln(x_i) - ln(p_i)) $$
`\(s.t.\)`

$$ \sum_{i=1}^I \hat{p_i} (Opinion_{i, 1} - Opinion_{i, 2}) \leq 0 $$ 
$$ \sum_{i=1}^I \hat{p_i} (Opinion_{i, 2} - Opinion_{i, 3}) \leq 0 $$ 

$$ ... $$ 

$$ \sum_{i=1}^I \hat{p_i} (Opinion_{i, j-1} - Opinion_{i, v}) \leq 0 $$ 

Com base no vetor `\(\hat{p}^*\)`, estimo os momentos _condicionais_ - `\(\mu^*\)` e `\(\sigma^*\)` - e uso essas estimativas para construir um portfolio de média-variância com um _tilt_ em momentum. 

Parar trabalhar com estruras em "rolling-window" dentro do `tidyverse` uso a função `rolling_origin` para construir uma `tidy-tibble` que suportará todas as operações daqui para frente:


```r
get_assessment_date <- function(x) {
  map(.x = x$splits, .f = ~ assessment(.x)) |> 
    map(.f = keep, .p = is.Date) |> 
    reduce(bind_rows) |> 
    pull(date)
}

optimin <- returns |> 
  rolling_origin(initial = 845, assess = 1, cumulative = TRUE)
optimin <- optimin |> 
  mutate(optimin, 
         .date       = get_assessment_date(optimin),  
         .analysis   = map(.x = splits, .f = analysis),
         .assessment = map(.x = splits, .f = assessment)) |> 
  select(-splits, -id)
optimin
```

```
## # A tibble: 5 x 3
##   .date      .analysis          .assessment     
##   <date>     <list>             <list>          
## 1 2022-04-22 <tibble [845 x 7]> <tibble [1 x 7]>
## 2 2022-04-29 <tibble [846 x 7]> <tibble [1 x 7]>
## 3 2022-05-06 <tibble [847 x 7]> <tibble [1 x 7]>
## 4 2022-05-13 <tibble [848 x 7]> <tibble [1 x 7]>
## 5 2022-05-20 <tibble [849 x 7]> <tibble [1 x 7]>
```

A coluna `.analysis` contém os dados nos quais conduzo o processo de estimação e coluna `.assessment` os dados _out-of-sample_ - `\(1\)` passo a frente - que utilizarei para calcular a performance do portfolio.

A estimação dos momentos _condicionais_ é realizada com a função `momentum_moments`:


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

  if (class(ep) == "try-error") {
    ep <- prior
  }

  # Compute the conditional moments
  ffp_moments(x = .x, p = ep)

}
```

Dado que o objeto `optimin`está no formato [tidy](https://r4ds.had.co.nz/tidy-data.html) é barbada aplicar a função `momentum_moments` em série para cada elemento contido na coluna `.analysis`[^1]:


```r
optimin <- optimin |>
  mutate(.moments = map(
    .x = .analysis, 
    .f = ~ momentum_moments(.x = .x, .period = 52))
  )
optimin
```

```
## # A tibble: 5 x 4
##   .date      .analysis          .assessment      .moments        
##   <date>     <list>             <list>           <list>          
## 1 2022-04-22 <tibble [845 x 7]> <tibble [1 x 7]> <named list [2]>
## 2 2022-04-29 <tibble [846 x 7]> <tibble [1 x 7]> <named list [2]>
## 3 2022-05-06 <tibble [847 x 7]> <tibble [1 x 7]> <named list [2]>
## 4 2022-05-13 <tibble [848 x 7]> <tibble [1 x 7]> <named list [2]>
## 5 2022-05-20 <tibble [849 x 7]> <tibble [1 x 7]> <named list [2]>
```

Dentro de cada elemento da coluna `.moments` há uma lista com as estimativas `\(\hat{\mu}^*\)` e `\(\hat{\sigma}^*\)` _condicionais_. Veja:


```r
# Primeira linha da coluna `.moments`
optimin$.moments[[1]]
```

```
## $mu
##        IDIV        IBOV        IEEX        IFNC        IMAT        INDX 
## 0.002299663 0.002299663 0.002299663 0.002299663 0.002299663 0.002299663 
## 
## $sigma
##              IDIV         IBOV         IEEX         IFNC         IMAT
## IDIV 0.0010905872 0.0010664901 0.0007776476 0.0011944998 0.0010026888
## IBOV 0.0010664901 0.0012542293 0.0007498298 0.0013498371 0.0012111289
## IEEX 0.0007776476 0.0007498298 0.0008448304 0.0008501776 0.0005792609
## IFNC 0.0011944998 0.0013498371 0.0008501776 0.0018610661 0.0010982520
## IMAT 0.0010026888 0.0012111289 0.0005792609 0.0010982520 0.0019238768
## INDX 0.0008502842 0.0010063115 0.0005884727 0.0010210307 0.0011840690
##              INDX
## IDIV 0.0008502842
## IBOV 0.0010063115
## IEEX 0.0005884727
## IFNC 0.0010210307
## IMAT 0.0011840690
## INDX 0.0010493940
```

As estimativas de média e variância são o principal insumo para construção de um portfolio eficiente bayesiano. A função que conduz o processo de otimização segue abaixo:  


```r
optimal_portfolio   <- function(sigma, mu, .wmin = 0, .wmax = 0.5) {

  num_assets <- ncol(sigma)

  # Full investment constraint
  Aeq  <- matrix(1, 1, num_assets)
  beq  <- 1

  # Assets constraint
  A <- rbind(-diag(num_assets), diag(num_assets))
  b <- c(
    -if (length(.wmax) == 1L) rep(.wmax, num_assets) else .wmax, 
     if (length(.wmin) == 1L) rep(.wmin, num_assets) else .wmin
  )

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

Novamente, o fato do objeto `optimin` estar no formato "tidy" facilita o processo de otimização recursiva:


```r
optimin <- optimin |>
  mutate(.weights = map(
    .x = .moments, 
    .f = ~ optimal_portfolio(
      sigma = .x$sigma, 
      mu    = .x$mu, 
      .wmin = 0, 
      .wmax = 0.4)
    )
  )
optimin
```

```
## # A tibble: 5 x 5
##   .date      .analysis          .assessment      .moments         .weights     
##   <date>     <list>             <list>           <list>           <list>       
## 1 2022-04-22 <tibble [845 x 7]> <tibble [1 x 7]> <named list [2]> <dbl [6 x 1]>
## 2 2022-04-29 <tibble [846 x 7]> <tibble [1 x 7]> <named list [2]> <dbl [6 x 1]>
## 3 2022-05-06 <tibble [847 x 7]> <tibble [1 x 7]> <named list [2]> <dbl [6 x 1]>
## 4 2022-05-13 <tibble [848 x 7]> <tibble [1 x 7]> <named list [2]> <dbl [6 x 1]>
## 5 2022-05-20 <tibble [849 x 7]> <tibble [1 x 7]> <named list [2]> <dbl [6 x 1]>
```

No qual cada elemento dentro da coluna em `.weights` contém um vetor de alocação ótimo:


```r
round(optimin$.weights[[1]], 4)
```

```
##      [,1]
## [1,]  0.2
## [2,]  0.0
## [3,]  0.4
## [4,]  0.0
## [5,]  0.0
## [6,]  0.4
```




```r
optimin <- optimin |>
  mutate(ret = map2_dbl(
    .x = .weights, 
    .y = .assessment, 
    .f = ~ as.matrix(.y[ , -1]) %*% .x)
  )
optimin
```

```
## # A tibble: 5 x 6
##   .date      .analysis          .assessment .moments         .weights        ret
##   <date>     <list>             <list>      <list>           <list>        <dbl>
## 1 2022-04-22 <tibble [845 x 7]> <tibble>    <named list [2]> <dbl[...]> -3.29e-2
## 2 2022-04-29 <tibble [846 x 7]> <tibble>    <named list [2]> <dbl[...]> -1.25e-2
## 3 2022-05-06 <tibble [847 x 7]> <tibble>    <named list [2]> <dbl[...]> -3.17e-2
## 4 2022-05-13 <tibble [848 x 7]> <tibble>    <named list [2]> <dbl[...]>  1.25e-2
## 5 2022-05-20 <tibble [849 x 7]> <tibble>    <named list [2]> <dbl[...]>  6.93e-4
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

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-11-1.png" width="672" />

O gráfico acima mostra a evolução dessa estratégia contra o ibovespa. Adiciono o custo de `\(1,5\%\)` ao ano, que considero alto para uma estratégia "de fator" capaz de ganhar escala. A titulo de comparação, a Black Rock cobra `\(0,3\%\)` a.a. pelo seu [ETF de momentum](https://www.blackrock.com/pt/profissionais/products/270051/ishares-msci-world-momentum-factor-ucits-etf) e o Itaú cobra `\(0,5\%\)` a.a. sobre seus [ETF's de renda variável](https://www.itnow.com.br/).

Há outras questões envolvidas. Por exemplo: a estratégia de momentum demanda um maior turnover do que a estratégia de value-investing, o que contribui para jogar os retornos para baixo. Por outro lado, dividendos e juros sobre capital próprio (típicos de value) geralmente possuem uma maior tributação do que ganhos de capital (momentum). No final, acho que esses efeitos se anulam, embora não tenha certeza.

O tamanho do dataset também é outro ponto importante. Aumentar o universo de ativos disponíveis faz a fronteira eficiente se deslocar para a esquerda e para cima, expandindo as possibilidades de investimento e o retorno da estratégia. Mas esse efeito tem um limite: o erro de estimação não é neutro as mudanças no conjunto de ativos disponíveis e deve aumentar com a diminuição dos graus de liberdade. No geral, acho que a expansão da fronteira domina os erros de estimação, pelo menos para datasets pequenos (entre `\(15-30\)` ativos). Se a estratégia for aplicada em índices, esse número deve ser suficiente para reduzir de maneira significativa o risco idiossincrático. 

[Survivorship bias](https://en.wikipedia.org/wiki/Survivorship_bias) é outro tema que poderia ser levantado, mas se toda a estratégia for montada em cima de índices, não acho que isso chegue a ser um problema.

Por fim, fica em aberto a questão de quão robusta é estratégia de momentum. Para o mercado internacional acredito que o veredito já está dado: [Fact, Fiction and Momentum Investing](https://www.aqr.com/Insights/Research/Journal-Article/Fact-Fiction-and-Momentum-Investing). Para o mercado brasileiro, conheco apenas dois fundos que implementam essa estratégia e ambos estão bem. Mas o fato é que precisamos pesquisar mais. 

No próximo post mostro uma maneira sofisticada de se realizar _backtests_. Embora esse seja um tema espinhoso, o fato que é que adoção de estratégias quantitativas no Brasil é quase muito emocional: o gestor da sua área só vai comprar a ideia se o gráfico for bonito, ainda que reflexo de _overfitting_.
 
[^1]: A função `map` no `R` têm uma função muito simular a função `map` no `Python`. Em ambas as linguagens evita-se um `for loop` explícito.

<!-- . Para o mercado brasileiro,  -->

