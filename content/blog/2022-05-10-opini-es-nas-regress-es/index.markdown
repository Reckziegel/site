---
title: Opiniões nas Regressões
author: 'Bernardo Reckziegel '
date: '2022-05-13'
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

Uma consequência bacana de se trabalhar com [entropy-pooling](https://papers.ssrn.com/sol3/papers.cfm?abstract_id=1213325) é que as _probabilidades flexíveis_ também podem ser utilizadas para _condicionar_ os betas das regressões lineares. 

Nos posts anteriores mostrei como utilizar a biblioteca [ffp](https://reckziegel.github.io/FFP/) para adicionar opiniões em diversos elementos das distribuições multivariadas ([retornos](https://www.bernardo.codes/blog/2022-03-30-opini-es-nos-retornos-esperados/), [volatilidades](https://www.bernardo.codes/blog/2022-04-04-opini-es-nas-volatilidades/), [correlações](https://www.bernardo.codes/blog/2022-04-06-opini-o-nas-correla-es/), [margens](https://www.bernardo.codes/blog/2022-04-12-opini-es-nas-margens/), [copulas](https://www.bernardo.codes/blog/2022-04-27-opini-es-nas-copulas/), etc.). Hoje mostro como combinar os objetos da classe `ffp` com a famosa função `lm`. 

Os pacotes utilizados são:


```r
library(tidyverse)      # Onde tudo é mais fácil...
library(lubridate)      # Manipulação de datas
library(quantmod)       # Download de dados financeiros
library(timetk)         # Coerção entre diferentes estruturas 
library(readxl)         # Leitura de arquivos em excel
library(broom)          # Visualização das Regressões
library(ggdist)         # Visualização das Regressões
library(distributional) # Visualização das Regressões
library(ffp)            # Probabilidades Flexíveis
```

Deixo de lado o dataset `EuStockMarkets` para brincar um pouquinho com o ibovespa e os _risk-factors_ calculados pelo time do [NEFIN](https://nefin.com.br).

Os dados do índice Ibovespa são coletados diretamente via [Yahoo Finance](https://finance.yahoo.com/quote/%5EBVSP/history?p=%5EBVSP):


```r
ibov <- getSymbols(Symbols = "^BVSP", auto.assign = FALSE) |> 
  tk_tbl(preserve_index = TRUE, rename_index = "date") |> 
  select(date, ibov = contains("Adjusted")) |> 
  transmute(date, ibov = c(0, diff(log(ibov))))
ibov
```

```
## # A tibble: 3,807 x 2
##    date           ibov
##    <date>        <dbl>
##  1 2007-01-02  0      
##  2 2007-01-03 -0.0209 
##  3 2007-01-04 -0.00961
##  4 2007-01-05 -0.0412 
##  5 2007-01-08  0.0138 
##  6 2007-01-09 -0.0194 
##  7 2007-01-10  0.00780
##  8 2007-01-11  0.00786
##  9 2007-01-12  0.00991
## 10 2007-01-15 -0.00409
## # ... with 3,797 more rows
```

Enquanto os dados do NEFIN são disponibilizados no endereço https://nefin.com.br/data/risk_factors.html. Esse link contém url's estáveis, o que facilita a coleta das informações: 


```r
urls <- list(market = "https://nefin.com.br/resources/risk_factors/Market_Factor.xls",
             smb    = "https://nefin.com.br/resources/risk_factors/SMB_Factor.xls",
             hml    = "https://nefin.com.br/resources/risk_factors/HML_Factor.xls",
             wml    = "https://nefin.com.br/resources/risk_factors/WML_Factor.xls",
             iml    = "https://nefin.com.br/resources/risk_factors/IML_Factor.xls",
             risk_free = "https://nefin.com.br/resources/risk_factors/Risk_Free.xls")

destfiles <- list("market.xls", "smb.xls", "hml.xls", "wml.xls", "iml.xls", "risk_free.xls")

risk_factors <- map2(.x = urls,
                     .y = destfiles,
                     .f = ~ curl::curl_download(url = .x, destfile = .y)) |> 
  map(read_excel) |> 
  reduce(left_join, by = c("year", "month", "day")) |> 
  mutate(date = make_date(year = year, month = month, day = day)) |> 
  rename_all(str_to_lower) |> 
  rename(rm = "rm_minus_rf", rf = "risk_free") |> 
  select(date, everything(), -year, -month, -day, -rf)
risk_factors
```

```
## # A tibble: 5,278 x 6
##    date              rm        smb       hml       wml      iml
##    <date>         <dbl>      <dbl>     <dbl>     <dbl>    <dbl>
##  1 2001-01-02  0.00660   0.000524   0.0655   -0.00631   0.0141 
##  2 2001-01-03  0.0624    0.00539    0.00939  -0.0286    0.00451
##  3 2001-01-04 -0.000310  0.00669   -0.00233  -0.000946 -0.00923
##  4 2001-01-05 -0.0128    0.00352   -0.00240   0.00598   0.0251 
##  5 2001-01-08  0.00398   0.00788    0.00195  -0.00410  -0.00118
##  6 2001-01-09  0.0200    0.00593   -0.000916  0.00956  -0.00398
##  7 2001-01-10 -0.00437   0.0132     0.0125   -0.000691  0.0221 
##  8 2001-01-11  0.00469  -0.0108    -0.00345  -0.00306  -0.00228
##  9 2001-01-12 -0.00678   0.00662    0.00347   0.00602   0.0103 
## 10 2001-01-15  0.00511   0.0000672 -0.000760 -0.0138    0.00689
## # ... with 5,268 more rows
```

Essa etapa é finalizada com a unificação dos objetos `ibov` e `risk_factors` em uma única `tibble`, que contem os retornos entre `03-01-2011` e `29-04-2022`:


```r
data <- left_join(ibov, risk_factors, by = "date") |> 
  filter(date > "2011-01-02") |> 
  na.omit()
data
```

```
## # A tibble: 2,790 x 7
##    date            ibov       rm       smb       hml       wml      iml
##    <date>         <dbl>    <dbl>     <dbl>     <dbl>     <dbl>    <dbl>
##  1 2011-01-03  0.00944   0.00776 -0.00653  -0.00590  -0.00569  -0.00909
##  2 2011-01-04  0.00508   0.00443 -0.00546   0.00780  -0.00671  -0.00151
##  3 2011-01-05  0.0109    0.00891 -0.0128    0.00615  -0.000305 -0.0100 
##  4 2011-01-06 -0.00723  -0.00824  0.00410   0.00214  -0.00407   0.00402
##  5 2011-01-07 -0.00742  -0.0102   0.000620 -0.000881  0.00668   0.00178
##  6 2011-01-10  0.000999  0.00328 -0.000814 -0.00610  -0.00341   0.00183
##  7 2011-01-11  0.00421   0.00407 -0.00439  -0.00135   0.00140  -0.00331
##  8 2011-01-12  0.0170    0.0133  -0.00522   0.0118   -0.00176  -0.00248
##  9 2011-01-13 -0.0128   -0.0120   0.000618  0.00307   0.00322   0.00392
## 10 2011-01-14  0.00309   0.00286  0.00396   0.00859   0.00118   0.00459
## # ... with 2,780 more rows
```

Com isso, o foco se direciona para o cálculo das probabilidades posteriores. Em particular, de dois métodos que estão no pacote `ffp` e ainda não foram discutidos nos posts anteriores: `crisp` e `exp_decay`. 

O objetivo da função `crisp` é restringir a análise para alguma situação macro/micro específica, dando `\(100\%\)` de peso para as condições que atendem a restrição e `\(0\%\)`% para aquelas não atendem:

`$$p = 
\begin{cases}
    1 & \text{se } x_t \text{ atende a restrição} \\
    0 & \text{se } x_t \text{ não atende a restrição}
\end{cases}$$`

Uma vez que as _dummies_ sejam computadas, o vetor `\(p\)` é normalizado para garantir que a soma do processo seja igual a `\(1\)`. 


```r
crisp_positive <- crisp(data$ibov, data$ibov > 0)
crisp_negative <- crisp(data$ibov, data$ibov < 0)
```

No exemplo acima, a análise é condicionada para os períodos que o ibovespa teve retornos acima e abaixo de zero. 

Já a função `exp_decay` busca dar mais importância para observações recentes por meio da relação de alisamento exponencial:

$$ p = e^{-\lambda(T-t)}$$
Em que `\(\lambda\)` é um parâmetro de decaimento que determina o grau de persistência das últimas observações. Via de regra, quando maior o parâmetro `\(\lambda\)`, menor o número de cenários efetivamente considerados. Utilizo `\(\lambda = 0.001359\)`, cuja a meia vida é próxima de 2 anos[^1]:


```r
exp_smoothing  <- exp_decay(data$ibov, 0.001359)
```

<!-- Caso tenha interesse em entender o contexto em que as probabilidades podem ser utilizadas para geração de cenários e risk-management, recomendo o paper [Historical Scenarios with Fully Flexible Probabilities](https://papers.ssrn.com/sol3/papers.cfm?abstract_id=1696802), mais uma grande contribuição do Meucci.  -->

Com base nessas probabilidades - ou qualquer outro vetor que você queira - é possível estimar os betas _condicionais_. A função `lm` possui um argumento pouco conhecido, mas incrivelmente útil, chamado `weights`. Quando esse argumento não é nulo, a função `lm` conduz a estimação por meio dos [mínimos quadrados ponderados](https://en.wikipedia.org/wiki/Weighted_least_squares#:~:text=Weighted%20least%20squares%20(WLS)%2C,is%20incorporated%20into%20the%20regression) (MQP), de modo que as observações com maior peso ganham uma importância relativa maior no processo de otimização:


```r
fml <- as.formula(ibov ~ rm + smb + hml + wml + iml)

unconditional <- lm(formula = fml, data = data) |> 
  tidy() |> 
  mutate(Cenário = "Incondicional")

positive <- lm(formula = fml, data = data, weights = crisp_positive) |> 
  tidy() |> 
  mutate(Cenário = "Otimista")

negative <- lm(formula = fml, data = data, weights = crisp_negative) |> 
  tidy() |> 
  mutate(Cenário = "Pessimista")

exponential <- lm(formula = fml, data = data, weights = exp_smoothing) |> 
  tidy() |> 
  mutate(Cenário = "Exponencial")

regression <- bind_rows(unconditional, positive, negative, exponential)
regression
```

```
## # A tibble: 24 x 6
##    term         estimate std.error statistic  p.value Cenário      
##    <chr>           <dbl>     <dbl>     <dbl>    <dbl> <chr>        
##  1 (Intercept)  0.000165 0.0000454      3.63 2.85e- 4 Incondicional
##  2 rm           1.05     0.00366      288.   0        Incondicional
##  3 smb          0.126    0.0104        12.0  1.50e-32 Incondicional
##  4 hml          0.0769   0.00629       12.2  1.80e-33 Incondicional
##  5 wml         -0.0621   0.00498      -12.5  9.53e-35 Incondicional
##  6 iml         -0.227    0.0110       -20.6  8.99e-88 Incondicional
##  7 (Intercept)  0.00113  0.0000638     17.7  7.94e-67 Otimista     
##  8 rm           0.989    0.00504      196.   0        Otimista     
##  9 smb          0.112    0.0104        10.8  1.78e-26 Otimista     
## 10 hml          0.0691   0.00618       11.2  2.32e-28 Otimista     
## # ... with 14 more rows
```

O output dessas regressões é mais fácil de ser digerido visualmente: 


```r
regression  |> 
  mutate(term = fct_reorder(as_factor(term), estimate)) |> 
  filter(term != "rm", term != "(Intercept)") |> 
  ggplot(aes(y = term, group = `Cenário`, color = `Cenário`, fill = `Cenário`) ) +
  stat_halfeye(aes(
    xdist = dist_student_t(df = 2784, mu = estimate, sigma = std.error)), alpha = 0.75
    ) + 
  geom_vline(xintercept = 0, size = 1, color = "grey", linetype = 2) + 
  scale_fill_viridis_d(end = 0.75, option = "C") + 
  scale_color_viridis_d(end = 0.75, option = "C") + 
  theme(legend.position = "bottom") + 
  labs(title    = "Regressão Linear via MQP-Entropy-Pooling",
       subtitle = "Distribuição dos parâmetros estimados sob diferentes cenários",
       x        = NULL, 
       y        = NULL)
```

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-8-1.png" width="672" />

<!-- O Ibovespa responde de `\(1\)` para `\(1\)` ao fator de mercado em praticamente qualquer cenário. Assim, excluí esse item para deixar o gráfico mais limpo. -->

A grosso modo, o Ibovespa é um índice com um _tilt_ em direção a empresas de _value_ com média capitalização. A entrada de novas companhias e a diminuição da importância de empresas tradicionais (PETR4, VALE3, BBAS3, etc.) explica parcialmente o direcionamento em relação ao fator SMB. A exposição a momentum é baixa e a liquidez média bastante elevada (prêmio de iliquidez negativo).

Sob o cenário definido como "pessimista", o Ibovespa parece ficar ainda mais exposto ao fator SMB, possivelmente porque em momentos de _sell-off_ as empresas cíclicas domésticas (que têm menor capitalização) são as primeiras a tomar porrada. Já no cenário "otimista", o _drift_ em direção as empresas de baixa capitalização diminui, sem muito impacto sobre os demais fatores. 

Também chama atenção como a performance recente - "exponencial" - diverge do comportamento médio dos últimos `\(10\)` anos.

É legal ver como uma análise que começa de maneira totalmente bayesiana consegue facilmente conversar com o mundo frequentista, sem muitas complicações.


[^1]: 510 dias para ser mais exato. Veja com o comando: `half_life(0.001359)`.
