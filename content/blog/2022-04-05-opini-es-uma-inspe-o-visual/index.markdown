---
title: Opiniões - Reprecificação em real-time
author: Bernardo Reckziegel
date: '2022-04-05'
slug: []
categories: []
tags:
  - shiny
  - ggplot2
meta_img: images/image.png
description: Description for the page
---

Hoje anexo um [shiny app](https://www.shinyapps.io/) construído com o objetivo de mostrar como entropy-pooling _distorce_ as funções de densidade de probabilidade.

Ao usuário é permitido escolher os parâmetros de locação, `\(\mu\)`, e dispersão, `\(\sigma\)`. 

A distribuição _posterior_ é comparada com uma _prior_ oriunda do processo `\(X \sim \mathcal{N}(0,\ 0.2^2)\)` com `\(n = 5.000\)`.

<iframe src="https://reckziegel.shinyapps.io/Entropy_Pooling?showcase=0" width="672" height="1390px" data-external="1"></iframe>

Perceba que ao contrário dos métodos bayesianos tradicionais, a otimização via entropy-pooling acontece numa fração de segundos, permitindo que as opiniões sejam reprecificadas em _real-time_. 

Isso é possível porque entropy-pooling reprecifica apenas as _probabilidades_ de cada evento, e não os eventos em si. Ou seja, a estimação acontece uma única vez. 

Bye-bye MCMC! :wave:
