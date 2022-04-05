---
title: Opiniões - uma inspeção visual
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

Hoje anexo um [shiny app](https://www.shinyapps.io/) construído com o objetivo de tornar mais fácil entender como entropy-pooling _distorce_ as funções de densidade de probabilidade.

Ao usuário é permitido escolher o tamanho da amostra, `\(n\)`, os parâmetros de locação, `\(\mu\)`, e dispersão, `\(\sigma\)`.

A distribuição _posterior_ é comparada com uma _prior_ oriunda do processo `\(X \sim \mathcal{N}(0,\ 0.2)\)`.

<iframe src="https://reckziegel.shinyapps.io/Entropy_Pooling?showcase=0" width="672" height="2050px" data-external="1"></iframe>

No próximo post continuaremos explorando o fantástico mundo bayesiano, em particular as correlações.
