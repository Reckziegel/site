
# THEORETICAL AND ECONOMETRIC MODELS --------------------------------------

# Random walk Models ------------------------------------------------------

# Arithmetic Random Walk Model
P0 <- 100
t <- seq(from = 1, to = 1000, by = 1)
prices <- vector('numeric', length = length(t))
error  <- vector('numeric', length = length(t))
mu <- 0.000027
sigma <- 0.03

for (i in seq_along(prices)) {
    
    error[[i]] <- rnorm(1, mean = mu, sd = sqrt(sigma))
    prices[[i]] <- P0 + sum(error)

}

plot(prices, type = 'l')
plot(error, type = 'l')

###
arithmetic_random_walk <- function(
    
    # function arguments
    initial_price = 100, 
    n             = 1000, 
    mu            = 0, 
    sigma         = 1, 
    drift         = 0
    ) {
    
    
    # sanity check
    stopifnot(!is.numeric(initial_price), initial_price <= 0, length(initial_price > 1))
    message('Initial price must be a single positive number.')

    stopifnot(!is.numeric(n), !is.integer(n), n <= 0, length(n) > 1)
    message('n must be an integer higher than 1.')

    stopifnot(!is.numeric(drift), length(drift) > 1)
    message('The drift must be numeric of length 1. That is, a single value.')
    
    # vectors to storage values
    noise               <- vector('numeric', length = length(n))
    deterministic_trend <- vector('numeric', length = length(n))
    variance            <- vector('numeric', length = length(n))
    
    # looping 
    for (i in 1:n) {
        
        deterministic_trend[[i]] <- initial_price + i * drift
        noise[[i]]               <- mu + rnorm(1) * sigma
        variance[[i]]            <- i * var(noise, na.rm = TRUE) 
        
    }
    
    random_walk <- deterministic_trend + cumsum(noise)
    
    # output
    out <- cbind(index = seq(1, n), random_walk, deterministic_trend, noise, variance) 
    
}

x <- arithmetic_random_walk()
x %>% head()

ggplot(x, aes(x = index, y = value)) +
    geom_point() + 
    facet_wrap(~model)

#   -----------------------------------------------------------------------

multivariate_arithmetic_random_walk <- function(
    
    # function arguments
    initial_price = 100,
    n             = 1000,
    paths         = 1000,
    mu            = 0,
    sigma         = 1,
    drift         = 0
    
) {
    
    # matrix to store values
    prices_m <- matrix(NA, nrow = n, ncol = paths)
    
    for (i in 1:paths) { 
        
        prices_m[ , i] <- arithmetic_random_walk(
            initial_price = initial_price, 
            n             = n, 
            mu            = mu, 
            sigma         = sigma, 
            drift         = drift
        )
        
    }
    
    mean_path <- apply(prices_m, 1, mean)
    
    out <- cbind(prices_m, mean_path)
    
}

multivariate_arithmetic_random_walk() 


# Geometric Random Walk Model ---------------------------------------------

geometric_random_walk <- function(
    
    # function arguments
    initial_price = 100, 
    n             = 1000, 
    mu            = 0, 
    sigma         = 1
    
) {
    
    # vectors to store values
    noise  <- vector('numeric', length = n)
    prices <- vector('numeric', length = n)
    prices[[1]] <- initial_price
    
    # looping
    for (i in 2:length(prices)) {
        
        noise[[i]]  <- mu + (rnorm(1) * sigma) / 100 
        
        prices[[i]] <- initial_price * prod(noise[2:i] + 1, na.rm = TRUE)
    
    }
    
    out <- cbind(index = seq(1, n), prices, noise)

}

x <- geometric_random_walk()

plot(x[ , 2], type = 'l')
plot(x[ , 3], type = 'l')


# Lognormal Model ---------------------------------------------------------

lognormal_random_walk <- function(
    
    # function arguments
    initial_price = 100, 
    n             = 1000, 
    mu            = 0.0003968254, 
    sigma         = 1
    
) {
    
    # initialize vectors to storage values
    noise      <- vector('numeric', length = n)
    prices     <- vector('numeric', length = n)
    log_prices <- vector('numeric', length = n)
    avg_prices <- vector('numeric', length = n)

    # looping
    for (i in seq_along(prices)) {
        
        noise[[i]] <- mu + (rnorm(1) * sigma) / 100
    
        log_prices[[i]] <- log(P0) + sum(noise)
    
        prices[[i]] <- exp(log_prices[[i]])
        
        #avg_prices[[i]]  <- initial_price * exp(noise[[i]]) 
    
    }

    # the final output
    out <- cbind(index = seq(1, n), prices, log_prices, noise)
    
}

y <- lognormal_random_walk()

head(y)
plot(y[, 2], type = 'l')
plot(cumsum(y[, 4]), type = 'l', col = 'red')
plot(R_t, type = 'l')

#   -----------------------------------------------------------------------

getSymbols.FRED('SP500', env = globalenv())

sp500 <- tq_get(x = 'SP500', get = 'economic.data')
sp500 %>% 
    fill(., .direction = 'down') %>% 
    ggplot(aes(x = date, y = price)) + 
    geom_line() + 
    theme_tq() + 
    scale_color_tq()



sp500 %>% 
    tq_transmute(
        select     = price,
        mutate_fun = dailyReturn, 
        type       = 'arithmetic'
        ) %>% 
    summarise(
        mean = mean(daily.returns), 
        var  =  var(daily.returns),
        n    = n()
    )


multivariate_arithmetic_random_walk(n_simulations = 10000)


#   -----------------------------------------------------------------------

many_arithmetic_random_walk() %>% 
    ggplot(aes(x = index, y = value, fill = key, alpha = 0.75)) + 
    geom_line(show.legend = FALSE, color = 'grey') + 
    labs(
        title = 'Simulated random walk paths', 
        y     = 'Price', 
        x     = 'Time Index'
    ) + 
    scale_y_continuous(labels = scales::dollar) +
    theme_classic() + 
    geom_line(simulation, aes(x = index, y = mean))

mean_path <- 
    many_arithmetic_random_walk(n_simulations = 50) %>% 
    group_by(index) %>%
    mutate(mean = mean(value)) %>% 
    ungroup()


mean_path %>% 
    ggplot(aes(x = index, y = value, fill = key, alpha = 0.75)) + 
    geom_line(show.legend = FALSE, color = 'grey') + 
    labs(
        title    = 'Simulated random walk paths', 
        y        = 'Price', 
        x        = 'Time Index'
    ) + 
    scale_y_continuous(labels = scales::dollar) +
    geom_line(aes(y       = mean),
              show.legend = FALSE,
              color       = 'steelblue',
              size        = 1
    ) +
    theme_classic()
    

