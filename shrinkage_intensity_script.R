
# libraries ---------------------------------------------------------------

library(tidyquant)
library(sweep)
library(timetk)
library(tibbletime)
library(forecast)
library(rvest)
library(Quandl)


# function to get sp500 tickers -------------------------------------------

get_sp500_tickers <- function() {
    
    raw_get <- function() {
        
        xml2::read_html('https://en.wikipedia.org/wiki/List_of_S%26P_500_companies') %>% 
            rvest::html_node('table.wikitable') %>% 
            rvest::html_table() %>% 
            dplyr::as_tibble()
        
    }
    
    possible_get <- purrr::possibly(
        .f        = raw_get, 
        otherwise = NA
    )
    
    possible_get() %>% 
        dplyr::rename(
            ticker_symbol     = 'Ticker symbol', 
            security          = 'Security',
            sec_filings       = 'SEC filings', 
            gics_sector       = 'GICS Sector',
            gics_sub_industry = 'GICS Sub Industry'
        )
    
}


# download the tickers ----------------------------------------------------

tickers <- get_sp500_tickers()
tickers


# function to download the sp500 stocks from tickers ----------------------

download_stocks_from_tickers <- function(data, ...) {
    
    # tidy eval
    dots_expr <- dplyr::quos(...)
    
    # defensive programming
    possible_quandl <- purrr::possibly(Quandl::Quandl, NA)
    
    # data manipulation
    data %>% 
        dplyr::mutate(possible_download = purrr::map(
            .x = ., 
            .f = ~ possible_quandl(., !!! dots_expr)
        )
    )
    
}


# download the sp500 stocks -----------------------------------------------

stocks <- tickers %>% 
    
    # create the list columns to map over
    dplyr::mutate(wiki_tickers = str_c('WIKI/', ticker_symbol, '.11')) %>% 
    tidyr::nest(wiki_tickers) %>%
    
    # download data from 2009 onwards
    dplyr::mutate(download_tickers = map(
        .x           = data, 
        .f           = download_stocks_from_tickers, # the custom function is here!
        order        = 'asc', 
        collapse     = 'daily', 
        type         = 'raw', 
        start_date   = '2000-01-01', 
        column_index = 11) # column 11 = adjusted close price
    ) %>% 
    
    # reorganize the data in a clean tibble format
    dplyr::select(ticker_symbol, 
                  security, 
                  gics_sector, 
                  gics_sub_industry, 
                  download_tickers
    ) %>% 
    tidyr::unnest(download_tickers) %>% 
    
    # exclude all the column-lists with NA 
    dplyr::filter(!is.na(possible_download)) %>% 
    
    # cleaning 
    tidyr::unnest(possible_download) %>% 
    dplyr::rename(prices = `Adj. Close`) %>% 
    dplyr::select(Date, 
                  ticker_symbol, 
                  prices
    ) %>% 
    
    # calculate returns
    dplyr::group_by(ticker_symbol) %>% 
    tidyquant::tq_mutate(
        select     = prices, 
        mutate_fun = periodReturn,
        col_rename = 'returns',
        period     = 'daily',
        type       = 'log'
    ) %>% 
    dplyr::ungroup()

stocks


# Some assets have only a small number for n() ----------------------------

stocks %>%
    dplyr::select(Date, ticker_symbol, returns) %>% 
    dplyr::group_by(ticker_symbol) %>% 
    dplyr::summarise(
        n          = dplyr::n(), 
        first_date = min(Date)
    ) %>% 
    dplyr::arrange(dplyr::desc(first_date))


# A simple way to solve this problem is to filter stocks under a prespecified level

stocks_filtered <- stocks %>% 
    dplyr::select(Date, ticker_symbol, returns) %>%
    dplyr::group_by(ticker_symbol) %>% 
    dplyr::mutate(n = dplyr::n()) %>% 
    dplyr::filter(n > 4000) %>% 
    dplyr::select(-n) %>% 
    dplyr::ungroup() %>% 
    tidyr::spread(ticker_symbol, returns) %>% 
    tidyr::drop_na()

stocks_filtered


# honey shrunk function (translated from MATLAB) --------------------------

honey_shrunk <- function(R, shrink = NULL) {
    
    # "Honey, I Shrunk the Sample Covariance Matrix"
    # http://www.ledoit.net/honey.pdf
    # https://www.econ.uzh.ch/en/people/faculty/wolf/publications.html
    
    n  <- nrow(R)
    p  <- ncol(R)
    mu <- apply(R, 2, mean)
    R  <- R - matrix(rep(mu, times = n), ncol = p, byrow = TRUE)
    
    # Covariancia amostral usando  (R - mean)
    sample  <- (1 / n) * (t(R) %*% R)
    
    # Prior
    var     <- matrix(diag(sample), ncol = 1)
    sqrtvar <- sqrt(var)
    tmpMat  <- matrix(rep(sqrtvar, times = p), nrow = p)
    rBar    <- (sum(sum(sample / (tmpMat * t(tmpMat)))) - p) / (p * (p - 1))
    prior   <- rBar * tmpMat * t(tmpMat)
    diag(prior) <- var
    
    if (is.null(shrink)) {
        
        # pi-hat
        y      <- R ^ 2
        phiMat <- t(y) %*% y / n - 2 * (t(R) %*% R) * sample / n + sample ^ 2
        phi    <- sum(phiMat)
        
        # Wrho-hat
        aux1     <- (t(R ^ 3) %*% R) / n
        help     <- t(R) %*% R / n
        helpDiag <- matrix(diag(help), ncol = 1)
        aux2     <- matrix(rep(helpDiag, times = p), ncol = p) * sample
        aux3     <- help * matrix(rep(var, times = p), ncol = p)
        aux4     <- matrix(rep(var, times = p), ncol = p) * sample
        thetaMat <- aux1 - aux2 - aux3 + aux4
        diag(thetaMat) <- 0
        rho      <- sum(diag(phiMat)) + rBar * sum(sum(((1 / sqrtvar) %*% t(sqrtvar)) * thetaMat))
        
        # gamma-hat
        gamma <- norm(sample - prior, "F")^2
        
        # Shrinkage constant
        kappa     <- (phi - rho) / gamma
        shrinkage <- max(0, min(1, kappa / n))
        
    } else {
        
        shrinkage <- shrink
        
    }
    
    # Estimador
    sigma <- shrinkage * prior + (1 - shrinkage) * sample
    out   <- list(cov = sigma, prior = prior, shrinkage = shrinkage)
    
    return(out)
    
}


# for loop to find different shrinkage values -----------------------------

rolling_windows <- c(90, 252, 756, 1260)
rollapply_list <- vector('list', length = length(rolling_windows))

for (i in seq_along(rolling_windows)) {
    
    rollapply_list[[i]] <- stocks_filtered %>% 
        tidyquant::tq_transmute(
            mutate_fun = rollapply, 
            width      = rolling_windows[[i]], 
            FUN        = function(x) honey_shrunk(x[ , -1])$shrinkage, 
            by.column  = FALSE, 
            col_rename = stringr::str_c('shrinkage_intensity_', rolling_windows[[i]])
        )    
    
}


# data wrangling and the 'intensity' plot ---------------------------------

rollapply_list[[1]] %>% 
    dplyr::left_join(rollapply_list[[2]], by = 'Date') %>% 
    dplyr::left_join(rollapply_list[[3]], by = 'Date') %>% 
    dplyr::left_join(rollapply_list[[4]], by = 'Date') %>% 
    dplyr::rename(
        `90 days`   = 'shrinkage_intensity_1', 
        `252 days`  = 'shrinkage_intensity_2', 
        `756 days`  = 'shrinkage_intensity_3', 
        `1260 days` = 'shrinkage_intensity_4') %>% 
    tidyr::gather(Intensity, values, -Date) %>% 
    dplyr::mutate_if(purrr::is_character, forcats::as_factor) %>% 
    ggplot2::ggplot(aes(x = Date, y = values, color = Intensity)) + 
    ggplot2::geom_line(size = 1) +
    ggplot2::labs(
        title    = 'Shrinkage Intensity',
        subtitle = 'Rolling Windows: 3 months, 1 year, 3 years and 5 years',
        y        = '', 
        x        = ''
    ) + 
    ggplot2::theme_classic() + 
    tidyquant::scale_color_tq()


# data wrangling and the 'amount of error' plot ---------------------------

rollapply_list[[1]] %>% 
    dplyr::left_join(rollapply_list[[2]], by = 'Date') %>% 
    dplyr::left_join(rollapply_list[[3]], by = 'Date') %>% 
    dplyr::left_join(rollapply_list[[4]], by = 'Date') %>% 
    dplyr::rename(
        `90 days`   = 'shrinkage_intensity_1', 
        `252 days`  = 'shrinkage_intensity_2', 
        `756 days`  = 'shrinkage_intensity_3', 
        `1260 days` = 'shrinkage_intensity_4'
        ) %>% 
    tidyr::gather(Intensity, values, -Date) %>% 
    dplyr::mutate_if(purrr::is_character, forcats::as_factor) %>% 
    dplyr::group_by(Intensity) %>% 
    dplyr::mutate(Error = values / (1 - values)) %>% 
    ggplot2::ggplot(aes(x = Date, y = Error, color = Intensity)) + 
    ggplot2::facet_wrap(~ Intensity, scales = 'free_y') + 
    ggplot2::geom_line(show.legend = FALSE, size = 1) +
    ggplot2::labs(
        title    = 'Estimation Error',
        subtitle = 'Rolling Windows: 3 months, 1 year, 3 years and 5 years',
        y        = '', 
        x        = ''
    ) + 
    ggplot2::theme_classic() + 
    tidyquant::scale_color_tq()
