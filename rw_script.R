# packages
library(tidyquant)
library(sweep)
library(timetk)
library(tibbletime)
library(forecast)
library(rvest)
quandl_api_key('wNTJQSHWqSsKsDQprJhb')


# FUNCTION: get_sp500_tickers ---------------------------------------------

get_sp500_tickers <- function() {
    
    raw_get <- function() {
        
        xml2::read_html('https://en.wikipedia.org/wiki/List_of_S%26P_500_companies') %>% 
            rvest:: html_node('table.wikitable') %>% 
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

tickers <- get_sp500_tickers()
tickers

# FUNCTION: download_stocks_from_tickers ----------------------------------

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


stocks <- tickers %>% 
    
    # create the list columns to map over
    dplyr::mutate(wiki_tickers = str_c('WIKI/', ticker_symbol, '.11')) %>% 
    tidyr::nest(wiki_tickers) %>%
    
    # download data from 2009 onwards
    dplyr::mutate(download_tickers = map(
        .x           = data, 
        .f           = download_stocks_from_tickers, 
        order        = 'asc', 
        collapse     = 'monthly', 
        type         = 'raw', 
        start_date   = '2009-01-01', 
        column_index = 11) # column 11 = adjusted close price
    ) %>% 
    
    # reorganize the data in a clean tibble format
    dplyr::select(ticker_symbol, security, gics_sector, gics_sub_industry, download_tickers) %>% 
    tidyr::unnest(download_tickers) %>% 
    
    # exclude all the column-lists with NA 
    dplyr::filter(!is.na(possible_download)) %>% 
    
    # cleaning 
    tidyr::unnest(possible_download) %>% 
    dplyr::rename(prices = `Adj. Close`) %>% 
    dplyr::select(Date, ticker_symbol:gics_sub_industry, prices) %>% 
    
    # calculate returns
    dplyr::group_by(ticker_symbol) %>% 
    tidyquant::tq_mutate(
        select     = prices, 
        mutate_fun = periodReturn,
        col_rename = 'returns',
        period     = 'monthly',
        type       = 'log') %>% 
    dplyr::ungroup()

stocks

# Forecasting steps: ------------------------------------------------------

# 1. coerce the data to ts ------------------------------------------------
stocks_to_ts <- stocks %>% 
    dplyr::group_by(ticker_symbol, security, gics_sector, gics_sub_industry) %>% 
    tidyr::nest(Date, returns) %>% 
    dplyr::mutate(ts = map(
        .x = data, 
        .f = timetk::tk_ts,
        start = 2009,
        frequency = 12)) %>% 
    dplyr::ungroup()

stocks_to_ts


# 2. Modeling a time series -----------------------------------------------
stocks_to_ts_modeling <- stocks_to_ts %>%
    dplyr::group_by(ticker_symbol) %>% 
    dplyr::mutate(model   = map(
        .x         = ts, 
        .f         = forecast::auto.arima, 
        seasonal   = FALSE, 
        stationary = TRUE, 
        max.p      = 2,
        max.q      = 2
    )) %>%
    dplyr::mutate(glance_model = map(
        .x = model, 
        .f = sweep::sw_glance
    )) %>% 
    tidyr::unnest(glance_model) %>% 
    dplyr::ungroup() 

stocks_to_ts_modeling 

stocks_to_ts_modeling %>% 
    dplyr::mutate_if(is.character, as_factor) %>% 
    dplyr::count(model.desc, sort = TRUE) %>% 
    tidyr::separate(
        data = ., 
        col  = model.desc, 
        into = c('arma', 'drift'), 
        sep  = ' ') %>% 
    dplyr::select(arma, n) %>% 
    dplyr::mutate_if(is.character, as_factor) %>% 
    dplyr::mutate(
        arma = fct_reorder(arma, n), 
        n    = if_else(arma == lag(arma), n + lag(n), n)
    ) %>% 
    dplyr::filter(!is.na(n)) %>% 
    dplyr::mutate(percent = n / sum(n)) %>% 
    dplyr::mutate(arma = fct_reorder(arma, percent)) %>% 
    
    ggplot(aes(x = arma, y = percent, color = arma)) + 
    geom_point(show.legend = FALSE) + 
    coord_flip() 
