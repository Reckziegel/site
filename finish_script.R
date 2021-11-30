# # download
# yields <- Quandl::Quandl(code = "USTREASURY/YIELD", type = "xts", start_date = "2010-01-01") 
# yields <- yields[ , c("1 YR", "2 YR", "3 YR", "5 YR", "7 YR", "10 YR")] |> 
#   timetk::tk_tbl(rename_index = "date", silent = TRUE) |> 
#   pivot_longer(cols = -date, names_to = "maturity", values_to = "rates") |> 
#   mutate(rates = rates / 100)
# 
# # analise in sample
# etas <- seq(from = 0.00, to = 0.05, length.out = 200)
# 
# extract_var1_loglik <- function(.eta) {
#   yields |> 
#     shadow_rates(.group_col = maturity, .dbl_col = rates, .eta = .eta) |> 
#     tidyr::pivot_wider(names_from = maturity, values_from = rates) |> 
#     timetk::tk_xts(date_var = "date", silent = TRUE) |> 
#     vars::VAR(type = "const") |> 
#     broom::glance() |> 
#     dplyr::pull("logLik")
# }
# 
# fff <- tibble(eta = etas, loglik = map_dbl(.x = etas, .f = extract_var1_loglik))
# 
# fff |> 
#   ggplot(aes(x = eta, y = loglik)) + 
#   geom_line() + 
#   scale_y_continuous(labels = scales::comma_format())
# 
# # anaalise out of sample
# yields |> 
#   tidyr::pivot_wider(names_from = maturity, values_from = rates) |> 
#   timetk::tk_xts(date_var = "date", silent = TRUE) |> 
#   rsample::rolling_origin(initial = 252 * 4, assess = 1, cumulative = FALSE) |> 
#   dplyr::mutate(.analysis   = purrr::map(.x = splits, .f = rsample::analysis),
#                 .assessment = purrr::map(.x = splits, .f = rsample::assessment), 
#                 .date       = lubridate::as_date(purrr::map_dbl(.x = .assessment, .f = zoo::index)), 
#                 .var        = purrr::map(.x = .analysis, .f = vars::VAR, p = 1, type = "const"))
#   
# 
# 
# 
# 
# get_assessment_date <- function(x) {
#     
#     purrr::map(.x = x$splits, .f = ~ rsample::assessment(.x)) %>%
#       purrr::map(~ timetk::tk_tbl(data = ., rename_index = "date", silent = TRUE)) %>%
#       purrr::map(.f = purrr::keep, .p = lubridate::is.Date) %>%
#       #purrr::map_depth(.x = ., .depth = 2, .f = ~ max(.x)) %>%
#       purrr::reduce(dplyr::bind_rows) %>%
#       dplyr::pull(date)
#   }
#   
