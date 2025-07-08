# WaveQTEX: Data Documentation
# Sample datasets and data loading utilities
# Version 1.0.0

#' Sample Stock Returns Data
#'
#' A dataset containing daily log returns for 10 major global stock market indices
#' covering a representative period for WaveQTE analysis demonstration.
#'
#' @format A data frame with 1000 rows and 10 columns:
#' \describe{
#'   \item{date}{Date of observation}
#'   \item{SP500}{S&P 500 log returns (USA)}
#'   \item{Nikkei}{Nikkei 225 log returns (Japan)}
#'   \item{DAX}{DAX log returns (Germany)}
#'   \item{FTSE}{FTSE 100 log returns (UK)}
#'   \item{CAC}{CAC 40 log returns (France)}
#'   \item{SSE}{SSE Composite log returns (China)}
#'   \item{BSE}{BSE Sensex log returns (India)}
#'   \item{MOEX}{MOEX log returns (Russia)}
#'   \item{Bovespa}{Bovespa log returns (Brazil)}
#'   \item{IPC}{IPC log returns (Mexico)}
#' }
#' @source Simulated data based on typical characteristics of global equity indices
"sample_stock_returns"

#' Crisis Indicator Data
#'
#' A dataset containing binary indicators for known financial crisis periods
#' corresponding to the sample stock returns data for model validation.
#'
#' @format A data frame with 1000 rows and 2 columns:
#' \describe{
#'   \item{date}{Date of observation}
#'   \item{crisis_indicator}{Binary indicator (1 = crisis period, 0 = normal)}
#' }
#' @source Based on historical financial crisis periods
"crisis_indicators"

#' Market Classifications
#'
#' A dataset containing regional and development classifications for the markets
#' included in the sample data for enhanced analysis capabilities.
#'
#' @format A data frame with 10 rows and 4 columns:
#' \describe{
#'   \item{market}{Market identifier}
#'   \item{country}{Country name}
#'   \item{region}{Geographic region}
#'   \item{development_level}{Development classification (Developed/Emerging)}
#' }
"market_classifications"