
# ------------------------------------------------------------------------------
# FUNCTIONS TO PULL FINANCIAL DATA
# ------------------------------------------------------------------------------

make_url <- function(
  ticker,
  period_type = 'TIME_SERIES_MONTHLY_ADJUSTED',
  api_key = readLines('api_key'), # save your API key in api_key file
  data_type = '&datatype=csv' # empty character '' for JSON
) {
  api_url <- paste0(
    'https://www.alphavantage.co/query?function=', period_type,
    '&symbol=', ticker,
    '&apikey=', api_key,
    data_type
  )
  return(api_url)
}

pull_fund_data <- function(fund_list = funds) {
  fund_data <- data.frame(
    timestamp = character(),
    open = numeric(),
    high = numeric(),
    low = numeric(),
    close = numeric(),
    volume = numeric(),
    ticker = character(),
    source = character(),
    category = character(),
    fund_family = character()
  )
  
  for (i in 1:nrow(fund_list)) {
    if(i > 1) Sys.sleep(15)
    tryCatch({
      download.file(make_url(fund_list$Ticker[i]), destfile = 'temp.txt', quiet = TRUE)
    }, warning = function(w) {
      cat(paste0(
        ifelse(i > 1, '\n', ''),
        fund_list$Ticker[i], ': API error, fund skipped'
      ))
      next
    })
    temp_df <- read.table('temp.txt', header = TRUE, sep = ',')
    temp_df <- temp_df %>% 
      mutate(
        ticker = fund_list$Ticker[i],
        source = fund_list$Source[i],
        category = fund_list$Category[i],
        fund_family = fund_list$Family[i]
      )
    fund_data <- rbind(fund_data, temp_df)
    cat(paste0(
      ifelse(i > 1, '\n', ''),
      fund_list$Ticker[i], ': ',
      nrow(temp_df), ' rows returned'
    ))
    rm(temp_df)
    file.remove('temp.txt')
  }
  
  fund_data$timestamp <- ymd(fund_data$timestamp)
  
  return(fund_data)
}

# ------------------------------------------------------------------------------
# FUNCTIONS TO DO FINANICAL CALCULATIONS
# ------------------------------------------------------------------------------

## SET UP SOME TIME PERIODS FOR COMPARISONS

get_big_dates <- function(df) {
  tdates <- df$timestamp
  current_date <- max(tdates)
  currentyr <- if_else(
    month(current_date) == 1,
    tdates[month(tdates) == 12 & year(tdates) == year(current_date) - 1],
    tdates[month(tdates) == month(current_date) - 1 & year(tdates) == year(current_date)]
  )
  back1yr <- tdates[month(tdates) == month(currentyr) & year(tdates) == (year(currentyr) - 1)]
  back3yr <- tdates[month(tdates) == month(currentyr) & year(tdates) == year(currentyr) - 3]
  back5yr <- tdates[month(tdates) == month(currentyr) & year(tdates) == year(currentyr) - 5]
  back10yr <- tdates[month(tdates) == month(currentyr) & year(tdates) == year(currentyr) - 10]
  big_dates <- list(
    currentyr = currentyr, 
    back1yr = back1yr, 
    back3yr = back3yr, 
    back5yr = back5yr, 
    back10yr = back10yr
  )
  return(big_dates)
}

durations <- list(
  vars = c('back1yr', 'back3yr', 'back5yr', 'back10yr'),
  ints = c(1, 3, 5, 10)
)

## SOME HELPER FUNCTIONS FOR OTHER CALCS

dividend_rebuys <- function(df, now, then) {
  addl_shares <- df %>% 
    filter(timestamp > then, timestamp <= now) %>% 
    summarise(addl_shares = sum(dividend.amount / close)) %>% 
    .$addl_shares
  return(addl_shares)
}

value_gain_tot <- function(df, now, then, addl_shares) {
  start_val <- filter(df, timestamp == then)$close
  end_val <- filter(df, timestamp == now)$close
  return((end_val * (1 + addl_shares)) - start_val)
}

## ANNUALIZED RETURNS

annualized_returns_calc <- function() {
  annualized_returns <- data.frame(
    source = character(),
    category = character(),
    fund_family = character(),
    ticker = character(),
    ar1 = numeric(),
    ar3 = numeric(),
    ar5 = numeric(),
    ar10 = numeric()
  )
  
  for (i in unique(fund_data$ticker)) {
    tdf <- filter(fund_data, ticker == i)
    new_ticker <- data.frame(
      source = tdf$source[1],
      category = tdf$category[1],
      fund_family = tdf$fund_family[1],
      ticker = i,
      ar1 = as.numeric(NA),
      ar3 = as.numeric(NA),
      ar5 = as.numeric(NA),
      ar10 = as.numeric(NA)
    )
    
    big_dates <- get_big_dates(tdf)
    
    for (j in 1:4) {
      now <- big_dates$currentyr
      then <- getElement(big_dates, durations$vars[j])
      if (length(then) == 0) next
      addl_shares <- dividend_rebuys(tdf, now, then)
      val_gain_tot <- value_gain_tot(tdf, now, then, addl_shares)
      start_val <- filter(tdf, timestamp == then)$close
      ar <- ((start_val + val_gain_tot) / start_val) ^ (1 / durations$ints[j]) - 1
      new_ticker[, j+4] <- ar * 100
    }
    
    annualized_returns <- rbind(annualized_returns, new_ticker)
  }
  
  return(annualized_returns)
}

## YEARLY RETURNS

yearly_returns_calc <- function() {
  yearly_returns <- data.frame(
    source = character(),
    category = character(),
    fund_family = character(),
    ticker = character(),
    year = numeric(),
    return = numeric()
  )
  
  for (i in unique(fund_data$ticker)) {
    tdf <- filter(fund_data, ticker == i)
    tyears <- unique(year(tdf$timestamp))
    for (j in tyears) {
      if (j == min(tyears)) next
      ydf <- filter(tdf, year(timestamp) == j | (year(timestamp) == j - 1 & month(timestamp) == 12))
      now <- max(ydf$timestamp)
      then <- min(ydf$timestamp)
      addl_shares <- dividend_rebuys(tdf, now, then)
      val_gain_tot <- value_gain_tot(tdf, now, then, addl_shares)
      start_val <- filter(tdf, timestamp == then)$close
      yreturn <- (val_gain_tot / start_val) * 100
      new_year <- data.frame(
        source = ydf$source[1],
        category = ydf$category[1],
        fund_family = ydf$fund_family[1],
        ticker = i,
        year = j,
        return = yreturn
      )
      yearly_returns <- rbind(yearly_returns, new_year)
    }
  }
  
  return(yearly_returns)
}

## CUMULATIVE DIVIDENDS

cumulative_dividends_calc <- function() {
  cumulative_dividends <- fund_data %>% 
    mutate(year = year(timestamp)) %>% 
    filter(year >= max(year) - 11) %>% 
    group_by(source, category, fund_family, ticker, year) %>% 
    summarise(dividends = sum(dividend.amount)) %>% 
    mutate(cum_dividends = cumsum(dividends)) %>% 
    ungroup()
  return(cumulative_dividends)
}



# ------------------------------------------------------------------------------
# FUNCTIONS FOR PLOTTING
# ------------------------------------------------------------------------------

## create consistent colors

good_colors <- function(data = fund_data, curr_cat = cats[i]) {
  i_tickers <- fund_data %>% 
    filter(category == cats[i]) %>% 
    .$ticker %>% 
    unique() %>% 
    as.character() %>% 
    factor()
  i_colors <- c(brewer.pal(8, 'Dark2'), brewer.pal(9, 'Set1'))[1:length(i_tickers)]
  names(i_colors) <- levels(i_tickers)
  color_info <- list(i_tickers = i_tickers, i_colors = i_colors)
  return(color_info)
}

## annualized returns plot

annualized_returns_plot <- function(data = annualized_returns, curr_cat = cats[i], the_colors = color_info) {
  p <- data %>% 
    filter(category == curr_cat) %>% 
    gather(key = 'term', value = 'ann_return', ar1:ar10) %>% 
    mutate(
      term = factor(
        paste0(gsub('[a-z]*', '', term), ' yr(s)'), 
        ordered = TRUE,
        levels = c('1 yr(s)', '3 yr(s)', '5 yr(s)', '10 yr(s)')
      ),
      ticker = factor(ticker, levels = levels(the_colors$i_tickers))
    ) %>% 
    filter(!is.na(ann_return)) %>% 
    ggplot(aes(x = term, y = ann_return, linetype = source, fill = ticker)) +
    geom_bar(stat = 'identity', position = 'dodge', color = 'black') +
    scale_fill_manual(values = the_colors$i_colors) +
    labs(
      title = 'Annualized Returns',
      y = 'Return (%)',
      x = 'Period Duration'
    ) +
    theme(plot.title = element_text(face = 'bold'))
  return(p)
}

## value time series plot

value_trend_plot <- function(data = fund_data, curr_cat = cats[i], the_colors = color_info) {
  p <- data %>% 
    filter(
      category == curr_cat,
      year(timestamp) >= max(year(fund_data$timestamp)) - 11
    ) %>% 
    mutate(ticker = factor(ticker, levels = levels(the_colors$i_tickers))) %>% 
    ggplot(aes(x = timestamp, y = close, linetype = source, color = ticker)) +
    geom_line() +
    scale_colour_manual(values = the_colors$i_colors) +
    labs(
      title = 'Fund Value Trend',
      y = 'Closing Price ($)',
      x = 'Year'
    ) +
    theme(plot.title = element_text(face = 'bold'))
  return(p)
}


## cumulative dividends plot

cumulative_dividends_plot <- function(data = cumulative_dividends, curr_cat = cats[i], the_colors = color_info) {
  p <- data %>% 
    filter(category == curr_cat) %>% 
    mutate(ticker = factor(ticker, levels = levels(the_colors$i_tickers))) %>% 
    ggplot(aes(x = year, y = cum_dividends, linetype = source, color = ticker)) +
    geom_step() +
    scale_colour_manual(values = the_colors$i_colors) +
    scale_x_continuous(breaks = unique(cumulative_dividends$year)) +
    labs(
      title = 'Cumulative Dividends',
      y = 'Cumulative Dividends ($)',
      x = 'Year'
    ) +
    theme(plot.title = element_text(face = 'bold'))
  return(p)
}

## yearly returns time series plot

yearly_returns_plot <- function(data = yearly_returns, curr_cat = cats[i], the_colors = color_info) {
  p <- data %>% 
    filter(
      category == curr_cat,
      year >= max(year(fund_data$timestamp)) - 11
    ) %>% 
    mutate(ticker = factor(ticker, levels = levels(the_colors$i_tickers))) %>% 
    ggplot(aes(x = year, y = return, linetype = source, color = ticker)) +
    geom_line() +
    scale_colour_manual(values = the_colors$i_colors) +
    scale_x_continuous(breaks = unique(cumulative_dividends$year)) +
    labs(
      title = 'Yearly Returns',
      y = 'Return (%)',
      x = 'Year'
    ) +
    theme(plot.title = element_text(face = 'bold'))
  return(p)
}

# ------------------------------------------------------------------------------
# RUN TO CLEAN UP ENVIRONMENT
# ------------------------------------------------------------------------------

clean_up <- function() {
  cat_length <- length(cats)
  plot_prefix <- c(
    rep('ar_plot_', cat_length),
    rep('vt_plot_', cat_length),
    rep('cd_plot_', cat_length),
    rep('yr_plot_', cat_length)
  )
  nums <- rep(1:cat_length, 4)
  plot_list <- paste0(plot_prefix, nums)
  file.remove('Write_up.Rmd')
  rm(
    #data sets
    annualized_returns, cumulative_dividends, durations, fund_data, funds,
    yearly_returns, color_info,
    
    #values
    cat_header, cats, i, new_cat, the_date, write_up,
    
    #functions
    annualized_returns_calc, annualized_returns_plot, cumulative_dividends_calc,
    cumulative_dividends_plot, dividend_rebuys, get_big_dates, good_colors,
    make_url, pull_fund_data, value_gain_tot, value_trend_plot,
    yearly_returns_calc, yearly_returns_plot,
    
    #plots
    list = plot_list,
    
    envir = .GlobalEnv
  )
}
