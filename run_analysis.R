
# Load Libraries
library(tidyverse)
library(lubridate)
library(whisker)
library(RColorBrewer)
library(rmarkdown)

# Load functions
source('functions.R')

# Read in data about funds to research
funds <- read.table(
  'fund_list.csv',
  header = TRUE,
  sep = ','
)

# Hit the API for Fund Data
fund_data <- pull_fund_data()

# Run analysis calculations
annualized_returns <- annualized_returns_calc()
yearly_returns <- yearly_returns_calc()
cumulative_dividends <- cumulative_dividends_calc()

# Create Write-Up Documents With Plots
the_date <- date(now())
write_up <- whisker.render(readLines('md_template_chunks/header.txt'))
cats <- unique(as.character(fund_data$category))

for (i in 1:length(cats)) {
  cat_header <- cats[i]
  color_info <- good_colors()
  assign(paste0('ar_plot_', i), annualized_returns_plot())
  assign(paste0('vt_plot_', i), value_trend_plot())
  assign(paste0('cd_plot_', i), cumulative_dividends_plot())
  assign(paste0('yr_plot_', i), yearly_returns_plot())
  new_cat <- whisker.render(readLines('md_template_chunks/category.txt'))
  write_up <- c(write_up, new_cat)
}

writeLines(write_up, 'Write_Up.Rmd')
render(
  'Write_Up.Rmd',
  output_format = c('html_document', 'md_document')
)

# Clean up Environment
clean_up()
rm(clean_up)

