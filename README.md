# Mutual Fund Review

## Work in Progress...

The nuts and bolts of this work, but need to clean up the plotting a little bit.

## How to Use This Code

#### 1. Get Your API Key

Go get your API key from [Alpha Vatage](https://www.alphavantage.co/) and paste it in the `api_key` file.

#### 2. Update the Fund List

Open up the `fund_list.csv` and update it with funds you would like to compare. The write up will compare funds within the same `category`.

#### 3. Run the Analysis Code

Before running the code, you will need to have [R installed](https://www.r-project.org/) along with the following packages:

- `tidyverse`
- `lubridate`
- `whisker`
- `RColorBrewer`
- `rmarkdown`

If you will be running the code in [RStudio](https://www.rstudio.com/), you're set, but if you'd like to run the script from the command line, you will also need to make sure you have [pandoc](https://pandoc.org/installing.html) installed.  

Once your software is installed, you can simply run `Rscript run_analysis.R` from the root directory of this repository. This will take at least 15 seconds per fund on your list, as Alpha Vantage has strict API usage limits of 5 calls per minute (and 500 calls per day), plus some time to render the plots.

#### 4. Review Your Funds!

Now to the point! You can now review a large number of funds, side-by-side in your personally defined categories for comparison. You can check out the one, three, five, and ten year annualized returns, valuation over time, dividend pay-outs, and yearly returns. Checkout what the markdown output looks like [here](Write_Up.md), or quickly review your own output in your web browser by opening your newly created `Write_Up.html` file after running the analysis script.
