# volva

**volva** is a simple forecasting application that turns any date-and-value
CSV into a forecast in seconds. Upload a two-column file, pick how far into the
future you want to look, and the app fits a time-series model and shows you the
projection as a table, a chart, and a downloadable forecast.

The forecasting engine is the [prophet](https://facebook.github.io/prophet/)
package from Facebook's core data science team.

**Try it live:** https://varmer.shinyapps.io/volva/

## Features

- Upload any `date, value` CSV (comma, semicolon or tab separated).
- Adjustable forecast horizon (1–730 days) via a slider.
- Three views of your data:
  - **Table** – the cleaned, aggregated input series.
  - **Plot** – the prophet forecast with trend and uncertainty interval.
  - **Predictions** – the point forecast (`yhat`) with lower/upper bounds.
- Friendly validation messages when a file is malformed.
- One-click **Download forecast (CSV)** of the full prediction.

## Data format

The file must contain (at least) two columns:

| Column | Meaning | Example     |
| ------ | ------- | ----------- |
| 1      | Date in `YYYY-MM-DD` format | `2007-12-10` |
| 2      | Numeric value to forecast   | `5001`       |

Example CSV:

```
ds,y
2007-12-10,5001
2007-12-11,5012
2007-12-12,3582
```

Rows that share the same date are summed together automatically. A sample file,
`manning.csv`, is included in this repository.

## Running locally

You need [R](https://www.r-project.org/) with the following packages:

```r
install.packages(c("shiny", "prophet", "lubridate", "dplyr"))
```

> Note: `prophet` depends on `rstan`, which compiles a Stan model on first use.
> Installation can take a few minutes.

Then, from the repository root, launch the app:

```r
shiny::runApp("volva")
```

## How it works

When you upload a CSV the app:

1. Reads the file **once** and validates that it has two columns and a parseable
   date column.
2. Aggregates the values by date into the `(ds, y)` frame prophet expects.
3. Fits a single prophet model and computes the forecast over the chosen
   horizon. The fitted model and forecast are shared across all tabs, so the
   model is only fit once per upload.

[prophet](https://facebook.github.io/prophet/) is an additive regression model
that decomposes a series into trend and seasonality (yearly, weekly, daily),
which makes it well suited to business time series with strong seasonal effects.

## License

Released under the [MIT License](LICENSE).
