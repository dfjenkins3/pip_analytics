# Pips performance report

Static Quarto dashboard for sharing Pips solve-time and head-to-head performance.

The published GitHub Pages entrypoint is `index.html`. The Quarto source is
`pip_analytics.qmd`; Google credentials are intentionally excluded from the repo.

To refresh the dashboard locally:

```sh
quarto render pip_analytics.qmd
cp pip_analytics.html index.html
```

The report groups solve-time summaries first, then recorded wins, paired daily
gaps, and statistical tests of relative Hard performance. The tests recalculate
from the sheet on each render. Both players' Hard times are capped at 20 minutes;
the supporting win model recomputes outcomes after capping.

`relative_performance.R` contains the calendar-aware Newey-West covariance,
trend models, and block-bootstrap analysis. It uses base R and the recommended
`boot` package. The generated `relative-performance-data.csv` contains capped
paired times and can reproduce the analysis without Google credentials:

```r
source("relative_performance.R")
results <- analyze_relative_performance(read.csv("relative-performance-data.csv"))
results$checks
```

Run the statistical regression checks against the October 4, 2026 snapshot:

```sh
Rscript tests/test-relative-performance.R
```
