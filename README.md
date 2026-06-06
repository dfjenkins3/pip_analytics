# Pips Macro-Performance Analytics

Static Quarto dashboard for sharing Pips solve-time and head-to-head performance.

The published GitHub Pages entrypoint is `index.html`. The Quarto source is
`pip_analytics.qmd`; Google credentials are intentionally excluded from the repo.

To refresh the dashboard locally:

```sh
quarto render pip_analytics.qmd
cp pip_analytics.html index.html
```
