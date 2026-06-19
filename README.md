# Calibration Design Simulations

This repository contains a small simulation series about calibration design in chemical analysis.

The goal is practical rather than theoretical: use reproducible R examples to show how calibration design, error structure, weighting, and mild non-linearity affect concentration prediction and prediction intervals.

## Audience

The material is written for:

- analytical chemists who routinely build calibration curves;
- R users interested in simulation-based method understanding;
- chemometricians interested in communicating calibration assumptions clearly.

## Questions explored

The series currently covers five questions:

1. With 15 calibration measurements, should we use many concentration levels or fewer replicated levels?
2. What changes when the response is linear but measurement variance is not constant?
3. When does weighted least squares help, and what do common weight functions assume?
4. What happens when a simple weight function is only partly true near the LOQ?
5. Can different calibration designs detect mild downward non-linearity equally well?

## Repository structure

```text
R/        Reusable simulation, fitting, prediction, metric, and plotting helpers
scripts/  Executable Quarto source articles with R code and figures
posts/    Short LinkedIn-ready QMD drafts for copy-paste publication
_site/    Rendered website output, created by Quarto and GitHub Actions
```

## Run locally

Install the R packages used by the examples:

```r
install.packages(c("data.table", "ggplot2", "here", "knitr", "rmarkdown"))
```

Render the website with Quarto:

```bash
quarto render
```

The rendered site is written to `_site/`.

## Deployment

The repository includes a GitHub Actions workflow in `.github/workflows/quarto-publish.yml`. On each push to `main` or `master`, the workflow installs Quarto, renders the site, uploads `_site/`, and deploys it to GitHub Pages.

In the GitHub repository settings, enable Pages with **GitHub Actions** as the source.

## Notes

The simulations are intentionally small and transparent. They are not meant to replace validation requirements or laboratory-specific method assessment. They are meant to make calibration assumptions visible and easier to discuss.
