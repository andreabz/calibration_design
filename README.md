# Calibration Design Simulations

This repository contains a small simulation series about calibration design in chemical analysis.

The goal is practical rather than theoretical: follow one question—**how should we invest calibration points?**—from method validation to routine analysis. The reproducible examples show how calibration design, error structure, weighting, mild non-linearity, concentration prediction, and prediction intervals fit into one workflow.

## Audience

The material is written for:

- analytical chemists who routinely build calibration curves;
- R users interested in simulation-based method understanding;
- chemometricians interested in communicating calibration assumptions clearly.

## Questions explored

The series covers six linked contexts:

1. With 15 calibration measurements, should we use many concentration levels or fewer replicated levels?
2. What changes when the response is linear but measurement variance is not constant?
3. When does weighted least squares help, and what do common weight functions assume?
4. What happens when a simple weight function is only partly true near the LOQ?
5. Can different calibration designs detect mild downward non-linearity equally well?
6. How should a broad validation design become a compact, monitored routine calibration design?

The narrative starts with a linear response and constant absolute error, then
introduces constant relative error, data-estimated weights, low-end weight
mismatch, and slight curvature. The conclusion is deliberately conditional:
use multiple distributed levels and replication during validation to establish
linearity and the variance structure; then, for a validated linear method, use
a 3 x 5 routine design with data-estimated WLS and QC checks at relevant low,
middle, and high concentrations. The routine design monitors a validated model;
it does not replace validation.

## Repository structure

```text
R/        Reusable simulation, fitting, prediction, metric, and plotting helpers
scripts/  Executable Quarto source articles with R code and figures
posts/    Short LinkedIn-ready QMD drafts and the final PDF-carousel source
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

The simulations are intentionally small and transparent. They are not meant to replace validation requirements or laboratory-specific method assessment. They are meant to make calibration assumptions visible and easier to discuss. WLS is useful when its weights describe the method's actual variability; it is not a blanket guarantee of lower bias in every finite simulated run.
