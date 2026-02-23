# SAMutils

`SAMutils` provides helper functions for Stock Synthesis Assessment Model (SAM) workflows built around the [`stockassessment`](https://github.com/fishfollower/SAM) R package.

The package focuses on:

- preparing SAM-ready input matrices from tidy data
- fitting and comparing model configurations
- extracting model outputs in tidy formats
- producing standard diagnostic plots and summary tables

## Installation

### Install from local source

From the project root:

```r
R CMD INSTALL .
```

Or from R:

```r
install.packages(".", repos = NULL, type = "source")
```

### Install development dependencies (optional)

```r
install.packages(c("devtools", "testthat"))
```

## Usage

Load the package:

```r
library(SAMutils)
```

### 1. Build SAM input matrices from tidy data

```r
model_dat <- tibble::tibble(
  year = c(2000, 2000, 2001, 2001),
  age = c(1, 2, 1, 2),
  M = c(0.20, 0.18, 0.21, 0.19)
)

M_mat <- sam.input(
  model_dat = model_dat,
  variable = "M",
  age_range = 1:2,
  year_range = 2000:2001,
  tail_f = mean
)
```

### 2. Run a SAM fit and extract tidy outputs

```r
# fit <- stockassessment::sam.fit(dat, conf)

# Yearly summaries (SSB, Fbar, recruitment, catch, etc.)
# by_year <- rby.sam(fit)

# Year-age summaries (N, F, optional stock attributes)
# by_year_age <- rbya.sam(fit)
```

### 3. Plot diagnostics

```r
# Observation residual bubble plot
# p_res <- model_resid_plot(list(res = residuals(fit)))

# Process residual bubble plot
# p_pres <- model_pres_resid_plot(list(process_res = safe_procres(fit)))
```

### 4. Render static dashboard from `full_sam_fit()`

```r
library(SAMutils)
library(stockassessment)

d <- "testmore/nsher"
cn <- read.ices(file.path(d, "cn.dat"))
cw <- read.ices(file.path(d, "cw.dat"))
dw <- read.ices(file.path(d, "dw.dat"))
lf <- read.ices(file.path(d, "lf.dat"))
lw <- read.ices(file.path(d, "lw.dat"))
mo <- read.ices(file.path(d, "mo.dat"))
nm <- read.ices(file.path(d, "nm.dat"))
pf <- read.ices(file.path(d, "pf.dat"))
pm <- read.ices(file.path(d, "pm.dat"))
sw <- read.ices(file.path(d, "sw.dat"))
surveys <- read.ices(file.path(d, "survey.dat"))

dat <- setup.sam.data(
  surveys = surveys,
  residual.fleets = cn,
  prop.mature = mo,
  stock.mean.weight = sw,
  catch.mean.weight = cw,
  dis.mean.weight = dw,
  land.mean.weight = lw,
  prop.f = pf,
  prop.m = pm,
  natural.mortality = nm,
  land.frac = lf
)

conf <- defcon(dat)
conf$fbarRange <- c(2, 6)
conf$corFlag <- 1
conf$keyLogFpar <- matrix(
  c(
    -1,-1,-1,-1,-1,-1,-1,-1,-1,
    -1, 0, 1, 2, 3, 4, 5, 6,-1,
    -1, 7,-1,-1,-1,-1,-1,-1,-1,
     8,-1,-1,-1,-1,-1,-1,-1,-1
  ),
  nrow = 4, byrow = TRUE
)

sam_fit <- full_sam_fit(dat, conf)
html_path <- dashboard_app(sam_fit, output_file = "sam-dashboard.html")
# or: html_path <- render_quarto_dashboard(sam_fit, output_file = "sam-dashboard.html")
html_path
```

## Main function groups

- Input preparation: `sam.input()`, `format_SAM()`, `sam_build_data()`
- Fitting/comparison: `full_sam_fit()`, `compare_configs()`
- Output extraction: `rby.sam()`, `rbya.sam()`, `annotated_par_table()`
- Diagnostics/plots: `model_data_plot()`, `model_lik_plot()`, `model_retro_plot()`, `model_selectivity_plot()`, `model_resid_plot()`
- Dashboard output: `dashboard_app()`, `render_quarto_dashboard()`

## Testing

Run unit tests from the package root:

```r
testthat::test_local(".")
```
