library(stockassessment)

#tyr <- lubridate::year(Sys.Date())

#md <- fs::dir_create(sprintf('assessment_model/01-initial-%s/',tyr))
#md <- fs::dir_create('exploratory_models/Sam/01-initial')

#' Write SAM Input Data to Disk
#'
#' @param d Data frame to write.
#' @param md Model directory path.
#'
#' @return Invisibly returns the written data frame.
sam.dir.write <- function(d, md) {
  d |>
    #  dplyr::select(-mat) |>
    readr::write_csv2(paste(md, 'input_data.csv', sep = '/'))
}


#' Build a SAM Matrix from Long-Format Data
#'
#' @param model_dat Input data with `year`, `age`, and target variable.
#' @param variable Name of the variable column to extract.
#' @param age_range Numeric age range to keep/collapse to.
#' @param year_range Numeric year range for output rownames/order.
#' @param time_window Optional SAM `time` attribute.
#' @param tail_f Aggregation function for duplicate year-age cells.
#' @param na.fill Value used to fill missing year-age combinations.
#' @param drop_na Whether to drop rows with missing values before aggregation.
#' @param complete_grid Whether to force a full year-age grid before aggregation.
#'
#' @return A numeric matrix formatted for SAM input.
#'
#' @examples
#' dat <- tibble::tibble(
#'   year = c(2000, 2000, 2001, 2001),
#'   age = c(1, 2, 1, 2),
#'   M = c(0.2, 0.18, 0.21, 0.19)
#' )
#' sam.input(dat, "M", age_range = 1:2, year_range = 2000:2001, tail_f = mean)
sam.input <- function(
  model_dat,
  variable,
  age_range = as.numeric(1:25),
  year_range = sort(unique(model_dat$year)),
  time_window = NULL,
  tail_f = sum,
  na.fill = -1,
  drop_na = TRUE,
  complete_grid = FALSE
) {
  d <-
    model_dat |>
    dplyr::select(year, age, value = !!rlang::quo(rlang::sym(variable))) |>
    dplyr::mutate(
      #value = ifelse(is.na(value),na.fill,value),
      age = dplyr::case_when(
        age < min(age_range) ~ min(age_range),
        age > max(age_range) ~ max(age_range),
        TRUE ~ age
      )
    )

  if (isTRUE(drop_na)) {
    d <- d |> stats::na.omit()
  }

  if (isTRUE(complete_grid)) {
    d <- d |>
      dplyr::right_join(
        tidyr::expand_grid(year = year_range, age = age_range),
        by = c("year", "age")
      )
  }

  d <- d |>
    dplyr::group_by(year, age) |>
    dplyr::summarise(value = tail_f(value, na.rm = TRUE)) |>
    dplyr::ungroup() |>
    dplyr::arrange(age, year) |>
    tidyr::pivot_wider(
      id_cols = 'year',
      names_from = 'age',
      values_from = 'value',
      values_fill = na.fill
    ) |>
    dplyr::arrange(year) |>
    dplyr::ungroup() |>
    dplyr::full_join(
      tibble::tibble(year = sort(unique(year_range))),
      by = 'year'
    ) |>
    dplyr::select(-year) |>
    as.matrix()

  rownames(d) <- sort(unique(year_range))

  attributes(d)$time <- time_window
  d
}

#' Clean Weight-at-Age Series
#'
#' Replaces outliers and sentinel missing values using a trimmed mean rule.
#'
#' @param x Numeric vector of weights.
#' @param missing_value Sentinel value treated as missing.
#' @param z Number of SDs used for outlier thresholding.
#' @param assessment_year Optional assessment year for protected years logic.
#' @param protected_years Optional years to skip cleaning for.
#'
#' @return A numeric vector with cleaned weights.
clean_weights <- function(
  x,
  missing_value = -1,
  z = 1.96,
  assessment_year = NULL,
  protected_years = NULL
) {
  valid_idx <- which(!is.na(x) & x != missing_value)
  if (length(valid_idx) < 3) {
    return(x)
  }

  if (is.null(protected_years) && !is.null(assessment_year)) {
    protected_years <- c(assessment_year - 1, assessment_year)
  }
  if (!is.null(protected_years) && any(x %in% protected_years, na.rm = TRUE)) {
    return(x)
  }

  valid_vals <- x[valid_idx]
  min_pos <- which.min(valid_vals)
  max_pos <- which.max(valid_vals)
  trimmed_vals <- valid_vals[-c(min_pos, max_pos)]
  if (length(trimmed_vals) < 2 || is.na(stats::sd(trimmed_vals))) {
    return(x)
  }

  center <- mean(trimmed_vals, na.rm = TRUE)
  spread <- stats::sd(trimmed_vals, na.rm = TRUE)
  lower <- center - z * spread
  upper <- center + z * spread

  x <- ifelse(
    (x > upper) | (x < lower) | (x == missing_value),
    center,
    x
  )
  x
}

#' Format a Wide Year-Age Table to SAM Matrix
#'
#' @param x Data frame with `year` and one column per age.
#' @param fin_year Upper year bound (exclusive).
#' @param first_year Lower year bound (inclusive).
#' @param smb Whether series is spring survey (sets default `time`).
#' @param smh Whether series is autumn survey (sets default `time`).
#' @param cn Whether series is catch numbers (sets default `time`).
#' @param replace_zero_with_na Whether zeros should be converted to `NA`.
#' @param add_ages Optional matrix/data frame of additional age columns to prepend.
#' @param time_window Optional explicit SAM `time` attribute.
#'
#' @return A numeric matrix formatted for SAM input.
format_SAM <- function(
  x,
  fin_year = NULL,
  first_year = -Inf,
  smb = FALSE,
  smh = FALSE,
  cn = FALSE,
  replace_zero_with_na = (smb || smh || cn),
  add_ages = NULL,
  time_window = NULL
) {
  if (is.null(fin_year)) {
    fin_year <- max(x$year, na.rm = TRUE) + 1
  }

  x <- x |>
    dplyr::filter(year < fin_year, year >= first_year)

  if (isTRUE(replace_zero_with_na)) {
    x <- x |>
      dplyr::mutate(dplyr::across(-year, ~ ifelse(. == 0, NA_real_, .)))
  }

  dat <- x |>
    dplyr::select(-year) |>
    as.matrix()
  rownames(dat) <- x$year

  if (is.null(time_window)) {
    if (isTRUE(smb)) {
      time_window <- c(0.15, 0.2)
    } else if (isTRUE(smh)) {
      time_window <- c(0.7, 0.8)
    } else if (isTRUE(cn)) {
      time_window <- c(0.45, 0.55)
    }
  }
  if (!is.null(time_window)) {
    attributes(dat)$time <- time_window
  }

  if (!is.null(add_ages)) {
    add_ages <- as.matrix(add_ages)
    if (nrow(add_ages) != nrow(dat)) {
      stop("`add_ages` must have the same number of rows as input years.")
    }
    dat <- cbind(add_ages, dat)
  }

  dat
}

#' Fit and Score a Candidate SAM Configuration
#'
#' @param x Configuration key/index in `conf_list`.
#' @param conf_list List of candidate SAM configurations.
#' @param dat SAM data object used for fitting.
#' @param trysim Whether to run simulation-based convergence checks.
#' @param allLogN0 Whether to set all `keyVarLogN` entries to 0.
#' @param config_mutator Optional function `(conf, x)` to mutate config before fit.
#' @param eval_max Optimizer maximum evaluations.
#' @param retro_year Number of retrospective peels.
#' @param ncores Number of cores for parallel steps.
#'
#' @return A tibble with fit diagnostics (AIC, convergence, Mohn metrics).
#'
#' @examples
#' \dontrun{
#' res <- compare_configs(
#'   x = "base",
#'   conf_list = conf_list,
#'   dat = dat,
#'   config_mutator = function(conf, id) conf
#' )
#' print(res)
#' }
compare_configs <- function(
  x,
  conf_list = NULL,
  dat = NULL,
  trysim = FALSE,
  allLogN0 = FALSE,
  config_mutator = NULL,
  eval_max = 3000,
  retro_year = 5,
  ncores = 1
) {
  conf_list <- sam_resolve_arg(conf_list, "conf_list")
  dat <- sam_resolve_arg(dat, "dat")
  conf <- conf_list[[x]]

  if (isTRUE(allLogN0) && !is.null(conf$keyVarLogN)) {
    conf$keyVarLogN[] <- 0
  }

  if (!is.null(conf$keyVarObs) && nrow(conf$keyVarObs) > 0) {
    conf$keyBiomassTreat[nrow(conf$keyVarObs)] <- 4
    conf$keyVarObs[nrow(conf$keyVarObs), 1] <- -1
    if (
      !is.null(conf$keyLogFpar) && nrow(conf$keyLogFpar) >= nrow(conf$keyVarObs)
    ) {
      conf$keyLogFpar[nrow(conf$keyLogFpar), 1] <- -1
    }
  }
  if (!is.null(conf$maxAgePlusGroup)) {
    conf$maxAgePlusGroup <- rep(1, length(conf$maxAgePlusGroup))
  }
  if (is.function(config_mutator)) {
    conf <- config_mutator(conf, x)
  }

  fit <- NULL
  par <- stockassessment::defpar(dat, conf)
  try(
    fit <- stockassessment::sam.fit(
      dat,
      conf,
      par,
      control = list(eval.max = eval_max)
    ),
    silent = TRUE
  )

  if (is.null(fit)) {
    summary_tbl <- tibble::tibble()
    aic <- NA_real_
    opt <- NA_real_
    maxsd <- NA_real_
    convrate <- NA_real_
  } else {
    retros <- try(
      stockassessment::retro(fit, year = retro_year, ncores = ncores),
      silent = TRUE
    )
    if (inherits(retros, "try-error") || is.null(retros)) {
      summary_tbl <- tibble::tibble()
    } else {
      summary_tbl <- stockassessment::mohn(retros) |>
        t() |>
        tibble::as_tibble()
    }

    aic <- stats::AIC(fit)
    opt <- fit$opt$convergence
    maxsd <- max(stockassessment::partable(fit)[, 2], na.rm = TRUE)
    convrate <- NA_real_

    if (isTRUE(trysim)) {
      sim_dat <- stats::simulate(fit, nsim = 100, full.data = TRUE)
      sim_fit <- parallel::mclapply(
        sim_dat,
        function(z) {
          try(
            stockassessment::sam.fit(z, fit$conf, fit$obj$env$par),
            silent = TRUE
          )
        },
        mc.cores = ncores
      )
      convrate <- sim_fit |>
        purrr::discard(~ inherits(.x, "try-error")) |>
        purrr::map_dbl(~ .x$opt$convergence) |>
        (\(v) if (length(v) == 0) NA_real_ else mean(v == 0))()
    }
  }

  summary_tbl |>
    dplyr::mutate(
      id = x,
      aic = aic,
      opt = opt,
      maxsd = maxsd,
      convrate = convrate
    )
}


#' Plot Input Data by Year and Age
#'
#' @param dat Long-format data including `year`, `age`, and value columns.
#'
#' @return A ggplot object.
input_data_plot <- function(dat) {
  dat |>
    pivot_longer(-c(year, age), names_to = "variable", values_to = "value") |>
    dplyr::mutate(value = ifelse(value == 0, NA_real_, value)) |>
    ggplot2::ggplot(ggplot2::aes(
      year,
      value,
      col = as.ordered(age),
      label = age
    )) +
    ggplot2::geom_line() +
    ggplot2::geom_text() +
    ggplot2::facet_wrap(~variable, scales = 'free_y') +
    ggplot2::theme(legend.position = 'none') +
    tidypax::scale_col_crayola()
}

#' Extract Results by Year and Age
#'
#' @param sam_fit A fitted SAM object.
#' @param res Deprecated/unused legacy argument.
#' @param include_stock_data Whether to join stock/catch/maturity/F/M data.
#' @param include_parameter_table Whether to join annotated parameter estimates.
#'
#' @return A tibble with year-age level outputs.
rbya.sam <- function(
  sam_fit,
  res = NULL,
  include_stock_data = TRUE,
  include_parameter_table = TRUE
) {
  to_year_age <- function(x, value_name) {
    to_num <- function(v) suppressWarnings(as.numeric(as.character(v)))
    x |>
      as.data.frame.table(
        stringsAsFactors = FALSE,
        responseName = value_name
      ) |>
      dplyr::rename(year = Var1, age = Var2) |>
      dplyr::mutate(year = to_num(year), age = to_num(age)) |>
      tibble::as_tibble()
  }

  d <- sam_fit |>
    stockassessment::ntable() |>
    as.data.frame.table(stringsAsFactors = FALSE, responseName = 'n') |>
    dplyr::left_join(
      sam_fit |>
        stockassessment::faytable() |>
        as.data.frame.table(stringsAsFactors = FALSE, responseName = 'f')
    ) |>
    dplyr::rename(year = Var1, age = Var2) |>
    dplyr::mutate(
      year = suppressWarnings(as.numeric(year)),
      age = suppressWarnings(as.numeric(age))
    ) |>
    tibble::as_tibble()

  if (isTRUE(include_stock_data)) {
    dat <- sam_fit[["data"]]
    join_optional <- function(x, y, value_name) {
      y_tbl <- tryCatch(
        to_year_age(y, value_name),
        error = function(e) warning(value_name, 'not found')
      )
      if (is.null(y_tbl)) {
        return(x)
      }
      dplyr::left_join(x, y_tbl, by = c("year", "age"))
    }

    d <- d |>
      join_optional(dat[["stockMeanWeight"]], "stock_weight") |>
      join_optional(dat[["catchMeanWeight"]], "catch_weight") |>
      join_optional(dat[["propMat"]], "maturity") |>
      join_optional(dat[["propF"]], "propF") |>
      join_optional(dat[["propM"]], "propM") |>
      join_optional(dat[["natMor"]], "M")
  }

  if (isTRUE(include_parameter_table)) {
    par_tbl <- tryCatch(
      sam_fit |>
        annotated_par_table() |>
        dplyr::select(par_name, fleet, age, est) |>
        dplyr::mutate(
          par_name = gsub('_[0-9]+', '', par_name),
          age = suppressWarnings(as.numeric(age))
        ) |>
        dplyr::filter(!is.na(age)) |>
        tidyr::pivot_wider(names_from = c(par_name, fleet), values_from = est),
      error = function(e) warning('annontated_par_table failed')
    )
    if (!is.null(par_tbl)) {
      d <- d |>
        dplyr::left_join(par_tbl, by = "age")
    }
  }

  d
}

#' Extract Results by Year
#'
#' @param sam_fit A fitted SAM object.
#' @param run_ref_bio Whether to include `ref_bio` and `hr` derived series.
#' @param ref_bio_type Reference biomass type (`"length"` or `"age"`).
#'
#' @return A tibble with median/lower/upper time series by variable.
rby.sam <- function(
  sam_fit,
  run_ref_bio = TRUE,
  ref_bio_type = "length"
) {
  sam_tibble <- function(x, varname = 'tsb') {
    years <- sam_fit[["data"]][["years"]]
    x <- as.matrix(x)
    if (ncol(x) < 3) {
      stop(sprintf("Expected at least 3 columns for '%s' series.", varname))
    }
    x <- x[, seq_len(3), drop = FALSE]
    tibble::as_tibble(x, .name_repair = "minimal") |>
      dplyr::mutate(
        year = years[seq_len(nrow(x))],
        variable = varname
      ) |>
      stats::setNames(c('median', 'lower', 'upper', 'year', 'variable'))
  }

  sam_ref_bio <- function(sam_fit, varname = 'ref_bio', type = 'length') {
    tableit <- get("tableit", envir = asNamespace("stockassessment"))
    if (varname == 'ref_bio' && type == 'length') {
      tableit(sam_fit, "IS_logRefBio", trans = exp)
    } else if (varname == 'hr' && type == 'length') {
      tableit(sam_fit, "IS_logHR", trans = exp)
    } else if (varname == 'ref_bio' && type == 'age') {
      tableit(sam_fit, "IS_logRefBio4plus", trans = exp)
    } else {
      tableit(sam_fit, "IS_logHR4plus", trans = exp)
    }
  }

  safe_series <- function(series_fn, varname) {
    tryCatch(
      series_fn() |>
        sam_tibble(varname),
      error = function(e) warning(paste('Series', varname, 'not found'))
    )
  }

  out <- list(
    safe_series(
      function() {
        sam_fit |>
          stockassessment::ssbtable()
      },
      "ssb"
    ),
    safe_series(
      function() {
        sam_fit |>
          stockassessment::tsbtable()
      },
      "tsb"
    ),
    safe_series(
      function() {
        sam_fit |>
          stockassessment::fbartable()
      },
      "fbar"
    ),
    safe_series(
      function() {
        sam_fit |>
          stockassessment::rectable()
      },
      "rec"
    ),
    safe_series(
      function() {
        sam_fit |>
          stockassessment::catchtable()
      },
      "catch"
    )
  )

  if (isTRUE(run_ref_bio)) {
    out <- c(
      out,
      list(
        safe_series(
          function() {
            sam_fit |>
              sam_ref_bio(type = ref_bio_type)
          },
          "ref_bio"
        ),
        safe_series(
          function() {
            sam_fit |>
              sam_ref_bio('hr', type = ref_bio_type)
          },
          "hr"
        )
      )
    )
  }

  out <- purrr::compact(out)
  if (length(out) == 0) {
    return(tibble::tibble(
      median = numeric(),
      lower = numeric(),
      upper = numeric(),
      year = numeric(),
      variable = character()
    ))
  }
  dplyr::bind_rows(out)
}


#' Build Annotated SAM Parameter Table
#'
#' @param sam_fit A fitted SAM object.
#'
#' @return A tibble of parameter estimates with fleet/age annotations.
annotated_par_table <- function(sam_fit) {
  tmp_func <- function(conf = 'keyLogFpar') {
    x <- sam_fit$conf[[conf]]
    if (is.null(x)) {
      return(tibble::tibble())
    }

    x |>
      (\(x) {
        x <- as.matrix(x)
        nr <- nrow(x)
        nc <- ncol(x)
        dn <- dimnames(x)

        default_fleet <- paste(
          ifelse(sam_fit$data$fleetTypes == 0, 'comm', 'survey'),
          seq_len(sam_fit$data$noFleets),
          sep = '_'
        )
        fleet_labels <- if (!is.null(dn[[1]]) && length(dn[[1]]) == nr) {
          dn[[1]]
        } else if (length(default_fleet) >= nr) {
          default_fleet[seq_len(nr)]
        } else {
          paste0("fleet_", seq_len(nr))
        }

        age_labels <- if (!is.null(dn[[2]]) && length(dn[[2]]) == nc) {
          dn[[2]]
        } else {
          as.character(seq_len(nc))
        }

        dimnames(x) <- list(fleet = fleet_labels, age = age_labels)
        x
      })() |>
      as.data.frame.table(stringsAsFactors = FALSE, responseName = 'id') |>
      tibble::as_tibble() |>
      dplyr::filter(!(id == -1 | is.na(id))) |>
      dplyr::mutate(par_name = paste(conf, id, sep = '_'))
  }

  meta_data <-
    tmp_func() |>
    dplyr::bind_rows(
      tmp_func('keyLogFsta'),
      tmp_func('keyQpow'),
      tmp_func('keyVarF'),
      tmp_func('keyVarObs'),
      tmp_func('keyVarLogN'),
      tmp_func('keyCorObs'),
      tmp_func('predVarObsLink')
    ) |>
    dplyr::mutate(
      par_name = dplyr::case_when(
        grepl('LogN', par_name) ~ gsub('LogN', 'logSdLogN', par_name),
        grepl('VarF', par_name) ~ gsub('VarF', 'logSdLogFsta', par_name),
        grepl('keyVarObs', par_name) ~ gsub('VarObs', 'logSdLogObs', par_name),
        grepl('CorObs', par_name) ~ gsub('CorObs', 'transfIRARdist', par_name),
        grepl('predVarObs', par_name) ~ gsub('Link', '', par_name),
        grepl('Qpow', par_name) ~ gsub('key', 'log', par_name),
        TRUE ~ gsub('Log', 'log', par_name)
      ),
      par_name = gsub('key|keyVar', '', par_name)
    )

  stockassessment::partable(sam_fit) |>
    (\(x) {
      tibble::as_tibble(x) |>
        dplyr::mutate(par_name = rownames(x))
    })() |>
    stats::setNames(c(
      'log_est',
      'log_sd',
      'est',
      'lower',
      'upper',
      'par_name'
    )) |>
    dplyr::left_join(meta_data)
}

#' Format Observation Residuals
#'
#' @param res Output from `residuals(sam_fit)`.
#'
#' @return A tibble with observation residual diagnostics.
format_sam_res <- function(res) {
  tibble::tibble(
    year = res$year,
    fleet = attr(res, 'fleetNames')[res$fleet],
    age = res$age,
    observation = res$observation,
    nll = res$nll,
    grad = res$grad,
    mean = res$mean,
    residual = res$residual
  ) |>
    dplyr::mutate(
      residual = ifelse(residual == 0, NA_real_, residual),
      year = as.numeric(year)
    )
}

#' Format Process Residuals
#'
#' @param res Output from `procres(sam_fit)`.
#'
#' @return A tibble with process residual diagnostics.
format_sam_pres <- function(res) {
  n <- max(
    length(res$year),
    length(res$fleet),
    length(res$age),
    length(res$residual),
    0
  )
  if (n == 0) {
    return(tibble::tibble(
      year = numeric(),
      fleet = character(),
      age = numeric(),
      residual = numeric()
    ))
  }

  re_n <- function(x) {
    if (length(x) == 0) {
      rep(NA, n)
    } else {
      rep_len(x, n)
    }
  }

  fleet_idx <- suppressWarnings(as.integer(re_n(res$fleet)))
  fleet_names <- attr(res, 'fleetNames')
  fleet <- if (!is.null(fleet_names) && length(fleet_names) > 0) {
    fleet_names[fleet_idx]
  } else {
    as.character(fleet_idx)
  }

  tibble::tibble(
    year = re_n(res$year),
    fleet = fleet,
    age = re_n(res$age),
    residual = re_n(res$residual)
  ) |>
    dplyr::mutate(
      residual = ifelse(residual == 0, NA_real_, residual),
      year = suppressWarnings(as.numeric(year))
    )
}

#' Safely Compute Process Residuals
#'
#' @param fit A fitted SAM object.
#' @param parallel Logical passed to `stockassessment::procres()`.
#'
#' @return Process residual object, or `NULL` if computation fails.
safe_procres <- function(fit, parallel = FALSE) {
  tryCatch(
    stockassessment::procres(fit, parallel = parallel),
    error = function(e) warning('procres failed')
  )
}


#' Resolve Argument from Explicit Value or Calling Environment
#'
#' @param x Explicit value, if provided.
#' @param name Object name to look up in `env` when `x` is `NULL`.
#' @param env Environment used for lookup.
#'
#' @return Resolved value.
sam_resolve_arg <- function(x, name, env = parent.frame()) {
  if (!is.null(x)) {
    return(x)
  }
  x <- get0(name, envir = env)
  if (is.null(x)) {
    stop(sprintf(
      "Missing `%s`. Pass it explicitly or define it in the calling environment.",
      name
    ))
  }
  x
}

#' Build a SAM Data Object with Flexible Inputs
#'
#' @param natural_mortality Natural mortality matrix.
#' @param surveys Survey matrix or named list of survey matrices.
#' @param residual_fleet Residual fleet matrix.
#' @param prop_mature Proportion mature matrix.
#' @param stock_mean_weight Stock mean weight matrix.
#' @param catch_mean_weight Catch mean weight matrix.
#' @param dis_mean_weight Discard mean weight matrix.
#' @param land_mean_weight Landings mean weight matrix.
#' @param prop_f Proportion F matrix.
#' @param prop_m Proportion M matrix.
#' @param land_frac Landings fraction matrix.
#' @param env Environment used for fallback object lookup.
#'
#' @return A `stockassessment::setup.sam.data` object.
sam_build_data <- function(
  natural_mortality,
  surveys = NULL,
  residual_fleet = NULL,
  prop_mature = NULL,
  stock_mean_weight = NULL,
  catch_mean_weight = NULL,
  dis_mean_weight = NULL,
  land_mean_weight = NULL,
  prop_f = NULL,
  prop_m = NULL,
  land_frac = NULL,
  env = parent.frame()
) {
  if (is.null(surveys)) {
    surveys <- get0("surveys", envir = env)
  }
  if (is.null(surveys)) {
    surveys <- sam_resolve_arg(surveys, "smb", env)
  }
  if (!is.list(surveys)) {
    surveys <- list(spring = surveys)
  }

  residual_fleet <- sam_resolve_arg(residual_fleet, "cn", env)
  prop_mature <- sam_resolve_arg(prop_mature, "mo", env)
  stock_mean_weight <- sam_resolve_arg(stock_mean_weight, "sw", env)
  catch_mean_weight <- sam_resolve_arg(catch_mean_weight, "cw", env)
  dis_mean_weight <- if (is.null(dis_mean_weight)) {
    catch_mean_weight
  } else {
    dis_mean_weight
  }
  land_mean_weight <- if (is.null(land_mean_weight)) {
    catch_mean_weight
  } else {
    land_mean_weight
  }
  prop_f <- sam_resolve_arg(prop_f, "pf", env)
  prop_m <- sam_resolve_arg(prop_m, "pm", env)
  land_frac <- sam_resolve_arg(land_frac, "lf", env)

  stockassessment::setup.sam.data(
    surveys = surveys,
    residual.fleets = residual_fleet,
    prop.mature = prop_mature,
    stock.mean.weight = stock_mean_weight,
    catch.mean.weight = catch_mean_weight,
    dis.mean.weight = dis_mean_weight,
    land.mean.weight = land_mean_weight,
    prop.f = prop_f,
    prop.m = prop_m,
    natural.mortality = natural_mortality,
    land.frac = land_frac
  )
}

#' Run Full SAM Diagnostics Bundle
#'
#' @param dat SAM data object.
#' @param conf SAM configuration list.
#'
#' @return A list with fitted model, residuals, retrospectives, and leave-out runs.
full_sam_fit <- function(dat, conf) {
  par <- stockassessment::defpar(dat, conf)
  ## Fit a model with SAM
  fit <- stockassessment::sam.fit(dat, conf, par)
  res <- try(stats::residuals(fit, parallel = FALSE))
  process_res <- safe_procres(fit, parallel = FALSE)
  retro <- try(stockassessment::retro(fit, year = 5, ncores = 1))
  lo <- try(stockassessment::leaveout(fit, ncores = 1))
  #sim_sam <- simstudy(fit)
  return(list(
    par = par,
    fit = fit,
    res = res,
    process_res = process_res,
    retro = retro,
    lo = lo
  ))
}


#' Fit SAM with Profiled Natural Mortality
#'
#' @param m_val Scalar M value or data frame with year-age M values.
#' @param conf SAM configuration list.
#' @param model_dat Model input data in long format.
#' @param maxage Maximum age to pass to `sam.input`.
#' @param surveys Optional survey matrix or named list of survey matrices.
#' @param residual_fleet Optional residual fleet matrix.
#' @param prop_mature Optional maturity matrix.
#' @param stock_mean_weight Optional stock mean weight matrix.
#' @param catch_mean_weight Optional catch mean weight matrix.
#' @param dis_mean_weight Optional discard mean weight matrix.
#' @param land_mean_weight Optional landings mean weight matrix.
#' @param prop_f Optional proportion-F matrix.
#' @param prop_m Optional proportion-M matrix.
#' @param land_frac Optional landings fraction matrix.
#'
#' @return A fitted SAM object.
#'
#' @examples
#' \dontrun{
#' fit <- profile_M_run(
#'   m_val = 0.2,
#'   conf = conf,
#'   model_dat = model_dat,
#'   surveys = list(spring = smb),
#'   residual_fleet = cn
#' )
#' }
profile_M_run <- function(
  m_val = 0.15,
  conf,
  model_dat = NULL,
  maxage = NULL,
  surveys = NULL,
  residual_fleet = NULL,
  prop_mature = NULL,
  stock_mean_weight = NULL,
  catch_mean_weight = NULL,
  dis_mean_weight = NULL,
  land_mean_weight = NULL,
  prop_f = NULL,
  prop_m = NULL,
  land_frac = NULL
) {
  model_dat <- sam_resolve_arg(model_dat, "model_dat")
  maxage <- if (is.null(maxage)) max(model_dat$age, na.rm = TRUE) else maxage

  if ('data.frame' %in% class(m_val)) {
    nm <-
      model_dat |>
      dplyr::select(-M) |>
      dplyr::left_join(m_val) |>
      dplyr::left_join(
        m_val |>
          dplyr::group_by(age) |>
          dplyr::summarise(meanM = mean(M, na.rm = TRUE))
      ) |>
      dplyr::mutate(M = ifelse(is.na(M), meanM, M)) |>
      sam.input("M", age_range = as.numeric(1:maxage), tail_f = mean)
  } else {
    nm <-
      model_dat |>
      dplyr::mutate(M = m_val) |>
      sam.input("M", age_range = as.numeric(1:maxage), tail_f = mean)
  }
  dat <- sam_build_data(
    natural_mortality = nm,
    surveys = surveys,
    residual_fleet = residual_fleet,
    prop_mature = prop_mature,
    stock_mean_weight = stock_mean_weight,
    catch_mean_weight = catch_mean_weight,
    dis_mean_weight = dis_mean_weight,
    land_mean_weight = land_mean_weight,
    prop_f = prop_f,
    prop_m = prop_m,
    land_frac = land_frac
  )
  par <- stockassessment::defpar(dat, conf)

  stockassessment::sam.fit(dat, conf, par)
}

#' Fit SAM with Infection-Adjusted Natural Mortality
#'
#' @param m_val Scalar multiplier applied to infection matrix.
#' @param conf SAM configuration list.
#' @param infection_dat Data frame with infection observations.
#' @param model_dat Model input data in long format.
#' @param maxage Maximum age to pass to `sam.input`.
#' @param infection_years Years used to expand infection matrix.
#' @param infection_ages Ages used to expand infection matrix.
#' @param infection_year_col Column name for infection year.
#' @param infection_age_col Column name for infection age.
#' @param infection_value_col Column name for infection value.
#' @param surveys Optional survey matrix or named list of survey matrices.
#' @param residual_fleet Optional residual fleet matrix.
#' @param prop_mature Optional maturity matrix.
#' @param stock_mean_weight Optional stock mean weight matrix.
#' @param catch_mean_weight Optional catch mean weight matrix.
#' @param dis_mean_weight Optional discard mean weight matrix.
#' @param land_mean_weight Optional landings mean weight matrix.
#' @param prop_f Optional proportion-F matrix.
#' @param prop_m Optional proportion-M matrix.
#' @param land_frac Optional landings fraction matrix.
#'
#' @return A fitted SAM object.
profile_infect_M_run <- function(
  m_val = 0.15,
  conf,
  infection_dat,
  model_dat = NULL,
  maxage = NULL,
  infection_years = NULL,
  infection_ages = NULL,
  infection_year_col = "ar",
  infection_age_col = "aldur",
  infection_value_col = "infection",
  surveys = NULL,
  residual_fleet = NULL,
  prop_mature = NULL,
  stock_mean_weight = NULL,
  catch_mean_weight = NULL,
  dis_mean_weight = NULL,
  land_mean_weight = NULL,
  prop_f = NULL,
  prop_m = NULL,
  land_frac = NULL
) {
  model_dat <- sam_resolve_arg(model_dat, "model_dat")
  maxage <- if (is.null(maxage)) max(model_dat$age, na.rm = TRUE) else maxage
  infection_years <- if (is.null(infection_years)) {
    sort(unique(model_dat$year))
  } else {
    infection_years
  }
  infection_ages <- if (is.null(infection_ages)) {
    sort(unique(model_dat$age))
  } else {
    infection_ages
  }

  nm <-
    model_dat |>
    sam.input("M", age_range = as.numeric(1:maxage), tail_f = mean)

  infect_m <-
    infection_dat |>
    dplyr::rename(
      year = !!rlang::sym(infection_year_col),
      age = !!rlang::sym(infection_age_col),
      infection = !!rlang::sym(infection_value_col)
    ) |>
    dplyr::right_join(tidyr::expand_grid(
      year = infection_years,
      age = infection_ages
    )) |>
    dplyr::arrange(year, age) |>
    dplyr::mutate(infection = tidyr::replace_na(infection, 0)) |>
    dplyr::ungroup() |>
    tidyr::pivot_wider(names_from = 'age', values_from = 'infection') |>
    dplyr::select(-year) |>
    as.matrix()

  nm <-
    nm +
    m_val * infect_m

  dat <- sam_build_data(
    natural_mortality = nm,
    surveys = surveys,
    residual_fleet = residual_fleet,
    prop_mature = prop_mature,
    stock_mean_weight = stock_mean_weight,
    catch_mean_weight = catch_mean_weight,
    dis_mean_weight = dis_mean_weight,
    land_mean_weight = land_mean_weight,
    prop_f = prop_f,
    prop_m = prop_m,
    land_frac = land_frac
  )
  par <- stockassessment::defpar(dat, conf)

  stockassessment::sam.fit(dat, conf, par)
}

#' Simulate and Refit SAM
#'
#' @param sam_fit A fitted SAM object.
#'
#' @return A `samset` list of simulated fits with the original fit as attribute.
sam_simulate <- function(sam_fit) {
  sim_dat <- stats::simulate(sam_fit, nsim = 100, full.data = TRUE)
  sim_fit <-
    parallel::mclapply(
      sim_dat,
      function(x) {
        try(stockassessment::sam.fit(x, sam_fit$conf, sam_fit$obj$env$par))
      },
      mc.cores = 30
    )
  attr(sim_fit, "fit") <- sam_fit
  class(sim_fit) <- "samset"
  return(sim_fit)
}

##
#' Compute Reference Biomass from Year-Age Outputs
#'
#' @param fit A fitted SAM object.
#'
#' @return A tibble with `year` and `ref_bio`.
ref_bio <- function(fit) {
  rbya.sam(fit) |>
    dplyr::group_by(year) |>
    dplyr::summarise(
      ref_bio = sum(
        n *
          stock_weight /
          (1 + exp(-25.224 - 5.307 * log(stock_weight * 1e3 / (44.5^3.0))))
      )
    )
}

## SAM plots

#' Plot Scaled Input Data Sources Over Time
#'
#' @param res Unused legacy argument.
#' @param model_dat Model input data.
#' @param maturation_cutoff_year Year before which maturity/weights are set to zero.
#'
#' @return A ggplot object.
model_data_plot <-
  function(
    res = NULL,
    model_dat = NULL,
    maturation_cutoff_year = NULL
  ) {
    model_dat <- sam_resolve_arg(model_dat, "model_dat")
    maturation_cutoff_year <- if (is.null(maturation_cutoff_year)) {
      min(model_dat$year, na.rm = TRUE)
    } else {
      maturation_cutoff_year
    }

    plot_dat <- tibble::tibble(year = sort(unique(model_dat$year)))

    if ("smb" %in% names(model_dat)) {
      plot_dat <- plot_dat |>
        dplyr::left_join(
          model_dat |>
            dplyr::group_by(year) |>
            dplyr::summarise(
              `Spring survey` = sum(smb, na.rm = TRUE),
              .groups = "drop"
            )
        )
    }
    if ("smh" %in% names(model_dat)) {
      plot_dat <- plot_dat |>
        dplyr::left_join(
          model_dat |>
            dplyr::group_by(year) |>
            dplyr::summarise(
              `Autumn survey` = sum(smh, na.rm = TRUE),
              .groups = "drop"
            )
        )
    }
    if ("catch" %in% names(model_dat)) {
      plot_dat <- plot_dat |>
        dplyr::left_join(
          model_dat |>
            dplyr::group_by(year) |>
            dplyr::summarise(Catch = sum(catch, na.rm = TRUE), .groups = "drop")
        )
    }
    if ("maturity" %in% names(model_dat)) {
      plot_dat <- plot_dat |>
        dplyr::left_join(
          model_dat |>
            dplyr::group_by(year) |>
            dplyr::summarise(
              Maturity = max(
                ifelse(year < maturation_cutoff_year, 0, maturity),
                na.rm = TRUE
              ),
              .groups = "drop"
            )
        )
    }
    if ("stock_weight" %in% names(model_dat)) {
      plot_dat <- plot_dat |>
        dplyr::left_join(
          model_dat |>
            dplyr::group_by(year) |>
            dplyr::summarise(
              `Stock weight` = max(
                ifelse(year < maturation_cutoff_year, 0, stock_weight),
                na.rm = TRUE
              ),
              .groups = "drop"
            )
        )
    }
    if ("catch_weight" %in% names(model_dat)) {
      plot_dat <- plot_dat |>
        dplyr::left_join(
          model_dat |>
            dplyr::group_by(year) |>
            dplyr::summarise(
              `Catch weight` = max(catch_weight, na.rm = TRUE),
              .groups = "drop"
            )
        )
    }

    plot_dat |>
      pivot_longer(-c(year), names_to = "variable", values_to = "value") |>
      dplyr::group_by(variable) |>
      dplyr::mutate(value = value / mean(value, na.rm = TRUE)) |>
      ggplot2::ggplot(ggplot2::aes(year, variable, size = value)) +
      ggplot2::geom_point(alpha = 0.5) +
      ggplot2::scale_size_area() +
      ggplot2::labs(x = "Year", y = "Data source") +
      ggplot2::theme(legend.position = "none")
  }

#' Plot Main SAM Time-Series with Optional Observed Catch Overlay
#'
#' @param res Result list with `$fit`.
#' @param model_dat Model input data.
#' @param observed_catch Whether to add observed catch points.
#' @param observed_catch_start_year First year for observed catch overlay.
#' @param observed_catch_end_year Last year for observed catch overlay.
#'
#' @return A ggplot object.
model_ices_plot <-
  function(
    res,
    model_dat,
    observed_catch = TRUE,
    observed_catch_start_year = NULL,
    observed_catch_end_year = NULL
  ) {
    observed_catch_start_year <- if (is.null(observed_catch_start_year)) {
      min(model_dat$year, na.rm = TRUE)
    } else {
      observed_catch_start_year
    }
    observed_catch_end_year <- if (is.null(observed_catch_end_year)) {
      max(model_dat$year, na.rm = TRUE)
    } else {
      observed_catch_end_year
    }

    p <- res$fit |>
      rby.sam() |>
      dplyr::filter(
        variable != "tsb",
        !(variable %in% c("fbar", "catch", "hr") & year == max(year))
      ) |>
      ggplot2::ggplot(ggplot2::aes(year, median)) +
      ggplot2::geom_ribbon(
        ggplot2::aes(ymin = lower, ymax = upper),
        alpha = 0.2,
        col = "white",
        fill = "blue"
      ) +
      ggplot2::geom_line() +
      ggplot2::facet_wrap(~variable, scales = "free_y") +
      ggplot2::labs(x = "Year", y = "", col = "") +
      ggplot2::expand_limits(y = 0)

    if (
      isTRUE(observed_catch) &&
        all(c("catch", "catch_weight") %in% names(model_dat))
    ) {
      p <- p +
        ggplot2::geom_point(
          data = model_dat |>
            dplyr::filter(
              year >= observed_catch_start_year,
              year <= observed_catch_end_year
            ) |>
            dplyr::group_by(year) |>
            dplyr::summarise(
              median = sum(catch * catch_weight, na.rm = TRUE) / 1e3,
              variable = "catch",
              .groups = "drop"
            )
        )
    }

    p
  }

#' Plot Retrospective Runs with Mohn's Rho
#'
#' @param res Result list with `$fit` and `$retro`.
#'
#' @return A ggplot object.
model_retro_plot <-
  function(res) {
    res$retro |>
      purrr::map(rby.sam) |>
      dplyr::bind_rows(.id = 'peel') |>
      dplyr::mutate(
        upper = NA_real_,
        lower = NA_real_
      ) |>
      dplyr::bind_rows(
        rby.sam(res$fit) |>
          dplyr::mutate(peel = '0')
      ) |>
      dplyr::mutate(
        assessment_year = max(year, na.rm = TRUE) - as.numeric(peel)
      ) |>
      dplyr::filter(
        variable != 'tsb',
        !(variable %in% c('fbar', 'hr') & year == assessment_year)
      ) |>
      ggplot2::ggplot(ggplot2::aes(year, median, col = peel)) +
      ggplot2::geom_ribbon(
        ggplot2::aes(ymin = lower, ymax = upper, group = peel),
        alpha = 0.2,
        col = 'white',
        fill = 'blue'
      ) +
      ggplot2::geom_line() +
      ggplot2::geom_text(
        data = stockassessment::mohn(res$retro) |>
          (\(x) {
            tibble::tibble(
              variable = c('rec', 'ssb', 'fbar'),
              value = as.numeric(x)
            )
          })(),
        ggplot2::aes(label = paste0('rho : ', round(value, 3))),
        x = -Inf,
        y = Inf,
        hjust = -.1,
        vjust = 2,
        col = 'black'
      ) +
      ggplot2::facet_wrap(~variable, scales = 'free_y') +
      ggplot2::labs(x = 'Year', y = '', col = '')
  }

#' Plot Leave-One-Out Comparisons
#'
#' @param res Result list with `$fit` and `$lo`.
#'
#' @return A ggplot object.
model_lo_plot <-
  function(res) {
    lo_rby <-
      res$lo |>
      purrr::map(rby.sam) |>
      dplyr::bind_rows(.id = 'lo')

    lo_rby |>
      dplyr::filter(variable != 'tsb') |>
      #dplyr::left_join(lnd_dat |> dplyr::mutate(variable = 'Catch'),by=c('year','variable')) |>
      ggplot2::ggplot(ggplot2::aes(year, median)) +
      ggplot2::geom_ribbon(
        ggplot2::aes(ymin = lower, ymax = upper),
        alpha = 0.5,
        fill = 'lightblue',
        data = rby.sam(res$fit) |>
          dplyr::filter(variable != 'tsb')
      ) +
      ggplot2::geom_line(ggplot2::aes(col = lo)) +
      #  ggplot2::geom_point(ggplot2::aes(y=total_landings),col='black') +
      ggplot2::geom_line(
        data = rby.sam(res$fit) |> dplyr::filter(variable != 'tsb'),
        col = 'black'
      ) +
      ggplot2::facet_wrap(~variable, scales = 'free_y') +
      ggplot2::expand_limits(y = 0) +
      ggplot2::labs(col = '', x = 'Year', y = '')
  }

#' Plot Observation Residuals by Fleet and Age
#'
#' @param res Result list with `$res`.
#'
#' @return A ggplot object.
model_resid_plot <-
  function(res) {
    res$res |>
      format_sam_res() |>
      ggplot2::ggplot(ggplot2::aes(
        year,
        age,
        size = residual,
        col = as.factor(sign(residual))
      )) +
      ggplot2::geom_point(pch = 20) +
      ggplot2::facet_wrap(~fleet, ncol = 1) +
      ggplot2::scale_size_area(max_size = 10) +
      ggplot2::labs(col = 'Sign', size = 'Size') +
      ggplot2::scale_colour_manual(values = c('red', 'blue'))
  }

#' Plot Process Residuals by Fleet and Age
#'
#' @param res Result list with `$process_res`.
#'
#' @return A ggplot object.
model_pres_resid_plot <-
  function(res) {
    if (is.null(res$process_res) || inherits(res$process_res, "try-error")) {
      return(
        ggplot2::ggplot() +
          ggplot2::annotate(
            "text",
            x = 0,
            y = 0,
            label = "Process residuals unavailable"
          ) +
          ggplot2::theme_void()
      )
    }
    res$process_res |>
      format_sam_pres() |>
      ggplot2::ggplot(ggplot2::aes(
        year,
        age,
        size = residual,
        col = as.factor(sign(residual))
      )) +
      ggplot2::geom_point(pch = 20) +
      ggplot2::facet_wrap(~fleet, ncol = 1) +
      ggplot2::scale_size_area(max_size = 10) +
      ggplot2::labs(col = 'Sign', size = 'Size') +
      ggplot2::scale_colour_manual(values = c('red', 'blue'))
  }

#' Plot Combined Survey Observed vs Predicted Fits
#'
#' @param res Result list with `$res`.
#' @param model_dat Model input data.
#' @param min_year Minimum year to include.
#' @param excluded_fleets Fleets excluded from plotting.
#' @param reference_fleet Fleet used for no age/year shifting.
#' @param age_shift_other_fleets Age shift for non-reference fleets.
#' @param year_shift_other_fleets Year shift for non-reference fleets.
#' @param max_age Maximum age after shifting.
#' @param colors Optional color vector for fleets.
#'
#' @return A ggplot object.
model_combfit <-
  function(
    res,
    model_dat,
    min_year = NULL,
    excluded_fleets = c("Residual catch"),
    reference_fleet = "spring",
    age_shift_other_fleets = 1,
    year_shift_other_fleets = 1,
    max_age = 12,
    colors = NULL
  ) {
    min_year <- if (is.null(min_year)) {
      min(model_dat$year, na.rm = TRUE)
    } else {
      min_year
    }
    if (is.null(colors)) {
      colors <- scales::hue_pal()(length(unique(format_sam_res(res$res)$fleet)))
    }

    res$res |>
      format_sam_res() |>
      dplyr::filter(
        year > min_year,
        residual != 0,
        !(fleet %in% excluded_fleets)
      ) |>
      dplyr::mutate(
        age = ifelse(
          fleet == reference_fleet,
          age,
          pmin(max_age, age + age_shift_other_fleets)
        ),
        year = ifelse(
          fleet == reference_fleet,
          year,
          year + year_shift_other_fleets
        )
      ) |>
      dplyr::left_join(
        model_dat |>
          dplyr::select(year, age, stock_weight)
      ) |>
      dplyr::group_by(year, fleet) |>
      dplyr::summarise(
        obs = sum(stock_weight * exp(observation), na.rm = TRUE) / 1e6,
        pred = sum(stock_weight * exp(mean), na.rm = TRUE) / 1e6
      ) |>
      ggplot2::ggplot(ggplot2::aes(year, obs, col = fleet)) +
      ggplot2::geom_point() +
      ggplot2::geom_line(ggplot2::aes(y = pred)) +
      ggplot2::scale_colour_manual(values = colors) +
      ggplot2::labs(col = '', y = 'Survey index', x = 'Year') +
      ggplot2::theme(
        legend.position = c(0.2, 0.8),
        legend.background = ggplot2::element_blank()
      )
  }

#' Plot Survey Index Fits by Fleet and Age
#'
#' @param res Result list with `$res`.
#'
#' @return A ggplot object.
model_sifit_plot <-
  function(res) {
    res$res |>
      format_sam_res() |>
      dplyr::filter(year > min(year) + 1, residual != 0) |>
      ggplot2::ggplot(ggplot2::aes(year, observation)) +
      ggplot2::geom_point() +
      ggplot2::facet_grid(age ~ fleet, scales = 'free_y') +
      ggplot2::geom_line(ggplot2::aes(y = mean))
  }


#' Plot Likelihood-Related Parameter Diagnostics
#'
#' @param res Result list with `$fit` and optional `$joined_F$fit`.
#' @param cor_fit Optional model fit to use for correlation parameters.
#'
#' @return A patchwork/ggplot composite.
model_lik_plot <-
  function(res, cor_fit = NULL) {
    cor_fit <- if (is.null(cor_fit)) {
      if (!is.null(res$joined_F$fit)) res$joined_F$fit else res$fit
    } else {
      cor_fit
    }

    safe_param_plot <- function(
      dat,
      y_lab = "",
      x_lab = "Age",
      expand_zero = FALSE,
      empty_lab = "No data"
    ) {
      if (nrow(dat) == 0) {
        return(
          ggplot2::ggplot() +
            ggplot2::annotate("text", x = 0, y = 0, label = empty_lab) +
            ggplot2::theme_void() +
            ggplot2::labs(y = y_lab, x = x_lab)
        )
      }
      p <- dat |>
        ggplot2::ggplot(ggplot2::aes(age, est)) +
        ggplot2::geom_errorbar(ggplot2::aes(ymin = lower, ymax = upper)) +
        ggplot2::facet_wrap(~fleet) +
        tidypax::scale_col_crayola() +
        ggplot2::labs(y = y_lab, x = x_lab)
      if (isTRUE(expand_zero)) {
        p <- p + ggplot2::expand_limits(y = 0)
      }
      p
    }

    d1 <- annotated_par_table(cor_fit) |>
      dplyr::filter(grepl('transfIRARdist', par_name)) |>
      dplyr::mutate(age = gsub('(.+)-.+', '\\1', age) |> as.numeric())
    p1 <- safe_param_plot(
      d1,
      y_lab = 'Estimated correlation',
      empty_lab = "No correlation parameters"
    )

    d2 <- annotated_par_table(res$fit) |>
      dplyr::filter(grepl('SdLogObs', par_name)) |>
      dplyr::mutate(age = gsub('(.+)-.+', '\\1', age) |> as.numeric())
    p2 <- safe_param_plot(
      d2,
      y_lab = 'Estimated predvar link alpha',
      empty_lab = "No SdLogObs parameters"
    )

    d3 <- annotated_par_table(res$fit) |>
      dplyr::filter(grepl('predVar', par_name)) |>
      dplyr::mutate(age = gsub('(.+)-.+', '\\1', age) |> as.numeric())
    p3 <- safe_param_plot(
      d3,
      y_lab = 'Estimated predvar link alpha',
      empty_lab = "No predVar parameters"
    )

    d4 <- annotated_par_table(res$fit) |>
      dplyr::filter(grepl('LogN|Fsta_', par_name)) |>
      dplyr::mutate(
        fleet = dplyr::case_when(
          grepl('LogN', par_name) ~ 'sd(log(N))',
          TRUE ~ 'sd(log(F))'
        ),
        age = gsub('(.+)-.+', '\\1', age) |> as.numeric()
      )
    p4 <- safe_param_plot(
      d4,
      y_lab = 'Process variances',
      x_lab = 'Age',
      expand_zero = TRUE,
      empty_lab = "No process-variance parameters"
    )

    patchwork::wrap_plots(p1, p2, p3, p4, ncol = 1)
  }


#' Map Fleet IDs to Display Labels
#'
#' @param fleet Character vector of fleet ids.
#' @param labels Optional named vector/list used for recoding.
#'
#' @return A character vector of fleet labels.
sam_default_fleet_label <- function(fleet, labels = NULL) {
  if (!is.null(labels)) {
    return(dplyr::recode(fleet, !!!labels, .default = fleet))
  }
  ifelse(
    grepl("2", fleet),
    "Spring survey (fleet 2)",
    "Autumn survey (fleet 1)"
  )
}

#' Plot Selectivity Diagnostics and Stock-Recruit Relationship
#'
#' @param res Result list with `$fit`.
#' @param stock_weight_limit Upper stock-weight bound for one panel.
#' @param fleet_labels Optional named recode mapping for fleet labels.
#'
#' @return A patchwork/ggplot composite.
model_selectivity_SR_plot <-
  function(
    res,
    stock_weight_limit = 4,
    fleet_labels = NULL
  ) {
    p1 <- res$fit |>
      rbya.sam() |>
      dplyr::group_by(year) |>
      dplyr::mutate(sel = f / max(f)) |>
      ggplot2::ggplot(ggplot2::aes(age, sel, col = as.ordered(year))) +
      ggplot2::geom_line() +
      ggplot2::theme(legend.position = 'none') +
      ggplot2::labs(x = 'Age', y = 'Selectivity')

    p2 <- res$fit |>
      rbya.sam() |>
      dplyr::group_by(year) |>
      dplyr::mutate(sel = f / max(f)) |>
      dplyr::filter(stock_weight < stock_weight_limit) |>
      ggplot2::ggplot(ggplot2::aes(stock_weight, sel, col = as.ordered(year))) +
      ggplot2::geom_line() +
      ggplot2::theme(legend.position = 'none') +
      ggplot2::labs(x = 'Stock weight', y = '')

    p3 <- annotated_par_table(res$fit) |>
      dplyr::filter(grepl('Fpar', par_name)) |>
      dplyr::mutate(
        age = gsub('(.+)-.+', '\\1', age) |> as.numeric(),
        fleet = sam_default_fleet_label(fleet, labels = fleet_labels)
      ) |>
      ggplot2::ggplot(ggplot2::aes(age, est)) +
      ggplot2::geom_errorbar(ggplot2::aes(ymin = lower, ymax = upper)) +
      ggplot2::facet_wrap(~fleet) +
      tidypax::scale_col_crayola() +
      ggplot2::labs(y = 'Survey catchability', x = 'Age')

    p4 <- res$fit |>
      rby.sam() |>
      dplyr::filter(variable %in% c('ssb', 'rec')) |>
      dplyr::mutate(year = ifelse(variable == 'ssb', year + 1, year)) |>
      tidyr::pivot_wider(
        names_from = 'variable',
        values_from = c(median, lower, upper)
      ) |>
      dplyr::arrange(year) |>
      ggplot2::ggplot(ggplot2::aes(median_ssb, median_rec)) +
      ggplot2::geom_path(col = 'gray') +
      ggplot2::geom_errorbarh(ggplot2::aes(
        xmin = lower_ssb,
        xmax = upper_ssb
      )) +
      ggplot2::geom_errorbar(ggplot2::aes(ymin = lower_rec, ymax = upper_rec)) +
      ggplot2::geom_text(ggplot2::aes(label = year), col = 'red') +
      ggplot2::labs(y = 'Recruitment', y = 'SSB')

    patchwork::wrap_plots(
      A = p1,
      B = p2,
      C = p3,
      D = p4,
      design = "AB
CC
DD"
    )
  }

#' Plot Selectivity Diagnostics
#'
#' @param res Result list with `$fit`.
#' @param stock_weight_limit Upper stock-weight bound for one panel.
#' @param fleet_labels Optional named recode mapping for fleet labels.
#'
#' @return A patchwork/ggplot composite.
model_selectivity_plot <-
  function(
    res,
    stock_weight_limit = 4,
    fleet_labels = NULL
  ) {
    p1 <- res$fit |>
      rbya.sam() |>
      dplyr::group_by(year) |>
      dplyr::mutate(sel = f / max(f)) |>
      ggplot2::ggplot(ggplot2::aes(age, sel, col = as.ordered(year))) +
      ggplot2::geom_line() +
      ggplot2::theme(legend.position = 'none') +
      ggplot2::labs(x = 'Age', y = 'Selectivity')

    p2 <- res$fit |>
      rbya.sam() |>
      dplyr::group_by(year) |>
      dplyr::mutate(sel = f / max(f)) |>
      dplyr::filter(stock_weight < stock_weight_limit) |>
      ggplot2::ggplot(ggplot2::aes(stock_weight, sel, col = as.ordered(year))) +
      ggplot2::geom_line() +
      ggplot2::theme(legend.position = 'none') +
      ggplot2::labs(x = 'Stock weight', y = '')

    p3 <- annotated_par_table(res$fit) |>
      dplyr::filter(grepl('Fpar', par_name)) |>
      dplyr::mutate(
        age = gsub('(.+)-.+', '\\1', age) |> as.numeric(),
        fleet = sam_default_fleet_label(fleet, labels = fleet_labels)
      ) |>
      ggplot2::ggplot(ggplot2::aes(age, est)) +
      ggplot2::geom_errorbar(ggplot2::aes(ymin = lower, ymax = upper)) +
      ggplot2::facet_wrap(~fleet) +
      tidypax::scale_col_crayola() +
      ggplot2::labs(y = 'Survey catchability', x = 'Age')

    patchwork::wrap_plots(
      A = p1,
      B = p2,
      C = p3,
      design = "AB
CC"
    )
  }

#' Plot Stock-Recruit Relationship with Uncertainty
#'
#' @param res Result list with `$fit`.
#' @param ssb_year_offset Offset applied to SSB year to align with recruitment.
#'
#' @return A ggplot object.
SR_plot <- function(res, ssb_year_offset = 1) {
  res$fit |>
    rby.sam() |>
    dplyr::filter(variable %in% c('ssb', 'rec')) |>
    dplyr::mutate(
      year = ifelse(variable == 'ssb', year + ssb_year_offset, year)
    ) |>
    tidyr::pivot_wider(
      names_from = 'variable',
      values_from = c(median, lower, upper)
    ) |>
    dplyr::arrange(year) |>
    ggplot2::ggplot(ggplot2::aes(median_ssb, median_rec)) +
    ggplot2::geom_path(col = 'gray') +
    ggplot2::geom_errorbarh(ggplot2::aes(xmin = lower_ssb, xmax = upper_ssb)) +
    ggplot2::geom_errorbar(ggplot2::aes(ymin = lower_rec, ymax = upper_rec)) +
    ggplot2::geom_text(ggplot2::aes(label = year), col = 'red') +
    ggplot2::labs(y = 'Recruitment', y = 'SSB')
}
