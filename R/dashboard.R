#' Convert SAM Input Object to Table-Friendly Data Frame
#'
#' @param x Object read by `stockassessment::read.ices()`.
#'
#' @return A data frame suitable for display in dashboard tables.
#' @keywords internal
sam_dashboard_as_table <- function(x) {
  if (is.array(x) && !is.matrix(x)) {
    dx <- dim(x)
    # Common SAM shape: year x age x fleet. If a single fleet, present as year-age table.
    if (length(dx) == 3 && !is.na(dx[3]) && dx[3] == 1) {
      return(sam_dashboard_as_table(x[, , 1, drop = TRUE]))
    }

    out <- as.data.frame.table(x, stringsAsFactors = FALSE, responseName = "value")
    dnn <- names(dimnames(x))
    if (!is.null(dnn) && length(dnn) == ncol(out) - 1) {
      names(out)[seq_along(dnn)] <- dnn
    }
    return(out)
  }

  if (is.matrix(x) || is.data.frame(x)) {
    out <- as.data.frame(x, stringsAsFactors = FALSE, check.names = FALSE)
    rn <- rownames(out)
    if (!is.null(rn) && any(nzchar(rn))) {
      out <- cbind(year = rn, out)
      rownames(out) <- NULL
    }
    return(out)
  }

  if (is.list(x)) {
    item_names <- names(x)
    if (is.null(item_names)) {
      item_names <- paste0("item_", seq_along(x))
    }
    preview <- vapply(x, function(el) {
      if (length(el) == 0) {
        return("")
      }
      paste(utils::head(as.vector(el), 5), collapse = ", ")
    }, FUN.VALUE = character(1))
    n <- vapply(x, length, FUN.VALUE = integer(1))
    return(data.frame(field = item_names, length = n, preview = preview, stringsAsFactors = FALSE))
  }

  if (is.vector(x)) {
    return(data.frame(value = x, stringsAsFactors = FALSE))
  }

  data.frame(value = as.character(x), stringsAsFactors = FALSE)
}

#' Read NSHER SAM Input Files
#'
#' @param model_dir Directory containing NSHER `.dat` input files.
#'
#' @return A named list of raw ICES inputs.
#' @keywords internal
sam_dashboard_read_inputs <- function(model_dir) {
  dat_files <- sort(list.files(model_dir, pattern = "\\.dat$", full.names = TRUE))
  if (length(dat_files) == 0) {
    stop("No .dat files found in `model_dir`.")
  }

  out <- lapply(dat_files, stockassessment::read.ices)
  names(out) <- tools::file_path_sans_ext(basename(dat_files))
  out
}

#' Prepare NSHER Model Bundle for Dashboard
#'
#' @param model_dir Directory containing NSHER inputs (defaults to `testmore/nsher`).
#' @param retro_year Number of retrospective peels.
#'
#' @return A list with raw inputs, table inputs, fit bundle, and standard outputs.
#' @export
sam_dashboard_prepare <- function(model_dir = "testmore/nsher", retro_year = 5) {
  raw_inputs <- sam_dashboard_read_inputs(model_dir)

  needed <- c("cn", "cw", "dw", "lf", "lw", "mo", "nm", "pf", "pm", "sw", "survey")
  missing <- setdiff(needed, names(raw_inputs))
  if (length(missing) > 0) {
    stop("Missing required NSHER input files: ", paste(missing, collapse = ", "))
  }

  dat <- stockassessment::setup.sam.data(
    surveys = raw_inputs$survey,
    residual.fleets = raw_inputs$cn,
    prop.mature = raw_inputs$mo,
    stock.mean.weight = raw_inputs$sw,
    catch.mean.weight = raw_inputs$cw,
    dis.mean.weight = raw_inputs$dw,
    land.mean.weight = raw_inputs$lw,
    prop.f = raw_inputs$pf,
    prop.m = raw_inputs$pm,
    natural.mortality = raw_inputs$nm,
    land.frac = raw_inputs$lf
  )

  conf <- stockassessment::defcon(dat)
  conf$fbarRange <- c(2, 6)
  conf$corFlag <- 1
  conf$keyLogFpar <- matrix(
    c(
      -1, -1, -1, -1, -1, -1, -1, -1, -1,
      -1, 0, 1, 2, 3, 4, 5, 6, -1,
      -1, 7, -1, -1, -1, -1, -1, -1, -1,
      8, -1, -1, -1, -1, -1, -1, -1, -1
    ),
    nrow = 4,
    byrow = TRUE
  )

  res <- {
    out <- NULL
    utils::capture.output({
      out <- full_sam_fit(dat, conf)
    })
    out
  }

  if (!is.null(retro_year) && !inherits(res$fit, "try-error")) {
    res$retro <- {
      out <- NULL
      utils::capture.output({
        out <- try(stockassessment::retro(res$fit, year = retro_year, ncores = 1))
      })
      out
    }
  }

  model_dat <- as.data.frame.table(raw_inputs$sw, stringsAsFactors = FALSE, responseName = "stock_weight") |>
    dplyr::rename(year = Var1, age = Var2) |>
    dplyr::mutate(
      year = suppressWarnings(as.numeric(as.character(year))),
      age = suppressWarnings(as.numeric(as.character(age)))
    ) |>
    tibble::as_tibble()

  std_out <- rby.sam(res$fit) |>
    dplyr::filter(variable %in% c("fbar", "ssb", "rec", "catch"))

  list(
    raw_inputs = raw_inputs,
    input_tables = lapply(raw_inputs, sam_dashboard_as_table),
    dat = dat,
    conf = conf,
    par = res$par,
    model_dat = model_dat,
    res = res,
    standard_output = std_out
  )
}

#' Build Input Tables from a Fitted SAM Object
#'
#' @param sam_fit Output object from `full_sam_fit()`.
#'
#' @return A named list of data frames for dashboard input tabs.
#' @keywords internal
sam_dashboard_obs_table <- function(sam_fit, fleet_idx) {
  obs <- sam_fit$res
  if (is.null(obs) || length(obs$year) == 0 || length(fleet_idx) == 0) {
    return(NULL)
  }

  d <- tibble::tibble(
    year = as.numeric(obs$year),
    fleet = as.integer(obs$fleet),
    age = as.numeric(obs$age),
    observation = as.numeric(obs$observation)
  ) |>
    dplyr::filter(
      fleet %in% fleet_idx,
      !is.na(year),
      !is.na(age),
      !is.na(observation)
    ) |>
    dplyr::mutate(value = exp(observation)) |>
    dplyr::group_by(year, age) |>
    dplyr::summarise(value = sum(value, na.rm = TRUE), .groups = "drop") |>
    tidyr::pivot_wider(names_from = age, values_from = value, values_fill = 0) |>
    dplyr::arrange(year)

  if (nrow(d) == 0) {
    return(NULL)
  }
  as.data.frame(d, stringsAsFactors = FALSE)
}

sam_dashboard_input_tables_from_fit <- function(sam_fit) {
  fit <- sam_fit$fit
  dat <- fit$data
  keep <- c(
    "stockMeanWeight", "catchMeanWeight", "natMor", "propMat",
    "propF", "propM", "landFrac", "disMeanWeight", "landMeanWeight"
  )
  keep <- keep[keep %in% names(dat)]
  out <- lapply(dat[keep], sam_dashboard_as_table)
  names(out) <- keep

  if (!is.null(fit$data$fleetTypes)) {
    fleet_names <- attr(sam_fit$res, "fleetNames")
    if (is.null(fleet_names) || length(fleet_names) < fit$data$noFleets) {
      fleet_names <- paste0("fleet_", seq_len(fit$data$noFleets))
    }

    catch_idx <- which(fit$data$fleetTypes == 0)
    survey_idx <- which(fit$data$fleetTypes != 0)

    if (length(catch_idx) > 0) {
      out[["Catch at age"]] <- sam_dashboard_obs_table(sam_fit, catch_idx)
    }
    if (length(survey_idx) > 0) {
      for (i in survey_idx) {
        nm <- fleet_names[i]
        out[[paste0("Survey at age: ", nm)]] <- sam_dashboard_obs_table(sam_fit, i)
      }
    }
  }

  out <- out[!vapply(out, is.null, logical(1))]
  out
}

#' Convert `full_sam_fit` Output to a Dashboard Bundle
#'
#' @param sam_fit Output object from [full_sam_fit()].
#' @param input_tables Optional named list of input tables for the Input Data tab.
#' @param model_dat Optional long data frame with `year`, `age`, `stock_weight`.
#'
#' @return A dashboard bundle consumed by [dashboard_app()].
#' @keywords internal
sam_dashboard_bundle <- function(sam_fit, input_tables = NULL, model_dat = NULL) {
  if (is.null(sam_fit$fit)) {
    stop("`sam_fit` must be output from `full_sam_fit()` and include `$fit`.")
  }

  if (is.null(model_dat)) {
    model_dat <- as.data.frame.table(sam_fit$fit$data$stockMeanWeight, stringsAsFactors = FALSE, responseName = "stock_weight") |>
      dplyr::rename(year = Var1, age = Var2) |>
      dplyr::mutate(
        year = suppressWarnings(as.numeric(as.character(year))),
        age = suppressWarnings(as.numeric(as.character(age)))
      ) |>
      tibble::as_tibble()
  }

  if (is.null(input_tables)) {
    input_tables <- sam_dashboard_input_tables_from_fit(sam_fit)
  }

  std_out <- rby.sam(sam_fit$fit) |>
    dplyr::filter(variable %in% c("fbar", "ssb", "rec", "catch"))

  list(
    input_tables = input_tables,
    model_dat = model_dat,
    res = sam_fit,
    standard_output = std_out
  )
}

#' Launch Dashboard from `full_sam_fit` Output
#'
#' @param sam_fit Output object from [full_sam_fit()].
#' @param input_tables Optional named list of input tables for the Input Data tab.
#' @param model_dat Optional long data frame with `year`, `age`, `stock_weight`.
#' @param title Dashboard title.
#' @param output_file Output HTML filename.
#' @param output_dir Output directory for rendered HTML.
#'
#' @return Path to rendered HTML dashboard.
#' @export
dashboard_app <- function(
  sam_fit,
  input_tables = NULL,
  model_dat = NULL,
  title = "SAMutils Dashboard",
  output_file = "sam-dashboard.html",
  output_dir = "."
) {
  render_quarto_dashboard(
    sam_fit = sam_fit,
    input_tables = input_tables,
    model_dat = model_dat,
    output_file = output_file,
    output_dir = output_dir,
    title = title
  )
}

#' Build SAM Dashboard App
#'
#' @param model_dir Directory containing NSHER inputs.
#' @param retro_year Number of retrospective peels.
#' @param output_file Output HTML filename.
#' @param output_dir Output directory for rendered HTML.
#'
#' @return Path to rendered HTML dashboard.
#' @export
sam_dashboard_app <- function(
  model_dir = "testmore/nsher",
  retro_year = 5,
  output_file = "sam-dashboard.html",
  output_dir = "."
) {
  bundle <- sam_dashboard_prepare(model_dir = model_dir, retro_year = retro_year)
  dashboard_app(
    sam_fit = bundle$res,
    input_tables = bundle$input_tables,
    model_dat = bundle$model_dat,
    output_file = output_file,
    output_dir = output_dir
  )
}

#' Run SAM Dashboard
#'
#' @param model_dir Directory containing NSHER inputs.
#' @param retro_year Number of retrospective peels.
#' @param output_file Output HTML filename.
#' @param output_dir Output directory for rendered HTML.
#' @param launch.browser If `TRUE`, attempts to open the rendered file.
#'
#' @return Path to rendered HTML dashboard.
#' @export
run_sam_dashboard <- function(
  model_dir = "testmore/nsher",
  retro_year = 5,
  output_file = "sam-dashboard.html",
  output_dir = ".",
  launch.browser = interactive()
) {
  out <- sam_dashboard_app(
    model_dir = model_dir,
    retro_year = retro_year,
    output_file = output_file,
    output_dir = output_dir
  )

  if (isTRUE(launch.browser)) {
    utils::browseURL(out)
  }

  invisible(out)
}
