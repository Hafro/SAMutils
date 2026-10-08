fake_sam_fit <- function() {
  years <- 2001:2003
  ages <- 1:3

  make_mat <- function(val) {
    matrix(
      val,
      nrow = length(years),
      ncol = length(ages),
      dimnames = list(as.character(years), as.character(ages))
    )
  }

  conf_mat <- matrix(
    c(1, 2, 3, 4, 5, 6),
    nrow = 2,
    ncol = 3
  )

  structure(
    list(
      data = list(
        years = years,
        minAge = c(1, 1),
        maxAge = c(3, 3),
        fleetTypes = c(0, 1),
        noFleets = 2,
        stockMeanWeight = make_mat(1),
        catchMeanWeight = make_mat(2),
        propMat = make_mat(0.5),
        propF = make_mat(0.7),
        propM = make_mat(0.3),
        natMor = make_mat(0.2)
      ),
      conf = list(
        keyLogFpar = conf_mat,
        keyLogFsta = conf_mat,
        keyQpow = conf_mat,
        keyVarF = conf_mat,
        keyVarObs = conf_mat,
        keyVarLogN = conf_mat,
        keyCorObs = matrix(c(1, 2, 3, 4), nrow = 2),
        predVarObsLink = matrix(c(1, 2), nrow = 1)
      )
    ),
    class = "sam_fake"
  )
}

register_sa_method <- function(generic, class, method) {
  ns <- asNamespace("stockassessment")
  old <- getS3method(generic, class, optional = TRUE, envir = ns)
  registerS3method(generic, class, method, envir = ns)
  function() {
    if (is.null(old)) {
      s3_table <- get(".__S3MethodsTable__.", envir = ns)
      key <- paste0(generic, ".", class)
      if (exists(key, envir = s3_table, inherits = FALSE)) {
        rm(list = key, envir = s3_table)
      }
    } else {
      registerS3method(generic, class, old, envir = ns)
    }
  }
}

register_core_fake_methods <- function(procres_method = NULL) {
  years <- as.character(2001:2003)

  series_mat <- function(base_val) {
    matrix(
      c(base_val + 0:2, base_val - 1 + 0:2, base_val + 1 + 0:2),
      nrow = 3,
      ncol = 3,
      dimnames = list(years, c("Estimate", "Low", "High"))
    )
  }

  n_age_mat <- function(base_val) {
    matrix(
      base_val + 0:8,
      nrow = 3,
      ncol = 3,
      dimnames = list(years, c("1", "2", "3"))
    )
  }

  methods <- list(
    register_sa_method("ssbtable", "sam_fake", function(object, ...) series_mat(10)),
    register_sa_method("tsbtable", "sam_fake", function(object, ...) series_mat(20)),
    register_sa_method("fbartable", "sam_fake", function(object, ...) series_mat(30)),
    register_sa_method("rectable", "sam_fake", function(object, ...) series_mat(40)),
    register_sa_method("catchtable", "sam_fake", function(object, ...) series_mat(50)),
    register_sa_method("ntable", "sam_fake", function(object, ...) n_age_mat(100)),
    register_sa_method("faytable", "sam_fake", function(object, ...) n_age_mat(1)),
    register_sa_method(
      "partable",
      "sam_fake",
      function(object, ...) {
        x <- matrix(
          c(
            0.1, 0.01, 1.1, 1.0, 1.2,
            0.2, 0.02, 1.2, 1.1, 1.3,
            0.3, 0.03, 1.3, 1.2, 1.4
          ),
          nrow = 3,
          byrow = TRUE
        )
        rownames(x) <- c("logFpar_1", "logSdLogN_2", "transfIRARdist_3")
        x
      }
    )
  )

  if (!is.null(procres_method)) {
    methods <- c(methods, list(register_sa_method("procres", "sam_fake", procres_method)))
  }

  methods
}

restore_all <- function(restores) {
  lapply(rev(restores), function(f) f())
}

test_that("rby.sam returns base series when ref-bio internals fail", {
  fit <- fake_sam_fit()
  restores <- register_core_fake_methods()
  on.exit(restore_all(restores), add = TRUE)

  old_tableit <- get("tableit", envir = asNamespace("stockassessment"))
  assignInNamespace(
    "tableit",
    function(...) stop("no ref"),
    ns = "stockassessment"
  )
  on.exit(assignInNamespace("tableit", old_tableit, ns = "stockassessment"), add = TRUE)

  out <- rby.sam(fit, run_ref_bio = TRUE)
  expect_s3_class(out, "tbl_df")
  expect_setequal(unique(out$variable), c("ssb", "tsb", "fbar", "rec", "catch"))
  expect_equal(nrow(out), 15)
})

test_that("rbya.sam returns year-age outputs with optional stock joins", {
  fit <- fake_sam_fit()
  restores <- register_core_fake_methods()
  on.exit(restore_all(restores), add = TRUE)

  out <- rbya.sam(fit, include_parameter_table = FALSE)
  expect_equal(nrow(out), 9)
  expect_true(all(c("year", "age", "n", "f", "stock_weight", "M") %in% names(out)))
})

test_that("rbya.sam is fail-open when annotated_par_table fails", {
  fit <- fake_sam_fit()
  restores <- register_core_fake_methods()
  on.exit(restore_all(restores), add = TRUE)

  old_fun <- SAMutils:::annotated_par_table
  assignInNamespace("annotated_par_table", function(...) stop("boom"), ns = "SAMutils")
  on.exit(assignInNamespace("annotated_par_table", old_fun, ns = "SAMutils"), add = TRUE)

  out <- rbya.sam(fit, include_parameter_table = TRUE)
  expect_equal(nrow(out), 9)
  expect_true(all(c("year", "age", "n", "f") %in% names(out)))
})

test_that("annotated_par_table handles variant conf matrix dimensions", {
  fit <- fake_sam_fit()
  restores <- register_core_fake_methods()
  on.exit(restore_all(restores), add = TRUE)

  out <- suppressWarnings(annotated_par_table(fit))
  expect_s3_class(out, "tbl_df")
  expect_true(all(c("par_name", "est", "lower", "upper") %in% names(out)))
  expect_gt(nrow(out), 0)
})

test_that("format_sam_pres handles uneven and empty residual components", {
  uneven <- list(
    year = c(2001, 2002),
    fleet = 1L,
    age = numeric(),
    residual = c(0.2, 0)
  )
  attr(uneven, "fleetNames") <- c("fleet_1")

  out <- format_sam_pres(uneven)
  expect_equal(nrow(out), 2)
  expect_equal(out$fleet[1], "fleet_1")
  expect_true(is.na(out$residual[2]))

  empty <- list(year = numeric(), fleet = numeric(), age = numeric(), residual = numeric())
  out_empty <- format_sam_pres(empty)
  expect_equal(nrow(out_empty), 0)
})

test_that("safe_procres returns NULL on procres error and object on success", {
  fit <- fake_sam_fit()

  ns <- asNamespace("stockassessment")
  old_procres <- get("procres", envir = ns)
  on.exit(assignInNamespace("procres", old_procres, ns = "stockassessment"), add = TRUE)

  assignInNamespace(
    "procres",
    function(fit, ...) stop("procres failed"),
    ns = "stockassessment"
  )
  expect_null(safe_procres(fit))

  assignInNamespace(
    "procres",
    function(fit, ...) {
      x <- list(
        year = c(2001, 2002),
        fleet = c(1, 1),
        age = c(1, 2),
        residual = c(0.1, -0.2)
      )
      attr(x, "fleetNames") <- c("fleet_1")
      x
    },
    ns = "stockassessment"
  )
  expect_type(safe_procres(fit), "list")
})

test_that("model_pres_resid_plot is resilient with missing process residuals", {
  p <- model_pres_resid_plot(list(process_res = NULL))
  expect_s3_class(p, "ggplot")

  process_res <- list(
    year = c(2001, 2002),
    fleet = c(1, 1),
    age = c(1, 2),
    residual = c(0.1, -0.2)
  )
  attr(process_res, "fleetNames") <- c("fleet_1")

  p2 <- model_pres_resid_plot(list(process_res = process_res))
  expect_s3_class(p2, "ggplot")
})

test_that("sam.input keeps rows and years together when a year has no data", {
  dat <- tibble::tibble(
    year = c(2000, 2000, 2001, 2001, 2002, 2002),
    age = c(1, 2, 1, 2, 1, 2),
    smb = c(NA, NA, 10, 20, 30, 40)
  )
  m <- sam.input(dat, "smb", age_range = 1:2, year_range = 2000:2002)
  expect_equal(rownames(m), c("2000", "2001", "2002"))
  expect_equal(unname(m["2001", ]), c(10, 20))
  expect_equal(unname(m["2002", ]), c(30, 40))
  expect_true(all(is.na(m["2000", ])))
})

test_that("input_data_plot works without tidyr attached", {
  dat <- tibble::tibble(year = c(2000, 2001), age = 1, catch = c(1, 2))
  expect_s3_class(input_data_plot(dat), "ggplot")
})
