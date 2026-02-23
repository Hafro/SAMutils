#' Render Static Quarto Dashboard from SAM Fit Output
#'
#' Renders a static HTML dashboard (Quarto format) using the output from
#' [full_sam_fit()].
#'
#' @param sam_fit Output object from [full_sam_fit()].
#' @param output_file Output HTML file name.
#' @param output_dir Directory where output file is written.
#' @param title Dashboard title (currently informational; template title is fixed).
#' @param input_tables Optional named list of input tables for the Input Data panel.
#' @param model_dat Optional long data frame with `year`, `age`, `stock_weight`.
#'
#' @return Invisibly returns the rendered output file path.
#' @export
render_quarto_dashboard <- function(
  sam_fit,
  output_file = "sam-dashboard.html",
  output_dir = ".",
  title = "SAM Dashboard",
  input_tables = NULL,
  model_dat = NULL
) {
  if (is.null(sam_fit$fit)) {
    stop("`sam_fit` must be output from `full_sam_fit()` and include `$fit`.")
  }

  template <- file.path("inst", "quarto", "sam-dashboard.qmd")
  if (!file.exists(template)) {
    template <- system.file("quarto", "sam-dashboard.qmd", package = "SAMutils")
  }
  if (!file.exists(template)) {
    stop("Quarto dashboard template not found.")
  }

  bundle <- sam_dashboard_bundle(
    sam_fit = sam_fit,
    input_tables = input_tables,
    model_dat = model_dat
  )

  render_dir <- tempfile("sam_dashboard_render_")
  dir.create(render_dir, recursive = TRUE, showWarnings = FALSE)

  bundle_path <- file.path(render_dir, "bundle.rds")
  saveRDS(bundle, bundle_path)

  qmd_path <- file.path(render_dir, "sam-dashboard.qmd")
  file.copy(template, qmd_path, overwrite = TRUE)

  base_wd <- getwd()
  output_dir_abs <- output_dir
  if (!grepl("^(/|~|[A-Za-z]:)", output_dir_abs)) {
    output_dir_abs <- file.path(base_wd, output_dir_abs)
  }
  output_dir_abs <- normalizePath(path.expand(output_dir_abs), mustWork = FALSE)
  dir.create(output_dir_abs, recursive = TRUE, showWarnings = FALSE)
  out_file <- file.path(output_dir_abs, output_file)

  q_bin <- Sys.which("quarto")
  if (!nzchar(q_bin)) {
    stop("Quarto not found. Install package 'quarto' or Quarto CLI.")
  }

  rendered_name <- basename(out_file)
  args <- c(
    "render", "sam-dashboard.qmd",
    "--output", rendered_name,
    "-P", paste0("bundle_path:", normalizePath(bundle_path, mustWork = TRUE)),
    "--quiet"
  )
  old_wd <- getwd()
  on.exit(setwd(old_wd), add = TRUE)
  setwd(render_dir)
  status <- system2(q_bin, args = args)
  if (!identical(status, 0L)) {
    stop("Quarto render failed with status: ", status)
  }

  rendered_path <- file.path(render_dir, rendered_name)
  if (!file.exists(rendered_path)) {
    stop("Quarto render did not produce expected file: ", rendered_path)
  }

  if (!file.copy(rendered_path, out_file, overwrite = TRUE)) {
    stop("Failed to copy rendered dashboard to: ", out_file)
  }

  invisible(normalizePath(out_file, mustWork = FALSE))
}
