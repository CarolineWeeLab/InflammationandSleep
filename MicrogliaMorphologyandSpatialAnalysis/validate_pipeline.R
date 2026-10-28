# Synthetic pipeline validation.
# Synthetic data checks; no manuscript data are read or changed.
source("Microglia_common.R")
source("2D_3D_cell_regeneration.R")
source("Microglia_Radar.R")
source("PCs_plotting.R")
source("Microglia_spatial_analysis.R")

expect_error <- function(expr) {
    result <- tryCatch({ force(expr); FALSE }, error = function(e) TRUE)
    stopifnot(result)
}
work <- Sys.getenv("MICROGLIA_TEST_OUTPUT", tempfile("microglia_checks_"))
dir.create(work, showWarnings = FALSE)
expts <- paste("20260101", sprintf("fish%02d", 0:4))
metadata <- data.frame(expt = expts, treatment = c("para", "para", "yolk", "yolk", "para"), origin = 20)
metadata_path <- file.path(work, "metadata.csv")
write.csv(metadata, metadata_path, row.names = FALSE)
df <- expand.grid(cell = 1:4, time_hr = c(0, 0.49, 1), expt = expts, stringsAsFactors = FALSE)
df <- df[!(df$expt == expts[5] & df$time_hr == 1), ]
df$condition <- "old_label"
df$coords <- lapply(seq_len(nrow(df)), function(i) {
    a <- as.matrix(expand.grid(z = 0:2, y = 0:3, x = 0:(2 + i %% 3)))
    sweep(a, 2, c(0, i %% 3, i %% 4), "+")
})
df$pandas_index <- as.character(seq_len(nrow(df)) - 1L)
df$source_row <- seq_len(nrow(df))
df$source_file <- "synthetic.pkl"
args <- list(df = df, metadata_csv = metadata_path, day = 0L, days = 0L,
    start_time = "20:00", end_time = "21:00", metadata_time_zero_col = "origin")
selected <- do.call(select_microglia, c(args, list(complete_cohort = TRUE)))
stopifnot(nrow(selected) == 48L, length(unique(selected$batch)) == 4L,
    all(selected$condition %in% c("para", "yolk")),
    any(attr(selected, "filter_audit")$reason == "incomplete_fish"),
    identical(sort(unique(selected$nominal_time)), c(20, 20.5, 21)))
# A globally absent scheduled time must not be treated as a complete cohort.
missing_time <- df[df$time_hr != 0.49, ]
expect_error(do.call(select_microglia, c(modifyList(args, list(df = missing_time)), list(complete_cohort = TRUE))))
# A midnight window uses the end-day offset, not a duration in days.
overnight <- args
overnight$metadata_csv <- transform(metadata, origin = 23.5)
overnight$days <- 1L
overnight$start_time <- "23:30"
overnight$end_time <- "00:30"
over <- do.call(select_microglia, overnight)
stopifnot(identical(sort(unique(over$nominal_time)), c(23.5, 24, 24.5)))
expect_error(.microglia_read_metadata(rbind(metadata, metadata[1, ]), "origin"))
wide <- data.frame(expt = "20260101 fish00", check.names = FALSE)
wide[["0 days 20:00:00"]] <- "0"
wide[["0 days 20:30:00"]] <- "1"
stopifnot(.microglia_read_metadata(wide)$metadata_start_hour == 20)

# Export names and selected names must agree, including after filtering treatments.
export <- do.call(export_microglia, c(args, list(image_output_dir = work, dry_run = TRUE)))
stopifnot(all(selected$filename %in% export$filename[export$selected]), all(export$status[export$selected] == "validated"))
mask <- make_cell_image(matrix(c(0, 4, 5, 1, 5, 6), ncol = 3, byrow = TRUE), "3D", 1L, 1L)
stopifnot(sum(mask > 0) == 2L, identical(dim(mask), c(4L, 4L, 1L, 4L)))
expect_error(make_fish_image(list(matrix(c(0, 600, 600), nrow = 1)), "2D", c(512, 512), 0L))

# Write and read back a small real TIFF export, including its saved lookup.
written <- export_microglia(df, file.path(work, "tiffs"), metadata_path,
    expts = expts[1], day = 0, days = 0, start_time = "20:00", end_time = "20:00",
    metadata_time_zero_col = "origin", mode = "3D")
written_dir <- attr(written, "output_dir")
stopifnot(sum(written$status == "written") == 4L,
    file.exists(file.path(written_dir, "microglia_lookup.csv")))
first_tiff <- file.path(written_dir, written$filename[written$status == "written"][1])
restored <- ijtiff::read_tif(first_tiff, msg = FALSE)
stopifnot(sum(restored > 0) == nrow(df$coords[[1]]))

set.seed(42)
measurements <- data.frame(filename = selected$filename, Volume = runif(48, 5, 20),
    SurfaceArea = runif(48, 10, 30), Solidity3D = runif(48, 0.1, 1), Tortuosity = runif(48, 1, 3))
measures <- names(measurements)[-1]
lookup <- export[export$status == "validated", ]
lookup_path <- file.path(work, "microglia_lookup.csv")
write.csv(lookup, lookup_path, row.names = FALSE)
data <- attach_microglia_measurements(selected, measurements, measures, lookup_path)
stopifnot(nrow(data) == 48L, identical(data$Volume, measurements$Volume))
expect_error(attach_microglia_measurements(selected, rbind(measurements, measurements[1, ]), measures))
expect_error(attach_microglia_measurements(selected, measurements[-1, ], measures))
dropped <- suppressWarnings(attach_microglia_measurements(selected, measurements[-1, ], measures, missing = "drop"))
stopifnot(nrow(dropped) == 47L, sum(!attr(dropped, "measurement_audit")$included) == 1L)
# Exercise actual CSV import and the Mean row produced by Fiji's Summarize command.
morph <- measurements
names(morph)[1] <- "Filename"
morph_path <- file.path(work, "morph.csv")
write.csv(morph, morph_path, row.names = FALSE)
skeleton_dir <- file.path(work, "skeleton")
dir.create(skeleton_dir)
write.csv(data.frame(Label = c("1", "Mean", "SD"), Branches = c(4, 4, 0)),
    file.path(skeleton_dir, paste0(morph$Filename[1], "_results.csv")), row.names = FALSE)
combined <- read_imagej_measurements(morph_path, skeleton_dir)
stopifnot(combined$Branches[match(morph$Filename[1], combined$filename)] == 4)

radar <- plot_morphology_circle(data, measures)
stopifnot(nrow(radar$fish_summary) == 4L)
pca <- fit_microglia_pca(data, measures)
reference <- prcomp(scale(log1p(as.matrix(data[measures]))), center = FALSE, scale. = FALSE)
stopifnot(isTRUE(all.equal(abs(pca$model$x), abs(reference$x), tolerance = 1e-10)),
    abs(sum(pca$variance$percent) - 100) < 1e-10)
plots <- list(radar$plot, plot_pca_variance(pca$model)$plot)
for (kind in c("over_time", "aggregate", "pc1_pc2")) {
    result <- plot_pca_comparison(pca$scores, treatments = c("para", "yolk"), plot_type = kind)
    plots <- c(plots, result$plots)
}
# Build graphics to detect failures that only appear during rendering.
for (plot in plots) if (inherits(plot, "ggplot")) ggplot2::ggplot_build(plot)
for (fun in c("plot_pca_3d", "plot_pca_3d_summary", "plot_pca_3d_time")) {
    result <- get(fun)(pca$scores, treatments = c("para", "yolk"))
    stopifnot(inherits(result$plot, "plotly"))
    plotly::plotly_build(result$plot)
}

day_night <- plot_pca_day_night(pca$scores, treatments = c("para", "yolk"),
    time_window = c(20, 21), night_start_inclusive = 20.5)
invisible(ggplot2::ggplot_build(day_night$plot))

spatial <- do.call(prepare_microglia_spatial_data, args)
for (metric in c("relative_density", "raw_pixel_count", "density_percent", "occupancy_rate")) {
    result <- analyse_microglia_spatial(spatial, n_bins = 4L, plot_metric = metric,
        comparison_treatments = "yolk", normalise_treatment_heat_0_to_1 = FALSE)
    stopifnot(nrow(result$data$fish_bins) > 0, all(is.finite(result$data$time_summary$mean_val)))
    ggplot2::ggplot_build(result$plots$individual_by_time)
}
for (contrast in c("difference", "normalized_difference", "ratio", "log2_ratio")) {
    result <- analyse_microglia_spatial(spatial, n_bins = 4L, comparison_metric = contrast,
        comparison_treatments = "yolk", normalise_treatment_heat_0_to_1 = FALSE)
    ggplot2::ggplot_build(result$plots$contrast_by_time_combined)
    values <- result$data$contrast_group1$time
    expected <- switch(contrast, difference = values$yolk - values$para,
        normalized_difference = (values$yolk - values$para) / (abs(values$yolk) + abs(values$para) + 1e-8),
        ratio = (values$yolk + 1e-8) / (values$para + 1e-8),
        log2_ratio = log2((values$yolk + 1e-8) / (values$para + 1e-8)))
    stopifnot(isTRUE(all.equal(values$Comparison, expected)))
}
# Save small visual checks outside the manuscript folder.
ggplot2::ggsave(file.path(work, "radar.png"), radar$plot, width = 7, height = 7)
ggplot2::ggsave(file.path(work, "spatial.png"), result$plots$individual_by_time, width = 8, height = 4)
ggplot2::ggsave(file.path(work, "pca.png"), plot_pca_variance(pca$model)$plot, width = 6, height = 4)
cat("Synthetic pipeline checks passed. Preview files:", work, "\n")
