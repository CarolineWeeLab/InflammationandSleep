# Synthetic pipeline validation.
source("Microglia_common.R")
source("MM_part1_adapted.R")
source("Microglia_Radar.R")
source("PCs_plotting.R")
expect_error <- function(expr) stopifnot(tryCatch({ force(expr); FALSE }, error = function(e) TRUE))
work <- tempfile("mm_part1_")
dir.create(work)
skel_dir <- file.path(work, "skeleton")
dir.create(skel_dir)
set.seed(7)
n <- 16L
filenames <- sprintf("20260101_t0.00h_para_fish00_M%04d.tif", seq_len(n))
shape <- data.frame(Filename = filenames, check.names = FALSE)
for (feature in MM_SHAPE_FEATURES) shape[[feature]] <- runif(n, 25, 100)
shape$Volume[1] <- 10
shape$SurfaceArea[2] <- NA_real_
shape_path <- file.path(work, "shape.csv")
write.csv(shape, shape_path, row.names = FALSE)
headings <- c("# Branches", "# Junctions", "# End-point voxels", "# Junction voxels", "# Slab voxels",
              "Average Branch Length", "# Triple points", "# Quadruple points", "Maximum Branch Length")
for (i in seq_len(n)) {
  row <- as.data.frame(as.list(setNames(runif(9, 1, 10), headings)), check.names = FALSE)
  if (i == 3L) row[["Average Branch Length"]] <- Inf
  write.csv(row, file.path(skel_dir, paste0(filenames[i], "_results.csv")), row.names = FALSE)
}
lookup <- data.frame(filename = filenames, expt = "20260101 fish00", condition = "para", time_hr = 0,
  actual_elapsed_hour = 20, nominal_elapsed_hour = 20, cell_number = seq_len(n), image_level = "cell")
lookup_path <- file.path(work, "lookup.csv")
write.csv(lookup, lookup_path, row.names = FALSE)
result <- run_mm_part1(shape_path, skel_dir, lookup_csv = lookup_path, output_dir = file.path(work, "out"))
stopifnot(nrow(result$final_data) == n, nrow(result$final_data_clean) == n - 1L,
  nrow(result$bad_values) == 2L, all(is.finite(as.matrix(result$final_data_clean[MM_FEATURES]))),
  all(result$pca_data$batch == "20260101 fish00"), length(result$pca_measures) == 19L,
  file.exists(file.path(work, "out", "pca_data.csv")), nrow(result$feature_correlations) == 19L^2,
  nrow(result$pc_correlations) == 21L * 6L)
reference <- prcomp(scale(log1p(as.matrix(result$final_data_clean[MM_PCA_FEATURES]))), scale. = TRUE)
stopifnot(isTRUE(all.equal(unname(reference$x[, 1:10]), unname(as.matrix(result$pca_data[paste0("PC", 1:10)])), tolerance = 1e-10)))
expect_error(run_mm_part1(shape_path, skel_dir, bad_value_action = "error"))
drop <- run_mm_part1(shape_path, skel_dir, bad_value_action = "drop", correlations = FALSE)
stopifnot(nrow(drop$final_data_clean) == 13L)
selected <- lookup[-1, ]
selected$coords <- rep(list(matrix(0, nrow = 1, ncol = 3)), nrow(selected))
subset <- run_mm_part1(shape_path, skel_dir, lookup_csv = lookup_path,
                      selected_cells = selected, correlations = FALSE)
stopifnot(nrow(subset$final_data) == 15L)
expect_error(merger_data(rbind(shape, shape[1, ]), data.frame(Name = filenames)))
# Plot inputs remain usable, without rerunning PCA on the already scaled features.
radar <- plot_morphology_circle(result$final_data_clean, measures = MM_SHAPE_FEATURES[1:4])
invisible(ggplot2::ggplot_build(radar$plot))
pc <- plot_pca_comparison(result$pca_data, treatments = "para", plot_type = "pc1_pc2", comparison_level = "cell")
invisible(ggplot2::ggplot_build(pc$plot))
invisible(ggplot2::ggplot_build(plot_distribution_outliers(data.frame(Volume = rep(25, 5)), Volume)))
stopifnot(nrow(get_extreme_cells(result$final_data, Volume, n = 3)) == 6L)
# TIFF suffix differences are normalized before the one-to-one merge.
file.rename(file.path(skel_dir, paste0(filenames[4], "_results.csv")),
            file.path(skel_dir, paste0(sub(".tif$", ".tiff", filenames[4]), "_results.csv")))
normalized <- read_mm_measurements(shape_path, skel_dir)
stopifnot(nrow(normalized) == n, all(attr(normalized, "merge_audit")$status == "matched"))
cat("MM part 1 checks passed.\n")
