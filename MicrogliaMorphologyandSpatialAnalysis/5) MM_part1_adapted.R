# Combine MorphoLibJ and skeleton measurements, inspect cells, and prepare PCA inputs.
# Source Microglia_common.R first. Source this file to define functions; examples are in README.md.

MM_SHAPE_FEATURES <- c(
  "Volume", "SurfaceArea", "Sphericity", "Elli_R1_R2", "Elli_R1_R3", "Elli_R2_R3",
  "MaxFeretDiam", "GeodesicDiameter", "InscrBall_Radius", "Tortuosity",
  "ConvexHullVolume", "Solidity3D"
)
MM_SKELETON_FEATURES <- c(
  "# of branches", "# of junctions", "# of end point voxels", "# of junction voxels",
  "# of slab voxels", "Average branch length", "# of triple points",
  "# of quadruple points", "Maximum branch length"
)
MM_FEATURES <- c(MM_SHAPE_FEATURES, MM_SKELETON_FEATURES)
# Default PCA uses 19 named morphology features.
MM_PCA_FEATURES <- setdiff(MM_FEATURES, c("# of quadruple points", "Maximum branch length"))

.mm_cell_name <- function(x) {
  x <- sub("_results\\.csv$", "", basename(as.character(x)), ignore.case = TRUE)
  sub("\\.tiff?$", "", x, ignore.case = TRUE)
}

.mm_check_names <- function(x, label) {
  if (anyNA(x) || any(!nzchar(trimws(x))) || anyDuplicated(x))
    stop(label, " must contain unique, nonempty cell names.")
}

# Standardize skeleton measurement headings.
.mm_skeleton_names <- function(data) {
  key <- function(x) sub("^of", "", sub("^x", "", gsub("[^a-z]", "", tolower(x))))
  for (feature in MM_SKELETON_FEATURES) {
    matches <- which(key(names(data)) == key(feature))
    if (length(matches) != 1L) stop("Expected one skeleton column for: ", feature)
    names(data)[matches] <- feature
  }
  data
}

# MorphoLibJ rows define the cell list; unmatched skeleton rows are reported separately.
merger_data <- function(fraclac, skeleton) {
  if (!is.data.frame(fraclac) || !is.data.frame(skeleton)) stop("Supply two measurement dataframes.")
  if (!"Filename" %in% names(fraclac) || !"Name" %in% names(skeleton))
    stop("Shape data need Filename; skeleton data need Name.")
  fraclac$Name <- .mm_cell_name(fraclac$Filename)
  skeleton$Name <- .mm_cell_name(skeleton$Name)
  .mm_check_names(fraclac$Name, "Shape data")
  .mm_check_names(skeleton$Name, "Skeleton data")
  skeleton <- .mm_skeleton_names(skeleton)
  if (!all(MM_SHAPE_FEATURES %in% names(fraclac)))
    stop("Missing shape columns: ", paste(setdiff(MM_SHAPE_FEATURES, names(fraclac)), collapse = ", "))
  for (data in list(fraclac[MM_SHAPE_FEATURES], skeleton[MM_SKELETON_FEATURES]))
    if (!all(vapply(data, is.numeric, logical(1)))) stop("All measurement columns must be numeric.")
  index <- match(fraclac$Name, skeleton$Name)
  result <- fraclac[c("Name", MM_SHAPE_FEATURES)]
  result[MM_SKELETON_FEATURES] <- skeleton[index, MM_SKELETON_FEATURES, drop = FALSE]
  result$filename <- as.character(fraclac$Filename)
  audit <- data.frame(
    Name = c(fraclac$Name, setdiff(skeleton$Name, fraclac$Name)),
    status = c(ifelse(is.na(index), "missing_skeleton", "matched"),
               rep("skeleton_only", sum(!skeleton$Name %in% fraclac$Name)))
  )
  attr(result, "merge_audit") <- audit
  message("Shape cells: ", nrow(fraclac), "; matched skeletons: ", sum(!is.na(index)),
          "; missing skeletons: ", sum(is.na(index)),
          "; skeleton-only cells: ", sum(audit$status == "skeleton_only"))
  result
}

read_mm_measurements <- function(morphology_csv, skeleton_dir) {
  shape <- utils::read.csv(morphology_csv, check.names = FALSE, stringsAsFactors = FALSE)
  files <- list.files(skeleton_dir, pattern = "_results\\.csv$", full.names = TRUE)
  if (!length(files)) stop("No skeleton _results.csv files found.")
  skeleton <- lapply(files, function(path) {
    data <- utils::read.csv(path, check.names = FALSE, stringsAsFactors = FALSE)
    mean_row <- which(apply(data, 1, function(row)
      any(tolower(trimws(row)) == "mean", na.rm = TRUE)))
    if (length(mean_row) == 1L) data <- data[mean_row, , drop = FALSE]
    if (nrow(data) != 1L) stop("Expected one cell row or one Mean summary row in: ", path)
    data <- .mm_skeleton_names(data)
    data <- data[MM_SKELETON_FEATURES]
    data$Name <- basename(path)
    data
  })
  merger_data(shape, do.call(rbind, skeleton))
}

# Optional lookup adds the exact acquisition times and full fish IDs from regeneration.
.mm_attach_lookup <- function(data, lookup_csv) {
  lookup <- utils::read.csv(lookup_csv, check.names = FALSE, stringsAsFactors = FALSE)
  required <- c("filename", "expt", "condition", "time_hr")
  if (!all(required %in% names(lookup))) stop("Use microglia_lookup.csv from cell regeneration.")
  if ("image_level" %in% names(lookup) && any(is.na(lookup$image_level) | lookup$image_level != "cell"))
    stop("Use a single-cell lookup, not a whole-fish lookup.")
  lookup$Name <- .mm_cell_name(lookup$filename)
  .mm_check_names(lookup$Name, "Export lookup")
  index <- match(data$Name, lookup$Name)
  if (anyNA(index)) stop("Some merged cells are absent from the export lookup.")
  columns <- setdiff(names(lookup), names(data))
  data[columns] <- lookup[index, columns, drop = FALSE]
  data$treatment <- as.character(data$condition)
  data$batch <- as.character(data$expt)
  data
}

# Spearman correlations with unadjusted significance thresholds.
.mm_correlations <- function(data, features, pcs = NULL, r_threshold = 0.8, p_threshold = 0.05) {
  if (!requireNamespace("Hmisc", quietly = TRUE)) stop("Install Hmisc for correlation tables, or set correlations=FALSE.")
  columns <- unique(c(pcs, features))
  if (nrow(data) < 5L) stop("Correlation tables require at least five cells; set correlations=FALSE for smaller inputs.")
  result <- Hmisc::rcorr(as.matrix(data[columns]), type = "spearman")
  pairs <- if (is.null(pcs)) expand.grid(measure_a = features, measure_b = features,
                                        stringsAsFactors = FALSE) else
    expand.grid(measure_a = features, measure_b = pcs, stringsAsFactors = FALSE)
  index <- cbind(match(pairs$measure_a, columns), match(pairs$measure_b, columns))
  pairs$correlation <- result$r[index]
  pairs$pvalues <- result$P[index]
  pairs$Significant <- ifelse(abs(pairs$correlation) >= r_threshold & pairs$pvalues < p_threshold,
                              "significant", "ns")
  pairs
}

run_mm_part1 <- function(
    morphology_csv,
    skeleton_dir,
    output_dir = NULL,
    lookup_csv = NULL,
    selected_cells = NULL,
    min_volume = 20,
    max_volume = Inf,
    bad_value_action = c("zero", "error", "drop"),
    pca_measures = MM_PCA_FEATURES,
    n_pcs = 10L,
    correlations = TRUE
) {
  bad_value_action <- match.arg(bad_value_action)
  if (!is.numeric(min_volume) || length(min_volume) != 1L || !is.finite(min_volume) || min_volume < 0 ||
      !is.numeric(max_volume) || length(max_volume) != 1L || is.na(max_volume) || max_volume < min_volume)
    stop("Use valid inclusive volume limits with max_volume >= min_volume.")
  if (length(n_pcs) != 1L || !is.finite(n_pcs) || n_pcs < 1 || n_pcs != as.integer(n_pcs))
    stop("n_pcs must be a positive integer.")
  if (!is.logical(correlations) || length(correlations) != 1L || is.na(correlations))
    stop("correlations must be TRUE or FALSE.")
  if (!is.character(pca_measures) || length(pca_measures) < 2L || anyDuplicated(pca_measures) ||
      !all(pca_measures %in% MM_FEATURES)) stop("Choose at least two distinct morphology features by name.")

  final_data <- read_mm_measurements(morphology_csv, skeleton_dir)
  merge_audit <- attr(final_data, "merge_audit")
  if (!is.null(lookup_csv)) final_data <- .mm_attach_lookup(final_data, lookup_csv)
  selection_audit <- NULL
  if (!is.null(selected_cells)) {
    if (is.null(lookup_csv)) stop("Supply lookup_csv with selected_cells to verify exact cell identities.")
    required <- c("filename", "expt", "time_hr", "cell_number")
    if (!all(required %in% names(selected_cells)) || !all(required %in% names(final_data)))
      stop("selected_cells must come from select_microglia(); the lookup must include cell_number.")
    key <- function(x) paste(x$expt, sprintf("%.17g", x$time_hr), x$cell_number, sep = "|")
    .mm_check_names(key(selected_cells), "Selected cells")
    .mm_check_names(key(final_data), "Merged cell identities")
    index <- match(key(selected_cells), key(final_data))
    if (anyNA(index)) stop("Some selected cells lack shape measurements; inspect merge_audit before continuing.")
    final_data <- final_data[index, , drop = FALSE]
    for (name in intersect(c("condition", "treatment", "batch", "actual_elapsed_hour", "nominal_elapsed_hour"), names(selected_cells)))
      final_data[[name]] <- selected_cells[[name]]
    selection_audit <- attr(selected_cells, "filter_audit")
  }

  # Volume filtering precedes replacement: an invalid volume never becomes a zero-size cell.
  keep <- is.finite(final_data$Volume) & final_data$Volume >= min_volume & final_data$Volume <= max_volume
  removed_rows <- final_data[!keep, , drop = FALSE]
  removed_rows$reason <- ifelse(!is.finite(removed_rows$Volume), "invalid_volume", "outside_volume_limits")
  final_data_clean <- final_data[keep, , drop = FALSE]
  if (nrow(final_data_clean) < 3L) stop("Fewer than three cells remain after volume filtering.")

  values <- as.matrix(final_data_clean[MM_FEATURES])
  bad <- which(!is.finite(values), arr.ind = TRUE)
  bad_values <- data.frame(Name = final_data_clean$Name[bad[, 1]], measure = colnames(values)[bad[, 2]],
                          original_value = values[bad], stringsAsFactors = FALSE)
  bad_values$action <- rep(bad_value_action, nrow(bad_values))
  bad_values$replaced_value <- rep(if (bad_value_action == "zero") 0 else NA_real_, nrow(bad_values))
  if (nrow(bad) && bad_value_action == "error")
    stop(nrow(bad), " nonfinite measurements found. Inspect the CSVs or choose zero/drop explicitly.")
  if (bad_value_action == "zero") {
    values[bad] <- 0
    final_data_clean[MM_FEATURES] <- values
  } else if (bad_value_action == "drop" && nrow(bad)) {
    excluded <- unique(bad[, 1])
    removed <- final_data_clean[excluded, , drop = FALSE]
    removed$reason <- "nonfinite_measurement"
    removed_rows <- rbind(removed_rows, removed)
    final_data_clean <- final_data_clean[-excluded, , drop = FALSE]
  }
  message("Removed cells: ", nrow(removed_rows), "; retained: ", nrow(final_data_clean),
          "; nonfinite measurements: ", nrow(bad_values), " (", bad_value_action, ").")
  if (any(final_data_clean[MM_FEATURES] < 0)) stop("Negative morphology measurements cannot enter this log1p workflow.")
  if (nrow(final_data_clean) < 3L) stop("PCA needs at least three retained cells.")

  # Log and scale morphology only; leave identifiers and times unchanged.
  log_values <- log1p(as.matrix(final_data_clean[MM_FEATURES]))
  deviations <- apply(log_values, 2, stats::sd)
  constant_features <- names(deviations)[!is.finite(deviations) | deviations == 0]
  if (any(pca_measures %in% constant_features))
    stop("Constant PCA features: ", paste(intersect(pca_measures, constant_features), collapse = ", "),
         ". Choose pca_measures explicitly without these features.")
  scaled <- scale(log_values)
  # Constant non-PCA features retain NA standardized values; do not silently turn them into zeros.
  final_data_scaled <- final_data_clean
  final_data_scaled[MM_FEATURES] <- scaled
  pca_model <- stats::prcomp(final_data_scaled[pca_measures], center = TRUE, scale. = TRUE)
  n_pcs <- min(n_pcs, ncol(pca_model$x))
  pca_data <- cbind(as.data.frame(pca_model$x[, seq_len(n_pcs), drop = FALSE]), final_data_scaled)
  final_data_gathered <- data.frame(
    Name = rep(final_data_clean$Name, times = length(MM_FEATURES)),
    measure = rep(MM_FEATURES, each = nrow(final_data_clean)),
    value = as.vector(as.matrix(final_data_clean[MM_FEATURES]))
  )

  feature_correlations <- pc_correlations <- NULL
  if (correlations) {
    feature_correlations <- .mm_correlations(final_data_clean, pca_measures)
    pc_correlations <- .mm_correlations(pca_data, setdiff(MM_FEATURES, constant_features),
      pcs = paste0("PC", seq_len(min(6L, n_pcs))), r_threshold = 0.75)
  }
  result <- list(final_data = final_data, final_data_clean = final_data_clean,
    final_data_scaled = final_data_scaled, final_data_gathered = final_data_gathered,
    pca_data = pca_data, pca_model = pca_model, pca_measures = pca_measures,
    removed_rows = removed_rows, bad_values = bad_values, merge_audit = merge_audit,
    selection_audit = selection_audit, constant_features = constant_features,
    feature_correlations = feature_correlations, pc_correlations = pc_correlations,
    settings = list(min_volume = min_volume, max_volume = max_volume,
      bad_value_action = bad_value_action, pca_measures = pca_measures, n_pcs = n_pcs))
  if (!is.null(output_dir)) {
    output_dir <- path.expand(output_dir)
    if (file.exists(output_dir) && (!dir.exists(output_dir) ||
        length(list.files(output_dir, all.files = TRUE, no.. = TRUE))))
      stop("Choose a new or empty output_dir to avoid overwriting an analysis.")
    if (!dir.exists(output_dir) && !dir.create(output_dir, recursive = TRUE)) stop("Cannot create output_dir.")
    tables <- c(final_data = "final_data_raw.csv", final_data_clean = "final_data_clean.csv",
      final_data_scaled = "final_data_scaled.csv", final_data_gathered = "final_data_gathered.csv",
      pca_data = "pca_data.csv", removed_rows = "removed_rows.csv", bad_values = "bad_values.csv",
      merge_audit = "merge_audit.csv", selection_audit = "selection_audit.csv",
      feature_correlations = "correlations_across_feats.csv", pc_correlations = "correlations_bw_PCs_and_feats.csv")
    for (name in names(tables)) if (!is.null(result[[name]]))
      utils::write.csv(result[[name]], file.path(output_dir, tables[[name]]), row.names = FALSE)
    saveRDS(pca_model, file.path(output_dir, "pca_model.rds"))
    dput(result$settings, file = file.path(output_dir, "analysis_settings.R"))
    result$output_dir <- output_dir
  }
  result
}

# Inspection helpers do not remove outliers automatically.
plot_distribution <- function(data, variable, bins = 30) {
  ggplot2::ggplot(data, ggplot2::aes(x = {{ variable }})) +
    ggplot2::geom_histogram(bins = bins, colour = "black") + ggplot2::theme_classic()
}

plot_distribution_outliers <- function(data, variable, bins = 30, inflate = 10) {
  name <- rlang::as_name(rlang::enquo(variable))
  x <- data[[name]]
  x <- x[is.finite(x)]
  if (!length(x)) stop("No finite values to plot.")
  if (length(bins) != 1 || !is.finite(bins) || bins < 1 || bins != floor(bins) ||
      length(inflate) != 1 || !is.finite(inflate) || inflate <= 0) stop("Use positive bins and inflate values.")
  limits <- stats::quantile(x, c(0.25, 0.75)) + c(-1.5, 1.5) * stats::IQR(x)
  extent <- range(x)
  if (diff(extent) == 0) extent <- extent + c(-0.5, 0.5)
  breaks <- seq(extent[1], extent[2], length.out = bins + 1)
  all <- graphics::hist(x, breaks = breaks, plot = FALSE)
  outside <- x[x < limits[1] | x > limits[2]]
  counts <- if (length(outside)) graphics::hist(outside, breaks = breaks, plot = FALSE)$counts else rep(0, bins)
  plot_data <- data.frame(x = all$mids, count = all$counts, outlier_count = counts * inflate)
  ggplot2::ggplot(plot_data, ggplot2::aes(x = x)) +
    ggplot2::geom_col(ggplot2::aes(y = count), width = diff(breaks)[1]) +
    ggplot2::geom_col(ggplot2::aes(y = outlier_count), width = diff(breaks)[1], fill = "#D95F02", alpha = 0.5) +
    ggplot2::geom_vline(xintercept = limits, linetype = "dashed") +
    ggplot2::scale_y_continuous(name = "Cell count", sec.axis = ggplot2::sec_axis(~ . / inflate, name = "Outlier count")) +
    ggplot2::labs(x = name, subtitle = paste("Outlier counts enlarged", inflate, "times; 1.5 × IQR boundaries")) +
    ggplot2::theme_classic()
}

get_extreme_cells <- function(data, variable, n = 10L, name_col = Name) {
  variable <- rlang::as_name(rlang::enquo(variable))
  name_col <- rlang::as_name(rlang::enquo(name_col))
  if (!all(c(variable, name_col) %in% names(data))) stop("Missing variable or name column.")
  if (length(n) != 1L || !is.finite(n) || n < 1 || n != floor(n)) stop("n must be a positive integer.")
  data <- data[is.finite(data[[variable]]), , drop = FALSE]
  smallest <- head(data[order(data[[variable]]), , drop = FALSE], n)
  largest <- head(data[order(data[[variable]], decreasing = TRUE), , drop = FALSE], n)
  result <- rbind(data.frame(Extreme = rep("Smallest", nrow(smallest)), Rank = seq_len(nrow(smallest)), smallest, check.names = FALSE),
                  data.frame(Extreme = rep("Largest", nrow(largest)), Rank = seq_len(nrow(largest)), largest, check.names = FALSE))
  result[, unique(c("Extreme", "Rank", name_col, variable, names(data))), drop = FALSE]
}

# Example: set the input files and output location, then run after sourcing.
# part1 <- run_mm_part1(
#   morphology_csv = "MorphoLibJ_3D_SelectedFeatures.csv",
#   skeleton_dir = "SKELETON_CSV_DIRECTORY",
#   lookup_csv = "microglia_lookup.csv",
#   output_dir = "ANALYSIS_OUTPUT_DIRECTORY"
# )
# final_data_raw <- part1$final_data
# final_data <- part1$final_data_clean
# pca_data <- part1$pca_data
