# Spatial analysis of binary 2D projections.
# Load Microglia_common.R first. Examples and input requirements are in README.md.
library(dplyr)
library(tidyr)
library(ggplot2)

`%||%` <- function(x, y) if (is.null(x) || length(x) == 0) y else x

# Coordinates are combined within an exact fish acquisition, then projected onto a shared canvas.
prepare_microglia_spatial_data <- function(
    pickle_path = NULL,
    metadata_csv,
    treatments = NULL,
    batches = NULL,
    expts = NULL,
    day = 0L,
    days = 1L,
    start_time = "20:00",
    end_time = "02:00",
    time_bin_hours = 0.5,
    times_of_day = NULL,
    include_days = NULL,
    metadata_time_zero_col = NULL,
    complete_cohort = TRUE,
    required_timepoints = NULL,
    projections = c("PA_LR", "VD_LR", "VD_PA"),
    padding = 5L,
    normalise_fish_to_canvas = TRUE,
    canvas_dimensions = NULL,
    reverse_ap = FALSE,
    reverse_dv = TRUE,
    reverse_lr = FALSE,
    z_coordinate_scale = 1,
    save_projections = FALSE,
    save_which = projections,
    output_dir = NULL,
    df = NULL
) {
    if (is.null(df)) df <- read_microglia(pickle_path)
    df <- select_microglia(df, metadata_csv, batches = batches, treatments = treatments,
        expts = expts, day = day, days = days, start_time = start_time, end_time = end_time,
        time_bin_hours = time_bin_hours, times_of_day = times_of_day, include_days = include_days,
        metadata_time_zero_col = metadata_time_zero_col, complete_cohort = complete_cohort,
        required_timepoints = required_timepoints)
    .microglia_integer(padding, "padding")
    for (flag in c("normalise_fish_to_canvas", "reverse_ap", "reverse_dv", "reverse_lr", "save_projections"))
        .microglia_flag(get(flag), flag)
    if (!is.numeric(z_coordinate_scale) || length(z_coordinate_scale) != 1L ||
        !is.finite(z_coordinate_scale) || z_coordinate_scale <= 0)
        stop("z_coordinate_scale must be a positive number.")
    if (!length(projections) || any(!projections %in% c("PA_LR", "VD_LR", "VD_PA")))
        stop("Choose PA_LR, VD_LR and/or VD_PA projections.")
    for (coords in df$coords) {
        if (!is.matrix(coords) || !is.numeric(coords) || ncol(coords) != 3L || !nrow(coords) ||
            any(!is.finite(coords)) || any(coords != trunc(coords)))
            stop("Each cell needs a nonempty numeric Z, Y, X matrix of integer coordinates.")
    }
    acquisitions <- unique(df[c("expt", "time_hr", "nominal_time")])
    if (anyDuplicated(acquisitions[c("expt", "nominal_time")]))
        stop("Multiple exact acquisitions fall in one fish/time bin. Use a finer time grid or select acquisitions explicitly.")
    timepoints <- sort(unique(df$nominal_time))
    cohort <- list(data = df, expected_timepoints = attr(df, "required_timepoints"),
        coverage = attr(df, "coverage"), filter_audit = attr(df, "filter_audit"))
    if (save_projections) {
        if (is.null(output_dir)) stop("Supply output_dir to save projections.")
        if (!requireNamespace("ijtiff", quietly = TRUE)) stop("Install ijtiff to save TIFF projections.")
        if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
    }
    acquisition_coordinates <- list()
    for (t in timepoints) {
        t_data <- df[df$nominal_time == t, , drop = FALSE]
        for (f_id in unique(t_data$expt)) {
            f_sub <- t_data[t_data$expt == f_id, , drop = FALSE]
            coords_merged <- do.call(rbind, lapply(f_sub$coords, function(m) {
                if (is.data.frame(m))
                  as.matrix(m)
                else m
            }))
            z <- coords_merged[, 1] * z_coordinate_scale
            y <- coords_merged[, 2]
            x <- coords_merged[, 3]
            key <- paste(f_id, sprintf("%.12g", t), sep = "@T")
            acquisition_coordinates[[key]] <- list(expt = f_id, condition = f_sub$condition[1], nominal_time = t,
                z = z, y = y, x = x, n_voxels = nrow(coords_merged))
        }
    }
    unique_fish_ids <- unique(df$expt)
    fish_bounds <- list()
    for (f_id in unique_fish_ids) {
        f_keys <- names(acquisition_coordinates)[vapply(acquisition_coordinates, function(f) f$expt == f_id, logical(1))]
        f_all_z <- unlist(lapply(acquisition_coordinates[f_keys], function(f) f$z))
        f_all_y <- unlist(lapply(acquisition_coordinates[f_keys], function(f) f$y))
        f_all_x <- unlist(lapply(acquisition_coordinates[f_keys], function(f) f$x))
        min_z <- min(f_all_z, na.rm = TRUE)
        max_z <- max(f_all_z, na.rm = TRUE)
        min_y <- min(f_all_y, na.rm = TRUE)
        max_y <- max(f_all_y, na.rm = TRUE)
        min_x <- min(f_all_x, na.rm = TRUE)
        max_x <- max(f_all_x, na.rm = TRUE)
        fish_bounds[[f_id]] <- list(min_z = min_z, max_z = max_z, span_z = max(1e-04, max_z - min_z),
            min_y = min_y, max_y = max_y, span_y = max(1e-04, max_y - min_y), min_x = min_x, max_x = max_x,
            span_x = max(1e-04, max_x - min_x))
    }
    if (!is.null(canvas_dimensions)) {
        if (!is.numeric(canvas_dimensions) || length(canvas_dimensions) != 3L ||
            any(!is.finite(canvas_dimensions)) || any(canvas_dimensions <= 0))
            stop("canvas_dimensions must contain positive AP, LR and DV spans.")
        if (!is.null(names(canvas_dimensions))) {
            if (!all(c("AP", "LR", "DV") %in% names(canvas_dimensions)))
                stop("Named canvas_dimensions must include AP, LR and DV.")
            canvas_dimensions <- canvas_dimensions[c("AP", "LR", "DV")]
        }
        cohort_max_y <- ceiling(canvas_dimensions[1])
        cohort_max_x <- ceiling(canvas_dimensions[2])
        cohort_max_z <- ceiling(canvas_dimensions[3])
    }
    else {
        cohort_max_y <- ceiling(max(vapply(fish_bounds, function(b) b$span_y, numeric(1))))
        cohort_max_x <- ceiling(max(vapply(fish_bounds, function(b) b$span_x, numeric(1))))
        cohort_max_z <- ceiling(max(vapply(fish_bounds, function(b) b$span_z, numeric(1))))
    }
    cat(sprintf("[Canvas] Decided shared canvas spans: AP = %d, LR = %d, DV = %d\n", cohort_max_y, cohort_max_x,
        cohort_max_z))
    if (isTRUE(normalise_fish_to_canvas)) {
        cat("[Normalization] Min-max normalizing each fish's bounding box across timepoints to the shared canvas.\n")
    }
    h_pa <- cohort_max_y + (2 * padding) + 1L
    w_lr <- cohort_max_x + (2 * padding) + 1L
    h_vd <- cohort_max_z + (2 * padding) + 1L
    reconstructed_projections <- list()
    manifest <- list()
    for (key in names(acquisition_coordinates)) {
        f_info <- acquisition_coordinates[[key]]
        f_id <- f_info$expt
        cond <- f_info$condition
        t <- f_info$nominal_time
        b <- fish_bounds[[f_id]]
        z <- f_info$z
        y <- f_info$y
        x <- f_info$x
        if (isTRUE(normalise_fish_to_canvas)) {
            y <- (y - b$min_y)/b$span_y * cohort_max_y
            x <- (x - b$min_x)/b$span_x * cohort_max_x
            z <- (z - b$min_z)/b$span_z * cohort_max_z
            if (isTRUE(reverse_ap))
                y <- cohort_max_y - y
            if (isTRUE(reverse_dv))
                z <- cohort_max_z - z
            if (isTRUE(reverse_lr))
                x <- cohort_max_x - x
            loc_y <- as.integer(round(y)) + padding + 1L
            loc_x <- as.integer(round(x)) + padding + 1L
            loc_z <- as.integer(round(z)) + padding + 1L
        }
        else {
            if (isTRUE(reverse_ap))
                y <- max(y) - y + min(y)
            if (isTRUE(reverse_dv))
                z <- max(z) - z + min(z)
            if (isTRUE(reverse_lr))
                x <- max(x) - x + min(x)
            loc_y <- as.integer(round(y - min(y))) + padding + 1L
            loc_x <- as.integer(round(x - min(x))) + padding + 1L
            loc_z <- as.integer(round(z - min(z))) + padding + 1L
        }
        if (any(loc_y < 1L | loc_y > h_pa | loc_x < 1L | loc_x > w_lr | loc_z < 1L | loc_z > h_vd))
            stop("Coordinates exceed the selected canvas; increase canvas_dimensions.")
        fish_matrices <- list()
        if ("PA_LR" %in% projections) {
            mat <- matrix(0L, nrow = h_pa, ncol = w_lr)
            mat[cbind(loc_y, loc_x)] <- 255L
            fish_matrices[["PA_LR"]] <- mat
        }
        if ("VD_LR" %in% projections) {
            mat <- matrix(0L, nrow = h_vd, ncol = w_lr)
            mat[cbind(loc_z, loc_x)] <- 255L
            fish_matrices[["VD_LR"]] <- mat
        }
        if ("VD_PA" %in% projections) {
            mat <- matrix(0L, nrow = h_vd, ncol = h_pa)
            mat[cbind(loc_z, loc_y)] <- 255L
            fish_matrices[["VD_PA"]] <- mat
        }
        if (save_projections) {
            for (proj_name in intersect(names(fish_matrices), save_which)) {
                safe_expt <- gsub("[^A-Za-z0-9+._-]", "-", f_id)
                fname <- sprintf("%s_T%.2fh_%s_%s_2D.tif", safe_expt, t, cond, proj_name)
                ijtiff::write_tif(fish_matrices[[proj_name]], file.path(output_dir, fname), bits_per_sample = 8,
                  compression = "Zip", overwrite = FALSE, msg = FALSE)
            }
        }
        reconstructed_projections[[key]] <- fish_matrices
        manifest[[length(manifest) + 1L]] <- data.frame(key = key, expt = f_id, condition = cond, nominal_time = t,
            n_voxels = f_info$n_voxels, stringsAsFactors = FALSE)
    }
    manifest_df <- dplyr::bind_rows(manifest)
    manifest_df <- manifest_df %>% dplyr::group_by(condition) %>% dplyr::mutate(fish_num = as.integer(factor(expt,
        levels = unique(expt))), fish_label = sprintf("%s_%02d", condition, fish_num)) %>% dplyr::ungroup()
    cat(sprintf("[Success] Spatial data ready for %d fish across %d timepoints.\n", n_distinct(manifest_df$expt),
        length(timepoints)))
    return(list(cohort = cohort, projections = reconstructed_projections, manifest = manifest_df))
}

.combine_vertical_plots <- function(plot_list) {
    if (length(plot_list) == 1)
        return(plot_list[[1]])
    if (requireNamespace("patchwork", quietly = TRUE)) {
        patchwork::wrap_plots(plot_list, ncol = 1)
    }
    else if (requireNamespace("cowplot", quietly = TRUE)) {
        cowplot::plot_grid(plotlist = plot_list, ncol = 1, align = "v")
    }
    else if (requireNamespace("gridExtra", quietly = TRUE)) {
        gridExtra::arrangeGrob(grobs = plot_list, ncol = 1)
    }
    else {
        warning("Package 'patchwork' or 'cowplot' recommended. Install 'patchwork'.")
        plot_list
    }
}

.combine_horizontal_plots <- function(plot_list) {
    if (length(plot_list) == 1)
        return(plot_list[[1]])
    if (requireNamespace("patchwork", quietly = TRUE)) {
        patchwork::wrap_plots(plot_list, nrow = 1)
    }
    else if (requireNamespace("cowplot", quietly = TRUE)) {
        cowplot::plot_grid(plotlist = plot_list, nrow = 1, align = "h")
    }
    else if (requireNamespace("gridExtra", quietly = TRUE)) {
        gridExtra::arrangeGrob(grobs = plot_list, nrow = 1)
    }
    else {
        warning("Package 'patchwork' or 'cowplot' recommended. Install 'patchwork'.")
        plot_list
    }
}

save_microglia_plot_svg <- function(plot, filename, dir = ".", width = 3.5, height = 3.5, units = "in",
    dpi = 600) {
    if (is.null(plot))
        return(invisible(NULL))
    target_dir <- path.expand(dir)
    if (!dir.exists(target_dir))
        dir.create(target_dir, recursive = TRUE, showWarnings = FALSE)
    out_path <- file.path(target_dir, filename)
    if (!grepl("\\.svg$", out_path, ignore.case = TRUE))
        out_path <- paste0(out_path, ".svg")
    ggplot2::ggsave(filename = out_path, plot = plot, device = "svg", width = width, height = height,
        units = units, dpi = dpi)
    cat(sprintf("[Export] Saved SVG plot: %s\n", out_path))
    invisible(out_path)
}

analyse_microglia_spatial <- function(
    spatial_data,
    target_projection = "PA_LR",
    target_timepoint = NULL,
    n_bins = 15,
    treatment1 = "para",
    comparison_treatments = NULL,
    treatment1_second = NULL,
    comparison_treatments_second = NULL,
    contrast_legend_mode = c("per_contrast", "shared"),
    contrast_heat_limits = NULL,
    plot_metric = c("relative_density", "raw_pixel_count", "density_percent", "occupancy_rate"),
    comparison_metric = c("difference", "normalized_difference", "ratio", "log2_ratio"),
    normalise_treatment_heat_0_to_1 = TRUE,
    show_main_title = TRUE,
    main_title = NULL,
    show_subtitle = TRUE,
    subtitle = NULL,
    heat_scale_bar_size = c(2.5, 18),
    show_heat_scale_bar_label = TRUE,
    heat_scale_bar_title = NULL,
    save_svg = FALSE,
    save_svg_dir = NULL,
    svg_width = 3.5,
    svg_height = 3.5,
    svg_units = "in",
    svg_dpi = 600,
    anterior_orientation = list(PA_LR = "R", VD_PA = "R", VD_LR = "U"),
    font_family = "Arial",
    font_size = 5,
    show_axis_titles = TRUE,
    show_all_labels = TRUE,
    tick_style = c("max_only", "all", "none", "custom"),
    custom_breaks_x = NULL,
    custom_breaks_y = NULL,
    palette_treatment = "viridis",
    palette_comparison = c(low = "#2166AC", mid = "#FFFFFF", high = "#B2182B"),
    plot_heat_limits = NULL,
    comparison_heat_limits = NULL,
    panel_spacing_mm = 0.5,
    panel_spacing_x_mm = panel_spacing_mm,
    panel_spacing_y_mm = panel_spacing_mm,
    pseudocount = 1e-08
) {
    .microglia_integer(n_bins, "n_bins", minimum = 2)
    if (!is.numeric(pseudocount) || length(pseudocount) != 1L || !is.finite(pseudocount) || pseudocount <= 0)
        stop("pseudocount must be positive.")
    plot_metric <- match.arg(plot_metric)
    comparison_metric <- match.arg(comparison_metric)
    contrast_legend_mode <- match.arg(contrast_legend_mode)
    tick_style <- match.arg(tick_style)
    manifest <- spatial_data$manifest
    if (!is.null(target_timepoint)) {
        manifest <- manifest %>% dplyr::filter(nominal_time == target_timepoint)
    }
    if (nrow(manifest) == 0)
        stop("No samples match criteria.")
    avail_conditions <- unique(manifest$condition)
    match_condition <- function(name, avail) {
        if (is.null(name))
            return(NULL)
        idx <- which(tolower(trimws(avail)) == tolower(trimws(name)))
        if (length(idx) > 0)
            avail[idx[1]]
        else NA_character_
    }
    t1_name <- match_condition(treatment1, avail_conditions)
    if (is.na(t1_name))
        stop(sprintf("treatment1 '%s' not found.", treatment1))
    if (!is.null(comparison_treatments)) {
        matched_comp <- vapply(comparison_treatments, match_condition, avail = avail_conditions, FUN.VALUE = character(1))
        if (anyNA(matched_comp)) stop("Unknown comparison treatment(s): ", paste(comparison_treatments[is.na(matched_comp)], collapse = ", "))
        comparison_treatments <- unique(matched_comp[!is.na(matched_comp) & tolower(matched_comp) !=
            tolower(t1_name)])
    }
    else {
        comparison_treatments <- setdiff(avail_conditions, t1_name)
    }
    t1_sec_name <- match_condition(treatment1_second, avail_conditions)
    if (!is.null(treatment1_second) && is.na(t1_sec_name)) {
        stop(sprintf("treatment1_second '%s' not found.", treatment1_second))
    }
    if (!is.null(t1_sec_name)) {
        if (!is.null(comparison_treatments_second)) {
            matched_comp_sec <- vapply(comparison_treatments_second, match_condition, avail = avail_conditions,
                FUN.VALUE = character(1))
            if (anyNA(matched_comp_sec)) stop("Unknown secondary comparison treatment.")
            comparison_treatments_second <- unique(matched_comp_sec[!is.na(matched_comp_sec) & tolower(matched_comp_sec) !=
                tolower(t1_sec_name)])
        }
        else {
            comparison_treatments_second <- setdiff(avail_conditions, t1_sec_name)
        }
    }
    if (!length(comparison_treatments)) stop("No comparison treatments are available.")
    if (!is.null(t1_sec_name) && !length(comparison_treatments_second)) stop("No secondary comparisons are available.")
    if (comparison_metric %in% c("ratio", "log2_ratio") && normalise_treatment_heat_0_to_1)
        message("Ratios compare separately normalized spatial patterns because normalise_treatment_heat_0_to_1=TRUE.")
    first_mat <- spatial_data$projections[[manifest$key[1]]][[target_projection]]
    if (is.null(first_mat)) stop("Requested projection was not prepared.")
    mat_h <- nrow(first_mat)
    mat_w <- ncol(first_mat)
    ref_span_2d <- max(mat_h, mat_w)
    bin_width_2d <- ref_span_2d/n_bins
    n_rows_raw <- max(1L, as.integer(ceiling(mat_h/bin_width_2d)))
    n_cols_raw <- max(1L, as.integer(ceiling(mat_w/bin_width_2d)))
    orient_choice <- toupper(anterior_orientation[[target_projection]] %||% "R")
    if (target_projection == "PA_LR") {
        if (orient_choice == "R") {
            map_coords <- function(r, c) list(x = r, y = n_cols_raw - c + 1L)
            n_plot_x <- n_rows_raw
            n_plot_y <- n_cols_raw
            axis_labels <- list(x = "Posterior → Anterior (Right: Forebrain)", y = "Right → Left (Top: Left)")
        }
        else if (orient_choice == "L") {
            map_coords <- function(r, c) list(x = n_rows_raw - r + 1L, y = c)
            n_plot_x <- n_rows_raw
            n_plot_y <- n_cols_raw
            axis_labels <- list(x = "Anterior → Posterior (Left: Forebrain)", y = "Left → Right (Top: Right)")
        }
        else if (orient_choice == "D") {
            map_coords <- function(r, c) list(x = c, y = n_rows_raw - r + 1L)
            n_plot_x <- n_cols_raw
            n_plot_y <- n_rows_raw
            axis_labels <- list(x = "Left → Right", y = "Anterior → Posterior (Bottom: Forebrain)")
        }
        else {
            map_coords <- function(r, c) list(x = c, y = r)
            n_plot_x <- n_cols_raw
            n_plot_y <- n_rows_raw
            axis_labels <- list(x = "Left → Right", y = "Posterior → Anterior (Top: Forebrain)")
        }
    }
    else if (target_projection == "VD_PA") {
        if (orient_choice == "L") {
            map_coords <- function(r, c) list(x = n_cols_raw - c + 1L, y = r)
            n_plot_x <- n_cols_raw
            n_plot_y <- n_rows_raw
            axis_labels <- list(x = "Anterior → Posterior (Left: Forebrain)", y = "Ventral → Dorsal (Top: Dorsal)")
        }
        else {
            map_coords <- function(r, c) list(x = c, y = r)
            n_plot_x <- n_cols_raw
            n_plot_y <- n_rows_raw
            axis_labels <- list(x = "Posterior → Anterior (Right: Forebrain)", y = "Ventral → Dorsal (Top: Dorsal)")
        }
    }
    else {
        if (orient_choice == "D") {
            map_coords <- function(r, c) list(x = c, y = n_rows_raw - r + 1L)
            n_plot_x <- n_cols_raw
            n_plot_y <- n_rows_raw
            axis_labels <- list(x = "Left → Right", y = "Dorsal → Ventral (Top: Ventral)")
        }
        else {
            map_coords <- function(r, c) list(x = c, y = r)
            n_plot_x <- n_cols_raw
            n_plot_y <- n_rows_raw
            axis_labels <- list(x = "Left → Right", y = "Ventral → Dorsal (Top: Dorsal)")
        }
    }
    binned_list_2d_raw <- list()
    for (i in seq_len(nrow(manifest))) {
        k <- manifest$key[i]
        f_mat <- spatial_data$projections[[k]][[target_projection]]
        if (is.null(f_mat))
            next
        pos_coords <- which(f_mat > 0, arr.ind = TRUE)
        if (nrow(pos_coords) == 0)
            next
        raw_r <- pmin(pmax(as.integer(floor((pos_coords[, 1] - 1L)/bin_width_2d) + 1L), 1L), n_rows_raw)
        raw_c <- pmin(pmax(as.integer(floor((pos_coords[, 2] - 1L)/bin_width_2d) + 1L), 1L), n_cols_raw)
        mapped <- map_coords(raw_r, raw_c)
        df_bins <- data.frame(row_bin = mapped$y, col_bin = mapped$x) %>% dplyr::count(col_bin, row_bin,
            name = "pixel_count") %>% tidyr::complete(col_bin = 1:n_plot_x, row_bin = 1:n_plot_y, fill = list(pixel_count = 0)) %>%
            dplyr::mutate(expt = manifest$expt[i], condition = manifest$condition[i], nominal_time = manifest$nominal_time[i])
        binned_list_2d_raw[[length(binned_list_2d_raw) + 1L]] <- df_bins
    }
    all_binned_2d_raw <- dplyr::bind_rows(binned_list_2d_raw)
    # Normalize each fish by its highest occupied-pixel bin across the selected times.
    all_binned_2d <- all_binned_2d_raw %>% dplyr::group_by(expt) %>% dplyr::mutate(peak_px = max(pixel_count,
        na.rm = TRUE), raw_pixel_count = pixel_count, relative_density = pixel_count/pmax(peak_px, 1e-08),
        density_percent = (pixel_count/pmax(peak_px, 1e-08)) * 100, occupancy_rate = as.numeric(pixel_count >
            0) * 100) %>% dplyr::ungroup()
    time_summary <- all_binned_2d %>% dplyr::group_by(condition, nominal_time, col_bin, row_bin) %>%
        dplyr::summarise(mean_val = mean(.data[[plot_metric]], na.rm = TRUE), .groups = "drop") %>% dplyr::mutate(time_label = sprintf("%.6gh", nominal_time))
    # Keep panels in chronological order, rather than alphabetic label order.
    time_summary$time_label <- factor(time_summary$time_label,
        levels = sprintf("%.6gh", sort(unique(time_summary$nominal_time))))
    if (normalise_treatment_heat_0_to_1) {
        time_summary <- time_summary %>% dplyr::group_by(condition) %>% dplyr::mutate(mean_val = mean_val/pmax(max(mean_val,
            na.rm = TRUE), 1e-08)) %>% dplyr::ungroup()
    }
    # Give each observed timepoint equal weight in the window summary.
    aggregate_treatment <- time_summary %>% dplyr::group_by(condition, col_bin, row_bin) %>% dplyr::summarise(agg_val = mean(mean_val,
        na.rm = TRUE), .groups = "drop")
    if (normalise_treatment_heat_0_to_1) {
        aggregate_treatment <- aggregate_treatment %>% dplyr::group_by(condition) %>% dplyr::mutate(agg_val = agg_val/pmax(max(agg_val,
            na.rm = TRUE), 1e-08)) %>% dplyr::ungroup()
    }
    .generate_contrast_tables <- function(base_t1, comp_trts) {
        base_key <- tolower(base_t1)
        time_list <- list()
        agg_list <- list()
        for (tk in comp_trts) {
            tk_key <- tolower(tk)
            sub_t <- time_summary %>% dplyr::mutate(c_low = tolower(condition)) %>% dplyr::filter(c_low %in%
                c(base_key, tk_key)) %>% dplyr::select(c_low, nominal_time, time_label, col_bin, row_bin,
                mean_val) %>% tidyr::pivot_wider(names_from = c_low, values_from = mean_val, values_fill = list(mean_val = NA_real_))
            if (base_key %in% names(sub_t) && tk_key %in% names(sub_t)) {
                v_base <- sub_t[[base_key]]
                v_tk <- sub_t[[tk_key]]
                if (comparison_metric == "difference") {
                  sub_t$Comparison <- v_tk - v_base
                }
                else if (comparison_metric == "normalized_difference") {
                  sub_t$Comparison <- (v_tk - v_base)/(abs(v_tk) + abs(v_base) + pseudocount)
                }
                else if (comparison_metric == "ratio") {
                  sub_t$Comparison <- (v_tk + pseudocount)/(v_base + pseudocount)
                }
                else {
                  sub_t$Comparison <- log2((v_tk + pseudocount)/(v_base + pseudocount))
                }
                sub_t$contrast_label <- sprintf("%s %s %s", tk, if (comparison_metric %in% c("ratio", "log2_ratio")) "/" else "-", base_t1)
                sub_t$comparison_treatment <- tk
                time_list[[length(time_list) + 1L]] <- sub_t
            }
            sub_a <- aggregate_treatment %>% dplyr::mutate(c_low = tolower(condition)) %>% dplyr::filter(c_low %in%
                c(base_key, tk_key)) %>% dplyr::select(c_low, col_bin, row_bin, agg_val) %>% tidyr::pivot_wider(names_from = c_low,
                values_from = agg_val, values_fill = list(agg_val = NA_real_))
            if (base_key %in% names(sub_a) && tk_key %in% names(sub_a)) {
                va_base <- sub_a[[base_key]]
                va_tk <- sub_a[[tk_key]]
                if (comparison_metric == "difference") {
                  sub_a$Comparison <- va_tk - va_base
                }
                else if (comparison_metric == "normalized_difference") {
                  sub_a$Comparison <- (va_tk - va_base)/(abs(va_tk) + abs(va_base) + pseudocount)
                }
                else if (comparison_metric == "ratio") {
                  sub_a$Comparison <- (va_tk + pseudocount)/(va_base + pseudocount)
                }
                else {
                  sub_a$Comparison <- log2((va_tk + pseudocount)/(va_base + pseudocount))
                }
                sub_a$contrast_label <- sprintf("%s %s %s", tk, if (comparison_metric %in% c("ratio", "log2_ratio")) "/" else "-", base_t1)
                sub_a$comparison_treatment <- tk
                agg_list[[length(agg_list) + 1L]] <- sub_a
            }
        }
        list(time = dplyr::bind_rows(time_list), agg = dplyr::bind_rows(agg_list))
    }
    c1 <- .generate_contrast_tables(t1_name, comparison_treatments)
    c2 <- if (!is.null(t1_sec_name))
        .generate_contrast_tables(t1_sec_name, comparison_treatments_second)
    else NULL
    r_c1_time <- range(c1$time$Comparison, na.rm = TRUE)
    r_c1_agg <- range(c1$agg$Comparison, na.rm = TRUE)
    max_abs_g1 <- max(abs(r_c1_time), abs(r_c1_agg), 0.01)
    if (!is.null(c2)) {
        r_c2_time <- range(c2$time$Comparison, na.rm = TRUE)
        r_c2_agg <- range(c2$agg$Comparison, na.rm = TRUE)
        max_abs_g2 <- max(abs(r_c2_time), abs(r_c2_agg), 0.01)
    }
    else {
        max_abs_g2 <- NULL
    }
    if (!is.null(contrast_heat_limits)) {
        lims_g1 <- contrast_heat_limits
        lims_g2 <- contrast_heat_limits
    }
    else if (!is.null(comparison_heat_limits)) {
        lims_g1 <- comparison_heat_limits
        lims_g2 <- comparison_heat_limits
    }
    else if (!is.null(c2) && contrast_legend_mode == "shared") {
        global_max_abs <- max(max_abs_g1, max_abs_g2)
        lims_g1 <- c(-global_max_abs, global_max_abs)
        lims_g2 <- c(-global_max_abs, global_max_abs)
    }
    else {
        lims_g1 <- c(-max_abs_g1, max_abs_g1)
        lims_g2 <- if (!is.null(max_abs_g2))
            c(-max_abs_g2, max_abs_g2)
        else NULL
    }
    # Ratios have a neutral value of 1; differences and log2 ratios have a neutral value of 0.
    contrast_midpoint <- if (comparison_metric == "ratio") 1 else 0
    if (comparison_metric == "ratio" && is.null(contrast_heat_limits) && is.null(comparison_heat_limits)) {
        ratio_max <- max(c(c1$time$Comparison, c1$agg$Comparison, 1), na.rm = TRUE)
        if (!is.null(c2) && contrast_legend_mode == "shared")
            ratio_max <- max(c(ratio_max, c2$time$Comparison, c2$agg$Comparison), na.rm = TRUE)
        lims_g1 <- c(0, ratio_max)
        if (!is.null(c2)) lims_g2 <- if (contrast_legend_mode == "shared") lims_g1 else
            c(0, max(c(c2$time$Comparison, c2$agg$Comparison, 1), na.rm = TRUE))
    }
    cb_width <- if (length(heat_scale_bar_size) >= 1)
        heat_scale_bar_size[1]
    else 2.5
    cb_height <- if (length(heat_scale_bar_size) >= 2)
        heat_scale_bar_size[2]
    else 18
    cb_title <- heat_scale_bar_title %||% plot_metric
    build_guide <- function() {
        ggplot2::guide_colorbar(title = cb_title, barwidth = grid::unit(cb_width, "mm"), barheight = grid::unit(cb_height,
            "mm"), label = isTRUE(show_heat_scale_bar_label), draw.ulim = TRUE, draw.llim = TRUE)
    }
    build_base_theme <- function() {
        th <- ggplot2::theme_minimal(base_size = font_size, base_family = font_family) + ggplot2::theme(panel.grid = ggplot2::element_blank(),
            panel.spacing.x = grid::unit(panel_spacing_x_mm, "mm"), panel.spacing.y = grid::unit(panel_spacing_y_mm,
                "mm"), legend.position = "right", legend.box.just = "center")
        if (!show_all_labels) {
            th <- th + ggplot2::theme(axis.text = ggplot2::element_blank(), axis.title = ggplot2::element_blank(),
                axis.ticks = ggplot2::element_blank())
        }
        else if (!show_axis_titles) {
            th <- th + ggplot2::theme(axis.title = ggplot2::element_blank())
        }
        if (tick_style == "none") {
            th <- th + ggplot2::theme(axis.ticks = ggplot2::element_blank(), axis.text = ggplot2::element_blank())
        }
        th
    }
    build_labs <- function(default_main, default_sub) {
        title_text <- if (isTRUE(show_main_title))
            (main_title %||% default_main)
        else NULL
        sub_text <- if (isTRUE(show_subtitle))
            (subtitle %||% default_sub)
        else NULL
        x_lab <- if (isTRUE(show_axis_titles))
            axis_labels$x
        else NULL
        y_lab <- if (isTRUE(show_axis_titles))
            axis_labels$y
        else NULL
        ggplot2::labs(title = title_text, subtitle = sub_text, x = x_lab, y = y_lab)
    }
    apply_ticks <- function(p) {
        if (tick_style == "max_only") {
            p + ggplot2::scale_x_continuous(breaks = c(1, n_plot_x), labels = c(1, n_plot_x)) + ggplot2::scale_y_continuous(breaks = c(1,
                n_plot_y), labels = c(1, n_plot_y))
        }
        else if (tick_style == "custom" && !is.null(custom_breaks_x) && !is.null(custom_breaks_y)) {
            p + ggplot2::scale_x_continuous(breaks = custom_breaks_x) + ggplot2::scale_y_continuous(breaks = custom_breaks_y)
        }
        else {
            p
        }
    }
    p_contrast_time_1 <- apply_ticks(ggplot2::ggplot(c1$time, ggplot2::aes(x = col_bin, y = row_bin,
        fill = Comparison)) + ggplot2::geom_tile() + ggplot2::facet_grid(contrast_label ~ time_label) +
        ggplot2::scale_fill_gradient2(low = palette_comparison["low"], mid = palette_comparison["mid"],
            high = palette_comparison["high"], midpoint = contrast_midpoint, limits = lims_g1, oob = scales::squish,
            guide = build_guide()) + ggplot2::coord_equal() + build_base_theme() + build_labs("Spatial Contrast Over Time",
        sprintf("Reference: %s", t1_name)))
    p_agg_contrast_1 <- apply_ticks(ggplot2::ggplot(c1$agg, ggplot2::aes(x = col_bin, y = row_bin, fill = Comparison)) +
        ggplot2::geom_tile() + ggplot2::facet_wrap(~contrast_label, nrow = 1) + ggplot2::scale_fill_gradient2(low = palette_comparison["low"],
        mid = palette_comparison["mid"], high = palette_comparison["high"], midpoint = contrast_midpoint, limits = lims_g1,
        oob = scales::squish, guide = build_guide()) + ggplot2::coord_equal() + build_base_theme() +
        build_labs("Aggregated Spatial Contrast", sprintf("Reference: %s", t1_name)))
    if (!is.null(c2)) {
        p_contrast_time_2 <- apply_ticks(ggplot2::ggplot(c2$time, ggplot2::aes(x = col_bin, y = row_bin,
            fill = Comparison)) + ggplot2::geom_tile() + ggplot2::facet_grid(contrast_label ~ time_label) +
            ggplot2::scale_fill_gradient2(low = palette_comparison["low"], mid = palette_comparison["mid"],
                high = palette_comparison["high"], midpoint = contrast_midpoint, limits = lims_g2, oob = scales::squish,
                guide = build_guide()) + ggplot2::coord_equal() + build_base_theme() + build_labs("Spatial Contrast Over Time (Secondary)",
            sprintf("Reference: %s", t1_sec_name)))
        p_agg_contrast_2 <- apply_ticks(ggplot2::ggplot(c2$agg, ggplot2::aes(x = col_bin, y = row_bin,
            fill = Comparison)) + ggplot2::geom_tile() + ggplot2::facet_wrap(~contrast_label, nrow = 1) +
            ggplot2::scale_fill_gradient2(low = palette_comparison["low"], mid = palette_comparison["mid"],
                high = palette_comparison["high"], midpoint = contrast_midpoint, limits = lims_g2, oob = scales::squish,
                guide = build_guide()) + ggplot2::coord_equal() + build_base_theme() + build_labs("Aggregated Spatial Contrast (Secondary)",
            sprintf("Reference: %s", t1_sec_name)))
        if (contrast_legend_mode == "shared") {
            all_contrast_time <- dplyr::bind_rows(c1$time, c2$time)
            order_levels_time <- unique(all_contrast_time$contrast_label)
            all_contrast_time$contrast_label <- factor(all_contrast_time$contrast_label, levels = order_levels_time)
            p_contrast_time_combined <- apply_ticks(ggplot2::ggplot(all_contrast_time, ggplot2::aes(x = col_bin,
                y = row_bin, fill = Comparison)) + ggplot2::geom_tile() + ggplot2::facet_grid(contrast_label ~
                time_label) + ggplot2::scale_fill_gradient2(low = palette_comparison["low"], mid = palette_comparison["mid"],
                high = palette_comparison["high"], midpoint = contrast_midpoint, limits = lims_g1, oob = scales::squish,
                guide = build_guide()) + ggplot2::coord_equal() + build_base_theme() + build_labs("Combined Spatial Contrast Over Time",
                "Shared Legend Scale"))
            all_contrast_agg <- dplyr::bind_rows(c1$agg, c2$agg)
            order_levels_agg <- unique(all_contrast_agg$contrast_label)
            all_contrast_agg$contrast_label <- factor(all_contrast_agg$contrast_label, levels = order_levels_agg)
            p_agg_contrast_combined <- apply_ticks(ggplot2::ggplot(all_contrast_agg, ggplot2::aes(x = col_bin,
                y = row_bin, fill = Comparison)) + ggplot2::geom_tile() + ggplot2::facet_wrap(~contrast_label,
                ncol = 1) + ggplot2::scale_fill_gradient2(low = palette_comparison["low"], mid = palette_comparison["mid"],
                high = palette_comparison["high"], midpoint = contrast_midpoint, limits = lims_g1, oob = scales::squish,
                guide = build_guide()) + ggplot2::coord_equal() + build_base_theme() + build_labs("Combined Aggregated Spatial Contrast",
                "Shared Legend Scale"))
        }
        else {
            p_contrast_time_combined <- .combine_vertical_plots(list(p_contrast_time_1, p_contrast_time_2))
            p_agg_contrast_combined <- .combine_vertical_plots(list(p_agg_contrast_1, p_agg_contrast_2))
        }
    }
    else {
        p_contrast_time_2 <- NULL
        p_agg_contrast_2 <- NULL
        p_contrast_time_combined <- p_contrast_time_1
        p_agg_contrast_combined <- p_agg_contrast_1
    }
    treatment_limits <- if (!is.null(plot_heat_limits)) plot_heat_limits else
        range(c(0, time_summary$mean_val, aggregate_treatment$agg_val), finite = TRUE)
    if (length(treatment_limits) != 2L || any(!is.finite(treatment_limits)) || diff(treatment_limits) <= 0)
        stop("Treatment heat limits must be two increasing finite numbers.")
    treatment_subtitle <- if (normalise_treatment_heat_0_to_1) "Scaled to each treatment's peak" else plot_metric
    p_indiv_time <- apply_ticks(ggplot2::ggplot(time_summary, ggplot2::aes(x = col_bin, y = row_bin,
        fill = mean_val)) + ggplot2::geom_tile() + ggplot2::facet_grid(condition ~ time_label) + ggplot2::scale_fill_viridis_c(option = palette_treatment,
        limits = treatment_limits, guide = build_guide()) + ggplot2::coord_equal() + build_base_theme() + build_labs("Treatment Mean Spatial Distribution",
        treatment_subtitle))
    p_agg_treatment <- apply_ticks(ggplot2::ggplot(aggregate_treatment, ggplot2::aes(x = col_bin, y = row_bin,
        fill = agg_val)) + ggplot2::geom_tile() + ggplot2::facet_wrap(~condition, nrow = 1) + ggplot2::scale_fill_viridis_c(option = palette_treatment,
        limits = treatment_limits, guide = build_guide()) + ggplot2::coord_equal() + build_base_theme() + build_labs("Aggregated Treatment Density",
        treatment_subtitle))
    if (isTRUE(save_svg) && is.null(save_svg_dir)) stop("Supply save_svg_dir to export plots.")
    if (isTRUE(save_svg) && !is.null(save_svg_dir)) {
        save_microglia_plot_svg(p_indiv_time, "individual_by_time.svg", save_svg_dir, svg_width, svg_height,
            svg_units, svg_dpi)
        save_microglia_plot_svg(p_contrast_time_combined, "contrast_by_time.svg", save_svg_dir, svg_width,
            svg_height, svg_units, svg_dpi)
        save_microglia_plot_svg(p_agg_treatment, "aggregate_treatment.svg", save_svg_dir, svg_width,
            svg_height, svg_units, svg_dpi)
        save_microglia_plot_svg(p_agg_contrast_combined, "aggregate_contrast.svg", save_svg_dir, svg_width,
            svg_height, svg_units, svg_dpi)
    }
    scale_ranges <- list(group1_contrast_limits = sprintf("%.3f to %.3f", lims_g1[1], lims_g1[2]), group2_contrast_limits = if (!is.null(lims_g2)) sprintf("%.3f to %.3f",
        lims_g2[1], lims_g2[2]) else "N/A", contrast_legend_mode = if (!is.null(c2)) contrast_legend_mode else "single_baseline")
    return(list(plots = list(individual_by_time = p_indiv_time, contrast_by_time_group1 = p_contrast_time_1,
        contrast_by_time_group2 = p_contrast_time_2, contrast_by_time_combined = p_contrast_time_combined,
        aggregate_treatment = p_agg_treatment, aggregate_contrast_group1 = p_agg_contrast_1, aggregate_contrast_group2 = p_agg_contrast_2,
        aggregate_contrast_combined = p_agg_contrast_combined), scale_ranges = scale_ranges, data = list(fish_bins = all_binned_2d, time_summary = time_summary,
        contrast_group1 = c1, contrast_group2 = c2, aggregate_treatment = aggregate_treatment)))
}

