# Load Microglia_common.R first. Examples and input requirements are in README.md.
library(dplyr)
library(tidyr)
library(ggplot2)

# Fit one PCA to the selected cells, using explicitly named morphology features.
# log1p reduces skew; scaling gives each feature equal variance before PCA.
fit_microglia_pca <- function(data, measures, log_transform = TRUE) {
    if (!is.data.frame(data) || nrow(data) < 3L) stop("PCA needs at least three cells.")
    if (length(measures) < 2L || anyDuplicated(measures) || !all(measures %in% names(data)))
        stop("Choose at least two distinct measurement columns by name.")
    if (!all(vapply(data[measures], is.numeric, logical(1)))) stop("PCA features must be numeric.")
    .microglia_flag(log_transform, "log_transform")
    x <- as.matrix(data[measures])
    if (any(!is.finite(x))) stop("PCA features contain missing or infinite values; inspect the measurement audit.")
    if (log_transform) {
        if (any(x < 0)) stop("Morphology features must be nonnegative for log1p transformation.")
        x <- log1p(x)
    }
    deviations <- apply(x, 2, stats::sd)
    constant <- names(deviations)[!is.finite(deviations) | deviations == 0]
    if (length(constant)) stop("Remove constant PCA features: ", paste(constant, collapse = ", "))
    model <- stats::prcomp(x, center = TRUE, scale. = TRUE)
    scores <- data[, names(data) != "coords", drop = FALSE]
    if (any(colnames(model$x) %in% names(scores))) stop("Input already contains PC score columns.")
    scores <- cbind(scores, as.data.frame(model$x))
    variance <- model$sdev^2 / sum(model$sdev^2) * 100
    list(model = model, scores = scores, loadings = model$rotation,
         variance = data.frame(PC = paste0("PC", seq_along(variance)), percent = variance),
         measures = measures, log_transform = log_transform,
         filter_audit = attr(data, "filter_audit"), measurement_audit = attr(data, "measurement_audit"))
}

# Show the variance explained by each PC and the cumulative percentage.
plot_pca_variance <- function(
    pca,
    n_pcs = 10,
    variance_explained = NULL,
    bar_colour = "#6BAED6",
    cumulative_colour = "black",
    show_percent = TRUE,
    percent_digits = 1,
    percent_inside = TRUE,
    percent_colour = "black",
    bar_width = 0.75,
    line_width = 0.5,
    point_size = 1.5,
    font_family = "Arial",
    axis_font_size = 8,
    title_font_size = 9
) {
    if (inherits(pca, "prcomp")) {
        variance <- pca$sdev^2
        variance_explained <- (variance/sum(variance)) * 100
    }
    else if (!is.null(variance_explained)) {
        variance_explained <- as.numeric(variance_explained)
    }
    else if (is.data.frame(pca)) {
        pc_columns <- grep("^PC[0-9]+$", names(pca), value = TRUE)
        if (!length(pc_columns)) {
            stop("No PC columns such as PC1, PC2, ... were found.")
        }
        pc_numbers <- as.numeric(sub("^PC", "", pc_columns))
        pc_columns <- pc_columns[order(pc_numbers)]
        score_variances <- vapply(pca[pc_columns], stats::var, numeric(1), na.rm = TRUE)
        variance_explained <- (score_variances/sum(score_variances)) * 100
        warning("pca is a scores dataframe rather than the original PCA object. ", "Variance percentages are therefore calculated relative to the PC columns ",
            "present in this dataframe, not necessarily relative to the full original PCA.")
    }
    else {
        stop("pca must be either a prcomp object or a dataframe containing PC columns.")
    }
    n_available <- length(variance_explained)
    n_pcs <- min(as.integer(n_pcs), n_available)
    if (n_pcs < 1) {
        stop("No principal components are available.")
    }
    variance_explained <- variance_explained[seq_len(n_pcs)]
    plot_data <- data.frame(PC = factor(paste0("PC", seq_len(n_pcs)), levels = paste0("PC", seq_len(n_pcs))),
        Variance = variance_explained, stringsAsFactors = FALSE)
    plot_data$Cumulative <- cumsum(plot_data$Variance)
    plot_data$Label <- paste0(formatC(plot_data$Variance, format = "f", digits = percent_digits), "%")
    if (isTRUE(percent_inside)) {
        label_offset <- max(plot_data$Variance, na.rm = TRUE) * 0.035
        plot_data$LabelY <- pmax(plot_data$Variance - label_offset, plot_data$Variance * 0.5)
        label_vjust <- 1
    }
    else {
        label_offset <- max(plot_data$Cumulative, na.rm = TRUE) * 0.02
        plot_data$LabelY <- plot_data$Variance + label_offset
        label_vjust <- 0
    }
    p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = PC)) + ggplot2::geom_col(ggplot2::aes(y = Variance),
        fill = bar_colour, width = bar_width) + ggplot2::geom_line(ggplot2::aes(y = Cumulative, group = 1),
        colour = cumulative_colour, linewidth = line_width) + ggplot2::geom_point(ggplot2::aes(y = Cumulative),
        colour = cumulative_colour, size = point_size)
    if (isTRUE(show_percent)) {
        p <- p + ggplot2::geom_text(ggplot2::aes(y = LabelY, label = Label), family = font_family, size = axis_font_size/ggplot2::.pt,
            colour = percent_colour, vjust = label_vjust)
    }
    upper_limit <- max(c(plot_data$Cumulative, plot_data$LabelY), na.rm = TRUE) * 1.05
    p <- p + ggplot2::scale_y_continuous(limits = c(0, upper_limit), breaks = seq(0, 100, by = 10), labels = function(x) {
        paste0(x, "%")
    }, expand = ggplot2::expansion(mult = c(0, 0.02))) + ggplot2::labs(x = "Principal component", y = "Variance explained",
        title = "PCA variance explained") + ggplot2::theme_classic(base_family = font_family) + ggplot2::theme(axis.text = ggplot2::element_text(family = font_family,
        size = axis_font_size, colour = "black"), axis.title = ggplot2::element_text(family = font_family,
        size = axis_font_size, colour = "black"), plot.title = ggplot2::element_text(family = font_family,
        size = title_font_size, colour = "black"))
    return(list(plot = p, data = plot_data))
}

# Compare PC scores over time, across the selected window, or in PC1-PC2 space.
plot_pca_comparison <- function(
    data,
    treatments = c("para", "yolk", "PC"),
    pcs = c("PC1", "PC2"),
    plot_type = c("over_time", "aggregate", "pc1_pc2"),
    comparison_level = c("fish", "cell"),
    treatment_col = "treatment",
    batch_col = "batch",
    time_col = "nominal_elapsed_hour",
    time_window = NULL,
    line_colours = NULL,
    fill_colours = NULL,
    time_colour_mode = c("distinct", "palette", "manual"),
    time_palette = "Dark 3",
    time_colours = NULL,
    time_hue_start = 15,
    time_chroma = 95,
    time_luminance = 55,
    save_overtime_csv = FALSE,
    csv_directory = getwd(),
    csv_prefix = "PCA_over_time",
    csv_pcs = c("PC1", "PC2"),
    show_ribbon = TRUE,
    show_points = TRUE,
    line_width = 0.8,
    point_size = 1.2,
    point_alpha = 0.5,
    fill_alpha = 0.25,
    box_width = 0.6,
    base_size = 10,
    font_family = "Arial"
) {
    plot_type <- match.arg(plot_type)
    comparison_level <- match.arg(comparison_level)
    time_colour_mode <- match.arg(time_colour_mode)
    if (!requireNamespace("dplyr", quietly = TRUE))
        stop("Please install dplyr.")
    if (!requireNamespace("tidyr", quietly = TRUE))
        stop("Please install tidyr.")
    if (!requireNamespace("ggplot2", quietly = TRUE))
        stop("Please install ggplot2.")
    required <- c(pcs, treatment_col, batch_col, time_col)
    missing_cols <- setdiff(required, names(data))
    if (length(missing_cols)) {
        stop("Missing columns: ", paste(missing_cols, collapse = ", "))
    }
    if (plot_type == "pc1_pc2" && length(pcs) < 2) {
        stop("pc1_pc2 requires at least two PC columns.")
    }
    show_treatment_shape <- !is.null(treatments)
    if (is.null(treatments)) {
        df <- data
        treatments_used <- unique(as.character(df[[treatment_col]]))
    }
    else {
        treatments_used <- treatments
        df <- dplyr::filter(data, .data[[treatment_col]] %in% treatments_used)
    }
    if (!is.null(time_window)) {
        if (length(time_window) != 2) {
            stop("time_window must contain c(start, end).")
        }
        df <- dplyr::filter(df, .data[[time_col]] >= time_window[1], .data[[time_col]] <= time_window[2])
    }
    if (!nrow(df)) {
        stop("No rows remain after filtering.")
    }
    df[[treatment_col]] <- factor(df[[treatment_col]], levels = treatments_used)
    fish_counts <- dplyr::count(dplyr::distinct(df, Treatment = .data[[treatment_col]], Batch = .data[[batch_col]]),
        Treatment, name = "n_fish")
    legend_labels <- setNames(paste0(fish_counts$Treatment, " (fish n = ", fish_counts$n_fish, ")"),
        as.character(fish_counts$Treatment))
    if (is.null(line_colours)) {
        line_colours <- c(para = "#285291", yolk = "#C99C16", PC = "#425E1A")
    }
    if (is.null(fill_colours)) {
        fill_colours <- c(para = "#7CA0D4", yolk = "#ECC54F", PC = "#BADE86")
    }
    missing_line_colours <- setdiff(treatments_used, names(line_colours))
    if (length(missing_line_colours)) {
        extra <- grDevices::hcl.colors(length(missing_line_colours), palette = "Dark 3")
        names(extra) <- missing_line_colours
        line_colours <- c(line_colours, extra)
    }
    missing_fill_colours <- setdiff(treatments_used, names(fill_colours))
    if (length(missing_fill_colours)) {
        extra <- grDevices::hcl.colors(length(missing_fill_colours), palette = "Pastel 1")
        names(extra) <- missing_fill_colours
        fill_colours <- c(fill_colours, extra)
    }
    line_colours <- line_colours[treatments_used]
    fill_colours <- fill_colours[treatments_used]
    format_timepoint <- function(x) {
        day <- floor(x/24)
        hour_decimal <- x%%24
        hour <- floor(hour_decimal)
        minute <- round((hour_decimal - hour) * 60)
        if (minute == 60) {
            hour <- hour + 1
            minute <- 0
        }
        if (hour == 24) {
            day <- day + 1
            hour <- 0
        }
        sprintf("Day %d %02d:%02d", day, hour, minute)
    }
    format_csv_time <- function(x) {
        total_seconds <- round((x%%24) * 3600)
        hour <- floor(total_seconds/3600)
        minute <- floor((total_seconds%%3600)/60)
        second <- total_seconds%%60
        sprintf("%d:%02d:%02d", hour, minute, second)
    }
    make_time_colours <- function(time_values) {
        time_labels <- vapply(time_values, format_timepoint, character(1))
        n <- length(time_values)
        if (time_colour_mode == "distinct") {
            hues <- seq(time_hue_start, time_hue_start + 360, length.out = n + 1)[seq_len(n)]
            cols <- grDevices::hcl(h = hues, c = time_chroma, l = time_luminance, fixup = TRUE)
        }
        else if (time_colour_mode == "palette") {
            cols <- grDevices::hcl.colors(n = n, palette = time_palette)
        }
        else {
            if (is.null(time_colours)) {
                stop("For time_colour_mode = 'manual', supply time_colours.")
            }
            if (length(time_colours) != n) {
                stop("time_colours must contain exactly ", n, " colours.")
            }
            cols <- unname(time_colours)
        }
        names(cols) <- time_labels
        cols
    }
    build_overtime_export <- function(pc_name, comparison_level) {
        export_raw <- dplyr::transmute(dplyr::filter(df, is.finite(.data[[time_col]]), is.finite(.data[[pc_name]])),
            Treatment = as.character(.data[[treatment_col]]), Batch = .data[[batch_col]], Time = .data[[time_col]],
            Value = .data[[pc_name]])
        time_values <- sort(unique(export_raw$Time))
        if (comparison_level == "fish") {
            export_long <- dplyr::summarise(dplyr::group_by(export_raw, Treatment, Batch, Time), Value = mean(Value,
                na.rm = TRUE), .groups = "drop")
            fish_order <- dplyr::arrange(dplyr::mutate(dplyr::distinct(export_long, Treatment, Batch),
                .TreatmentOrder = match(Treatment, treatments_used)), .TreatmentOrder, Batch)
            columns <- vector("list", nrow(fish_order))
            column_names <- fish_order$Treatment
            for (i in seq_len(nrow(fish_order))) {
                tr <- fish_order$Treatment[i]
                ba <- fish_order$Batch[i]
                temp <- dplyr::filter(export_long, Treatment == tr, Batch == ba)
                columns[[i]] <- temp$Value[match(time_values, temp$Time)]
            }
            export_df <- as.data.frame(columns, check.names = FALSE)
            names(export_df) <- column_names
            column_map <- dplyr::transmute(fish_order, csv_column = dplyr::row_number() + 1, Treatment,
                Batch, observation_level = "fish")
        }
        else {
            cell_counts <- dplyr::count(export_raw, Treatment, Time, name = "n_cells")
            max_cells <- dplyr::arrange(dplyr::mutate(dplyr::summarise(dplyr::group_by(cell_counts, Treatment),
                max_cells = max(n_cells, na.rm = TRUE), .groups = "drop"), .TreatmentOrder = match(Treatment,
                treatments_used)), .TreatmentOrder)
            total_columns <- sum(max_cells$max_cells)
            columns <- vector("list", total_columns)
            column_names <- character(total_columns)
            column_map_list <- vector("list", total_columns)
            col_index <- 1
            for (tr in max_cells$Treatment) {
                n_slots <- max_cells$max_cells[max_cells$Treatment == tr]
                tr_data <- dplyr::filter(export_raw, Treatment == tr)
                for (slot in seq_len(n_slots)) {
                  values <- rep(NA_real_, length(time_values))
                  for (j in seq_along(time_values)) {
                    vals <- dplyr::pull(dplyr::filter(tr_data, Time == time_values[j]), Value)
                    if (length(vals) >= slot) {
                      values[j] <- vals[slot]
                    }
                  }
                  columns[[col_index]] <- values
                  column_names[col_index] <- tr
                  column_map_list[[col_index]] <- data.frame(csv_column = col_index + 1, Treatment = tr,
                    Batch = NA, observation_slot = slot, observation_level = "cell", stringsAsFactors = FALSE)
                  col_index <- col_index + 1
                }
            }
            export_df <- as.data.frame(columns, check.names = FALSE)
            names(export_df) <- column_names
            column_map <- dplyr::bind_rows(column_map_list)
        }
        rownames(export_df) <- vapply(time_values, format_csv_time, character(1))
        list(data = export_df, column_map = column_map)
    }
    csv_files <- NULL
    csv_exports <- NULL
    csv_column_maps <- NULL
    if (isTRUE(save_overtime_csv)) {
        missing_csv_pcs <- setdiff(csv_pcs, names(df))
        if (length(missing_csv_pcs)) {
            stop("Cannot export missing PC columns: ", paste(missing_csv_pcs, collapse = ", "))
        }
        csv_directory <- path.expand(csv_directory)
        if (!dir.exists(csv_directory)) {
            dir.create(csv_directory, recursive = TRUE, showWarnings = FALSE)
        }
        if (!dir.exists(csv_directory)) {
            stop("Could not create CSV directory: ", csv_directory)
        }
        csv_files <- setNames(character(length(csv_pcs)), csv_pcs)
        csv_exports <- setNames(vector("list", length(csv_pcs)), csv_pcs)
        csv_column_maps <- setNames(vector("list", length(csv_pcs)), csv_pcs)
        for (pc_name in csv_pcs) {
            export_object <- build_overtime_export(pc_name = pc_name, comparison_level = comparison_level)
            file_name <- paste0(csv_prefix, "_", comparison_level, "_", pc_name, ".csv")
            file_path <- file.path(csv_directory, file_name)
            utils::write.table(export_object$data, file = file_path, sep = ",", row.names = TRUE, col.names = NA,
                quote = FALSE, na = "")
            csv_files[pc_name] <- file_path
            csv_exports[[pc_name]] <- export_object$data
            csv_column_maps[[pc_name]] <- export_object$column_map
        }
    }
    if (plot_type == "pc1_pc2") {
        pc_x <- pcs[1]
        pc_y <- pcs[2]
        if (comparison_level == "fish") {
            plotting_data <- dplyr::summarise(dplyr::group_by(df, Treatment = .data[[treatment_col]],
                Batch = .data[[batch_col]], Time = .data[[time_col]]), PC_X = mean(.data[[pc_x]], na.rm = TRUE),
                PC_Y = mean(.data[[pc_y]], na.rm = TRUE), .groups = "drop")
        }
        else {
            plotting_data <- dplyr::transmute(df, Treatment = .data[[treatment_col]], Batch = .data[[batch_col]],
                Time = .data[[time_col]], PC_X = .data[[pc_x]], PC_Y = .data[[pc_y]])
        }
        plotting_data <- dplyr::filter(plotting_data, is.finite(Time), is.finite(PC_X), is.finite(PC_Y))
        time_values <- sort(unique(plotting_data$Time))
        time_labels <- vapply(time_values, format_timepoint, character(1))
        time_colours_used <- make_time_colours(time_values)
        plotting_data$Timepoint <- factor(plotting_data$Time, levels = time_values, labels = time_labels)
        plotting_data$Treatment <- factor(plotting_data$Treatment, levels = treatments_used)
        if (show_treatment_shape) {
            p <- ggplot2::ggplot(plotting_data, ggplot2::aes(x = PC_X, y = PC_Y, colour = Timepoint,
                shape = Treatment))
        }
        else {
            p <- ggplot2::ggplot(plotting_data, ggplot2::aes(x = PC_X, y = PC_Y, colour = Timepoint))
        }
        p <- p + ggplot2::geom_point(size = point_size, alpha = point_alpha) + ggplot2::scale_colour_manual(values = time_colours_used,
            drop = FALSE, name = "Timepoint") + ggplot2::labs(x = pc_x, y = pc_y) + ggplot2::theme_classic(base_size = base_size,
            base_family = font_family) + ggplot2::theme(axis.text = ggplot2::element_text(colour = "black"),
            legend.title = ggplot2::element_text(face = "bold"))
        if (show_treatment_shape) {
            p <- p + ggplot2::scale_shape_discrete(breaks = treatments_used, labels = legend_labels[treatments_used],
                name = "Treatment")
        }
        return(list(plot = p, plots = list(PC1_PC2 = p), plotting_data = plotting_data, summary_data = NULL,
            fish_counts = fish_counts, time_colours = time_colours_used, csv_files = csv_files, csv_exports = csv_exports,
            csv_column_maps = csv_column_maps, settings = list(treatments = treatments, pcs = pcs, plot_type = plot_type,
                comparison_level = comparison_level, time_window = time_window, time_colour_mode = time_colour_mode,
                time_palette = time_palette, save_overtime_csv = save_overtime_csv, csv_directory = csv_directory,
                csv_prefix = csv_prefix, csv_pcs = csv_pcs)))
    }
    long_data <- dplyr::filter(tidyr::pivot_longer(dplyr::select(df, Treatment = dplyr::all_of(treatment_col),
        Batch = dplyr::all_of(batch_col), Time = dplyr::all_of(time_col), dplyr::all_of(pcs)), cols = dplyr::all_of(pcs),
        names_to = "PC", values_to = "PC_value"), is.finite(Time), is.finite(PC_value))
    if (plot_type == "over_time") {
        if (comparison_level == "fish") {
            plotting_data <- dplyr::summarise(dplyr::group_by(long_data, Treatment, Batch, Time, PC),
                Value = mean(PC_value, na.rm = TRUE), .groups = "drop")
        }
        else {
            plotting_data <- dplyr::transmute(long_data, Treatment, Batch, Time, PC, Value = PC_value)
        }
        summary_data <- dplyr::summarise(dplyr::group_by(plotting_data, Treatment, Time, PC), Mean = mean(Value,
            na.rm = TRUE), SD = stats::sd(Value, na.rm = TRUE), N = dplyr::n(), SEM = SD/sqrt(N), .groups = "drop")
        make_plot <- function(pc_name) {
            dat <- dplyr::filter(summary_data, PC == pc_name)
            p <- ggplot2::ggplot(dat, ggplot2::aes(x = Time, y = Mean, colour = Treatment, fill = Treatment,
                group = Treatment))
            if (isTRUE(show_ribbon)) {
                p <- p + ggplot2::geom_ribbon(ggplot2::aes(ymin = Mean - SEM, ymax = Mean + SEM), alpha = fill_alpha,
                  colour = NA)
            }
            p <- p + ggplot2::geom_line(linewidth = line_width)
            if (isTRUE(show_points)) {
                p <- p + ggplot2::geom_point(size = point_size)
            }
            p + ggplot2::scale_colour_manual(values = line_colours, breaks = treatments_used, labels = legend_labels[treatments_used],
                drop = FALSE) + ggplot2::scale_fill_manual(values = fill_colours, breaks = treatments_used,
                labels = legend_labels[treatments_used], drop = FALSE) + ggplot2::labs(x = "Elapsed time (h)",
                y = paste0(pc_name, " score"), colour = NULL, fill = NULL) + ggplot2::theme_classic(base_size = base_size,
                base_family = font_family) + ggplot2::theme(axis.text = ggplot2::element_text(colour = "black"),
                legend.title = ggplot2::element_blank())
        }
    }
    else {
        if (comparison_level == "fish") {
            plotting_data <- dplyr::summarise(dplyr::group_by(long_data, Treatment, Batch, PC), Value = mean(PC_value,
                na.rm = TRUE), .groups = "drop")
        }
        else {
            plotting_data <- dplyr::transmute(long_data, Treatment, Batch, PC, Value = PC_value)
        }
        summary_data <- NULL
        make_plot <- function(pc_name) {
            dat <- dplyr::filter(plotting_data, PC == pc_name)
            p <- ggplot2::ggplot(dat, ggplot2::aes(x = Treatment, y = Value, colour = Treatment, fill = Treatment)) +
                ggplot2::geom_boxplot(width = box_width, alpha = fill_alpha, outlier.shape = NA, linewidth = line_width)
            if (isTRUE(show_points)) {
                p <- p + ggplot2::geom_point(position = ggplot2::position_jitter(width = 0.12, height = 0,
                  seed = 123), size = point_size, alpha = point_alpha)
            }
            p + ggplot2::scale_colour_manual(values = line_colours, breaks = treatments_used, labels = legend_labels[treatments_used],
                drop = FALSE) + ggplot2::scale_fill_manual(values = fill_colours, breaks = treatments_used,
                labels = legend_labels[treatments_used], drop = FALSE) + ggplot2::labs(x = NULL, y = paste0(pc_name,
                " score"), colour = NULL, fill = NULL) + ggplot2::theme_classic(base_size = base_size,
                base_family = font_family) + ggplot2::theme(axis.text = ggplot2::element_text(colour = "black"),
                legend.title = ggplot2::element_blank())
        }
    }
    plots <- setNames(lapply(pcs, make_plot), pcs)
    list(plots = plots, plotting_data = plotting_data, summary_data = summary_data, fish_counts = fish_counts,
        csv_files = csv_files, csv_exports = csv_exports, csv_column_maps = csv_column_maps, settings = list(treatments = treatments,
            pcs = pcs, plot_type = plot_type, comparison_level = comparison_level, time_window = time_window,
            save_overtime_csv = save_overtime_csv, csv_directory = csv_directory, csv_prefix = csv_prefix,
            csv_pcs = csv_pcs))
}

# Split the selected window at a user-defined night start; average timepoints within fish.
plot_pca_day_night <- function(
    data,
    treatments = c("para", "yolk", "PC"),
    pc = "PC1",
    comparison_level = c("fish", "cell"),
    treatment_col = "treatment",
    batch_col = "batch",
    time_col = "nominal_elapsed_hour",
    time_window = c(20, 26),
    night_start_inclusive = 22,
    line_colours = NULL,
    day_night_fills = c(Day = "white", Night = "grey55"),
    show_points = TRUE,
    box_width = 0.65,
    box_linewidth = 0.5,
    point_size = 1.2,
    point_alpha = 0.5,
    fill_alpha = 0.65,
    base_size = 10,
    font_family = "Arial"
) {
    comparison_level <- match.arg(comparison_level)
    if (!requireNamespace("dplyr", quietly = TRUE)) {
        stop("Please install dplyr.")
    }
    if (!requireNamespace("ggplot2", quietly = TRUE)) {
        stop("Please install ggplot2.")
    }
    required <- c(pc, treatment_col, batch_col, time_col)
    missing_cols <- setdiff(required, names(data))
    if (length(missing_cols)) {
        stop("Missing columns: ", paste(missing_cols, collapse = ", "))
    }
    if (length(time_window) != 2) {
        stop("time_window must contain c(start, end).")
    }
    if (night_start_inclusive < time_window[1] || night_start_inclusive > time_window[2]) {
        stop("night_start_inclusive must fall inside time_window.")
    }
    df <- dplyr::mutate(dplyr::filter(data, .data[[treatment_col]] %in% treatments, is.finite(.data[[time_col]]),
        is.finite(.data[[pc]]), .data[[time_col]] >= time_window[1], .data[[time_col]] <= time_window[2]),
        Treatment = factor(.data[[treatment_col]], levels = treatments), Batch = as.character(.data[[batch_col]]),
        Time = as.numeric(.data[[time_col]]), Phase = ifelse(Time >= night_start_inclusive, "Night",
            "Day"), Phase = factor(Phase, levels = c("Day", "Night")))
    if (!nrow(df)) {
        stop("No rows remain after treatment and time filtering.")
    }
    if (comparison_level == "fish") {
        fish_time <- dplyr::summarise(dplyr::group_by(df, Treatment, Batch, Time, Phase), TimepointMean = mean(.data[[pc]],
            na.rm = TRUE), .groups = "drop")
        plotting_data <- dplyr::summarise(dplyr::group_by(fish_time, Treatment, Batch, Phase), Value = mean(TimepointMean,
            na.rm = TRUE), N_timepoints = dplyr::n(), .groups = "drop")
    }
    else {
        plotting_data <- dplyr::transmute(df, Treatment, Batch, Time, Phase, Value = .data[[pc]])
    }
    plotting_data <- dplyr::filter(plotting_data, is.finite(Value))
    if (!nrow(plotting_data)) {
        stop("No finite values remain for plotting.")
    }
    if (is.null(line_colours)) {
        line_colours <- c(para = "#285291", yolk = "#C99C16", PC = "#425E1A", `yolk+DMSO` = "#B46B16",
            `yolk+SB225002` = "#A00000")
    }
    missing_colours <- setdiff(treatments, names(line_colours))
    if (length(missing_colours)) {
        extra <- grDevices::hcl.colors(length(missing_colours), palette = "Dark 3")
        names(extra) <- missing_colours
        line_colours <- c(line_colours, extra)
    }
    dodge <- ggplot2::position_dodge(width = 0.75)
    p <- ggplot2::ggplot(plotting_data, ggplot2::aes(x = Treatment, y = Value, fill = Phase, colour = Treatment,
        group = interaction(Treatment, Phase))) + ggplot2::geom_boxplot(position = dodge, width = box_width,
        linewidth = box_linewidth, alpha = fill_alpha, outlier.shape = NA)
    if (isTRUE(show_points)) {
        p <- p + ggplot2::geom_point(ggplot2::aes(group = Phase), position = ggplot2::position_jitterdodge(jitter.width = 0.2,
            dodge.width = 0.75, seed = 123), size = point_size, alpha = point_alpha)
    }
    p <- p + ggplot2::scale_colour_manual(values = line_colours[treatments], drop = FALSE) + ggplot2::scale_fill_manual(values = day_night_fills,
        drop = FALSE) + ggplot2::labs(x = NULL, y = paste0(pc, " score"), fill = NULL, colour = NULL) +
        ggplot2::theme_classic(base_size = base_size, base_family = font_family) + ggplot2::theme(axis.text = ggplot2::element_text(colour = "black"),
        legend.position = "right")
    summary_data <- dplyr::summarise(dplyr::group_by(plotting_data, Treatment, Phase), Mean = mean(Value,
        na.rm = TRUE), Median = stats::median(Value, na.rm = TRUE), SD = stats::sd(Value, na.rm = TRUE),
        N = dplyr::n(), .groups = "drop")
    list(plot = p, plotting_data = plotting_data, summary_data = summary_data, settings = list(treatments = treatments,
        pc = pc, comparison_level = comparison_level, time_window = time_window, night_start_inclusive = night_start_inclusive))
}

# Interactive PC1-PC2-PC3 score plot.
plot_pca_3d <- function(
    data,
    treatments = c("para", "yolk", "PC"),
    pcs = c("PC1", "PC2", "PC3"),
    comparison_level = c("cell", "fish"),
    treatment_col = "treatment",
    batch_col = "batch",
    time_col = "nominal_elapsed_hour",
    cluster_col = "Clusters",
    line_colours = NULL,
    point_size = 3,
    point_opacity = 0.7
) {
    comparison_level <- match.arg(comparison_level)
    if (!requireNamespace("dplyr", quietly = TRUE))
        stop("Please install dplyr.")
    if (!requireNamespace("plotly", quietly = TRUE))
        stop("Please install plotly.")
    required <- c(pcs, treatment_col, batch_col, time_col)
    missing_cols <- setdiff(required, names(data))
    if (length(missing_cols)) {
        stop("Missing columns: ", paste(missing_cols, collapse = ", "))
    }
    df <- dplyr::filter(data, .data[[treatment_col]] %in% treatments)
    if (!nrow(df))
        stop("No rows remain for the selected treatments.")
    df[[treatment_col]] <- factor(df[[treatment_col]], levels = treatments)
    fish_counts <- dplyr::count(dplyr::distinct(df, Treatment = .data[[treatment_col]], Batch = .data[[batch_col]]),
        Treatment, name = "n_fish")
    if (comparison_level == "fish") {
        group_vars <- c(treatment_col, batch_col, time_col)
        if (cluster_col %in% names(df))
            group_vars <- c(group_vars, cluster_col)
        df <- dplyr::summarise(dplyr::group_by(df, dplyr::across(dplyr::all_of(group_vars))), dplyr::across(dplyr::all_of(pcs),
            ~mean(.x, na.rm = TRUE)), .groups = "drop")
    }
    df$Treatment <- as.character(df[[treatment_col]])
    df$Batch <- as.character(df[[batch_col]])
    df$Time <- as.numeric(df[[time_col]])
    format_time <- function(x) {
        day <- floor(x/24)
        hour_decimal <- x%%24
        hour <- floor(hour_decimal)
        minute <- round((hour_decimal - hour) * 60)
        sprintf("Day %d %02d:%02d", day, hour, minute)
    }
    df$Timepoint <- vapply(df$Time, format_time, character(1))
    df$TreatmentLabel <- df$Treatment
    for (tr in treatments) {
        n <- fish_counts$n_fish[as.character(fish_counts$Treatment) == tr]
        if (length(n)) {
            df$TreatmentLabel[df$Treatment == tr] <- paste0(tr, " (fish n = ", n, ")")
        }
    }
    if (is.null(line_colours)) {
        line_colours <- c(para = "#285291", yolk = "#C99C16", PC = "#425E1A")
    }
    missing_colours <- setdiff(treatments, names(line_colours))
    if (length(missing_colours)) {
        extra_colours <- grDevices::hcl.colors(length(missing_colours), palette = "Dark 3")
        names(extra_colours) <- missing_colours
        line_colours <- c(line_colours, extra_colours)
    }
    time_values <- sort(unique(df$Time))
    time_labels <- vapply(time_values, format_time, character(1))
    time_colours <- grDevices::hcl.colors(length(time_values), palette = "Viridis")
    names(time_colours) <- time_labels
    hover_text <- paste0("Treatment: ", df$Treatment, "<br>Fish/batch: ", df$Batch, "<br>Time: ", df$Timepoint,
        "<br>", pcs[1], ": ", round(df[[pcs[1]]], 3), "<br>", pcs[2], ": ", round(df[[pcs[2]]], 3), "<br>",
        pcs[3], ": ", round(df[[pcs[3]]], 3))
    if (cluster_col %in% names(df)) {
        hover_text <- paste0(hover_text, "<br>Cluster: ", df[[cluster_col]])
    }
    df$.hover <- hover_text
    p <- plotly::plot_ly()
    treatment_levels <- treatments[treatments %in% unique(df$Treatment)]
    for (tr in treatment_levels) {
        dat <- df[df$Treatment == tr, , drop = FALSE]
        label <- unique(dat$TreatmentLabel)[1]
        p <- plotly::add_trace(p, data = dat, x = dat[[pcs[1]]], y = dat[[pcs[2]]], z = dat[[pcs[3]]],
            type = "scatter3d", mode = "markers", name = label, text = dat$.hover, hoverinfo = "text",
            marker = list(size = point_size, opacity = point_opacity, color = unname(line_colours[tr])),
            visible = TRUE, showlegend = TRUE)
    }
    n_treatment_traces <- length(treatment_levels)
    for (i in seq_along(time_values)) {
        tm <- time_values[i]
        dat <- df[abs(df$Time - tm) < 1e-08, , drop = FALSE]
        p <- plotly::add_trace(p, data = dat, x = dat[[pcs[1]]], y = dat[[pcs[2]]], z = dat[[pcs[3]]],
            type = "scatter3d", mode = "markers", name = time_labels[i], text = dat$.hover, hoverinfo = "text",
            marker = list(size = point_size, opacity = point_opacity, color = time_colours[time_labels[i]]),
            visible = FALSE, showlegend = TRUE)
    }
    n_time_traces <- length(time_values)
    cluster_levels <- character(0)
    if (cluster_col %in% names(df)) {
        cluster_levels <- unique(as.character(df[[cluster_col]]))
        cluster_levels <- cluster_levels[!is.na(cluster_levels)]
        cluster_colours <- grDevices::hcl.colors(length(cluster_levels), palette = "Dark 3")
        names(cluster_colours) <- cluster_levels
        for (cl in cluster_levels) {
            dat <- df[as.character(df[[cluster_col]]) == cl, , drop = FALSE]
            p <- plotly::add_trace(p, data = dat, x = dat[[pcs[1]]], y = dat[[pcs[2]]], z = dat[[pcs[3]]],
                type = "scatter3d", mode = "markers", name = cl, text = dat$.hover, hoverinfo = "text",
                marker = list(size = point_size, opacity = point_opacity, color = cluster_colours[cl]),
                visible = FALSE, showlegend = TRUE)
        }
    }
    n_cluster_traces <- length(cluster_levels)
    total_traces <- n_treatment_traces + n_time_traces + n_cluster_traces
    treatment_visibility <- c(rep(TRUE, n_treatment_traces), rep(FALSE, total_traces - n_treatment_traces))
    time_visibility <- c(rep(FALSE, n_treatment_traces), rep(TRUE, n_time_traces), rep(FALSE, n_cluster_traces))
    buttons <- list(list(method = "update", args = list(list(visible = treatment_visibility), list(title = "PCA — coloured by treatment")),
        label = "Treatment"), list(method = "update", args = list(list(visible = time_visibility), list(title = "PCA — coloured by timepoint")),
        label = "Timepoint"))
    if (n_cluster_traces > 0) {
        cluster_visibility <- c(rep(FALSE, n_treatment_traces + n_time_traces), rep(TRUE, n_cluster_traces))
        buttons[[length(buttons) + 1]] <- list(method = "update", args = list(list(visible = cluster_visibility),
            list(title = "PCA — coloured by cluster")), label = "Clusters")
    }
    p <- plotly::layout(p, title = "PCA — coloured by treatment", scene = list(xaxis = list(title = pcs[1]),
        yaxis = list(title = pcs[2]), zaxis = list(title = pcs[3]), aspectmode = "cube"), legend = list(title = list(text = "")),
        updatemenus = list(list(type = "dropdown", direction = "down", x = 0.02, y = 1.08, xanchor = "left",
            yanchor = "top", buttons = buttons)))
    list(plot = p, plotting_data = df, fish_counts = fish_counts)
}

# Interactive mean PC1-PC2 trajectory with time on the third axis.
plot_pca_3d_summary <- function(
    data,
    treatments = c("para", "yolk", "PC"),
    summary_level = c("cell", "fish"),
    error = c("SEM", "SD", "none"),
    treatment_col = "treatment",
    batch_col = "batch",
    time_col = "nominal_elapsed_hour",
    pc1_col = "PC1",
    pc2_col = "PC2",
    time_window = NULL,
    line_colours = NULL,
    point_size = 5,
    line_width = 5,
    error_line_width = 3,
    error_cap_width = 0.08,
    point_opacity = 1,
    show_points = TRUE,
    show_errorbars = TRUE,
    axis_title_standoff = 20
) {
    summary_level <- match.arg(summary_level)
    error <- match.arg(error)
    if (!requireNamespace("dplyr", quietly = TRUE))
        stop("Please install dplyr.")
    if (!requireNamespace("plotly", quietly = TRUE))
        stop("Please install plotly.")
    required <- c(treatment_col, batch_col, time_col, pc1_col, pc2_col)
    missing_cols <- setdiff(required, names(data))
    if (length(missing_cols)) {
        stop("Missing columns: ", paste(missing_cols, collapse = ", "))
    }
    if (is.null(treatments)) {
        df <- data
        treatments_used <- unique(as.character(df[[treatment_col]]))
    }
    else {
        treatments_used <- treatments
        df <- dplyr::filter(data, .data[[treatment_col]] %in% treatments_used)
    }
    if (!is.null(time_window)) {
        if (length(time_window) != 2)
            stop("time_window must contain c(start, end).")
        df <- dplyr::filter(df, .data[[time_col]] >= time_window[1], .data[[time_col]] <= time_window[2])
    }
    df <- dplyr::filter(df, is.finite(.data[[time_col]]), is.finite(.data[[pc1_col]]), is.finite(.data[[pc2_col]]))
    if (!nrow(df))
        stop("No rows remain after filtering.")
    fish_counts <- dplyr::count(dplyr::distinct(df, Treatment = .data[[treatment_col]], Batch = .data[[batch_col]]),
        Treatment, name = "n_fish")
    if (is.null(line_colours)) {
        line_colours <- c(para = "#285291", yolk = "#C99C16", PC = "#425E1A")
    }
    missing_colours <- setdiff(treatments_used, names(line_colours))
    if (length(missing_colours)) {
        extra <- grDevices::hcl.colors(length(missing_colours), palette = "Dark 3")
        names(extra) <- missing_colours
        line_colours <- c(line_colours, extra)
    }
    line_colours <- line_colours[treatments_used]
    if (summary_level == "cell") {
        summary_data <- dplyr::summarise(dplyr::group_by(df, Treatment = .data[[treatment_col]], Time = .data[[time_col]]),
            Mean_PC1 = mean(.data[[pc1_col]], na.rm = TRUE), Mean_PC2 = mean(.data[[pc2_col]], na.rm = TRUE),
            SD_PC1 = stats::sd(.data[[pc1_col]], na.rm = TRUE), SD_PC2 = stats::sd(.data[[pc2_col]],
                na.rm = TRUE), N = dplyr::n(), .groups = "drop")
    }
    else {
        fish_data <- dplyr::summarise(dplyr::group_by(df, Treatment = .data[[treatment_col]], Batch = .data[[batch_col]],
            Time = .data[[time_col]]), PC1 = mean(.data[[pc1_col]], na.rm = TRUE), PC2 = mean(.data[[pc2_col]],
            na.rm = TRUE), .groups = "drop")
        summary_data <- dplyr::summarise(dplyr::group_by(fish_data, Treatment, Time), Mean_PC1 = mean(PC1,
            na.rm = TRUE), Mean_PC2 = mean(PC2, na.rm = TRUE), SD_PC1 = stats::sd(PC1, na.rm = TRUE),
            SD_PC2 = stats::sd(PC2, na.rm = TRUE), N = dplyr::n(), .groups = "drop")
    }
    if (error == "SEM") {
        summary_data <- dplyr::mutate(summary_data, Error_PC1 = SD_PC1/sqrt(N), Error_PC2 = SD_PC2/sqrt(N))
    }
    else if (error == "SD") {
        summary_data <- dplyr::mutate(summary_data, Error_PC1 = SD_PC1, Error_PC2 = SD_PC2)
    }
    else {
        summary_data <- dplyr::mutate(summary_data, Error_PC1 = 0, Error_PC2 = 0)
    }
    format_time <- function(x) {
        day <- floor(x/24)
        hour_decimal <- x%%24
        hour <- floor(hour_decimal)
        minute <- round((hour_decimal - hour) * 60)
        if (minute == 60) {
            hour <- hour + 1
            minute <- 0
        }
        if (hour == 24) {
            day <- day + 1
            hour <- 0
        }
        sprintf("Day %d %02d:%02d", day, hour, minute)
    }
    summary_data$TimeLabel <- vapply(summary_data$Time, format_time, character(1))
    summary_data <- dplyr::mutate(dplyr::left_join(summary_data, fish_counts, by = "Treatment"), LegendLabel = paste0(Treatment,
        " (fish n = ", n_fish, ")"))
    p <- plotly::plot_ly()
    for (tr in treatments_used) {
        dat <- dplyr::arrange(dplyr::filter(summary_data, Treatment == tr), Time)
        if (!nrow(dat))
            next
        colour <- unname(line_colours[tr])
        hover_text <- paste0("Treatment: ", dat$Treatment, "<br>Time: ", dat$TimeLabel, "<br>Mean PC1: ",
            round(dat$Mean_PC1, 3), "<br>Mean PC2: ", round(dat$Mean_PC2, 3), "<br>", error, " PC1: ",
            round(dat$Error_PC1, 3), "<br>", error, " PC2: ", round(dat$Error_PC2, 3), "<br>Cells/fish used: ",
            dat$N, "<br>Fish n: ", dat$n_fish)
        mode_used <- if (show_points) {
            "lines+markers"
        }
        else {
            "lines"
        }
        p <- plotly::add_trace(p, x = dat$Mean_PC1, y = dat$Mean_PC2, z = dat$Time, type = "scatter3d",
            mode = mode_used, name = unique(dat$LegendLabel)[1], text = hover_text, hoverinfo = "text",
            line = list(color = colour, width = line_width), marker = list(color = colour, size = point_size,
                opacity = point_opacity), showlegend = TRUE)
        if (isTRUE(show_errorbars) && error != "none") {
            for (i in seq_len(nrow(dat))) {
                x <- dat$Mean_PC1[i]
                y <- dat$Mean_PC2[i]
                z <- dat$Time[i]
                ex <- dat$Error_PC1[i]
                ey <- dat$Error_PC2[i]
                if (is.finite(ex) && ex > 0) {
                  p <- plotly::add_trace(p, x = c(x - ex, x + ex), y = c(y, y), z = c(z, z), type = "scatter3d",
                    mode = "lines", line = list(color = colour, width = error_line_width), hoverinfo = "skip",
                    showlegend = FALSE)
                  p <- plotly::add_trace(p, x = c(x - ex, x - ex), y = c(y - error_cap_width, y + error_cap_width),
                    z = c(z, z), type = "scatter3d", mode = "lines", line = list(color = colour, width = error_line_width),
                    hoverinfo = "skip", showlegend = FALSE)
                  p <- plotly::add_trace(p, x = c(x + ex, x + ex), y = c(y - error_cap_width, y + error_cap_width),
                    z = c(z, z), type = "scatter3d", mode = "lines", line = list(color = colour, width = error_line_width),
                    hoverinfo = "skip", showlegend = FALSE)
                }
                if (is.finite(ey) && ey > 0) {
                  p <- plotly::add_trace(p, x = c(x, x), y = c(y - ey, y + ey), z = c(z, z), type = "scatter3d",
                    mode = "lines", line = list(color = colour, width = error_line_width), hoverinfo = "skip",
                    showlegend = FALSE)
                  p <- plotly::add_trace(p, x = c(x - error_cap_width, x + error_cap_width), y = c(y -
                    ey, y - ey), z = c(z, z), type = "scatter3d", mode = "lines", line = list(color = colour,
                    width = error_line_width), hoverinfo = "skip", showlegend = FALSE)
                  p <- plotly::add_trace(p, x = c(x - error_cap_width, x + error_cap_width), y = c(y +
                    ey, y + ey), z = c(z, z), type = "scatter3d", mode = "lines", line = list(color = colour,
                    width = error_line_width), hoverinfo = "skip", showlegend = FALSE)
                }
            }
        }
    }
    time_values <- sort(unique(summary_data$Time))
    time_labels <- vapply(time_values, format_time, character(1))
    p <- plotly::layout(p, scene = list(xaxis = list(title = list(text = pc1_col, standoff = axis_title_standoff)),
        yaxis = list(title = list(text = pc2_col, standoff = axis_title_standoff)), zaxis = list(title = list(text = "Time",
            standoff = axis_title_standoff), tickvals = time_values, ticktext = time_labels), aspectmode = "cube"),
        legend = list(title = list(text = "")))
    list(plot = p, summary_data = summary_data, fish_counts = fish_counts, settings = list(treatments = treatments,
        summary_level = summary_level, error = error, time_window = time_window, axis_title_standoff = axis_title_standoff))
}

# Interactive PC1-PC2 plot with time on the third axis.
plot_pca_3d_time <- function(
    data,
    treatments = c("para", "yolk", "PC"),
    comparison_level = c("cell", "fish"),
    treatment_col = "treatment",
    batch_col = "batch",
    time_col = "nominal_elapsed_hour",
    pc1_col = "PC1",
    pc2_col = "PC2",
    cluster_col = "Clusters",
    line_colours = NULL,
    point_size = 3,
    point_opacity = 0.7
) {
    comparison_level <- match.arg(comparison_level)
    if (!requireNamespace("dplyr", quietly = TRUE))
        stop("Please install dplyr.")
    if (!requireNamespace("plotly", quietly = TRUE))
        stop("Please install plotly.")
    required <- c(pc1_col, pc2_col, treatment_col, batch_col, time_col)
    missing_cols <- setdiff(required, names(data))
    if (length(missing_cols)) {
        stop("Missing columns: ", paste(missing_cols, collapse = ", "))
    }
    df <- dplyr::filter(data, .data[[treatment_col]] %in% treatments, is.finite(.data[[pc1_col]]), is.finite(.data[[pc2_col]]),
        is.finite(.data[[time_col]]))
    if (!nrow(df))
        stop("No rows remain after filtering.")
    fish_counts <- dplyr::count(dplyr::distinct(df, Treatment = .data[[treatment_col]], Batch = .data[[batch_col]]),
        Treatment, name = "n_fish")
    if (comparison_level == "fish") {
        grouping <- c(treatment_col, batch_col, time_col)
        df <- dplyr::summarise(dplyr::group_by(df, dplyr::across(dplyr::all_of(grouping))), PC1_plot = mean(.data[[pc1_col]],
            na.rm = TRUE), PC2_plot = mean(.data[[pc2_col]], na.rm = TRUE), Cluster_plot = if (cluster_col %in%
            names(data)) {
            names(sort(table(.data[[cluster_col]]), decreasing = TRUE))[1]
        }
        else {
            NA_character_
        }, .groups = "drop")
    }
    else {
        df$PC1_plot <- df[[pc1_col]]
        df$PC2_plot <- df[[pc2_col]]
        df$Cluster_plot <- if (cluster_col %in% names(df)) {
            as.character(df[[cluster_col]])
        }
        else {
            NA_character_
        }
    }
    df$Treatment <- as.character(df[[treatment_col]])
    df$Batch <- as.character(df[[batch_col]])
    df$Time <- as.numeric(df[[time_col]])
    format_time <- function(x) {
        day <- floor(x/24)
        hour_decimal <- x%%24
        hour <- floor(hour_decimal)
        minute <- round((hour_decimal - hour) * 60)
        sprintf("Day %d %02d:%02d", day, hour, minute)
    }
    df$Timepoint <- vapply(df$Time, format_time, character(1))
    df$TreatmentLabel <- df$Treatment
    for (tr in treatments) {
        n <- fish_counts$n_fish[fish_counts$Treatment == tr]
        if (length(n)) {
            df$TreatmentLabel[df$Treatment == tr] <- paste0(tr, " (fish n = ", n, ")")
        }
    }
    if (is.null(line_colours)) {
        line_colours <- c(para = "#285291", yolk = "#C99C16", PC = "#425E1A")
    }
    missing_colours <- setdiff(treatments, names(line_colours))
    if (length(missing_colours)) {
        extra <- grDevices::hcl.colors(length(missing_colours), "Dark 3")
        names(extra) <- missing_colours
        line_colours <- c(line_colours, extra)
    }
    time_values <- sort(unique(df$Time))
    time_labels <- vapply(time_values, format_time, character(1))
    time_colours <- grDevices::hcl.colors(length(time_values), "Viridis")
    names(time_colours) <- time_labels
    df$.hover <- paste0("Treatment: ", df$Treatment, "<br>Fish/batch: ", df$Batch, "<br>Time: ", df$Timepoint,
        "<br>PC1: ", round(df$PC1_plot, 3), "<br>PC2: ", round(df$PC2_plot, 3))
    if (any(!is.na(df$Cluster_plot))) {
        df$.hover <- paste0(df$.hover, "<br>Cluster: ", df$Cluster_plot)
    }
    p <- plotly::plot_ly()
    treatment_levels <- treatments[treatments %in% unique(df$Treatment)]
    for (tr in treatment_levels) {
        dat <- df[df$Treatment == tr, , drop = FALSE]
        p <- plotly::add_trace(p, x = dat$PC1_plot, y = dat$PC2_plot, z = dat$Time, type = "scatter3d",
            mode = "markers", name = unique(dat$TreatmentLabel)[1], text = dat$.hover, hoverinfo = "text",
            marker = list(size = point_size, opacity = point_opacity, color = unname(line_colours[tr])),
            visible = TRUE)
    }
    n_treatment <- length(treatment_levels)
    for (i in seq_along(time_values)) {
        dat <- df[abs(df$Time - time_values[i]) < 1e-08, , drop = FALSE]
        p <- plotly::add_trace(p, x = dat$PC1_plot, y = dat$PC2_plot, z = dat$Time, type = "scatter3d",
            mode = "markers", name = time_labels[i], text = dat$.hover, hoverinfo = "text", marker = list(size = point_size,
                opacity = point_opacity, color = time_colours[time_labels[i]]), visible = FALSE)
    }
    n_time <- length(time_values)
    cluster_levels <- unique(df$Cluster_plot[!is.na(df$Cluster_plot)])
    if (length(cluster_levels)) {
        cluster_colours <- grDevices::hcl.colors(length(cluster_levels), "Dark 3")
        names(cluster_colours) <- cluster_levels
        for (cl in cluster_levels) {
            dat <- df[df$Cluster_plot == cl, , drop = FALSE]
            p <- plotly::add_trace(p, x = dat$PC1_plot, y = dat$PC2_plot, z = dat$Time, type = "scatter3d",
                mode = "markers", name = cl, text = dat$.hover, hoverinfo = "text", marker = list(size = point_size,
                  opacity = point_opacity, color = cluster_colours[cl]), visible = FALSE)
        }
    }
    n_cluster <- length(cluster_levels)
    total <- n_treatment + n_time + n_cluster
    treatment_visible <- c(rep(TRUE, n_treatment), rep(FALSE, n_time + n_cluster))
    time_visible <- c(rep(FALSE, n_treatment), rep(TRUE, n_time), rep(FALSE, n_cluster))
    buttons <- list(list(method = "update", label = "Treatment", args = list(list(visible = treatment_visible))),
        list(method = "update", label = "Timepoint", args = list(list(visible = time_visible))))
    if (n_cluster > 0) {
        cluster_visible <- c(rep(FALSE, n_treatment + n_time), rep(TRUE, n_cluster))
        buttons[[3]] <- list(method = "update", label = "Clusters", args = list(list(visible = cluster_visible)))
    }
    p <- plotly::layout(p, scene = list(xaxis = list(title = pc1_col), yaxis = list(title = pc2_col),
        zaxis = list(title = "Elapsed time (h)", tickvals = time_values, ticktext = time_labels), aspectmode = "cube"),
        legend = list(title = list(text = "")), updatemenus = list(list(type = "dropdown", x = 0.02,
            y = 1.08, buttons = buttons)))
    list(plot = p, plotting_data = df, fish_counts = fish_counts)
}


