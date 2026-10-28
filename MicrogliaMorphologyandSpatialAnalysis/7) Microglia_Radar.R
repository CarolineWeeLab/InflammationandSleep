# Load Microglia_common.R first. Examples and input requirements are in README.md.
library(dplyr)
library(tidyr)
library(ggplot2)

plot_morphology_circle <- function(
    data,
    measures,
    treatment_col = "treatment",
    treatments = NULL,
    treatment_colors = NULL,
    measure_groups = NULL,
    measure_order = NULL,
    summarise_level = c("fish", "cell"),
    fish_id_col = "batch",
    scale_reference = c("data", "data_max", "treatment_summary"),
    show_fill = TRUE,
    show_points = TRUE,
    fill_alpha = NULL,
    line_size = 1,
    point_size = 2.5,
    font_family = "sans",
    base_font_size = 12,
    measure_label_size = 3.5,
    group_label_size = 4,
    circle_radius = 1,
    measure_label_offset = 0.14,
    group_label_offset = 0.28,
    plot_limit = 1.62,
    show_group_labels = TRUE,
    show_grid_labels = TRUE,
    line_colour = NULL,
    fill_colour = NULL,
    show_percentage_labels = show_grid_labels
) {
    if (!is.logical(show_percentage_labels) || length(show_percentage_labels) != 1L || is.na(show_percentage_labels)) {
        stop("show_percentage_labels must be TRUE or FALSE.")
    }
    if (length(measures) < 3L || anyDuplicated(measures)) stop("Choose at least three distinct radar measures.")
    if (!all(measures %in% names(data)) || !all(vapply(data[measures], is.numeric, logical(1))))
        stop("Radar measures must be existing numeric columns.")
    if (any(!is.finite(as.matrix(data[measures]))))
        stop("Radar measurements contain missing/infinite values; inspect the measurement audit.")
    summarise_level <- match.arg(summarise_level)
    scale_reference <- match.arg(scale_reference)
    finite_mean <- function(x) {
        x <- x[is.finite(x)]
        if (length(x) == 0) {
            return(NA_real_)
        }
        mean(x)
    }
    if (!all(measures %in% names(data))) {
        stop("These measures were not found in data: ", paste(measures[!measures %in% names(data)], collapse = ", "))
    }
    if (!treatment_col %in% names(data)) {
        stop("treatment_col was not found in data.")
    }
    if (summarise_level == "fish" && !fish_id_col %in% names(data)) {
        stop("fish_id_col was not found in data.")
    }
    work <- data %>% dplyr::mutate(Treatment = as.character(.data[[treatment_col]]))
    if (is.null(treatments)) {
        treatments <- unique(work$Treatment)
    }
    work <- work %>% dplyr::filter(Treatment %in% treatments) %>% dplyr::mutate(Treatment = factor(Treatment,
        levels = treatments))
    if (nrow(work) == 0) {
        stop("No rows remain after filtering treatments.")
    }
    if (!is.null(measure_groups)) {
        if (is.null(names(measure_groups)) || anyNA(names(measure_groups)) || any(!nzchar(names(measure_groups))) ||
            anyDuplicated(names(measure_groups))) {
            stop("measure_groups must have unique, non-empty measure names.")
        }
        measure_groups <- stats::setNames(as.character(measure_groups), names(measure_groups))
        measure_groups[is.na(measure_groups) | !nzchar(measure_groups)] <- "Other"
    }
    if (is.null(measure_order)) {
        if (is.null(measure_groups)) {
            measure_order <- measures
        }
        else {
            group_levels <- unique(unname(measure_groups))
            grouped_order <- unlist(lapply(group_levels, function(g) {
                names(measure_groups)[measure_groups == g & names(measure_groups) %in% measures]
            }), use.names = FALSE)
            measure_order <- c(grouped_order, setdiff(measures, grouped_order))
        }
    }
    else {
        missing_order <- setdiff(measures, measure_order)
        if (length(missing_order) > 0) {
            stop("These measures are missing from measure_order: ", paste(missing_order, collapse = ", "))
        }
        measure_order <- measure_order[measure_order %in% measures]
    }
    if (anyDuplicated(measure_order))
        stop("measure_order contains duplicate measures.")
    n_measures <- length(measure_order)
    angles <- seq(pi/2, pi/2 - 2 * pi + 2 * pi/n_measures, length.out = n_measures)
    measure_meta <- data.frame(Measure = measure_order, Position = seq_len(n_measures), Angle = angles,
        stringsAsFactors = FALSE)
    if (!is.null(measure_groups)) {
        measure_meta$Group <- measure_groups[measure_meta$Measure]
        measure_meta$Group[is.na(measure_meta$Group)] <- "Other"
    }
    else {
        measure_meta$Group <- "Measures"
    }
    # Average cells within each fish, then weight fish equally within a treatment.
    if (summarise_level == "fish") {
        fish_summary <- work %>% dplyr::group_by(Treatment, Fish = .data[[fish_id_col]]) %>% dplyr::summarise(dplyr::across(dplyr::all_of(measures),
            ~finite_mean(.x)), .groups = "drop")
        treatment_summary <- fish_summary %>% dplyr::group_by(Treatment) %>% dplyr::summarise(dplyr::across(dplyr::all_of(measures),
            ~finite_mean(.x)), .groups = "drop")
    }
    else {
        fish_summary <- NULL
        treatment_summary <- work %>% dplyr::group_by(Treatment) %>% dplyr::summarise(dplyr::across(dplyr::all_of(measures),
            ~finite_mean(.x)), .groups = "drop")
    }
    treatment_long <- treatment_summary %>% tidyr::pivot_longer(cols = dplyr::all_of(measures), names_to = "Measure",
        values_to = "Value")
    # Scaling is per measure; percentages describe the chosen reference range.
    if (scale_reference == "data" || scale_reference == "data_max") {
        range_df <- work %>% tidyr::pivot_longer(cols = dplyr::all_of(measures), names_to = "Measure",
            values_to = "RawValue") %>% dplyr::group_by(Measure) %>% dplyr::summarise(Min = {
            x <- RawValue[is.finite(RawValue)]
            if (length(x) == 0)
                0
            else min(x)
        }, Max = {
            x <- RawValue[is.finite(RawValue)]
            if (length(x) == 0)
                0
            else max(x)
        }, .groups = "drop")
    }
    else {
        range_df <- treatment_long %>% dplyr::group_by(Measure) %>% dplyr::summarise(Min = {
            x <- Value[is.finite(Value)]
            if (length(x) == 0)
                0
            else min(x)
        }, Max = {
            x <- Value[is.finite(Value)]
            if (length(x) == 0)
                0
            else max(x)
        }, .groups = "drop")
    }
    plot_data <- treatment_long %>% dplyr::mutate(ValueWasMissing = !is.finite(Value), PlotValue = dplyr::if_else(ValueWasMissing,
        0, Value)) %>% dplyr::left_join(range_df, by = "Measure") %>% dplyr::left_join(measure_meta,
        by = "Measure") %>% dplyr::mutate(DataScaledValue = dplyr::case_when(ValueWasMissing ~ 0, Max ==
        Min ~ 0.5, TRUE ~ (PlotValue - Min)/(Max - Min)), DataScaledValue = pmax(0, pmin(1, DataScaledValue)))
    if (scale_reference == "data_max") {
        global_max <- max(plot_data$DataScaledValue, na.rm = TRUE)
        if (!is.finite(global_max) || global_max <= 0) {
            global_max <- 1
        }
        plot_data <- plot_data %>% dplyr::mutate(ScaledValue = DataScaledValue/global_max)
    }
    else {
        global_max <- 1
        plot_data <- plot_data %>% dplyr::mutate(ScaledValue = DataScaledValue)
    }
    plot_data <- plot_data %>% dplyr::mutate(ScaledValue = pmax(0, pmin(1, ScaledValue)), PlotRadius = ScaledValue *
        circle_radius, x = PlotRadius * cos(Angle), y = PlotRadius * sin(Angle), Treatment = factor(Treatment,
        levels = treatments)) %>% dplyr::arrange(Treatment, Position)
    circle_levels <- c(0.25, 0.5, 0.75, 1)
    circle_angles <- seq(0, 2 * pi, length.out = 361)
    circle_grid <- expand.grid(Radius = circle_levels, Angle = circle_angles) %>% dplyr::mutate(PlotRadius = Radius *
        circle_radius, x = PlotRadius * cos(Angle), y = PlotRadius * sin(Angle))
    spoke_data <- measure_meta %>% dplyr::mutate(x = circle_radius * cos(Angle), y = circle_radius *
        sin(Angle))
    label_data <- measure_meta %>% dplyr::mutate(radius = circle_radius + measure_label_offset, x = radius *
        cos(Angle), y = radius * sin(Angle), hjust = dplyr::case_when(cos(Angle) > 0.15 ~ 0, cos(Angle) <
        -0.15 ~ 1, TRUE ~ 0.5))
    group_data <- measure_meta %>% dplyr::group_by(Group) %>% dplyr::summarise(x_mean = mean(cos(Angle)),
        y_mean = mean(sin(Angle)), .groups = "drop") %>% dplyr::mutate(GroupAngle = atan2(y_mean, x_mean),
        radius = circle_radius + measure_label_offset + group_label_offset, x = radius * cos(GroupAngle),
        y = radius * sin(GroupAngle))
    grid_label_levels <- c(0.25, 0.5, 0.75)
    grid_label_data <- data.frame(Radius = grid_label_levels, x = 0.04 * circle_radius, y = grid_label_levels *
        circle_radius, Label = paste0(grid_label_levels * 100, "%"))
    path_data <- plot_data %>% dplyr::group_by(Treatment) %>% dplyr::group_modify(~{
        temp <- .x %>% dplyr::arrange(Position)
        dplyr::bind_rows(temp, temp[1, , drop = FALSE])
    }) %>% dplyr::ungroup()
    segment_data <- plot_data %>% dplyr::group_by(Treatment) %>% dplyr::arrange(Position, .by_group = TRUE) %>%
        dplyr::mutate(xend = dplyr::lead(x), yend = dplyr::lead(y), xend = dplyr::if_else(dplyr::row_number() ==
            dplyr::n(), dplyr::first(x), xend), yend = dplyr::if_else(dplyr::row_number() == dplyr::n(),
            dplyr::first(y), yend)) %>% dplyr::ungroup()
    p <- ggplot2::ggplot()
    p <- p + ggplot2::geom_path(data = circle_grid, ggplot2::aes(x = x, y = y, group = Radius), colour = "grey85",
        linewidth = 0.123)
    p <- p + ggplot2::geom_segment(data = spoke_data, ggplot2::aes(x = 0, y = 0, xend = x, yend = y),
        colour = "grey88", linewidth = 0.123)
    if (show_percentage_labels) {
        p <- p + ggplot2::geom_text(data = grid_label_data, ggplot2::aes(x = x, y = y, label = Label),
            inherit.aes = FALSE, size = measure_label_size * 0.8, family = font_family, colour = "grey50",
            vjust = -0.35)
    }
    if (show_fill) {
        p <- p + ggplot2::geom_polygon(data = path_data, ggplot2::aes(x = x, y = y, group = Treatment,
            fill = Treatment), alpha = if (is.null(fill_alpha)) {
            min(0.4, 0.8/length(treatments))
        }
        else {
            fill_alpha
        }, colour = NA)
    }
    p <- p + ggplot2::geom_segment(data = segment_data, ggplot2::aes(x = x, y = y, xend = xend, yend = yend,
        colour = Treatment), linewidth = line_size, lineend = "round")
    if (show_points) {
        p <- p + ggplot2::geom_point(data = plot_data, ggplot2::aes(x = x, y = y, colour = Treatment),
            size = point_size)
    }
    p <- p + ggplot2::geom_text(data = label_data, ggplot2::aes(x = x, y = y, label = Measure, hjust = hjust),
        inherit.aes = FALSE, size = measure_label_size, family = font_family)
    if (show_group_labels && length(unique(measure_meta$Group)) > 1) {
        p <- p + ggplot2::geom_text(data = group_data, ggplot2::aes(x = x, y = y, label = Group), inherit.aes = FALSE,
            size = group_label_size, family = font_family, fontface = "bold")
    }
    if (is.null(line_colour))
        line_colour <- treatment_colors
    if (is.null(fill_colour))
        fill_colour <- treatment_colors
    if (is.null(line_colour))
        line_colour <- fill_colour
    if (is.null(fill_colour))
        fill_colour <- line_colour
    validate_palette <- function(palette, argument) {
        if (is.null(palette))
            return(NULL)
        if (length(palette) == 1L && is.null(names(palette))) {
            palette <- stats::setNames(rep(palette, length(treatments)), treatments)
        }
        if (is.null(names(palette)) || anyDuplicated(names(palette))) {
            stop(argument, " must be one colour or a uniquely named vector keyed by treatment.")
        }
        missing <- setdiff(treatments, names(palette))
        if (length(missing))
            stop(argument, " is missing treatments: ", paste(missing, collapse = ", "))
        palette[treatments]
    }
    line_colour <- validate_palette(line_colour, "line_colour")
    if (show_fill)
        fill_colour <- validate_palette(fill_colour, "fill_colour")
    if (!is.null(line_colour)) {
        p <- p + ggplot2::scale_colour_manual(values = line_colour, drop = FALSE)
    }
    if (show_fill && !is.null(fill_colour)) {
        p <- p + ggplot2::scale_fill_manual(values = fill_colour, drop = FALSE)
    }
    p <- p + ggplot2::coord_equal(xlim = c(-plot_limit, plot_limit), ylim = c(-plot_limit, plot_limit),
        clip = "off") + ggplot2::labs(colour = "Treatment", fill = "Treatment") + ggplot2::theme_void(base_size = base_font_size,
        base_family = font_family) + ggplot2::theme(text = ggplot2::element_text(family = font_family,
        size = base_font_size), legend.text = ggplot2::element_text(family = font_family, size = base_font_size),
        legend.title = ggplot2::element_text(family = font_family, size = base_font_size), legend.position = "right",
        plot.margin = ggplot2::margin(30, 30, 30, 30))
    p <- p + ggplot2::theme(plot.background = ggplot2::element_rect(fill = "white", colour = NA))
    return(list(plot = p, treatment_summary = treatment_summary, fish_summary = fish_summary, plot_data = plot_data,
        path_data = path_data, segment_data = segment_data, scale_ranges = range_df, global_max = global_max,
        measure_meta = measure_meta))
}

