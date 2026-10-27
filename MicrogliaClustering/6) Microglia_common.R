# Shared input checks and cell selection. Source this file before the analysis scripts.

.read_microglia_file <- function(pickle_path) {
  if (!requireNamespace("reticulate", quietly = TRUE)) stop("Install reticulate first.")
  reticulate::py_require(c("pandas", "pyarrow", "numpy"))
  if (!file.exists(path.expand(pickle_path))) stop("Pickle file not found: ", pickle_path)
  pd <- reticulate::import("pandas", convert = FALSE)
  p <- pd$read_pickle(path.expand(pickle_path))
  required <- c("coords", "expt", "condition", "time_hr")
  if (!all(required %in% reticulate::py_to_r(p$columns$tolist()))) {
    stop("Pickle is missing required columns: coords, expt, condition, time_hr.")
  }
  column <- function(name) reticulate::py_to_r(reticulate::py_get_item(p, name)$tolist())
  coords <- column("coords")
  expt <- column("expt")
  condition <- column("condition")
  time_hr <- column("time_hr")
  if (!is.list(coords) || !is.character(expt) || !is.character(condition) ||
      !is.numeric(time_hr) || any(c(length(expt), length(condition), length(time_hr)) != length(coords))) {
    stop("Column conversion failed; metadata and coordinates must be native R objects.")
  }
  df <- data.frame(expt = expt, condition = condition, time_hr = time_hr,
                   stringsAsFactors = FALSE)
  df$coords <- coords
  df$pandas_index <- reticulate::py_to_r(
    p$index$map(reticulate::import_builtins(FALSE)$str)$tolist())
  df$source_file <- basename(pickle_path)
  df$source_row <- seq_len(nrow(df))
  .microglia_validate_df(df)
  df
}

.microglia_validate_df <- function(df) {
  if (!is.data.frame(df) || !nrow(df) ||
      !all(c("coords", "expt", "condition", "time_hr") %in% names(df)))
    stop("Expected a nonempty dataframe with coords, expt, condition and time_hr.")
  if (!is.list(df$coords) || !is.character(df$expt) ||
      !is.character(df$condition) || !is.numeric(df$time_hr) ||
      anyNA(df$expt) || any(!nzchar(trimws(df$expt))) || any(!is.finite(df$time_hr)))
    stop("Invalid dataframe column types or experiment/time values.")
}

.microglia_groups <- function(df) {
  interaction(match(df$expt, unique(df$expt)),
              match(df$time_hr, unique(df$time_hr)), drop = TRUE)
}

.microglia_flag <- function(x, name) {
  if (!is.logical(x) || length(x) != 1L || is.na(x)) stop(name, " must be TRUE or FALSE.")
}

.microglia_integer <- function(x, name, minimum = 0) {
  if (!is.numeric(x) || length(x) != 1L || !is.finite(x) ||
      x < minimum || x != trunc(x)) stop(name, " must be an integer >= ", minimum, ".")
}

.microglia_clock <- function(x) {
  if (!is.character(x) || !length(x) || anyNA(x)) stop("Clock times must be HH:MM or HH:MM:SS strings.")
  vapply(x, function(s) {
    parts <- strsplit(trimws(s), ":", fixed = TRUE)[[1]]
    if (!(length(parts) %in% c(2L, 3L)) || any(!grepl("^[0-9]{1,2}$", parts))) {
      stop("Invalid clock time: ", s)
    }
    v <- as.numeric(parts)
    if (v[1] > 23 || v[2] > 59 || (length(v) == 3L && v[3] > 59)) stop("Invalid clock time: ", s)
    v[1] + v[2] / 60 + if (length(v) == 3L) v[3] / 3600 else 0
  }, numeric(1), USE.NAMES = FALSE)
}

.microglia_metadata_hour <- function(x) {
  vapply(x, function(s) {
    # Handles '0 days 20:00:00', 'Day 0 20:00', and R-sanitised equivalents.
    s <- trimws(s)
    s <- sub("^X(?=[0-9])", "", s, perl = TRUE)
    s <- gsub("[._]", " ", s)
    s <- gsub(":", " ", s, fixed = TRUE)
    m <- regexec("^(?:[Dd]ay[s]? +)?([0-9]+) +(?:[Dd]ays? +)?([0-9]{1,2}) +([0-9]{2})(?: +([0-9]{2}))?$", s, perl = TRUE)
    p <- regmatches(s, m)[[1]]
    if (!length(p)) return(NA_real_)
    secs <- if (length(p) >= 5L && nzchar(p[5])) as.numeric(p[5]) else 0
    v <- as.numeric(p[2:4])
    if (v[2] > 23 || v[3] > 59 || secs > 59) return(NA_real_)
    v[1] * 24 + v[2] + v[3] / 60 + secs / 3600
  }, numeric(1), USE.NAMES = FALSE)
}

.microglia_read_metadata <- function(metadata_csv, metadata_time_zero_col = NULL) {
  if (is.data.frame(metadata_csv)) {
    meta <- metadata_csv
  } else {
    tables <- lapply(metadata_csv, function(path) {
      if (!file.exists(path.expand(path))) stop("Metadata file not found: ", path)
      utils::read.csv(path.expand(path), check.names = FALSE,
                     stringsAsFactors = FALSE, colClasses = "character")
    })
    columns <- unique(unlist(lapply(tables, names)))
    tables <- lapply(tables, function(x) {
      for (name in setdiff(columns, names(x))) x[[name]] <- NA_character_
      x[, columns, drop = FALSE]
    })
    meta <- do.call(rbind, tables)
  }
  if (!"expt" %in% names(meta) || anyNA(meta$expt) || any(!nzchar(trimws(meta$expt)))) {
    stop("Metadata must contain nonempty expt values.")
  }
  meta$expt <- trimws(meta$expt)
  if (anyDuplicated(meta$expt)) stop("Metadata must have one row per full expt; duplicate expt values found.")
  # Optional explicit numeric elapsed-hour origin, e.g. start_elapsed_hour = 20.
  if (!is.null(metadata_time_zero_col)) {
    if (!metadata_time_zero_col %in% names(meta)) stop("metadata_time_zero_col was not found.")
    origin <- suppressWarnings(as.numeric(meta[[metadata_time_zero_col]]))
  } else {
    hours <- .microglia_metadata_hour(names(meta))
    ids <- which(is.finite(hours))
    if (!length(ids)) stop("No recognised day/time headers in metadata. Supply metadata_time_zero_col if appropriate.")
    origin <- vapply(seq_len(nrow(meta)), function(i) {
      values <- unlist(meta[i, ids, drop = FALSE], use.names = FALSE)
      present <- !is.na(values) & nzchar(trimws(values))
      # Same presence convention as the spatial pipeline: any nonblank value,
      # including 0, is present. Blank/NA is absent.
      if (!any(present)) NA_real_ else min(hours[ids[present]])
    }, numeric(1))
  }
  meta$metadata_start_hour <- origin
  meta
}

.microglia_filter <- function(
    df,
    batches = NULL,
    treatments = NULL,
    expts = NULL,
    time_hr = NULL,
    metadata_csv = NULL,
    day = 0L,
    days = 1L,
    start_time = "20:00",
    end_time = "02:00",
    time_bin_hours = 0.5,
    times_of_day = NULL,
    include_days = NULL,
    metadata_time_zero_col = NULL
) {
  .microglia_validate_df(df)
  if (!is.null(metadata_csv) && !is.null(time_hr))
    stop("Choose metadata clock filtering OR raw time_hr filtering, not both.")
  if (is.null(metadata_csv) &&
      (!missing(day) || !missing(days) || !missing(start_time) || !missing(end_time) ||
       !is.null(times_of_day) || !is.null(include_days)))
    stop("Clock/day filters require metadata_csv. For raw times use time_hr.")
  data <- df
  if (!"source_row" %in% names(data)) data$source_row <- seq_len(nrow(data))
  data$expt <- trimws(data$expt)
  data$condition_pkl <- data$condition
  data$batchdate <- sub("^([0-9]{8}).*$", "\\1", data$expt)
  if (any(!grepl("^[0-9]{8}$", data$batchdate))) stop("Cannot extract batch date from expt.")
  reason <- rep("", nrow(data))
  exclude <- function(test, why) {
    test[is.na(test)] <- TRUE
    reason[reason == "" & test] <<- why
  }
  if (!is.null(batches)) exclude(!data$batchdate %in% as.character(batches), "batch_not_selected")
  if (!is.null(expts)) exclude(!data$expt %in% trimws(expts), "experiment_not_selected")
  if (!is.null(metadata_csv)) {
    meta <- .microglia_read_metadata(metadata_csv, metadata_time_zero_col)
    ix <- match(data$expt, meta$expt)
    data$metadata_start_hour <- meta$metadata_start_hour[ix]
    exclude(is.na(ix), "no_metadata_match")
    exclude(!is.finite(data$metadata_start_hour), "missing_metadata_time_origin")
    treatment_col <- if ("condition" %in% names(meta)) "condition" else
      if ("treatment" %in% names(meta)) "treatment" else NULL
    if (!is.null(treatment_col)) {
      values <- trimws(meta[[treatment_col]][ix])
      use <- !is.na(values) & nzchar(values)
      data$condition[use] <- values[use]
    }
    .microglia_integer(day, "day"); .microglia_integer(days, "days")
    if (length(start_time) != 1L || length(end_time) != 1L) stop("Use one start and end time.")
    start <- day * 24 + .microglia_clock(start_time)
    end <- (day + days) * 24 + .microglia_clock(end_time)
    if (end < start) stop("End precedes start; use days=1 for an overnight window.")
    if (!is.numeric(time_bin_hours) || length(time_bin_hours) != 1L ||
        !is.finite(time_bin_hours) || time_bin_hours <= 0) stop("Invalid time_bin_hours.")
    data$actual_elapsed_hour <- data$metadata_start_hour + data$time_hr
    data$nominal_elapsed_hour <- round(data$actual_elapsed_hour / time_bin_hours) * time_bin_hours
    data$day <- floor(data$nominal_elapsed_hour / 24)
    clock <- data$nominal_elapsed_hour %% 24
    exclude(data$nominal_elapsed_hour < start - 1e-8 |
            data$nominal_elapsed_hour > end + 1e-8, "outside_time_window")
    if (!is.null(include_days)) {
      if (!is.numeric(include_days) || any(!is.finite(include_days)) ||
          any(include_days < 0 | include_days != trunc(include_days))) stop("Invalid include_days.")
      exclude(!data$day %in% include_days, "day_not_selected")
    }
    if (!is.null(times_of_day)) {
      clocks <- .microglia_clock(times_of_day)
      if (any(abs(clocks / time_bin_hours - round(clocks / time_bin_hours)) > 1e-7))
        stop("times_of_day must lie on the time_bin_hours grid.")
      keep_clock <- vapply(clock, function(v) is.finite(v) && any(abs(v - clocks) < 1e-7), logical(1))
      exclude(!keep_clock, "clock_time_not_selected")
    }
  } else if (!is.null(time_hr)) {
    if (!is.numeric(time_hr) || any(!is.finite(time_hr))) stop("Invalid raw time_hr values.")
    exclude(!data$time_hr %in% time_hr, "raw_time_not_selected")
  }
  data$condition <- trimws(data$condition)
  exclude(is.na(data$condition) | !nzchar(data$condition), "missing_treatment")
  if (!is.null(treatments)) exclude(!tolower(data$condition) %in% tolower(trimws(treatments)), "treatment_not_selected")
  data$cell_number <- integer(nrow(data))
  for (ids in split(seq_len(nrow(data)), .microglia_groups(data))) data$cell_number[ids] <- seq_along(ids)
  audit <- data[, names(data) != "coords", drop = FALSE]
  audit$selected <- reason == ""; audit$reason <- reason
  selected <- data[reason == "", , drop = FALSE]
  if (!nrow(selected)) stop("No cells match. Check batch, treatment, experiment and time selection.")
  attr(selected, "filter_audit") <- audit
  if (any(reason %in% c("no_metadata_match", "missing_metadata_time_origin")))
    warning("Some candidate rows lack metadata/time origins; inspect attr(selected_df, 'filter_audit').")
  message("Selected ", nrow(selected), " cells in ", nlevels(.microglia_groups(selected)), " exact acquisitions.")
  selected
}


# Optional standalone filtering step for inspecting selected rows before export.
filter_microglia <- function(
    df,
    metadata_csv,
    batches=NULL,
    treatments=NULL,
    expts=NULL,
    day=0L,
    days=1L,
    start_time="20:00",
    end_time="02:00",
    times_of_day=NULL,
    include_days=NULL,
    time_bin_hours=0.5,
    metadata_time_zero_col=NULL
) {
  if (is.null(metadata_csv)) stop("Supply metadata_csv for clock/day filtering.")
  .microglia_filter(df, batches=batches, treatments=treatments, expts=expts,
    metadata_csv=metadata_csv, day=day, days=days,
    start_time=start_time, end_time=end_time, times_of_day=times_of_day,
    include_days=include_days, time_bin_hours=time_bin_hours,
    metadata_time_zero_col=metadata_time_zero_col)
}


# Read each column separately so coordinate arrays remain native R matrices.
read_microglia <- function(pickle_path) {
  if (!is.character(pickle_path) || !length(pickle_path) || anyNA(pickle_path))
    stop("Supply one or more pickle paths.")
  paths <- normalizePath(path.expand(pickle_path), mustWork = TRUE)
  if (anyDuplicated(paths)) stop("The same pickle was supplied more than once.")
  source_labels <- make.unique(basename(paths), sep = "_")
  tables <- Map(function(path, label) {
    data <- .read_microglia_file(path)
    data$source_file <- label
    data
  }, paths, source_labels)
  data <- do.call(rbind, tables)
  rownames(data) <- NULL
  acquisition <- unique(data[c("source_file", "expt", "time_hr")])
  if (anyDuplicated(acquisition[c("expt", "time_hr")]))
    stop("Different pickles contain the same experiment and acquisition time. Resolve overlap first.")
  data
}

# Cell filenames encode batch, acquisition time, treatment, fish and cell number.
.microglia_cell_filenames <- function(data) {
  fish_match <- regexpr("fish[0-9]+", data$expt)
  fish <- rep("fishUnknown", nrow(data))
  found <- which(fish_match > 0L)
  fish[found] <- regmatches(data$expt, fish_match)
  safe <- function(x) gsub("[^A-Za-z0-9+._-]", "-", x)
  filename <- sprintf("%s_t%.2fh_%s_%s_M%04d.tif", data$batchdate,
                      data$time_hr, safe(data$condition), fish, data$cell_number)
  if (anyDuplicated(filename))
    stop("Cell filenames collide. Check experiment IDs and exact acquisition times.")
  filename
}

# Use the same selection for TIFF export, radar, PCA and spatial analysis.
# Completeness is optional and checks the requested schedule, including absent times.
select_microglia <- function(
    df,
    metadata_csv,
    batches = NULL,
    treatments = NULL,
    expts = NULL,
    day = 0L,
    days = 1L,
    start_time = "20:00",
    end_time = "02:00",
    time_bin_hours = 0.5,
    times_of_day = NULL,
    include_days = NULL,
    metadata_time_zero_col = NULL,
    complete_cohort = FALSE,
    required_timepoints = NULL
) {
  .microglia_flag(complete_cohort, "complete_cohort")
  selected <- filter_microglia(df, metadata_csv, batches, treatments, expts,
    day, days, start_time, end_time, times_of_day, include_days, time_bin_hours,
    metadata_time_zero_col)
  audit <- attr(selected, "filter_audit")
  start <- day * 24 + .microglia_clock(start_time)
  end <- (day + days) * 24 + .microglia_clock(end_time)
  if (is.null(required_timepoints)) {
    required_timepoints <- seq(ceiling((start - 1e-8) / time_bin_hours),
                              floor((end + 1e-8) / time_bin_hours)) * time_bin_hours
    if (!is.null(times_of_day)) required_timepoints <- required_timepoints[
      vapply(required_timepoints %% 24, function(t)
        any(abs(t - .microglia_clock(times_of_day)) < 1e-7), logical(1))]
    if (!is.null(include_days)) required_timepoints <- required_timepoints[
      floor(required_timepoints / 24) %in% include_days]
  }
  if (!is.numeric(required_timepoints) || !length(required_timepoints) ||
      any(!is.finite(required_timepoints)) || anyDuplicated(required_timepoints) ||
      any(required_timepoints < start - 1e-8 | required_timepoints > end + 1e-8) ||
      any(abs(required_timepoints / time_bin_hours - round(required_timepoints / time_bin_hours)) > 1e-7))
    stop("required_timepoints must be distinct hours on the selected time grid and inside the window.")
  # A supplied schedule also selects those times; extra acquisitions are not substituted.
  on_schedule <- vapply(selected$nominal_elapsed_hour, function(t)
    any(abs(t - required_timepoints) < 1e-7), logical(1))
  audit_indices <- which(audit$selected)
  audit$selected[audit_indices[!on_schedule]] <- FALSE
  audit$reason[audit_indices[!on_schedule]] <- "outside_required_schedule"
  selected <- selected[on_schedule, , drop = FALSE]
  coverage <- lapply(split(selected, selected$expt), function(x) {
    missing_times <- required_timepoints[!vapply(required_timepoints, function(t)
      any(abs(x$nominal_elapsed_hour - t) < 1e-7), logical(1))]
    if (length(unique(x$condition)) != 1L) stop("Conflicting treatments for experiment: ", x$expt[1])
    data.frame(expt = x$expt[1], condition = x$condition[1],
      observed = length(unique(x$nominal_elapsed_hour)), required = length(required_timepoints),
      complete = !length(missing_times), missing_times = paste(missing_times, collapse = ";"))
  })
  coverage <- do.call(rbind, coverage)
  if (complete_cohort && !is.null(coverage)) {
    incomplete <- coverage$expt[!coverage$complete]
    excluded <- audit$selected & audit$expt %in% incomplete
    audit$selected[excluded] <- FALSE
    audit$reason[excluded] <- "incomplete_fish"
    selected <- selected[!selected$expt %in% incomplete, , drop = FALSE]
  }
  if (!nrow(selected)) stop("No cells remain after schedule/cohort selection.")
  selected$filename <- .microglia_cell_filenames(selected)
  selected$treatment <- selected$condition
  selected$batch <- selected$expt  # Full fish identity, never the date alone.
  selected$nominal_time <- selected$nominal_elapsed_hour
  attr(selected, "filter_audit") <- audit
  attr(selected, "coverage") <- coverage
  attr(selected, "required_timepoints") <- required_timepoints
  message("Retained ", nrow(selected), " cells from ", length(unique(selected$expt)), " fish.")
  selected
}

# Read the MorphoLibJ master table and, optionally, per-cell skeleton summaries.
read_imagej_measurements <- function(morphology_csv, skeleton_dir = NULL) {
  data <- utils::read.csv(morphology_csv, check.names = FALSE, stringsAsFactors = FALSE)
  if (!"Filename" %in% names(data)) stop("MorphoLibJ table must contain Filename.")
  names(data)[names(data) == "Filename"] <- "filename"
  if (anyNA(data$filename) || anyDuplicated(data$filename)) stop("Missing or duplicate measurement filenames.")
  if (!is.null(skeleton_dir)) {
    files <- list.files(skeleton_dir, pattern = "_results\\.csv$", full.names = TRUE)
    if (!length(files)) stop("No skeleton _results.csv files found.")
    summaries <- lapply(files, function(path) {
      x <- utils::read.csv(path, check.names = FALSE, stringsAsFactors = FALSE)
      mean_row <- which(apply(x, 1, function(row) any(tolower(trimws(row)) == "mean", na.rm = TRUE)))
      if (length(mean_row) == 1L) x <- x[mean_row, , drop = FALSE]
      if (nrow(x) != 1L) stop("Expected one Mean summary row in: ", path)
      numeric_columns <- vapply(x, is.numeric, logical(1)) & !names(x) %in% c("", " ", "X", "Label")
      x <- x[, numeric_columns, drop = FALSE]
      names(x) <- make.names(names(x), unique = TRUE)
      x$filename <- sub("_results\\.csv$", "", basename(path))
      x
    })
    skeleton <- do.call(rbind, summaries)
    if (anyDuplicated(skeleton$filename)) stop("Duplicate skeleton filenames.")
    overlap <- intersect(setdiff(names(skeleton), "filename"), names(data))
    if (length(overlap)) stop("Repeated measurement columns: ", paste(overlap, collapse = ", "))
    data <- merge(data, skeleton, by = "filename", all = TRUE, sort = FALSE)
  }
  data
}

# Match by the regeneration filename, or by its saved lookup when available.
# No approximate time matching or row-order matching is used for measurements.
attach_microglia_measurements <- function(
    selected,
    measurements,
    measures,
    lookup_csv = NULL,
    missing = c("error", "drop")
) {
  missing <- match.arg(missing)
  if (is.character(measurements) && length(measurements) == 1L)
    measurements <- utils::read.csv(measurements, check.names = FALSE, stringsAsFactors = FALSE)
  if ("Filename" %in% names(measurements)) names(measurements)[names(measurements) == "Filename"] <- "filename"
  if (!is.data.frame(measurements) || !all(c("filename", measures) %in% names(measurements)))
    stop("Measurements must contain filename and every requested measure.")
  if (!length(measures) || anyDuplicated(measures) || !all(vapply(measurements[measures], is.numeric, logical(1))))
    stop("Select distinct numeric measurement columns by name.")
  if (any(measures %in% names(selected))) stop("Measurement names conflict with cell metadata.")
  if (anyNA(measurements$filename) || anyDuplicated(measurements$filename))
    stop("Measurement filenames must be present and unique.")
  filenames <- selected$filename
  if (!is.null(lookup_csv)) {
    lookup <- utils::read.csv(lookup_csv, check.names = FALSE, stringsAsFactors = FALSE)
    required <- c("expt", "time_hr", "cell_number", "filename")
    if (!all(required %in% names(lookup))) stop("Use microglia_lookup.csv from cell regeneration.")
    if ("image_level" %in% names(lookup) && any(lookup$image_level != "cell"))
      stop("Whole-fish lookup rows cannot be used for cell morphology.")
    key <- function(x) paste(x$expt, sprintf("%.17g", x$time_hr), x$cell_number, sep = "|")
    if (anyDuplicated(key(lookup)) || anyDuplicated(lookup$filename)) stop("Lookup contains duplicate cells or filenames.")
    index <- match(key(selected), key(lookup))
    if ("pandas_index" %in% names(selected) && "pandas_index" %in% names(lookup)) {
      different <- !is.na(index) & !is.na(selected$pandas_index) & !is.na(lookup$pandas_index[index]) &
        selected$pandas_index != lookup$pandas_index[index]
      if (any(different)) stop("Pickle row identities disagree with the saved export lookup.")
    }
    filenames <- lookup$filename[index]
  }
  index <- match(filenames, measurements$filename)
  values <- measurements[index, measures, drop = FALSE]
  valid <- !is.na(index) & apply(values, 1, function(x) all(is.finite(x)))
  audit <- selected[, intersect(c("source_file", "source_row", "expt", "time_hr", "cell_number"), names(selected)), drop = FALSE]
  audit$filename <- filenames
  audit$included <- valid
  audit$reason <- ifelse(is.na(index), "measurement_missing", ifelse(valid, "", "nonfinite_measurement"))
  if (any(!valid) && missing == "error")
    stop(sum(!valid), " selected cells lack complete measurements. Inspect filenames/NaNs, or use missing='drop' to obtain an audit.")
  result <- selected[valid, , drop = FALSE]
  result$filename <- filenames[valid]
  result[measures] <- values[valid, , drop = FALSE]
  if (!nrow(result)) stop("No cells have all requested measurements.")
  attr(result, "measurement_audit") <- audit
  if (any(!valid)) warning(sum(!valid), " cells excluded for missing/nonfinite measurements; inspect measurement_audit. Cohort coverage may change.")
  result
}
