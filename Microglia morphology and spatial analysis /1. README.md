# Rebuild binary cell or whole-fish TIFF images from Z, Y, X coordinates.
# Load Microglia_common.R first. See README.md for examples.

.microglia_output_directory <- function(path) {
  if (!is.character(path) || length(path) != 1L || is.na(path) || !nzchar(trimws(path))) {
    stop("image_output_dir must be one nonempty directory path.")
  }
  path <- path.expand(path)
  if (file.exists(path) && !dir.exists(path)) stop("Output path is a file, not a directory.")
  if (!dir.exists(path) && !dir.create(path, recursive = TRUE)) stop("Cannot create output directory.")
  if (!length(list.files(path, all.files = TRUE, no.. = TRUE))) return(path)
  # Keep images, settings and audit tables together; never overwrite a previous run.
  stem <- paste0("run_", format(Sys.time(), "%Y%m%d_%H%M%S"))
  for (i in seq_len(10000L)) {
    name <- if (i == 1L) stem else sprintf("%s_%03d", stem, i - 1L)
    candidate <- file.path(path, name)
    if (file.exists(candidate)) next
    if (dir.create(candidate, showWarnings = FALSE)) return(candidate)
    if (!file.exists(candidate)) stop("Cannot create export run subfolder in: ", path)
  }
  stop("Cannot allocate a unique export run subfolder.")
}

make_cell_image <- function(coords, mode, padding_xy, padding_z) {
  if (is.null(coords) || length(coords) == 0L) stop("Empty coordinates.")
  if (!is.matrix(coords) || !is.numeric(coords) || ncol(coords) != 3L)
    stop("Coordinates must be a numeric N x 3 matrix ordered Z, Y, X.")
  if (any(!is.finite(coords))) stop("Coordinates contain non-finite values.")
  if (any(coords != trunc(coords))) stop("Coordinates contain fractional values.")
  xy <- coords[, c(2L, 3L), drop = FALSE]
  local <- sweep(xy, 2L, apply(xy, 2L, min), "-") + padding_xy + 1
  dims <- apply(local, 2L, max) + padding_xy
  if (mode == "3D") {
    z <- coords[, 1L] - min(coords[, 1L]) + padding_z + 1
    local <- cbind(local, z)
    dims <- c(dims, max(z) + padding_z)
  }
  if (any(dims > .Machine$integer.max) || prod(dims) > .Machine$integer.max)
    stop("Image dimensions exceed supported allocation limit.")
  img <- array(0L, dim = dims)
  img[local] <- 255L
  dim(img) <- c(dims[1L], dims[2L], 1L, if (mode == "3D") dims[3L] else 1L)
  img
}

.microglia_canvas <- function(canvas_size) {
  if (!is.numeric(canvas_size) || length(canvas_size) != 2L ||
      any(!is.finite(canvas_size)) || any(canvas_size < 1) ||
      any(canvas_size != trunc(canvas_size)) || any(canvas_size > .Machine$integer.max))
    stop("canvas_size must be c(x, y): two positive integer pixel dimensions.")
  unname(canvas_size)
}

make_fish_image <- function(cells, mode, canvas_size, padding_z) {
  canvas_size <- .microglia_canvas(canvas_size)
  # Validate each cell before combining: an empty/invalid cell must not vanish in rbind.
  for (j in seq_along(cells)) {
    coords <- cells[[j]]
    if (!is.matrix(coords) || !is.numeric(coords) || ncol(coords) != 3L || !nrow(coords) ||
        any(!is.finite(coords)) || any(coords != trunc(coords)))
      stop("Invalid coordinates in fish cell ", j, "; no partial fish image generated.")
  }
  if (!length(cells)) stop("No cells for this fish/timepoint.")
  coords <- do.call(rbind, cells)
  # Source voxel indices are zero-based; TIFF/R indices are one-based.
  x <- coords[, 3L] + 1
  y <- coords[, 2L] + 1
  if (any(x < 1 | x > canvas_size[1L] | y < 1 | y > canvas_size[2L]))
    stop("Coordinates outside canvas_size=c(", paste(canvas_size, collapse=", "),
      "). Observed zero-based X range: ", paste(range(coords[,3L]), collapse=" to "),
      "; Y range: ", paste(range(coords[,2L]), collapse=" to "),
      ". Increase canvas_size or check coordinate origin; no pixels were clipped.")
  local <- cbind(y, x)
  dims <- rev(canvas_size)
  if (mode == "3D") {
    z <- coords[,1L] - min(coords[,1L]) + padding_z + 1
    local <- cbind(local, z)
    dims <- c(dims, max(z) + padding_z)
  }
  if (any(dims > .Machine$integer.max) || prod(dims) > .Machine$integer.max)
    stop("Fish image dimensions exceed supported allocation limit.")
  img <- array(0L, dim=dims)
  img[local] <- 255L
  dim(img) <- c(dims[1L], dims[2L], 1L, if (mode=="3D") dims[3L] else 1L)
  img
}

export_microglia <- function(
    df,
    image_output_dir,
    metadata_csv,
    mode=c("2D", "3D"),
    padding_xy=5L,
    padding_z=2L,
    dry_run=FALSE,
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
    metadata_time_zero_col=NULL,
    image_level=c("cell", "fish"),
    canvas_size=c(512L, 512L)
) {
  mode <- match.arg(mode)
  image_level <- match.arg(image_level)
  if (image_level=="fish") canvas_size <- .microglia_canvas(canvas_size)
  .microglia_flag(dry_run, "dry_run")
  .microglia_integer(padding_xy, "padding_xy"); .microglia_integer(padding_z, "padding_z")
  # Original positions are assigned before filtering so audit IDs remain traceable.
  if (!is.data.frame(df)) stop("Use read_microglia() first.")
  df$input_row <- seq_len(nrow(df))
  if (!"pandas_index" %in% names(df)) df$pandas_index <- NA_character_
  selected_df <- filter_microglia(df, metadata_csv,
    batches=batches, treatments=treatments, expts=expts,
    day=day, days=days, start_time=start_time, end_time=end_time,
    times_of_day=times_of_day, include_days=include_days,
    time_bin_hours=time_bin_hours, metadata_time_zero_col=metadata_time_zero_col)
  audit <- attr(selected_df, "filter_audit")
  audit$row_index <- audit$input_row
  audit$python_position <- audit$source_row - 1L
  match_fish <- regexpr("fish[0-9]+", audit$expt)
  audit$fish_label_missing <- match_fish < 0L
  audit$fish <- "fishUnknown"
  found <- which(!audit$fish_label_missing)
  audit$fish[found] <- regmatches(audit$expt, match_fish)
  selected <- which(audit$selected)
  if (any(audit$fish_label_missing[selected])) message("Some selected rows have no fish ID; using fishUnknown.")
  audit$microglia_id <- sprintf("M%04d", audit$cell_number)
  audit$image_level <- image_level
  audit$image_mode <- mode
  audit$status <- ifelse(audit$selected, "pending", "excluded")
  audit$error <- ""
  audit$filename <- ""
  safe <- function(x) gsub("[^A-Za-z0-9+._-]", "-", x)
  jobs <- if (image_level=="cell") as.list(selected) else
    split(selected, .microglia_groups(audit)[selected], drop=TRUE)
  job_names <- character(length(jobs))
  for (k in seq_along(jobs)) {
    ids <- jobs[[k]]; i <- ids[1L]
    if (length(unique(audit$condition[ids])) != 1L)
      stop("Conflicting treatments within fish/timepoint: ", audit$expt[i])
    job_names[k] <- if (image_level=="cell")
      sprintf("%s_t%.2fh_%s_%s_%s.tif", audit$batchdate[i], audit$time_hr[i],
        safe(audit$condition[i]), audit$fish[i], audit$microglia_id[i]) else
      sprintf("%s_t%.2fh_%s_%s_fish_S%06d.tif", audit$batchdate[i], audit$time_hr[i],
        safe(audit$condition[i]), audit$fish[i], min(audit$row_index[ids]))
    audit$filename[ids] <- job_names[k]
  }
  if (anyDuplicated(job_names))
    stop("Filename collision between selected experiments/times; no TIFFs exported.")
  file_audit <- data.frame(filename=job_names, image_level=image_level,
    n_cells=lengths(jobs), source_rows=vapply(jobs, function(ids)
      paste(audit$row_index[ids], collapse=";"), character(1)),
    status="pending", error="", stringsAsFactors=FALSE)
  output_dir <- NULL
  if (!dry_run) {
    if (!requireNamespace("ijtiff", quietly=TRUE)) stop("Install ijtiff first.")
    output_dir <- .microglia_output_directory(image_output_dir)
    message("Output directory: ", output_dir)
  }
  save_audit <- function() {
    if (!dry_run) {
      utils::write.csv(audit, file.path(output_dir, "microglia_export_audit.csv"), row.names=FALSE)
      utils::write.csv(file_audit, file.path(output_dir, "exported_files.csv"), row.names=FALSE)
      utils::write.csv(audit[audit$status=="written", , drop=FALSE],
        file.path(output_dir, "microglia_lookup.csv"), row.names=FALSE)
    }
  }
  on.exit(save_audit(), add=TRUE)
  if (!dry_run) dput(list(metadata_csv=basename(metadata_csv), batches=batches,
    treatments=treatments, expts=expts, day=day, days=days, start_time=start_time,
    end_time=end_time, times_of_day=times_of_day, include_days=include_days,
    time_bin_hours=time_bin_hours, metadata_time_zero_col=metadata_time_zero_col,
    mode=mode, image_level=image_level,
    canvas_size=if (image_level=="fish") canvas_size else NULL,
    xy_coordinate_origin=if (image_level=="fish") 0L else NULL,
    padding_xy=padding_xy, padding_z=padding_z),
    file=file.path(output_dir, "export_settings.R"))
  for (k in seq_along(jobs)) {
    ids <- jobs[[k]]
    tryCatch({
      img <- if (image_level=="cell")
        make_cell_image(df$coords[[audit$row_index[ids[1L]]]], mode, padding_xy, padding_z) else
        make_fish_image(df$coords[audit$row_index[ids]], mode, canvas_size, padding_z)
      if (!dry_run) {
        target <- file.path(output_dir, job_names[k])
        ijtiff::write_tif(img, target, bits_per_sample=8,
          compression="Zip", overwrite=FALSE, msg=FALSE)
        if (!file.exists(target) || file.info(target)$size<=0) stop("TIFF not created.")
      }
      audit$status[ids] <- if (dry_run) "validated" else "written"
      file_audit$status[k] <- if (dry_run) "validated" else "written"
    }, error=function(e) {
      audit$status[ids] <<- "failed"
      audit$error[ids] <<- conditionMessage(e)
      file_audit$status[k] <<- "failed"
      file_audit$error[k] <<- conditionMessage(e)
    })
    if (k %% 1000L == 0L || image_level=="fish") {
      message("Processed images: ", k, "/", length(jobs))
      save_audit()
    }
  }
  message("Images ", if (dry_run) "validated: " else "written: ",
    sum(file_audit$status %in% c("validated", "written")),
    "; failed images: ", sum(file_audit$status=="failed"))
  message("Input rows: ", nrow(audit), "; selected: ", length(selected),
    "; ", if (dry_run) "validated: " else "written: ",
    sum(audit$status %in% c("validated", "written")), "; failed: ", sum(audit$status=="failed"))
  if (any(audit$status=="failed")) warning("Some images failed; inspect exported_files.csv and the audit error column.")
  attr(audit, "files") <- file_audit
  attr(audit, "output_dir") <- output_dir
  invisible(audit) # Includes excluded rows and their reasons.
}

regenerate_microglia <- function(
    pickle_path,
    metadata_csv,
    image_output_dir,
    mode="2D",
    padding_xy=5L,
    padding_z=2L,
    dry_run=FALSE,
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
    metadata_time_zero_col=NULL,
    image_level="cell",
    canvas_size=c(512L, 512L)
) {
  df <- read_microglia(pickle_path)
  export_microglia(df, image_output_dir, metadata_csv,
    mode=mode, padding_xy=padding_xy, padding_z=padding_z, dry_run=dry_run,
    batches=batches, treatments=treatments, expts=expts, day=day, days=days,
    start_time=start_time, end_time=end_time, times_of_day=times_of_day,
    include_days=include_days, time_bin_hours=time_bin_hours,
    metadata_time_zero_col=metadata_time_zero_col,
    image_level=image_level, canvas_size=canvas_size)
}

