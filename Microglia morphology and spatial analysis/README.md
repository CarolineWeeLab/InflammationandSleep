# Microglia morphology and spatial analysis

This folder contains the scripts for rebuilding microglia images, measuring their morphology in Fiji, and making radar, principal-component (PC), and spatial plots.

Start with the cell-coordinate pickle and its matching metadata CSV. Keep the original pickle unchanged: cell numbers follow its row order. The scripts do not perform segmentation or cell tracking.

## Files and workflow

| File | What it does |
|---|---|
| `Microglia_common.R` | Reads pickles, checks inputs, selects cells, and connects measurements to their original cells. Load this first. |
| `2D_3D_cell_regeneration.R` | Rebuilds binary TIFF images for individual cells or whole fish. |
| `MM_step4_adapted_SkeletonAnalysis.ijm` | Measures skeletons of individual cells in Fiji. |
| `MorphoLib_3D_FeatureAnalysis.ijm` | Measures 3D shape and solidity in Fiji. |
| `MM_part1_adapted.R` | Combines the Fiji CSVs, filters cells by volume, logs missing-value handling, and creates `final_data`/`pca_data` inputs. |
| `Microglia_Radar.R` | Compares morphology profiles using circular/radar plots. |
| `PCs_plotting.R` | Runs PCA and plots PC scores, distributions, and trajectories. |
| `Microglia_spatial_analysis.R` | Creates 2D spatial projections and compares their distributions. |
| `tests/validate_pipeline.R` | Checks the R pipeline with small artificial datasets. |

The morphology route is **pickle → single-cell TIFFs → Fiji measurements → matched measurement table → radar/PCA**.

The spatial route is **pickle → selected fish and times → projections → spatial plots**. It does not require the Fiji measurements.

## Before starting

In R or RStudio, set the working directory to this folder. The examples use generic filenames and uppercase directory placeholders. Replace these with your chosen inputs and output locations. Install the packages you need once:

```r
install.packages(c(
  "reticulate", "ijtiff", "dplyr", "tidyr", "ggplot2",
  "patchwork", "scales", "svglite", "plotly", "Hmisc"
))
```

`Hmisc` is used for the optional Part 1 correlation tables. `plotly` is needed for interactive PC plots; `svglite` for SVG exports. The pickle reader uses Python packages `pandas`, `numpy`, and `pyarrow` through `reticulate`. It requests these packages using `py_require()`. If you use an existing Python environment, install them there and select that environment before reading a pickle.

For Fiji, install MorphoLibJ and 3D Suite, and make sure the commands named in the macros are available. The skeleton macro uses **Skeletonize (2D/3D)** and **Analyze Skeleton (2D/3D)**. The shape macro uses **Analyze Regions 3D**, **3D Volume**, and **3D Convex Hull (slow)**.

Run one or two representative images first and inspect their results before starting a batch.

## Input files

### Cell-coordinate pickle

The pickle must contain a pandas dataframe with these columns:

| Column | Expected contents |
|---|---|
| `expt` | Full experiment/fish name, for example `20260101 fish00`. The first eight digits identify the batch date. |
| `condition` | Treatment label. |
| `time_hr` | Numeric hours since that fish's first acquisition. |
| `coords` | One coordinate matrix per cell, with columns in **Z, Y, X** order. Coordinates must be finite integers for image reconstruction. |

The reader converts each column separately. It retains the pandas index, source filename, and row number. Source labels omit local directories and are disambiguated when filenames repeat. You can supply several pickle paths, but repeated files or overlapping experiment/acquisition pairs are rejected.

### Metadata CSV

Use the metadata belonging to the same dataset. It must contain one row per full `expt`. Duplicate experiment names are rejected, including duplicates across multiple CSVs.

Time zero can be supplied in either of two ways:

- **Existing day/time headers**, such as `0 days 20:00:00` and `1 days 00:00:00`. The earliest nonblank entry in each fish's row marks its acquisition start. **A value of 0 counts as present**; blank or NA counts as absent.
- **An explicit start-hour column**, for example `start_elapsed_hour`. Set `metadata_time_zero_col = "start_elapsed_hour"`. A start at day 1, 01:00 is 25 hours.

A nonblank metadata `condition` overrides the pickle treatment. If the metadata has no `condition` column, `treatment` is used instead. Otherwise the pickle label is retained.

## 1. Rebuild cell images

```r
source("Microglia_common.R")
source("2D_3D_cell_regeneration.R")

cells <- read_microglia("cells.pkl")

export_audit <- export_microglia(
  df = cells,
  metadata_csv = "metadata.csv",
  image_output_dir = "CELL_OUTPUT_DIRECTORY",
  treatments = c("para", "yolk", "PC"),
  day = 0, days = 1,
  start_time = "20:00", end_time = "02:00",
  time_bin_hours = 0.5,
  image_level = "cell",
  mode = "3D",
  dry_run = TRUE
)
table(export_audit$status)
```

First use `dry_run = TRUE` to check the selection and build masks without writing files. Then repeat with `dry_run = FALSE` to export. Alternatively, `regenerate_microglia(pickle_path = ..., ...)` reads the pickle and exports in one call.

| Setting | Meaning |
|---|---|
| `mode = "2D"` | Collapses Z into a binary Y–X projection. |
| `mode = "3D"` | Keeps the Z slices as a TIFF stack. Use this for 3D shape measurements. |
| `image_level = "cell"` | One cropped image per cell. Default padding is 5 pixels in X/Y and 2 slices in Z. |
| `image_level = "fish"` | One combined image per full experiment and exact acquisition time. |
| `canvas_size = c(512, 512)` | Whole-fish X width and Y height. Original X/Y positions must be zero-based and fit inside this canvas. Ignored for cell images. |

Whole-fish mode ignores X/Y padding. In 3D, Z is cropped with the requested padding. Coordinates are not stretched or rounded during regeneration. Invalid cells are recorded as failures; an invalid cell prevents export of its whole-fish image rather than disappearing from it.

The output folder contains TIFFs and:

- `microglia_export_audit.csv`: all input cells, including exclusions and export failures.
- `exported_files.csv`: one row per output image and its status.
- `microglia_lookup.csv`: successfully exported cells and their filenames. **Keep this for matching Fiji results.**
- `export_settings.R`: the export settings, with metadata filenames rather than local directory paths.

If the destination is not empty, a new `run_...` subfolder is created. Use `attr(export_audit, "output_dir")` to find it. Existing files are not overwritten.

An example filename is `20260101_t0.50h_para_fish00_M0001.tif`. Here `t0.50h` is the original acquisition time relative to that fish's start, not the metadata clock time. Cell numbering restarts within each full experiment/exact acquisition and is assigned **before selection**, so selecting a treatment or time window does not renumber its cells. A collision caused by rounded filename times stops the export.

### Image calibration

The TIFF exporter writes binary voxel masks; it does not set physical voxel sizes. Before measurements, check X/Y pixel size, Z spacing, and units in Fiji. Use consistent calibration for all images. Without physical calibration, lengths and volumes are in pixel/voxel units. Do not mix 2D and 3D measurements in the same feature table.

## 2. Run the Fiji macros

Open a macro in Fiji's script editor and run it. Both macros process TIFFs in filename order. The start and stop numbers refer to the sorted TIFF list, not to cell IDs. The stop number is inclusive.

### Skeleton analysis

Run `MM_step4_adapted_SkeletonAnalysis.ijm` and select:

1. **Input folder:** the regenerated **single-cell** binary TIFFs. Do not select the whole-fish export folder.
2. **Results folder:** a separate folder for skeleton CSV files.
3. **Image output folder:** a separate folder for skeleton TIFFs.
4. **Image range:** use 1 through the displayed total for all cells, or a smaller range for a test.

The macro skeletonizes each cell and runs Analyze Skeleton with `prune=none`. It saves one `<original filename>_results.csv` per cell, containing component measurements and Fiji's summary rows. Measurements include branch and junction counts and branch lengths, depending on the plugin's results table.

A tagged skeleton image is saved when the plugin produces one. Otherwise the binary skeleton is saved with `_skeleton.tif`. Empty results are reported in the Log. Existing result/tagged-image outputs are skipped; use a fresh results folder to rerun a cell after changing settings.

The R importer uses the **Mean** summary row for a cell with multiple skeleton components. It does not sum component measurements. Review disconnected objects before interpreting these means as single-cell morphology.

### 3D shape analysis

Run `MorphoLib_3D_FeatureAnalysis.ijm` and select:

1. **Input folder:** the regenerated **3D single-cell stacks**, with 8-bit foreground 255 and background 0.
2. **Output folder:** a separate folder for the master measurement CSV.
3. **Image range:** choose the first and last TIFF to process.

The output is `MorphoLibJ_3D_SelectedFeatures.csv`. It contains the filename, volume, surface area, sphericity, three ellipsoid ratios, maximum Feret diameter, geodesic diameter, inscribed-ball radius, tortuosity, cell volume from 3D Suite, convex-hull volume, and 3D solidity.

Measurements use Crofton surface area with 13 directions and Euler connectivity 6. Solidity is cell volume divided by convex-hull volume. Missing measurements are written as `NaN`.

The macro appends to an existing master CSV and skips filenames already listed. **Rows containing NaN are also considered processed.** Inspect these rows and rerun affected cells into a fresh output folder after fixing the problem. The convex-hull step may take longer than the other measurements.

## 3. Select the cells for analysis

Radar, PCA, and spatial analysis use the same pickle reader and the same basic filters as regeneration.

```r
selected <- select_microglia(
  cells,
  metadata_csv = "metadata.csv",
  treatments = c("para", "yolk", "PC"),
  day = 0, days = 1,
  start_time = "20:00", end_time = "02:00",
  time_bin_hours = 0.5,
  complete_cohort = TRUE
)

table(selected$treatment)
attr(selected, "coverage")
selection_audit <- attr(selected, "filter_audit")
write.csv(selection_audit, "selection_audit.csv", row.names = FALSE)
```

### How filtering works

1. Select batch dates with `batches`, full fish IDs with `expts`, and treatments with `treatments`. `NULL` means no restriction. Treatment matching ignores capitalization at this stage.
2. Match each cell to metadata using the full experiment name. Missing metadata or a missing time origin excludes the cell.
3. Add `time_hr` to the metadata start hour to get the actual elapsed hour.
4. Round that hour to the `time_bin_hours` grid for selection. With `0.5`, 20.49 becomes 20.5. Original acquisition times remain available. Exact halfway values follow R's nearest-even rounding rule.
5. Keep the inclusive start–end window, then apply any requested day or clock-time restrictions.
6. If requested, keep only fish observed at **every required timepoint**.

`day` is the starting calendar day. `days` is the offset of the ending day. For example:

| Selection | Settings |
|---|---|
| Day 0, 20:00 through day 1, 02:00 | `day=0, days=1, start_time="20:00", end_time="02:00"` |
| Day 1, 01:00 only | `day=1, days=0, start_time="01:00", end_time="01:00"` |
| Day 0, 20:00 through 23:00 | `day=0, days=0, start_time="20:00", end_time="23:00"` |

`times_of_day = c("20:00", "00:00", "02:00")` selects these clock times **within** the window. `include_days` adds a calendar-day restriction. Neither extends the window.

`complete_cohort = TRUE` checks all grid times in the requested window, after these restrictions. The 20:00–02:00 window on a half-hour grid requires 13 timepoints. A time missing from every fish is still required; it is not silently removed from the schedule. For an intentionally irregular schedule, supply `required_timepoints`, such as `c(20, 22, 24, 26)`. These are elapsed hours; 24 is midnight on day 1. Supplying a schedule also restricts the selection to those times.

Completeness is optional for radar/PCA selection and **on by default for spatial analysis**. Regeneration itself exports all cells matching its basic filters; it does not require a complete longitudinal cohort. Use the same explicit selection settings for plots you intend to compare.

The audit records the first exclusion reason per cell. `coverage` lists the observed and required timepoints and missing times for fish that have observations in the requested schedule. Missing segmentation at a timepoint is treated as missing coverage, not as a confirmed zero-cell observation.

## 4. Combine the Fiji CSVs with MM Part 1

`MM_part1_adapted.R` combines MorphoLibJ shape and skeleton measurements. It reads the CSVs directly without changing the working directory. Use `merger_data()` when the two measurement tables are already loaded.

```r
source("Microglia_common.R")
source("MM_part1_adapted.R")

part1 <- run_mm_part1(
  morphology_csv = "MorphoLibJ_3D_SelectedFeatures.csv",
  skeleton_dir = "SKELETON_CSV_DIRECTORY",
  lookup_csv = "microglia_lookup.csv",
  selected_cells = selected,  # From step 3; omit to use all cells in the shape CSV.
  min_volume = 20,
  bad_value_action = "zero",
  output_dir = "ANALYSIS_OUTPUT_DIRECTORY"
)

final_data_raw <- part1$final_data
final_data <- part1$final_data_clean
pca_data <- part1$pca_data
```

Use a new or empty output folder. `lookup_csv` adds treatment, full fish identity, and acquisition/clock times using the regeneration lookup. It is required when `selected_cells` is supplied. Without a lookup, CSV merging and PCA still work, but treatment/time plots need this metadata added before they can run. Selection uses exact fish/acquisition/cell identities; it does not infer clock times from rounded filenames.

The processing steps are:

1. Match shape and skeleton rows by image name after removing TIFF/result-file suffixes. Duplicate cell names stop the merge. The shape CSV defines the cell list; missing skeleton matches and skeleton-only cells are listed in `merge_audit`.
2. Remove invalid volumes and cells below `min_volume` (default 20). The lower limit is inclusive. `max_volume` optionally sets an inclusive upper limit. The units depend on Fiji calibration; 20 is a configurable analysis threshold, not a universal biological cutoff.
3. Handle missing or infinite measurements. `bad_value_action="zero"` replaces nonfinite values with zero and records every replacement in `bad_values`. Valid zeros stay zero. Choose `"error"` to stop instead, or `"drop"` to exclude affected cells. A missing measurement and a measured zero are different observations: review this log before using the results. Both NA and Inf receive the same handling in the plotted and PCA data.
4. Apply `log(1 + value)` and then scale the morphology features. Identifiers and times are not transformed. Constant PCA features stop the run and must be excluded explicitly with `pca_measures`.
5. Fit PCA and optionally produce Spearman feature/PC correlation tables. The unadjusted thresholds are: |r| ≥ 0.8 for feature pairs and |r| ≥ 0.75 for PC/feature pairs, both with p < 0.05. These are descriptive screening tables, not fish-level hypothesis tests. Set `correlations=FALSE` to skip them.

The default `MM_PCA_FEATURES` vector selects **19 features**, excluding `# of quadruple points` and `Maximum branch length`. To intentionally use all 21 merged features, set `pca_measures=MM_FEATURES`. `n_pcs=10` retains up to ten available PC score columns. No random seed is needed for this PCA.

The main returned objects and exports are:

| Object | Contents / CSV |
|---|---|
| `part1$final_data` | Merged, selected measurements before volume filtering or replacement; `final_data_raw.csv`. |
| `part1$final_data_clean` | Filtered measurements in their original units, with the chosen missing-value handling; `final_data_clean.csv`. Use these for radar plots. |
| `part1$final_data_scaled` | Log-transformed, standardized measurements; `final_data_scaled.csv`. |
| `part1$pca_data` | PC scores followed by standardized features and available metadata; `pca_data.csv`. Use this directly for PC plots. |
| `part1$pca_model` | Full fitted PCA; also saved as `pca_model.rds`. Use it for explained-variance plots. |
| `part1$removed_rows`, `$bad_values`, `$merge_audit` | Filtering, replacement, and matching records, saved as separate CSVs. |

The long-format measurement table, correlation tables, and analysis settings are also saved. **`final_data_clean.csv` contains untransformed measurements**; standardized values are saved separately. Correlation tables use the same cleaned/replaced values as the rest of this workflow. Constant non-PCA features have undefined standardized values and are omitted from PC/feature correlations. Volume filtering and missing-value removal can change the cohort's time coverage after step 3; inspect retained fish/times before reporting a complete cohort.

You can inspect cells before choosing a cutoff:

```r
raw <- read_mm_measurements(
  "MorphoLibJ_3D_SelectedFeatures.csv", "SKELETON_CSV_DIRECTORY"
)
plot_distribution(raw, Volume)
plot_distribution_outliers(raw, Volume, inflate = 20)
get_extreme_cells(raw, Volume, n = 10)
```

The 1.5 × IQR boundaries are visual guides only; outliers are not removed automatically.

Use the resulting tables with the plotting files:

```r
source("Microglia_Radar.R")
source("PCs_plotting.R")

radar <- plot_morphology_circle(final_data, measures = MM_PCA_FEATURES)
pc_plots <- plot_pca_comparison(
  pca_data, treatments = unique(pca_data$treatment), plot_type = "over_time"
)
variance_plot <- plot_pca_variance(part1$pca_model)
```

Do not pass `pca_data` or `final_data_scaled` back into `fit_microglia_pca()`: their morphology columns are already transformed. If you want a new PCA with a different feature set, use the untransformed `final_data` instead.

### Alternative: attach an independently prepared measurement table

The following helper is useful when you already have a prepared per-cell table and want strict missing-value checks instead of the MM Part 1 replacement workflow.

The matching uses **cell numbering and the saved export lookup**. It does not assume the measurement rows are in the same order as the pickle.

```r
measurements <- read_imagej_measurements(
  morphology_csv = "MorphoLibJ_3D_SelectedFeatures.csv",
  skeleton_dir = "SKELETON_CSV_DIRECTORY"
)

# Choose features by name. This short list is an example, not the manuscript feature set.
measures <- c("Volume", "SurfaceArea", "Sphericity", "Solidity3D")

morphology <- attach_microglia_measurements(
  selected,
  measurements,
  measures = measures,
  lookup_csv = "microglia_lookup.csv"
)
```

Omit `skeleton_dir` if only shape measurements are needed. Check `names(measurements)` before choosing skeleton features: their headings are converted to R-friendly names. You may also supply your own per-cell measurement dataframe or CSV with `filename` (or `Filename`) and numeric measurement columns.

The lookup matches **full experiment + exact acquisition time + cell number** to the exported filename. When present, the pandas index is checked too. Whole-fish lookups cannot be used for single-cell morphology. If the lookup is omitted, filenames are rebuilt using the filename convention described above; this requires the original pickle ordering and the same metadata treatment labels used for export.

Duplicate filenames, missing matches, or nonfinite values in the requested features stop the join by default. To deliberately exclude affected cells, use `missing = "drop"` and inspect `attr(morphology, "measurement_audit")`. This may change timepoint coverage; review it before claiming a complete cohort.

Extra measurement rows are harmless: only the selected cells are attached. Derived ratios must be provided explicitly. If you use additional ratios, calculate and document their definitions in your prepared per-cell table before choosing them as features.

## 5. Radar plots

```r
source("Microglia_Radar.R")
radar <- plot_morphology_circle(
  morphology, measures = measures,
  summarise_level = "fish",
  scale_reference = "data",
  show_percentage_labels = TRUE
)
print(radar$plot)
ggplot2::ggsave("radar.svg", radar$plot, width = 7, height = 7)
```

Use at least three measures. With `summarise_level = "fish"`, cells are averaged within each fish across the selected window, then fish are averaged equally within each treatment. Fish with more cells do not receive extra weight in the treatment mean. Within a fish, timepoints with more cells still contribute more to that fish's mean. `"cell"` averages all selected cells directly.

| `scale_reference` | How values are shown |
|---|---|
| `"data"` | Each feature is scaled between its minimum and maximum across selected cells. |
| `"data_max"` | Starts with `"data"`, then enlarges all radii by one common factor so the largest plotted value reaches the outer ring. |
| `"treatment_summary"` | Each feature is scaled between the lowest and highest treatment means. This emphasizes differences between treatments. |

A constant feature is placed halfway along its axis. Ring percentages describe the chosen scale, **not percentage changes from a control**. Changing the selected treatments or cells can change the reference range.

Use `measure_order` to arrange the features, named `measure_groups` to add headings, and `treatment_colors` to set colours. `radar$treatment_summary`, `radar$fish_summary`, and `radar$scale_ranges` make the plotted values and scaling available for inspection.

## 6. PCA and PC plots

```r
source("PCs_plotting.R")
pca <- fit_microglia_pca(morphology, measures = measures)

variance_plot <- plot_pca_variance(pca$model)
print(variance_plot$plot)

pc_plots <- plot_pca_comparison(
  pca$scores,
  treatments = unique(morphology$treatment),
  pcs = c("PC1", "PC2"),
  plot_type = "over_time",
  comparison_level = "fish"
)
print(pc_plots$plots$PC1)
```

PCA uses `log(1 + value)` followed by centering and scaling each chosen feature to equal variance. It fits one PCA across the selected cells; it does not fit a separate PCA per treatment. Missing, infinite, negative (with log transformation), or constant features must be resolved first. Set `log_transform = FALSE` only when an unlogged analysis is intended.

`pca$model` contains the fitted PCA; `pca$scores` contains cell scores and metadata; `pca$loadings` shows each feature's contribution; `pca$variance` gives explained variance. Use the original model for variance plots. PCA axes can reverse sign between equivalent fits without changing the relationships between cells.

| Function or option | Purpose |
|---|---|
| `plot_pca_comparison(..., plot_type="over_time")` | PC score trajectories, with mean/SEM summaries. |
| `plot_type="aggregate"` | PC score distributions over the selected window. |
| `plot_type="pc1_pc2"` | Two-PC scatter plot. |
| `plot_pca_day_night()` | Splits the selected window at `night_start_inclusive`; the default split is 22 hours. This is a user-defined boundary, not automatic light-cycle detection. |
| `plot_pca_3d()` | Interactive PC1–PC2–PC3 scatter plot. |
| `plot_pca_3d_time()` | Interactive PC1–PC2 scatter plot with time on the third axis. |
| `plot_pca_3d_summary()` | Interactive mean PC1–PC2 trajectories with time on the third axis and optional SEM/SD. |

For comparison plots, `comparison_level = "fish"` summarizes cells within each fish/timepoint; `"cell"` uses cells directly. For the aggregate fish plot, all cells within each fish/window are averaged directly; timepoints with more cells have more influence. The day/night function instead averages timepoint means within each phase. The fitted PCA remains cell-based in either case. `batch` in the plot data means the **full fish identity**, not the batch date. The default time column is `nominal_elapsed_hour`, so plots use the same rounded time grid as selection. Use `time_col = "actual_elapsed_hour"` only if exact clock timing is intended.

The two-PC and specialized plots return `$plot`; over-time and aggregate comparisons return `$plots`, named by PC. Interactive functions also return `$plot`; print it in RStudio to view it. Use `save_overtime_csv = TRUE` and `csv_directory` in `plot_pca_comparison()` to export its PC summaries.

These are descriptive plots. Cell-level points and SEM should not be interpreted as independent fish-level replication.

## 7. Spatial analysis

```r
source("Microglia_spatial_analysis.R")

spatial <- prepare_microglia_spatial_data(
  df = cells,
  metadata_csv = "metadata.csv",
  treatments = c("para", "yolk", "PC"),
  day = 0, days = 1,
  start_time = "20:00", end_time = "02:00",
  time_bin_hours = 0.5,
  complete_cohort = TRUE,
  normalise_fish_to_canvas = TRUE
)

maps <- analyse_microglia_spatial(
  spatial,
  target_projection = "PA_LR",
  n_bins = 15,
  treatment1 = "para",
  comparison_treatments = c("yolk", "PC"),
  plot_metric = "relative_density",
  comparison_metric = "difference",
  normalise_treatment_heat_0_to_1 = TRUE
)
print(maps$plots$aggregate_treatment)
print(maps$plots$aggregate_contrast_combined)
```

You may supply `pickle_path` instead of `df`. Preparation accepts the same metadata, batch, treatment, day, clock-time, and cohort settings as `select_microglia()`.

### Preparing the projections

All cell coordinates from one fish/acquisition are combined. The source axes are assumed to be **Y = anterior–posterior, X = left–right, Z = dorsal–ventral**. Verify this for your images before interpreting anatomical labels.

- `PA_LR`: anterior–posterior versus left–right view.
- `VD_LR`: dorsal–ventral versus left–right view.
- `VD_PA`: dorsal–ventral versus anterior–posterior view.

Overlapping coordinates become one foreground pixel in a projection. The maps measure **occupied projected pixels**, not numbers of cells or 3D voxel density.

With `normalise_fish_to_canvas = TRUE`, each fish's bounding box across the selected times is stretched to a common canvas. This compares relative spatial patterns, and changes physical distances and projected pixel counts. It is not anatomical landmark registration. With `FALSE`, coordinates are not stretched, but each snapshot is shifted to its own minimum position and placed on the common canvas; absolute inter-snapshot position is not preserved.

`canvas_dimensions = c(AP=..., LR=..., DV=...)` sets the canvas spans; otherwise the largest fish spans are used. `padding` adds a border. Out-of-canvas coordinates cause an error rather than being clipped. `z_coordinate_scale` multiplies Z before constructing the canvas; it does not calibrate the exported cell TIFFs.

`reverse_ap`, `reverse_dv`, and `reverse_lr` flip reconstruction axes. The defaults are FALSE, TRUE, FALSE. `anterior_orientation` in the plotting function controls how a prepared projection is displayed. Check both settings against a known image.

Two exact acquisitions from the same fish cannot be pooled into one nominal time bin. If that happens, preparation stops; use a finer grid or resolve the acquisition selection. `save_projections = TRUE`, `save_which`, and `output_dir` optionally save the prepared binary projections. Existing projection files are not overwritten.

### What the analysis settings mean

`n_bins` is the number of bins along the longer projected canvas dimension. The shorter dimension uses the corresponding number of bins, rounded up. Bins include canvas padding; partial bins at the edge are not corrected for their smaller area. These settings therefore affect the numbers in a map.

`target_timepoint = NULL` uses all prepared times. Set it to an elapsed hour, such as 24, to analyze only that time. Peak normalization is recomputed over the analyzed times, so a single-time map may differ from that panel in an all-time analysis.

| `plot_metric` | Meaning before optional treatment normalization |
|---|---|
| `"relative_density"` | Occupied pixels in a bin divided by that fish's largest bin count across the analyzed times. Range 0–1. |
| `"raw_pixel_count"` | Occupied projected pixels in the bin. |
| `"density_percent"` | Relative density multiplied by 100. It is not the percentage of all pixels or cells. |
| `"occupancy_rate"` | 100 if a fish occupies the bin, otherwise 0. The treatment mean is the percentage of observed fish occupying it. |

Fish are averaged equally within each treatment/timepoint. Timepoint means are then averaged equally for the window summary. With an incomplete cohort, these are means of the fish observed at each time; missing fish are not filled with zero.

**`normalise_treatment_heat_0_to_1 = TRUE` rescales each treatment map to its own peak.** This helps compare spatial patterns, but removes absolute differences in magnitude. The aggregate treatment map is also rescaled to its own peak after averaging timepoints. Contrasts are calculated from these resulting maps. Set this option to FALSE when you want to retain counts, percentages, or the original relative-density values.

### Treatment comparisons

`treatment1` is the reference. `comparison_treatments` lists the groups to compare against it; if omitted, all other available treatments are used. A second reference can be specified with `treatment1_second` and `comparison_treatments_second`.

For the table below, **A is the reference and B is the comparison treatment**:

| `comparison_metric` | Calculation and interpretation |
|---|---|
| `"difference"` | B − A. Positive values mean more in B. |
| `"normalized_difference"` | (B − A) / (abs(B) + abs(A) + a small constant). Limits the size of differences relative to the combined signal. |
| `"ratio"` | (B + small constant) / (A + small constant). 1 means equal. |
| `"log2_ratio"` | Log2 of that ratio. 0 means equal; +1 means twice as large. |

`pseudocount` is the small constant, default `1e-8`. It avoids division by zero; ratios can still be very large where the reference is near zero. These maps are descriptive contrasts, not significance tests.

`contrast_legend_mode = "shared"` uses one contrast scale for both reference groups; `"per_contrast"` uses a separate scale for each reference group, shared across that group's comparison panels. `contrast_heat_limits` sets manual limits. `comparison_heat_limits` is an equivalent fallback; `contrast_heat_limits` takes precedence. `plot_heat_limits` controls treatment-map limits. Without manual limits, treatment scales fit the plotted values, including raw counts above 1.

Other settings control appearance: `tick_style` and custom breaks, titles, font sizes, treatment/comparison palettes, panel spacing, and colour-bar dimensions. Axis values are **bin indices**, not micrometres. `save_svg = TRUE` requires `save_svg_dir`; the four SVG filenames are fixed, so use a separate output folder for each analysis if you want to retain earlier plots.

### Results to inspect

- `spatial$cohort$filter_audit` and `$coverage`: selection and missing-time checks.
- `spatial$manifest`: fish, treatment and time represented in each projection.
- `maps$plots$individual_by_time`: **treatment means by time**.
- `maps$plots$aggregate_treatment`: treatment maps averaged over time.
- `maps$plots$contrast_by_time_combined` and `$aggregate_contrast_combined`: comparison maps.
- `maps$data$fish_bins`: per-fish bin counts and metrics.
- `maps$data$time_summary` and `$aggregate_treatment`: values underlying the treatment plots.
- `maps$data$contrast_group1` and `$contrast_group2`: values underlying the contrasts.

The spatial script returns 2D projections, their fish/time metadata, and cohort selection records. The Z coordinate is used only to construct the dorsal–ventral projection views.

## Validation and troubleshooting

Run the synthetic checks from this folder:

```r
source("tests/validate_pipeline.R")
source("tests/validate_mm_part1.R")
```

The synthetic checks cover cell selection, overnight windows, missing timepoints, mask geometry, TIFF export/readback, measurement matching, missing-value handling, PCA calculations, PC plots, and spatial metrics/contrasts.

Validate the Fiji plugin outputs on representative images, including calibration and orientation, before analyzing a full dataset. Synthetic tests do not establish the validity of biological conclusions.

| Problem | What to check |
|---|---|
| No cells selected | Metadata experiment names, time origins, treatment labels, and overnight `days` setting. |
| No complete fish | Inspect coverage with `complete_cohort=FALSE`; check whether the required schedule matches the experiment. |
| Missing measurement matches | Use the lookup from the actual export subfolder; keep the original pickle order and TIFF filenames. |
| NaN shape results | Inspect the image and Fiji Log; confirm 3D binary input, calibration and plugin commands. |
| PCA rejects a feature | Check for missing values, negative values, or a feature that is constant in the selected data. |
| Spatial colours differ after changing selections | Canvas size, fish peak normalization, and treatment peak normalization all depend on the analyzed data. |

Keep the input metadata, export lookup, selection/measurement audits, chosen feature names, and analysis settings with the final figures. Record R/Python and Fiji plugin versions used for the manuscript; `sessionInfo()` records the R environment.
