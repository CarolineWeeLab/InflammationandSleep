# ================================================================
# GSE243907 | 6 dpf zebrafish brain scRNA-seq | CXCR1 / CXCR2
# Reproducible FINAL workflow (Seurat 5)
# ================================================================
# Samples: GSM7798174_Zbe_6dpf and GSM9455950_Zbe_6dpf_2.
# Workflow: retain rare genes -> cell QC -> RPCA integrate replicates
# -> cluster/UMAP -> annotate clusters -> quantify ORIGINAL RNA.
# Integration is used for clustering ONLY, never receptor counts.
# Cell-type labels below are our marker-guided interpretations,
# not original study-provided per-cell annotations.
# ================================================================

library(Seurat)
library(Matrix)
library(dplyr)
library(ggplot2)

# 1. Locate the GSE243907_RAW parent directory.
#    In the file chooser, open EITHER 6 dpf sample folder and
#    select its matrix.mtx.gz (or matrix.mtx) file.
#    Each sample must have its own subfolder inside GSE243907_RAW.
selected_file <- file.choose()
sample_folder <- dirname(selected_file)
data_dir <- dirname(sample_folder)

samples <- c(
  "6dpf_rep1" = "GSM7798174_Zbe_6dpf",
  "6dpf_rep2" = "GSM9455950_Zbe_6dpf_2"
)
if (!basename(sample_folder) %in% unname(samples)) {
  stop("Select a matrix file inside GSM7798174_Zbe_6dpf or GSM9455950_Zbe_6dpf_2")
}
if (!all(dir.exists(file.path(data_dir, unname(samples))))) {
  stop("Both sample subfolders must be directly inside GSE243907_RAW")
}

output_dir <- file.path(data_dir, "6dpf_CXCR_final")
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
set.seed(1234)  # Reproducible stochastic clustering/UMAP initialization
cat("Parent data folder:", data_dir, "\n")

# Locate a 10X file regardless of whether it is gzipped.
# Windows Explorer sometimes hides .gz filename extensions.
find_10x_file <- function(folder, basename) {
  options <- file.path(folder, c(paste0(basename, ".gz"), basename))
  existing <- options[file.exists(options)]
  if (!length(existing)) stop("Missing ", basename, " in ", folder)
  existing[1]
}

# 2. Load raw matrices and apply the QC cutoffs used in our analysis.
#    min.cells = 0 retains low-abundance genes such as cxcr2.
read_and_qc <- function(sample_id, folder_name) {
  folder <- file.path(data_dir, folder_name)
  mat <- ReadMtx(
    mtx = find_10x_file(folder, "matrix.mtx"),
    features = find_10x_file(folder, "features.tsv"),
    cells = find_10x_file(folder, "barcodes.tsv"),
    feature.column = 2,
    unique.features = TRUE
  )
  obj <- CreateSeuratObject(counts = mat, project = sample_id,
                            min.cells = 0, min.features = 0)
  obj$sample <- sample_id
  obj$stage <- "6 dpf"
  obj[["percent.mt"]] <- PercentageFeatureSet(obj, pattern = "^mt-")
  obj <- subset(obj, subset = nFeature_RNA > 200 & nFeature_RNA < 8000 &
                  !is.na(percent.mt) & percent.mt < 10)
  return(obj)
}

obj_list <- lapply(names(samples), function(id) read_and_qc(id, samples[id]))
names(obj_list) <- names(samples)
qc_summary <- data.frame(
  sample = names(obj_list),
  cells_after_QC = vapply(obj_list, ncol, numeric(1))
)

print(qc_summary)
write.csv(qc_summary, file.path(output_dir, "QC_cell_counts.csv"), row.names = FALSE)

# 3. RPCA integration of the two replicates. Use 2,000 shared genes
#    for integration, 30 PCs and clustering resolution 1.
obj_list <- lapply(obj_list, function(obj) {
  obj <- NormalizeData(obj, normalization.method = "LogNormalize",
                       scale.factor = 10000, verbose = FALSE)
  FindVariableFeatures(obj, selection.method = "vst",
                       nfeatures = 2000, verbose = FALSE)
})
features <- SelectIntegrationFeatures(object.list = obj_list, nfeatures = 2000)
obj_list <- lapply(obj_list, function(obj) {
  obj <- ScaleData(obj, features = features, verbose = FALSE)
  RunPCA(obj, features = features, npcs = 30, verbose = FALSE)
})
anchors <- FindIntegrationAnchors(object.list = obj_list,
                                  anchor.features = features,
                                  reduction = "rpca", dims = 1:30)
endo_6dpf_int <- IntegrateData(anchorset = anchors, dims = 1:30)
DefaultAssay(endo_6dpf_int) <- "integrated"
endo_6dpf_int <- ScaleData(endo_6dpf_int, verbose = FALSE)
endo_6dpf_int <- RunPCA(endo_6dpf_int, npcs = 30, verbose = FALSE)
endo_6dpf_int <- FindNeighbors(endo_6dpf_int, dims = 1:30, verbose = FALSE)
endo_6dpf_int <- FindClusters(endo_6dpf_int, resolution = 1, verbose = FALSE)
endo_6dpf_int <- RunUMAP(endo_6dpf_int, dims = 1:30, seed.use = 1234,
                         verbose = FALSE)

# 4. Retain essential QC plots: integrated clusters and replicate mixing.
p_clusters <- DimPlot(endo_6dpf_int, reduction = "umap",
                      label = TRUE, repel = TRUE) +
  ggtitle("GSE243907 | integrated 6 dpf clusters")
p_samples <- DimPlot(endo_6dpf_int, reduction = "umap", group.by = "sample") +
  ggtitle("GSE243907 | 6 dpf replicates after integration")
ggsave(file.path(output_dir, "integrated_clusters_UMAP.pdf"),
       p_clusters, width = 9, height = 7)
ggsave(file.path(output_dir, "integrated_replicates_UMAP.pdf"),
       p_samples, width = 9, height = 7)
p_clusters
p_samples
# 5. Use uncorrected RNA for marker identification and expression.
DefaultAssay(endo_6dpf_int) <- "RNA"
if (length(Layers(endo_6dpf_int[["RNA"]], search = "^counts")) > 1) {
  endo_6dpf_int <- JoinLayers(endo_6dpf_int, assay = "RNA")
}
stopifnot(all(c("cxcr1", "cxcr2") %in% rownames(endo_6dpf_int)))
Idents(endo_6dpf_int) <- "seurat_clusters"

markers <- FindAllMarkers(endo_6dpf_int, assay = "RNA", only.pos = TRUE,
                          min.pct = 0.10, logfc.threshold = 0.25,
                          verbose = FALSE)
if (nrow(markers) == 0) stop("FindAllMarkers returned no markers; check the RNA data layer.")
top20 <- markers %>%
  group_by(cluster) %>%
  slice_max(avg_log2FC, n = 20, with_ties = FALSE) %>%
  arrange(cluster, desc(avg_log2FC)) %>%
  ungroup()
write.csv(markers, file.path(output_dir, "integrated_all_cluster_markers.csv"),
          row.names = FALSE)
write.csv(top20, file.path(output_dir, "integrated_top20_markers.csv"),
          row.names = FALSE)

# 6. Biological labels from prior marker analysis.
#    IMPORTANT: Cluster numbers are not inherently stable across reruns.
#    Validate the clustering against saved outputs/markers before using
#    this mapping. Do not silently reuse labels if the
#    number or identities of clusters change.
cluster_labels <- c(
  "0"  = "Endothelial-like",
  "1"  = "Li-AngEC",
  "3"  = "Li-CapEC",
  "9"  = "Li-VEC",
  "11" = "Endothelial-like",
  "12" = "Li-MEC",
  "13" = "Li-MEC",
  "33" = "Li-VEC-like",
  "2"  = "Brain-associated myeloid-like immune",
  "6"  = "Brain-associated lymphoid-like immune",
  "8"  = "Brain-associated lymphoid-like immune",
  "10" = "Hematopoietic/erythroid",
  "15" = "Thrombocyte",
  "16" = "Brain-associated macrophage/microglia-like",
  "20" = "Brain-associated granulocyte-like",
  "21" = "Erythrocyte",
  "22" = "Cycling hematopoietic",
  "25" = "Brain-associated immune-like",
  "32" = "Brain-associated immune-like",
  "7"  = "Neuronal",
  "19" = "Neuronal",
  "23" = "Neural progenitor/glial-like",
  "26" = "Sensory-like",
  "30" = "Photoreceptor",
  "31" = "Neuronal/sensory-like",
  "5"  = "Stromal/vascular-like",
  "14" = "Mesenchymal-like",
  "17" = "Stromal/vascular-like",
  "18" = "Mural/pericyte-like",
  "27" = "Mixed/uncertain",
  "28" = "Mixed/uncertain",
  "4"  = "Secretory/hepatic-like",
  "24" = "Epithelial-like",
  "29" = "Secretory/hepatic-like"
)

actual_clusters <- sort(unique(as.character(endo_6dpf_int$seurat_clusters)))
if (!setequal(actual_clusters, names(cluster_labels))) {
  stop("Clusters differ from the prior 0-33 annotation set. Review markers and update labels before continuing.")
}
cluster_mapping <- data.frame(cluster = actual_clusters,
                              proposed_celltype = unname(cluster_labels[actual_clusters]))
write.csv(cluster_mapping, file.path(output_dir, "cluster_annotation_key.csv"),
          row.names = FALSE)
endo_6dpf_int$celltype_label <- unname(
  cluster_labels[as.character(endo_6dpf_int$seurat_clusters)]
)
stopifnot(!anyNA(endo_6dpf_int$celltype_label))

# 7. Quantify detected cxcr1/cxcr2 transcripts (raw UMI > 0),
#    both per cluster and per cluster x replicate. Crucial for
#    distinguishing reproducible signal from replicate-specific signal.
counts <- GetAssayData(endo_6dpf_int, assay = "RNA", layer = "counts")
meta <- endo_6dpf_int[[]]
meta$cell_barcode <- rownames(meta)

summarize_detection <- function(cell_ids, cluster, sample, gene) {
  n <- length(cell_ids)
  values <- as.numeric(counts[gene, cell_ids, drop = TRUE])
  data.frame(cluster = cluster, sample = sample, gene = gene,
             n_cells = n, positive_cells = sum(values > 0),
             percent_positive = 100 * sum(values > 0) / n,
             total_UMIs = sum(values))
}

cluster_summary <- bind_rows(lapply(actual_clusters, function(cl) {
  cells <- meta$cell_barcode[as.character(meta$seurat_clusters) == cl]
  bind_rows(lapply(c("cxcr1", "cxcr2"), function(gene) {
    summarize_detection(cells, cl, "all", gene)
  }))
}))

cluster_sample_summary <- bind_rows(lapply(actual_clusters, function(cl) {
  bind_rows(lapply(names(samples), function(s) {
    cells <- meta$cell_barcode[as.character(meta$seurat_clusters) == cl &
                                 meta$sample == s]
    if (length(cells) == 0) return(NULL)
    bind_rows(lapply(c("cxcr1", "cxcr2"), function(gene) {
      summarize_detection(cells, cl, s, gene)
    }))
  }))
}))

write.csv(cluster_summary, file.path(output_dir, "CXCR_by_cluster.csv"),
          row.names = FALSE)
write.csv(cluster_sample_summary,
          file.path(output_dir, "CXCR_by_cluster_and_replicate.csv"),
          row.names = FALSE)
print(subset(cluster_summary, gene == "cxcr2"), row.names = FALSE)
print(subset(cluster_sample_summary, gene == "cxcr2"), row.names = FALSE)

# 8. Final figures: UMAP uses integrated coordinates but original RNA
#    cxcr2 expression; dot plot groups cells by proposed biological type.
Idents(endo_6dpf_int) <- "celltype_label"
p_cxcr2 <- FeaturePlot(endo_6dpf_int, features = "cxcr2",
                       reduction = "umap", order = TRUE, label = TRUE,
                       repel = TRUE, label.size = 3) +
  ggtitle("cxcr2 expression | integrated 6 dpf")
p_dot <- DotPlot(endo_6dpf_int, features = c("cxcr1", "cxcr2"),
                 group.by = "celltype_label", assay = "RNA") +
  labs(title = "CXCR1 / CXCR2 expression | 6 dpf",
       x = "Gene", y = "Proposed cell type") +
  theme_classic() + theme(panel.grid = element_blank(),
                          axis.text.y = element_text(size = 8))
print(p_cxcr2)
print(p_dot)
ggsave(file.path(output_dir, "CXCR2_integrated_UMAP.pdf"),
       p_cxcr2, width = 9, height = 7)
ggsave(file.path(output_dir, "CXCR1_CXCR2_by_celltype_dotplot.pdf"),
       p_dot, width = 9, height = 9)
ggsave(file.path(output_dir, "CXCR1_CXCR2_by_celltype_dotplot.tiff"),
       p_dot, width = 9, height = 9, dpi = 600, compression = "lzw", bg = "white")

# 9. Save reusable object, reproducibility details, and cell annotations.
#    Use final cluster labels as well as numeric cluster IDs.
meta$celltype_label <- endo_6dpf_int$celltype_label
saveRDS(endo_6dpf_int, file.path(output_dir, "GSE243907_6dpf_integrated_final.rds"))
write.csv(meta[, c("cell_barcode", "sample", "seurat_clusters", "celltype_label")],
          file.path(output_dir, "cell_cluster_assignments.csv"), row.names = FALSE)
writeLines(capture.output(sessionInfo()), file.path(output_dir, "R_sessionInfo.txt"))
cat("\nFinished. Outputs saved to:\n", output_dir, "\n")
