# Neutrophil Cell Counting Analysis

## Overview

This folder contains ImageJ/Fiji macros for automated image preprocessing, thresholding, and quantification of neutrophils in zebrafish larvae. The workflow converts microscopy images into TIFF format, generates binary masks, and performs region-of-interest (ROI)-based cell counting using particle analysis and 3D Objects Counter.

## Software Requirements

- Fiji/ImageJ (version 1.54p)
- Bio-Formats plugin
- 3D Objects Counter plugin

## Scripts

### 1. A_ConvertImagestoTiff_CW_v3.ijm

Converts microscopy images (e.g., CZI, LSM, OIB) into TIFF format using Bio-Formats.

**Instructions:**
1. Open the macro in Fiji.
2. Run the macro and select the parent directory containing the image subfolders.
3. The macro processes files within each subfolder.
4. Converted TIFF images are automatically saved in a newly created `Raw` folder within each subfolder.

### 2. B_ThresholdImages_CW_immune_v3.ijm

Performs image preprocessing and thresholding to generate binary masks for cell identification.

**Instructions:**
1. Open the macro in Fiji.
2. Adjust the threshold value if necessary (default: 190).
3. Run the macro and select the parent directory containing the image subfolders.
4. The macro processes TIFF images within each `Raw` folder.
5. Binary masks are automatically saved in a `Thresholded` folder.

**Processing parameters:**
- Background subtraction: Rolling ball radius = 10
- Contrast enhancement: 0.2% saturated pixels
- Threshold range: 0–190 (default)
- Binary mask generation

### 3. C_CountCellsinROI_unregistered_CW_immune_MAX_v5.ijm

Quantifies neutrophils within manually selected ROIs using particle analysis and 3D Objects Counter.

**Instructions:**
1. Open the macro in Fiji.
2. Run the macro and select an individual image subfolder containing `Raw` and `Thresholded` directories.
3. The macro generates a maximum-intensity Z-projection of the original image for ROI selection.
4. Manually draw the desired ROI using the freehand selection tool and click OK.
5. The macro applies the ROI to the thresholded image and performs particle analysis followed by 3D object counting.
6. Results are automatically saved in a `CellCount` folder.

**Analysis parameters:**
- Display intensity range: 0–150
- Particle size: 10–500 pixels
- 3D Objects Counter threshold: 128
- 3D object size range: 1–10,000 voxels

## Analysis Workflow

Run the macros in the following order:

1. **Image conversion:** Convert microscopy images to TIFF format using Script A.
2. **Image thresholding:** Generate binary masks using Script B.
3. **Cell quantification:** Select ROIs and quantify neutrophils using Script C.

## Input Directory Structure

Organize the original microscopy images into separate subfolders within a parent directory.

Example:

```text
Parent_Directory/
├── Control/
│   ├── Fish1.czi
│   └── Fish2.czi
├── Treatment1/
│   ├── Fish1.czi
│   └── Fish2.czi
```

## Output Directory Structure

After running the three macros, the directory structure will resemble:

```text
Parent_Directory/
├── Control/
│   ├── Raw/
│   ├── Thresholded/
│   └── CellCount/
├── Treatment1/
│   ├── Raw/
│   ├── Thresholded/
│   └── CellCount/
```

**Output files:**

- `Raw/`: Converted TIFF images.
- `Thresholded/`: Binary threshold masks (`_thresholdedmask.tiff`).
- `CellCount/`: Saved ROI files (`_ROI0.zip`), filtered object masks (`_ROI0_FilteredObjects.tiff`), object statistics (`_ROI0_Statistics.xls`), 3D object maps (`_ROI0_3DObjectMap.tif`), and a summary CSV (`summary.csv`).

The summary CSV contains the image filename and the corresponding neutrophil count.

## Important Notes

- Scripts A and B process multiple subfolders automatically, whereas Script C processes one selected image subfolder at a time.
- ROI selection in Script C is performed manually for each image.
- Threshold and particle size parameters may require adjustment depending on image quality and experimental conditions.
- Maintain consistent image acquisition and analysis parameters when comparing experimental groups.
- The scripts are intended to be executed sequentially in Fiji/ImageJ.
