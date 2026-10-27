DispThreshold = 150

print("\\Clear");
run("Clear Results");
run("Close All"); 

setBatchMode(false);

main_dir = getDirectory("Main Directory");

source_dir = main_dir + "/Thresholded/";
source_dir2 = main_dir + "/Raw/";

File.makeDirectory(main_dir + "/CellCount/"); 
target_dir = main_dir + "/CellCount/";

summaryFile = target_dir + "summary.csv";

// Initialize CSV file with headers
if (!File.exists(summaryFile)) {
    File.append("File name,No. of Neutrophils", summaryFile);
}

list = getFileList(source_dir2);

for (i = 0; i < list.length; i++) {
    Image_original = source_dir2 + list[i];
    Image_threshold = source_dir + list[i] + "_thresholdedmask.tiff";

    open(Image_original);
    Original = getTitle();

    open(Image_threshold);
    Threshold = getTitle();

    // ---- Select ROI manually ----
    selectWindow(Original);
    run("Z Project...", "projection=[Max Intensity]");
    selectWindow("MAX_" + Original);
    setMinAndMax(0, DispThreshold);  // display threshold
    run("Remove Overlay");

    setTool("freehand");
    waitForUser("Select region", "Select the area to analyze and click 'OK'");
    run("Add Selection...");
    run("To ROI Manager");
    roiManager("Select", 0);
    roiManager("Save", target_dir + "/" + list[i] + "_ROI0.zip");

    // ---- Analyze particles ----
    selectWindow(Threshold);
    run("Invert LUT");
    roiManager("Select", 0);

    run("Analyze Particles...", "size=10-500 pixel show=Masks display stack");

    maskTitle = "Mask of " + Threshold;
    if (isOpen(maskTitle)) {
        selectWindow(maskTitle);
        saveAs("tiff", target_dir + "/" + list[i] + "_ROI0_FilteredObjects.tiff");
        rename("FilteredMask");
    } else {
        print("Mask window not found for " + list[i]);
    }

    // ---- 3D Objects Counter ----
    run("3D Objects Counter", "threshold=128 slice=1 min.=1 max.=10000 objects statistics summary");

    // Save statistics table
    statsWindows = getList("window.titles");
    for (w = 0; w < statsWindows.length; w++) {
        if (startsWith(statsWindows[w], "Statistics for")) {
            selectWindow(statsWindows[w]);
            saveAs("Text", target_dir + "/" + list[i] + "_ROI0_Statistics.xls");
    		neutro_count = Table.size;
    		print("Neutrophil count for " + list[i] + ": " + neutro_count);
        }
    }

    // Save 3D Objects map
    objmap_title = "Objects map of FilteredMask";
    if (isOpen(objmap_title)) {
        selectWindow(objmap_title);
        saveAs("Tiff", target_dir + "/" + list[i] + "_ROI0_3DObjectMap.tif");
    }


    // Append to CSV
    File.append(list[i] + "," + neutro_count, summaryFile);

    // Close everything before next image
    run("Close");
    run("Clear Results");
    run("Close All");

    showProgress(i, list.length);
    run("Collect Garbage");
}