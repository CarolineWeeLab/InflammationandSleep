// Threshold all TIFF images inside 'Raw' subfolders of a chosen parent folder
// Each subfolder gets its own "Thresholded" folder with results

// Specify threshold below
threshold = 190;  

main_dir = getDirectory("Choose Parent Folder");

// Get list of all items inside parent
subfolders = getFileList(main_dir);

for (s=0; s<subfolders.length; s++) {
    subdir = main_dir + subfolders[s];

    // Only process real folders, skip files
    if (!File.isDirectory(subdir)) continue;

    // Define Raw folder inside this subfolder
    raw_dir = subdir + "Raw/";
    if (!File.isDirectory(raw_dir)) continue;  // Skip if no Raw folder

    // Make thresholded output folder inside this subfolder
    target_dir = subdir + "Thresholded/";
    File.makeDirectory(target_dir); 

    // List all files in Raw subdir
    list = getFileList(raw_dir);

    for (i=0; i<list.length; i++) {
        image = raw_dir + list[i];

        // Skip folders
        if (File.isDirectory(image)) continue;

        // Skip non-TIFF files
        if (!(endsWith(list[i], ".tif") || endsWith(list[i], ".tiff"))) continue;

        open(image); 
        resetMinAndMax();

        run("Subtract Background...", "rolling=10 stack");
        run("Enhance Contrast", "saturated=0.2"); 

        // Thresholding and Masking
        setThreshold(0, threshold);
        run("Convert to Mask", "method=Default background=Default");
        run("Make Binary", "method=Default background=Default");

        // Save output into Thresholded folder
        saveAs("tiff", target_dir + "/" + list[i] + "_thresholdedmask.tiff");
        close();
    }
}