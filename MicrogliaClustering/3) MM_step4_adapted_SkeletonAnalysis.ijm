// Single-cell skeleton analysis.
// Input: one binary 2D image or 3D stack per cell, foreground 255 and background 0.
// Requires Fiji's Skeletonize (2D/3D) and Analyze Skeleton (2D/3D).

function containsTitle(titles, title) {
    for (j = 0; j < titles.length; j++) {
        if (titles[j] == title) return true;
    }
    return false;
}

function skeleton(input, output, imageOutput, filename) {
    resultPath = output + filename + "_results.csv";
    imagePath = imageOutput + filename + "_taggedskeleton.tif";
    if (File.exists(resultPath) || File.exists(imagePath)) {
        print("Skipping existing output: " + filename);
        return;
    }
    beforeTitles = getList("image.titles");
    open(input + filename);
    originalID = getImageID();
    run("Clear Results");
    run("Skeletonize (2D/3D)");
    run("Analyze Skeleton (2D/3D)", "prune=none");
    if (nResults > 0) {
        // Keep the component rows and Fiji's summary rows in the CSV.
        run("Summarize");
        saveAs("Results", resultPath);
        afterTitles = getList("image.titles");
        taggedTitle = "";
        for (j = 0; j < afterTitles.length; j++) {
            if (!containsTitle(beforeTitles, afterTitles[j]) &&
                indexOf(toLowerCase(afterTitles[j]), "tagged") >= 0)
                taggedTitle = afterTitles[j];
        }
        if (taggedTitle != "") {
            selectWindow(taggedTitle);
            saveAs("Tiff", imagePath);
        } else {
            print("No tagged image was produced; saving the binary skeleton: " + filename);
            selectImage(originalID);
            saveAs("Tiff", imageOutput + filename + "_skeleton.tif");
        }
    } else {
        print("No skeleton detected; no results written: " + filename);
    }
    // Close only images opened or generated for this cell.
    afterTitles = getList("image.titles");
    for (j = 0; j < afterTitles.length; j++) {
        if (!containsTitle(beforeTitles, afterTitles[j])) {
            selectWindow(afterTitles[j]);
            close();
        }
    }
}

setOption("JFileChooser", true);
cell_dir = getDirectory("Choose folder containing single-cell binary TIFF images");
files = getFileList(cell_dir);
cell_input = newArray(0);
for (i = 0; i < files.length; i++) {
    name = toLowerCase(files[i]);
    if (endsWith(name, ".tif") || endsWith(name, ".tiff"))
        cell_input = Array.concat(cell_input, files[i]);
}
Array.sort(cell_input);
cell_count = cell_input.length;
if (cell_count == 0) exit("No TIFF images found in the selected folder.");
skeleton_output = getDirectory("Choose output folder for skeleton CSV results");
skeleton_image_output = getDirectory("Choose output folder for skeleton TIFF images");
if (cell_dir == skeleton_output || cell_dir == skeleton_image_output)
    exit("Choose output folders separate from the input folder.");
Dialog.create("Single-cell skeleton analysis");
Dialog.addMessage("TIFF images are processed in filename order. Found: " + cell_count);
Dialog.addNumber("Start at Image:", 1);
Dialog.addNumber("Stop at Image:", cell_count);
Dialog.show();
startAt = Dialog.getNumber();
endAt = Dialog.getNumber();
if (startAt != floor(startAt) || endAt != floor(endAt) ||
    startAt < 1 || endAt > cell_count || startAt > endAt)
    exit("Choose a valid whole-number image range between 1 and " + cell_count + ".");
setBatchMode(true);
for (i = startAt - 1; i < endAt; i++)
    skeleton(cell_dir, skeleton_output, skeleton_image_output, cell_input[i]);
setBatchMode(false);
print("Finished skeleton analysis. Check the Log for skipped images.");
