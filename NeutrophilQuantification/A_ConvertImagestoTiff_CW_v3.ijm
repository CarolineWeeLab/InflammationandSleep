//converts OIBs, LSMs, CZIs etc to Tiff Files

setBatchMode(true);

main_dir = getDirectory("Source Directory");

// Get list of all items inside parent
subfolders = getFileList(main_dir);

for (s=0; s<subfolders.length; s++) {
    subdir = main_dir + subfolders[s];

    // Only process real folders, skip files
    if (!File.isDirectory(subdir)) continue;

    // Define Raw folder inside this subfolder
    target_dir = subdir + "Raw/";
    File.makeDirectory(target_dir); 

	list = getFileList(subdir);
	list = Array.sort(list);
	
	for (i=0; i<list.length; i++){
	    image = subdir + list[i];
		// 🔥 FIX: skip folders (like Raw/)
        if (File.isDirectory(image)) continue;
	    run("Bio-Formats Importer", "open='"+ image +"' color_mode=Default view=Hyperstack stack_order=XYCZT");
	   	//run("Flip Vertically", "stack");
	    saveAs("tiff", target_dir + "/" + list[i] + ".tiff");
	    close();
	}
}