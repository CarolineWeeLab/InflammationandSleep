// Batch 3D single-cell morphology: MorphoLibJ and 3D Suite.
// Input: one binary 8-bit TIFF stack per cell (background 0, foreground 255).
// Output: MorphoLibJ_3D_SelectedFeatures.csv, one row per filename.
// Existing filenames are skipped, including rows containing NaN.
// Solidity3D is cell volume divided by convex-hull volume.
// See README.md for measurements, calibration and folder selections.

function arrayContains(array, value) {
    for (k = 0; k < array.length; k++) {
        if (array[k] == value) return true;
    }
    return false;
}
// Find new window created by morpholibj

function findNewWindow(beforeTitles) {
    afterTitles = getList("window.titles");
    for (k = 0; k < afterTitles.length; k++) {
        if ( !arrayContains( beforeTitles, afterTitles[k] ) ) {
            return afterTitles[k];
        }
    }
    return "";
}
// Check whether active table has column

function tableHasColumn(columnName) {
    headings = split( Table.headings, "\t" );
    for (k = 0; k < headings.length; k++) {
        if (headings[k] == columnName) return true;
    }
    return false;
}
// Get column safely

function getColumnSafe(columnName, row) {
    if ( tableHasColumn( columnName ) ) {
        return Table.get( columnName, row );
    }
    return NaN;
}
// 3D SOLIDITY:
// Measure volume using 3d suite
// Uses exact 3D Volume output column:
// Volume(unit)

function measure3DVolume(imageID) {
    // Select image to measure
    selectImage( imageID );
    // Remember windows before 3D Volume
    beforeVolumeTitles = getList( "window.titles" );
    // Run 3D Suite volume measurement
    run( "3D Volume" );
    // Find newly created results table
    afterVolumeTitles = getList( "window.titles" );
    volumeTable = "";
    for ( v = 0; v < afterVolumeTitles.length; v++ ) {
        if ( !arrayContains( beforeVolumeTitles, afterVolumeTitles[v] ) ) {
            volumeTable = afterVolumeTitles[v];
            break;
        }
    }
    // No results table found
    if ( volumeTable == "" ) {
        return NaN;
    }
    // Select 3D Volume table
    selectWindow( volumeTable );
    // Read calibrated volume
    measuredVolume = Table.get( "Volume(unit)", 0 );
    // Important:
    // Close ONLY the volume results table.
    // Do not use plain close() here.
    close( volumeTable );
    return measuredVolume;
}
// WRITE NaN ROW

function writeNaNRow(outputFile, filename) {
    line = filename + "," + "NaN," + "NaN," + "NaN," + "NaN," + "NaN," + "NaN," + "NaN," + "NaN," + "NaN," +
        "NaN," + "NaN," + "NaN," + "NaN";
    File.append( line, outputFile );
}
// Load filenames already present in csv
// Returns an array containing the first column of all
// existing data rows.
// Header is skipped.

function loadProcessedFilenames(outputFile) {
    processedFiles = newArray(0);
    if ( !File.exists(outputFile) ) {
        return processedFiles;
    }
    text = File.openAsString( outputFile );
    lines = split( text, "\n" );
    // Start at row 1 to skip header
    for ( r = 1; r < lines.length; r++ ) {
        row = lines[r];
        if ( lengthOf(row) == 0 ) {
            continue;
        }
        columns = split( row, "," );
        if ( columns.length > 0 ) {
            existingFilename = columns[0];
            // Remove quotes if they happen to exist
            existingFilename = replace( existingFilename, "\"", "" );
            processedFiles = Array.concat( processedFiles, existingFilename );
        }
    }
    return processedFiles;
}
// Analyze one 3d cell

function analyzeCell3D( input, outputFile, filename ) {
    // Open image
    open( input + filename );
    imageID = getImageID();
    // Get dimensions
    getDimensions( width, height, channels, slices, frames );
    // Require z-stack
    if ( slices <= 1 ) {
        print( "WARNING: Not a Z-stack: " + filename );
        writeNaNRow( outputFile, filename );
        if ( isOpen(imageID) ) {
            selectImage( imageID );
            close();
        }
        return;
    }
    // Require one channel
    if ( channels != 1 ) {
        print( "WARNING: Expected 1 channel but found " + channels + " in " + filename );
        writeNaNRow( outputFile, filename );
        if ( isOpen(imageID) ) {
            selectImage( imageID );
            close();
        }
        return;
    }
    // Require 8-bit
    if ( bitDepth() != 8 ) {
        print( "WARNING: Expected 8-bit binary image: " + filename );
        writeNaNRow( outputFile, filename );
        if ( isOpen(imageID) ) {
            selectImage( imageID );
            close();
        }
        return;
    }
    // Check foreground
    // Binary:
    // 0   = background
    // 255 = cell
    totalForeground = 0;
    for ( z = 1; z <= slices; z++ ) {
        setSlice( z );
        getHistogram( values, counts, 256 );
        totalForeground += counts[255];
    }
    if ( totalForeground == 0 ) {
        print( "WARNING: No foreground detected in " + filename );
        writeNaNRow( outputFile, filename );
        if ( isOpen(imageID) ) {
            selectImage( imageID );
            close();
        }
        return;
    }
    // Record current windows
    beforeTitles = getList( "window.titles" );
    // Run morpholibj
    selectImage( imageID );
    run( "Analyze Regions 3D", "voxel_count " + "volume " + "surface_area " + "mean_breadth " + "sphericity " +
        "bounding_box " + "centroid " + "equivalent_ellipsoid " + "ellipsoid_elong. " + "max._feret_diameter " +
        "geodesic_diameter " + "max._inscribed_ball " + "geodesic_elong. " + "tortuosity " + "euler_number " +
        "surface_area_method=[Crofton (13 dirs.)] " + "euler_connectivity=6" );
    // Find morpholibj table
    morphoTable = findNewWindow( beforeTitles );
    if ( morphoTable == "" ) {
        print( "WARNING: MorphoLibJ table not found for " + filename );
        writeNaNRow( outputFile, filename );
        if ( isOpen(imageID) ) {
            selectImage( imageID );
            close();
        }
        return;
    }
    // Select morpholibj results table
    selectWindow( morphoTable );
    // Read selected morpholibj features
    volume = getColumnSafe( "Volume", 0 );
    surfaceArea = getColumnSafe( "SurfaceArea", 0 );
    sphericity = getColumnSafe( "Sphericity", 0 );
    elong12 = getColumnSafe( "Elli.R1/R2", 0 );
    elong13 = getColumnSafe( "Elli.R1/R3", 0 );
    elong23 = getColumnSafe( "Elli.R2/R3", 0 );
    maxFeret = getColumnSafe( "MaxFeretDiam", 0 );
    geodesicDiameter = getColumnSafe( "GeodesicDiameter", 0 );
    inscrBallRadius = getColumnSafe( "InscrBall.Radius", 0 );
    tortuosity = getColumnSafe( "Tortuosity", 0 );
    // Close only morpholibj table
    // Important:
    // Keep original cell image open for Solidity analysis.
    if ( isOpen( morphoTable ) ) {
        selectWindow( morphoTable );
        close( morphoTable );
    }
    // 3D SOLIDITY ANALYSIS
    // 1. MEASURE ORIGINAL CELL VOLUME USING 3D SUITE
    if ( isOpen( imageID ) ) {
        selectImage( imageID );
    }
    solidityCellVolume = measure3DVolume( imageID );
    // Default solidity values
    convexHullVolume = NaN;
    solidity3D = NaN;
    // 2. CREATE 3D CONVEX HULL
    if ( isOpen( imageID ) ) {
        selectImage( imageID );
        beforeHull = getList( "image.titles" );
        run( "3D Convex Hull (slow)" );
        afterHull = getList( "image.titles" );
        hullTitle = "";
        for ( h = 0; h < afterHull.length; h++ ) {
            if ( !arrayContains( beforeHull, afterHull[h] ) ) {
                hullTitle = afterHull[h];
                break;
            }
        }
        // Hull found
        if ( hullTitle != "" ) {
            selectWindow( hullTitle );
            hullID = getImageID();
            // 3. MEASURE CONVEX HULL VOLUME
            convexHullVolume = measure3DVolume( hullID );
            // 4. CALCULATE SOLIDITY
            if ( !isNaN( solidityCellVolume ) && !isNaN( convexHullVolume ) && convexHullVolume > 0 ) {
                solidity3D = solidityCellVolume / convexHullVolume;
            }
            // Sanity check
            if ( !isNaN( solidity3D ) && solidity3D > 1.0001 ) {
                print( "WARNING: Solidity > 1 for " + filename + " : " + solidity3D );
            }
            // Close hull image
            if ( isOpen( hullID ) ) {
                selectImage( hullID );
                close();
            }
        }
        else {
            print( "WARNING: Convex hull image not found for " + filename );
        }
    }
    // Save one row
    line = filename + "," + volume + "," + surfaceArea + "," + sphericity + "," + elong12 + "," + elong13 +
        "," + elong23 + "," + maxFeret + "," + geodesicDiameter + "," + inscrBallRadius + "," + tortuosity +
        "," + solidityCellVolume + "," + convexHullVolume + "," + solidity3D;
    File.append( line, outputFile );
    // Close original cell image
    if ( isOpen( imageID ) ) {
        selectImage( imageID );
        close();
    }
}
// Macro start
// Input folder
setOption( "JFileChooser", true );
cell_dir = getDirectory( "Choose folder containing binary 3D single-cell TIFF stacks" );
files = getFileList(cell_dir);
cell_input = newArray(0);
for (i = 0; i < files.length; i++) {
    lowerName = toLowerCase(files[i]);
    if (endsWith(lowerName, ".tif") || endsWith(lowerName, ".tiff"))
        cell_input = Array.concat(cell_input, files[i]);
}
Array.sort(cell_input);
if (cell_input.length == 0) exit("No TIFF stacks found in the selected folder.");
cell_count = cell_input.length;
// Output folder
setOption( "JFileChooser", true );
results_output = getDirectory( "Choose output folder for results" );
if (results_output == cell_dir) exit("Choose an output folder separate from the input folder.");
// Master csv
outputFile = results_output + "MorphoLibJ_3D_SelectedFeatures.csv";
// Create header only if file does not exist
if ( !File.exists( outputFile ) ) {
    File.saveString( "Filename," + "Volume," + "SurfaceArea," + "Sphericity," + "Elli_R1_R2," + "Elli_R1_R3," +
        "Elli_R2_R3," + "MaxFeretDiam," + "GeodesicDiameter," + "InscrBall_Radius," + "Tortuosity," +
        "Solidity_CellVolume," + "ConvexHullVolume," + "Solidity3D\n", outputFile );
    print( "Created new results file." );
}
else {
    print( "Existing results file detected." );
    print( "New results will be appended." );
}
// Load already-processed filenames
processedFiles = loadProcessedFilenames( outputFile );
print( "Already processed cells found in CSV: " + processedFiles.length );
// Select range
Dialog.create( "MorphoLibJ 3D Resume-Safe Analysis" );
Dialog.addMessage( "Number of files found: " + cell_count );
Dialog.addMessage( "Already in CSV: " + processedFiles.length );
Dialog.addMessage( "Cells already present in the CSV will be skipped automatically." );
Dialog.addNumber( "Start at Image:", 1 );
Dialog.addNumber( "Stop at Image:", cell_count );
Dialog.show();
startAt = Dialog.getNumber();
endAt = Dialog.getNumber();
if (startAt != floor(startAt) || endAt != floor(endAt)) exit("Image ranges must use whole numbers.");
// Validate range
if ( startAt < 1 ) {
    startAt = 1;
}
if ( endAt > cell_count ) {
    endAt = cell_count;
}
if ( endAt < startAt ) {
    exit( "Stop image must be greater than or equal to start image." );
}
// Process
setBatchMode( true );
newlyProcessed = 0;
alreadySkipped = 0;
nonTiffSkipped = 0;
for ( i = startAt - 1; i < endAt; i++ ) {
    filename = cell_input[i];
    lowerName = toLowerCase( filename );
    // Tiff files only
    if ( !endsWith( lowerName, ".tif" ) && !endsWith( lowerName, ".tiff" ) ) {
        nonTiffSkipped++;
        continue;
    }
    // Skip if already present in csv
    if ( arrayContains( processedFiles, filename ) ) {
        alreadySkipped++;
        continue;
    }
    // Analyze cell
    analyzeCell3D( cell_dir, outputFile, filename );
    newlyProcessed++;
    // Add filename to in-memory processed list too
    // Prevents duplicate processing during same run.
    processedFiles = Array.concat( processedFiles, filename );
    // Progress update
    if ( newlyProcessed > 0 && newlyProcessed % 1000 == 0 ) {
        print( "New cells processed this run: " + newlyProcessed );
        print( "Current file position: " + (i + 1) + " / " + endAt );
    }
}
// Finish
setBatchMode( false );
print( "Finished MorphoLibJ + Solidity 3D analysis" );
print( "New cells processed this run: " + newlyProcessed );
print( "Already-completed cells skipped: " + alreadySkipped );
print( "Non-TIFF entries skipped: " + nonTiffSkipped );
print( "Results saved to:" );
print( outputFile );
print( "done!" );
