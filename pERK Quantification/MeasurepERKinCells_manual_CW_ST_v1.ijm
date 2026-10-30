// First, open an image and use multipoint TOOL to select all points. 
// Remember to click on CENTER of each cell at correct Z! 
// Remember to change oval size if necessary
// Need to download latest FIJI version for Z-information to be accurate
// Output will be in "Results", ordered by each cell (and all channels per cell)

run("Clear Results");
roiManager("reset");
print("\\Clear");
run("Set Measurements...", "mean center stack nan redirect=None decimal=3");
run("Measure");
npoints = nResults;

//Dialog.create("Set parameters");
//Dialog.addNumber("Oval diameter (pixels)", 20);
//Dialog.addNumber("Number of channels", 4);
//Dialog.show();
ovalsize = 20;
//ovalsize = 20; //sets number of pixels of circle diameter)
nchannels = 4;

xvalue = newArray(npoints);
yvalue = newArray(npoints);
zvalue = newArray(npoints);

// to get the x y z coordinates for all points selected
for (i=0; i<npoints; i++) {
    x = getResult("X",i);
    y = getResult("Y", i);
    z = getResult("Slice", i);
    xvalue[i]= x;
    yvalue[i]= y;
    zvalue[i]= z;
}

//Save the X Y Z coordinates as a table using the file name and path of the CZI image being processed 
czi_title = getTitle();
czi_title = replace(czi_title, ".czi", ""); //image name without CZI extension
czi_path = getDirectory("Choose an output directory for ROI save:");
//Dialog.createNonBlocking("Specify Hypothalamic Region");
//Dialog.addString("Caudal or Lateral?", "cH");
//Dialog.show();
//cLH_string = Dialog.getString();
//czi_title = czi_title+"_"+cLH_string;
czi_title = czi_title;

Table.create("XYZ to store");
selectWindow("XYZ to store");
Table.setColumn("X", xvalue);
Table.setColumn("Y", yvalue);
Table.setColumn("Z", zvalue);
Table.rename("XYZ to store", "Results");
saveAs("Results", czi_path+czi_title+"_XYZ.csv");

run("Clear Results"); //now is when I make the actual measurements 

// to draw ovals over each point, store its ROI and measure the intensity values

for (i=0; i<npoints; i++) {
    Stack.setChannel(1);	
    Stack.setSlice(zvalue[i]) 
    toUnscaled(xvalue[i], yvalue[i]);
    makeOval(xvalue[i]-ovalsize/2, yvalue[i]-ovalsize/2, ovalsize, ovalsize);
    roiManager("add"); //stores the OVAL centre-point and diameter=ovalsize

	run("Measure");
	//setColor("#4C7F7F7F");
	//fill();

    //measure other channels
	for (j=2; j<nchannels+1; j++) {
		Stack.setChannel(j);
		run("Measure");
	}
	
	run("Select None");
	Stack.setChannel(1);
}

//After measuring and saving every point, save the ROI list as a file of the same name as the image and an indication of cH/LH
roiManager("Save", czi_path+czi_title+"_ROI.zip"); //save all ROIs 
print("Saved ROI in " + czi_path + " as: " + czi_title); //print for notification
	
//Add custom calculations/columns to the Results table
	//IJ.renameResults("Results");
//Prepare 1 array per channel for storage of mean intensities per 'cell', slice number, and normalised ch1/ch2 values
ch1 = newArray(npoints);
ch2 = newArray(npoints);
ch3 = newArray(npoints);
ch4 = newArray(npoints);
//ch1_ch2 = newArray(npoints);
slice = newArray(npoints);
//Make new/Reset table to store these arrays
Table.create("Per cell");
//
for (j=1; j<=npoints; j++) { //i.e. for every 'cell'
	//print("Cell number "+j);
	selectWindow("Results");
	//Store values for inputting to "Per cell" table
	ch1[j-1] = Table.get("Mean", 4*j-4);
	ch2[j-1] = Table.get("Mean", 4*j-3);
	ch3[j-1] = Table.get("Mean", 4*j-2);
	ch4[j-1] = Table.get("Mean", 4*j-1);
	//ch1_ch2[j-1] = ch1[j-1]/ch2[j-1];
	slice[j-1] = Table.get("Slice", 4*j-4);
	//Array.print(ch1);
}

//Create an array of 'indexes' from 1 to npoints+1, for leftmost column of "Per cell" table;
count = Array.getSequence(npoints+1);
count = Array.deleteIndex(count, 0);
selectWindow("Per cell");
Table.setColumn("", count);	
//Add slice numbers to "Per cell" table
Table.setColumn("Slice", slice);
//Add the 5 intensity arrays to "Per cell" table
Table.setColumn("Ch1", ch1);
Table.setColumn("Ch2", ch2);
Table.setColumn("Ch3", ch3);
Table.setColumn("Ch4", ch4);
//Table.setColumn("Ch1/Ch2", ch1_ch2);

//Calculate means across all cells measured for Ch1, Ch2, Ch3 and Ch1/Ch2
Array.getStatistics(ch1, min, max, mean, stdDev);
ch1mean = mean;	
Array.getStatistics(ch2, min, max, mean, stdDev);
ch2mean = mean;	
Array.getStatistics(ch3, min, max, mean, stdDev);
ch3mean = mean; 
Array.getStatistics(ch4, min, max, mean, stdDev);
ch4mean = mean; 
//Array.getStatistics(ch1_ch2, min, max, mean, stdDev);
//ch1_2mean = mean;
//Number of cells
Array.getStatistics(count, min, max, mean, stdDev);
no_cell = max;

//Add these values to a new table "Means"
Table.create("Means");
selectWindow("Means");
Table.set("Ch1 Mean", 0, ch1mean);
Table.set("Ch2 Mean", 0, ch2mean);
Table.set("Ch3 Mean", 0, ch3mean);
Table.set("Ch4 Mean", 0, ch4mean);
//Table.set("Ch1/Ch2 Mean", 0, ch1_2mean);
Table.set("No. of cells", 0, no_cell);
//Table.setLocationAndSize(500, 500,400,200);


//selectWindow("Results");