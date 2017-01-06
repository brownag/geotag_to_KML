#Photo geotag to KML script
#@author: andrew brown
#@version: 0.1b; 12/1/16 
#@description: extracts geotag/EXIF data from images in a user-specified directory.
###            clusters pictures/coordinates that fall within a defined threshold (default 50m) 
###            creates a KML file that can be imported into AvenzaMaps with average cluster coordinates and  
###            the corresponding photos. Note that mapping between file names of pictures and their  
###            corresponding file names on the phone are necessary in order to display them through  
###            PDFMAPs interface. 
