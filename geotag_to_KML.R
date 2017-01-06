#Photo geotag to KML script
#@author: andrew brown
#@version: 0.1b; 12/1/16
#@description: extracts geotag/EXIF data from images in a user-specified directory.
###            clusters pictures/coordinates that fall within a defined threshold (default 50m)
###            creates a KML file that can be imported into AvenzaMaps with average cluster coordinates and 
###            the corresponding photos. Note that mapping between file names of pictures and their 
###            corresponding file names on the phone are necessary in order to display them through 
###            PDFMAPs interface.

### install required packages if needed
packz <- c("sp","rgdal","stringr","pixmap","RCurl","utils")
newpackz <- packz[!(packz %in% installed.packages()[,"Package"])]
if(length(newpackz)) 
  install.packages(newpackz)

### load packages
loaded <- lapply(packz,FUN=require,character.only=TRUE)
if(sum(as.numeric(loaded))!=length(packz)) {
  stop("Failed to load one or more required packages!")
  geterrmessage()
}

###SETUP###
script_dir <- "E:/scripts/geotag_to_KML/"
image_directory <- paste0(script_dir,"testimages/")
exiftool_path <- paste0(script_dir,"exiftool(-k).exe")          #this executable is required for extracting EXIF data from JPGs
winzip_path <- "\"C:\\Program Files (x86)\\WinZip\\wzzip.exe\"" #winzip is used for creating KMZ files
template_file <- 'AvenzaMaps_kml_template.dat'
device_projection <- '+proj=longlat +datum=WGS84'           #projection information for data extracted from EXIF

output_path <- paste0(script_dir,"~sorted") #this is the path to where site folder output will be placed
if(!dir.exists(output_path))
  dir.create(output_path,showWarnings=FALSE,recursive=TRUE)

threshold <- 50      #meters; maximum euclidean distance from centroid for cluster membership

###SITE ID NAMING SCHEME###
placemark_names <- NA #default is NA; alternately can specify a vector containing pre-defined site IDs. 
                     # if this is specified, no other naming scheme will be used. 
                     # Need to know a priori the number of clusters, and give a name for each, otherwise an error will occur.
placemark_postfix_start <- 53 #default numbering starts from 1. Change to the first site ID number used for this picture set. 
                             # Sites will be incremented based on the temporal order of the pictures.
placemark_prefix <- "2016CA63060" #string to precede the site ID number. can be used to make NASIS site IDs. default is ""

name_by_nearby_point <- FALSE #default: FALSE; creates a new site ID for each cluster of photos
                             # if TRUE allows a separate shapefile of points to be loaded (e.g. exported from GPS, dp layer, NASIS sites etc). 
                               # if one of the points in that feature class falls within the specified threshold then the name of that point will
                             # be used for the corresponding cluster. orherwise it will just get the next available numeric value.
if(name_by_nearby_point)
  dp_points <- readOGR(dsn = 'L:/CA630/FG_CA630_OFFICIAL.gdb', layer = 'ca630_dp', stringsAsFactors=FALSE) 
  #path to feature class containing existing site points for labeling clusters

###
###########

###INTERNAL SETUP###
centroid_function <- mean                                   # function to use for aggregating x,y,z data from multiple images in a cluster;
degms_regex <- "([0-9]+) deg ([0-9]+)' ([0-9]+\\.[0-9]+)\"" # pattern for capturing degrees, min and sec for conversion to decimal degrees
elev_regex <- "([0-9\\.]+) m.*"                             # pattern for capturing elevation numeric value from EXIF string
###########

### FUNCTION DEFINITIONS ###
getDecDegrees = function(s) {
  ff <- str_match(s,degms_regex)
  ff <- as.numeric(ff[2:4])
  return(ff[1] + (ff[2]/60) + (ff[3]/3600))
  #Decimal degrees = Degrees + (Minutes/60) + (Seconds/3600)
}

getNumericElevation = function(s) {
  ff <- str_match(s,elev_regex)
  return(as.numeric(ff[2]))
}

makePhotoStringByCentroid=function(x) {
  #takes vector of file names and creates CDATA string for export
  imagez <- paste0("<img src=\"",x,"\" />")
  buf <- "<![CDATA["
  for(i in imagez) {
    buf <- paste0(buf,i,"<br />")
  }
  buf <- paste0(buf,"]]>")
  return(buf)
}

makePlacemarkByCentroid = function(x,n) {
  #takes a SpatialPointsDataFrame (or subset of one) and creates a placemark for it, using first coordinate
  #x is the data, n is a unique id to use for the name field (probably an integer unless otherwise specified)
  datetime <- x$date[1]
  photostring <- makePhotoStringByCentroid(x$filename)
  buf <- paste0("\t\t\t<Placemark>\n\t\t\t\t",paste0("<name>",n,"</name>\n\t\t\t\t"))
  buf <- paste0(buf,"<TimeStamp><when>",datetime,"</when></TimeStamp>\n\t\t\t\t")
  buf <- paste0(buf,"<ExtendedData><SchemaData schemaUrl=\"#schema0\"><SimpleData name=\"pdfmaps_photos\">",photostring,"</SimpleData></SchemaData></ExtendedData>\n\t\t\t\t")
  buf <- paste0(buf,"<Point><coordinates>",-x$clng[1],",",x$clat[1],",",floor(x$celev[1]),"</coordinates></Point>\n\t\t\t</Placemark>")
  return(buf)
}

makeKML = function(output,placemarks,folder) {
  fileName <- paste0(script_dir,template_file) 
  buf <- readChar(fileName, file.info(fileName)$size)
  buf <- sub("%%%FOLDERNAME%%%",folder,buf)
  buf <- sub("%%%PLACEMARKS%%%",placemarks,buf)
  sink(output)
  cat(buf)
  sink()
}
######

### MAIN APPLICATION LOGIC ###

# load exif data from images in target directory  
filez <- as.character(list.files(path=image_directory,full.names=TRUE))
filez <- filez[grepl(pattern=".*\\.JPG",x=filez,ignore.case=TRUE)] #keep only JPEG
exiftool_callz <- paste0(exiftool_path," \"",filez,"\"")
dat <- data.frame(path=character(), filename = character(), date = character(), 
               lat=numeric(),lng=numeric(), elev = numeric(), 
               bearing = character(),imgh=numeric(),imgw=numeric(),etime=character(),flen=character())
for(g in exiftool_callz) {
  raw <- system(g,intern=TRUE)
  r <- strsplit(raw,"\n")
  goober <- str_match(r, "(.*): (.*)")
  l <- goober[,3]
  names(l) <- str_trim(goober[,2])
  field_names <- c('Directory','File Name',"GPS Date/Time","GPS Altitude","GPS Latitude","GPS Longitude")
  if(sum(field_names %in% names(l)) >= length(field_names)) {
    pname <- paste0(l[['Directory']],'/',l[['File Name']])
    fname <- l[['File Name']]
    print(l[["GPS Date/Time"]])#debug
    datz <- strptime(l[["GPS Date/Time"]],"%Y:%m:%d %H:%M:%S")
    elez <- getNumericElevation(l[["GPS Altitude"]])
    latz <- getDecDegrees(l[["GPS Latitude"]])
    lngz <- getDecDegrees(l[["GPS Longitude"]])
    beaz <- NA
    try(expr=(beaz<-as.numeric(l[['GPS Img Direction']])),silent = TRUE) 
    #sometimes this fails beause there is no bearing information, so we do not require it
    imghz <- as.numeric(l[['Image Height']])
    imgwz <- as.numeric(l[['Image Width']])
    dat <- rbind(dat,data.frame(path=pname,filename=fname, date=datz, lat=latz, lng=lngz, elev=elez, bearing=beaz, imgh=imghz, imgw=imgwz))
  }
}

#elevate dataframe to SpatialPointsDataFrame and specify device projection system
coordinates(dat) <- ~lng+lat
proj4string(dat) <- device_projection

# cluster coordinates using threshold
distz <- spDists(dat,longlat=TRUE)*1000 #gives distance between points in meters (KM*1000); may generate warning due to issues with different projections and calculation of distance
hr <- hclust(dist(distz), method = "complete", members=NULL)
#plot(hr) #debug: check tree visually
dat$centroid <- cutree(hr, h=threshold)#cut the tree at distance threshold to define clusters
print(paste0("Created ",length(levels(factor(as.numeric(dat$centroid))))," clusters from ",length(dat$centroid)," images. ",(length(filez)-length(dat$centroid)), " were missing spatial information in EXIF data."))

#calculate "centroids" by aggregating values for each cluster (defined centroid_function in setup)
c_lat <- aggregate(dat$lat,by=list(dat$centroid),FUN=centroid_function)[,2]
c_lng <- aggregate(dat$lng,by=list(dat$centroid),FUN=centroid_function)[,2]
c_elev <- aggregate(dat$elev,by=list(dat$centroid),FUN=centroid_function)[,2]

#add centroid values to SpatialPointsDataFrame along with individual data
dat$clat <- numeric(length(dat$filename))
dat$clng <- numeric(length(dat$filename))
dat$celev <- numeric(length(dat$filename))
for(i in as.numeric(levels(factor(dat$centroid)))) {
  who=which(dat@data$centroid == i)
  dat@data[who,]$clat <- c_lat[i]
  dat@data[who,]$clng <- -c_lng[i]
  dat@data[who,]$celev <- c_elev[i]
}
dat@data$filename <- as.character(dat@data$filename)
dat2=dat@data #make a copy of the data we have updated
coordinates(dat2) <- ~clng+clat+celev #elevate this copy to SpatialPointsDataFrame this time using the centroid values for points rather than data from individual images. each record still retains individual location as well as centroid.
proj4string(dat2) <- device_projection

ncclust <- levels(factor(dat2$centroid))

if(name_by_nearby_point) {
  placemark_names <- c()
  dp_points_tagged <- 0
  #try to NAME centroids based on DP LAYER POINTS WITHIN THRESHOLD DISTANCE
  dat_dp <- spTransform(dat2,proj4string(dp_points)) # convert to CRS of dp layer
  for(ddp in 1:length(ncclust)) {
    sdat_dp <- dat_dp[which(dat_dp$centroid == ddp),]
    dpdistz <- spDistsN1(pt=coordinates(sdat_dp)[1,1:2],pts=dp_points) 
    dpid <- which(dpdistz==min(dpdistz))[1]
    print(paste("Cluster",ddp,"is",dpdistz[dpid],"meters from",dp_points[dpid,]$IDENT))
    if(dpdistz[dpid] <= threshold) {
      placemark_names <- c(placemark_names,(dp_points[dpid,]$IDENT))
      dp_points_tagged <- dp_points_tagged+1
    } else {
      placemark_names <- c(placemark_names,paste0(ddp))
    }  
  }
  print(paste0("Tagged ",dp_points_tagged, " out of ", length(placemark_names), " clusters to existing points in the DP layer."))
}

foldername <- Sys.Date()
placemarkz <- ""
if(is.na(placemark_names) && placemark_postfix_start > 0) { # if the placemark name list is NA then we will just number them from start to start+number of clusters
  placemark_names <- placemark_postfix_start:(placemark_postfix_start+length(ncclust)) 
  placemark_names <- paste0(placemark_prefix, placemark_names) #adds the prefix, default is ""
}

for(j in ncclust) {
  who <- which(dat2$centroid == j)
  subse <- dat2[who,]
  placemarkz <- paste0(placemarkz,makePlacemarkByCentroid(subse,placemark_names[j]),"\n") #TODO: fail elegantly if there aren't enough names for centroids;
}
makeKML(paste0(output_path,"/doc.kml"),placemarkz,foldername)



#make KMZ file
if(make_kmz) {
  dir.create(paste0(output_path,"/images"),recursive=TRUE,showWarnings=FALSE)
  for(f in 1:length(filez)) {
    file.copy(filez[f],paste0(output_path,"\\images\\",dat2$filename[f]))
  }
  system(paste0(winzip_path, " ",output_path,"/",foldername,".kmz \"",output_path,"/*.*\""," \"",output_path,"/images/*.*\""))
}

for(p in 1:length(ncclust)) {
  clust <- ncclust[p]
  outdir <- paste0(output_path,"/",placemark_names[p])
  dir.create(outdir,recursive=TRUE,showWarnings=FALSE)
  who <- which(dat2$centroid == p)
  subse <- dat2[who,]
  for(s in 1:length(subse)) {
    file.copy(paste0(subse[s,]$path),paste0(outdir,"/",subse[s,]$filename))
  }
}


#########################EXTRAS############################
### KML output example
  ## Sample photostring for two images
  #<![CDATA[<img src="image0.jpg" /><br /><img src="image1.jpg" /><br />]]>
  
  ## Sample placemark 
  # <Placemark>
  #   <name>%%%SITEID%%%</name>
  #   <TimeStamp>
  #     <when>%%%DATETIME%%%</when>
  #   </TimeStamp>
  #   <ExtendedData>
  #     <SchemaData schemaUrl="#schema0">
  #       <SimpleData name="pdfmaps_photos">%%%PHOTOSTRING%%%</SimpleData>
  #     </SchemaData>
  #   </ExtendedData>
  #   <Point>
  #     <coordinates>
  #     %%%LONGITUDE%%%,%%%LATITUDE%%%,%%%ALTITUDE%%%
  #     </coordinates>
  #   </Point>
  # </Placemark>
  
  ## Sample KML file in AvenzaMaps_kml_template.dat

### END ###


##### --- Old stuff
### export collection of SpatialPhotoOverlay objects to a single KML/KMZ
# create dummy "EXIF" data as a named list, using the cluster values rather than individual
# d1=dat@data[1,]
# dexif=list()
# dexif[['GPSLatitude']] = d1$clat
# dexif[['GPSLongitude']] = d1$clng
# dexif[['GPSAltitude']] = d1$celev
# dexif[['DateTime']] = d1$date
# dexif[['ExposureTime']] = "1/999"
# dexif[['FocalLength']] = "4.2 mm"
# dexif[['Flash']] = NA

#actually SPO does not do what we need for use in PDF maps. Could work for Google Earth visualization with some refinements.

# overlay <- as.list(data.frame(rotation=0, leftFov=-180, rightFov=180, bottomFov=-90, 
#                               topFov=90, near=1, shape="rectangle", range=0, tilt=0, 
#                               heading=0, roll=0))
# kml_open("test.kml")  
# nombre=paste0("file:///",dat$filename[1])
# z=RCurl::getURI(nombre, .opts=RCurl::curlOptions(header=TRUE, nobody=TRUE, transfertext=TRUE, failonerror=FALSE, ssl.verifypeer = FALSE))
# bbox=c(0,0,3/36000*d1$imgw,3/36000*d1$imgh)
# bands=rep(rep(1, d1$imgh*d1$imgw), 3)
# pmap=pixmapRGB(bands, d1$imgh, d1$imgw, bbox = bbox)
# spo=new("SpatialPhotoOverlay", filename = nombre, pixmap = pmap, exif.info = dexif, PhotoOverlay=overlay, sp = dat2[1,])
# kml_layer(spo, method="PhotoOverlay")
# kml_close("test.kml")

