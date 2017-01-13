#Photo geotag to KML script
#@author: andrew brown
#@version: 0.2b; 1/13/17

### install & load required packages, if needed
packz <- c("sp","rgdal","stringr","pixmap","RCurl","utils","magick")
newpackz <- packz[!(packz %in% installed.packages()[,"Package"])]
if(length(newpackz)) install.packages(newpackz)
loaded <- lapply(packz,FUN=require,character.only=TRUE)
if(sum(as.numeric(loaded))!=length(packz)) {
  stop("Failed to load one or more required packages!")
  geterrmessage()
}

###SETUP###
#User-defined settings
threshold <- 50               #meters; maximum distance between points for cluster membership and cluster naming from DP layer

make_kmz <- TRUE              #requires WinZip. Creates a standalone file containing KML and images

scaling_factor <- "25%"       #string supplied to magick::image_scale() for adjusting image output

name_by_nearby_point <- TRUE  #default: FALSE; creates a new site ID for each cluster of photos
                              # if TRUE allows a separate shapefile of points to be loaded (e.g. exported from GPS, dp layer, NASIS sites etc). 
                              # if one of the points in that feature class falls within the specified threshold then the name of that point will
                              # be used for the corresponding cluster. orherwise it will just get the next available numeric value.

placemark_names <- NA         #default is NA; uses either numeric or nearby point names. Need to know a priori the number of clusters.
                              # alternately can specify a vector containing pre-defined site IDs. if this is non-NA, no other naming scheme will be used. 
                            
placemark_postfix_start <- 53 #default numbering starts from 1. Change to the first site ID number used for this picture set. 
                              # Sites will be incremented based on the temporal order of the pictures.

placemark_prefix <- "2016CA63060" #default: ""; string to precede the site ID number. can be used to make e.g. NASIS user site IDs. 

point_source = 'L:/CA630/FG_CA630_OFFICIAL.gdb' #path to feature class containing existing site points for labeling clusters

point_layer = 'ca630_dp' #what layer to use within geodatabase. supplied to rgdal::readOGR()

centroid_function <- mean     #default: mean; function to use for aggregating x,y,z data from multiple images in a cluster;

script_dir <- "E:/scripts/geotag_to_KML/"    #path to script directory (e.g. git repository instance)

image_directory <- paste0(script_dir,"/testimages/")

output_path <- paste0(script_dir,"~testsorted") #this is the path to KML/KMZ output and "sorted" site folders

#Implementation specific parameters
template_file <- 'AvenzaMaps_kml_template.dat'
device_projection <- '+proj=longlat +datum=WGS84'           #projection information for data extracted from EXIF

#Script Paths

#External dependencies
exiftool_path <- paste0(script_dir,"exiftool(-k).exe")          #this executable is required for extracting EXIF data from JPGs
winzip_path <- "\"C:\\Program Files (x86)\\WinZip\\wzzip.exe\"" #winzip is used for creating KMZ files

if(name_by_nearby_point)
  dp_points <- readOGR(dsn = point_source, layer = point_layer, stringsAsFactors=FALSE) 
if(!dir.exists(output_path))
  dir.create(output_path,showWarnings=FALSE,recursive=TRUE)
###########

###INTERNAL SETUP###
degms_regex <- "([0-9]+) deg ([0-9]+)' ([0-9]+\\.[0-9]+)\" (.)" # pattern for capturing degrees, min and sec for conversion to decimal degrees
elev_regex <- "([0-9\\.]+) m.*"                             # pattern for capturing elevation numeric value from EXIF string
###########

### FUNCTION DEFINITIONS ###
getDecDegrees = function(s) {
  ff <- str_match(s,degms_regex)
  ff1 <- as.numeric(ff[2:4])
  hemi <- ff[5]
  sign=1
  if(hemi == "S" || hemi == "W")  sign = -1
  return(sign*(ff1[1] + (ff1[2]/60) + (ff1[3]/3600)))
  #Decimal degrees = Degrees + (Minutes/60) + (Seconds/3600)
}

getNumericElevation = function(s) {
  ff <- str_match(s,elev_regex)
  return(as.numeric(ff[2]))
}

makePhotoStringByCentroid=function(x,usecdata=TRUE) {
  #takes vector of file names and creates CDATA string for export
  imagez <- paste0("<img src=\"./",x,"\" />")
  buf=""
  if(usecdata)
    buf <- "<![CDATA["
  for(i in imagez) {
    buf <- paste0(buf,i,"<br />")
  }
  if(usecdata)
    buf <- paste0(buf,"]]>")
  return(buf)
}

makePlacemarkByCentroid = function(x,n) {
  #takes a SpatialPointsDataFrame (or subset of one) and creates a placemark for it, using first coordinate
  #x is the data, n is a unique id to use for the name field (probably an integer unless otherwise specified)
  datetime <- x$date[1]
  photostring <- makePhotoStringByCentroid(x$filename)
  buf <- ""
  buf <- paste0("\t\t\t<Placemark>\n\t\t\t\t",paste0("<name>",n,"</name>\n\t\t\t\t"))
  buf <- paste0(buf,"<TimeStamp><when>",datetime,"</when></TimeStamp>\n\t\t\t\t")
  buf <- paste0(buf,"<description>",makePhotoStringByCentroid(x$filename,usecdata=TRUE),"</description>\n\t\t\t\t")
  buf <- paste0(buf,"<ExtendedData><SchemaData schemaUrl=\"#schema0\"><SimpleData name=\"pdfmaps_photos\">",photostring,"</SimpleData></SchemaData></ExtendedData>\n\t\t\t\t")
  buf <- paste0(buf,"<Point><coordinates>",x$clng[1],",",x$clat[1],",",floor(x$celev[1]),"</coordinates></Point>\n\t\t\t</Placemark>")
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
distz <- spDists(dat,longlat=TRUE)*1000 #gives distance between points in meters (KM*1000); 
#      may generate warning due to issues with different projections and calculation of distance
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
  dat@data[who,]$clng <- c_lng[i]
  dat@data[who,]$celev <- c_elev[i]
}
dat@data$filename <- as.character(dat@data$filename)
dat2=dat@data                         #make a copy of the data we have updated
coordinates(dat2) <- ~clng+clat+celev #elevate the copy to SpatialPointsDataFrame, this time using the centroid values for points. 
                                      #each record still retains individual locations.
proj4string(dat2) <- device_projection

ncclust <- levels(factor(dat2$centroid))
placemark_names <- c()

if(name_by_nearby_point) {
  dp_points_tagged <- 0
  #try to name centroids based on DP layer points within threshold distance
  dat_dp <- spTransform(dat2,proj4string(dp_points)) # convert to CRS of dp layer
  for(ddp in 1:length(ncclust)) {
    sdat_dp <- dat_dp[which(dat_dp$centroid == ddp),]
    dpdistz <- spDistsN1(pt=coordinates(sdat_dp)[1,1:2],pts=dp_points) #calculates distances from all centroids to all dp points
    dpid <- which(dpdistz==min(dpdistz))[1] #if multiple meet the threshold, take the first (closest)
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
if(is.na(placemark_names) && placemark_postfix_start > 0) { 
  # if the placemark name list is NA then we will just number them from start to start+number of clusters
  placemark_names <- placemark_postfix_start:(placemark_postfix_start+length(ncclust)) 
  placemark_names <- paste0(placemark_prefix, placemark_names) #adds the prefix, default is ""
}

for(j in as.numeric(ncclust)) {
  who <- which(dat2$centroid == j)
  subse <- dat2[who,]
  placemarkz <- paste0(placemarkz,makePlacemarkByCentroid(subse,placemark_names[j]),"\n") 
  #TODO: fail elegantly if there aren't enough names for centroids;
}
makeKML(paste0(output_path,"/doc.kml"),placemarkz,foldername)

#make KMZ file
if(make_kmz) {
  dir.create(paste0(output_path,"/images"),recursive=TRUE,showWarnings=FALSE)
  for(f in 1:length(filez)) {
    #instead of using system copy, use magick to read in source, resize and write to target directory
    #file.copy(filez[f],paste0(output_path,"\\images\\",dat2$filename[f]))
    img <- image_read(filez[f])
    img_s <- image_scale(img,scaling_factor)
    image_write(image=img_s,path=paste0(output_path,"\\images\\",dat2$filename[f]))
  }
  system(paste0(winzip_path, " ",output_path,"/",foldername,".kmz \"",output_path,"/*.*\""," \"",output_path,"/images/*.*\""))
}

#make a folder for each cluster, named by site id/placemark name
for(p in 1:length(ncclust)) {
  clust <- ncclust[p]
  outdir <- paste0(output_path,"/",placemark_names[p])
  dir.create(outdir,recursive=TRUE,showWarnings=FALSE)
  who <- which(dat2$centroid == p)
  subse <- dat2[who,]
  for(s in 1:length(subse)) {
    #instead of using system copy, use magick to read in source, resize and write to target directory
    #file.copy(paste0(subse[s,]$path),paste0(outdir,"/",subse[s,]$filename))
    img <- image_read(paste0(subse[s,]$path))
    img_s <- image_scale(img,scaling_factor)
    image_write(image=img_s,path=paste0(outdir,"/",subse[s,]$filename))
  }
}