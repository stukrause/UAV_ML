pacman::p_load(raster, rgdal, parallel, snow, terra)



############################################################################################################
# import pheno
# 2019
#buche2 <- readOGR("D:/process/Krause/input/shapes/boundbuche.shp")

traw <- read.csv("./2_data/csv/pheno2019.csv")
traw
doyt <- c("094", "105", "108", "113", "116", "123",
          "130", "137", "143", "147", "154", "158", "169")
doyt

trsubd <- subset(traw, (doy %in% doyt))

trsubt <- trsubd[!trsubd$geo_id == "420", ]

View(trsubt)

########################################################################
# merge with shapes
poly <- vect("./2_data/shapes/buche_crowns2020.shp")
poly

trsubt

output <- "./2_data/shapes2019/"
for (i in 1:length(trsubt)){
  #temp <- read.csv(trsubt[i])
  tr_mer <- merge(x = poly, y = trsubt,  by.x = "tree", 
                  by.y = "geo_id", all.y = TRUE) 
  #tr_mer@data <- tr_mer@data[, c(2, 1, 3, 4, 5, 6, 7)]
  writeVector(tr_mer, paste0(output, "crowns_2019_", doyt[i], ".shp"), 
           filetype = "ESRI Shapefile", overwrite = TRUE)
}
poly
tr_mer@data

########################

for (i in 1:length(polys)){
  rast <- rasload[[i]]
  poly <- readOGR(polys[i])
  uniqueClasses <- unique(poly@data$clss_fl)
  for (j in 1:length(uniqueClasses)){
    class_data <- subset(poly, clss_fl == uniqueClasses[j])
    classpts <- spsample(class_data, type = "random", n = 1000)
    classpts$class <- rep(uniqueClasses[j], length(classpts))
    if ( j == 1) {
      xy <- classpts
    } else{
      xy <- rbind(xy, classpts)
    }
  }
  trainvalsa <- extract(rast, xy, cellnumbers = TRUE)
  trainvalsa <- data.frame(response = xy$class, trainvalsa)
  if (i == 1) {  
    trainvals <- as.data.frame(trainvalsa)
  } else {
    #change to rbind
    trainvals <- rbind.na(trainvals, trainvalsa)
  }
}













for (i in 1:length(polys)){
  rast <- rasload[[i]]
  poly <- readOGR(polys[i])
  uniqueClasses <- unique(poly@data$clss_fl)
  for (j in 1:length(uniqueClasses)){
    class_data <- subset(poly, clss_fl == uniqueClasses[j])
    classpts <- spsample(class_data, type = "random", n = 1000)
    classpts$class <- rep(uniqueClasses[j], length(classpts))
    if ( j == 1) {
      xy <- classpts
    } else{
      xy <- rbind(xy, classpts)
    }
  }
  trainvalsa <- extract(rast, xy, cellnumbers = TRUE)
  trainvalsa <- data.frame(response = xy$class, trainvalsa)
  if (i == 1) {  
    trainvals <- as.data.frame(trainvalsa)
  } else {
    #change to rbind
    trainvals <- rbind.na(trainvals, trainvalsa)
  }
}