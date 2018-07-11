library(camtrapR)
library(dplyr)
library(secr)

#rm(list=ls()) #removes objects
#.rs.restartR() #restarts R

################################# Organizing Raw Images ############################################

#vector containing subfolders for locations
location<-c("/Lingthi nalla", "/Pin Valley", "/Tabo", "/Upper Kinnaur", "/Upper Spiti Landscape")

#vector containing subfolders for years within these locations
location.year<-c("/2011", "/2012", "/2013", "/2014", "/2015", "/2016","/2017")

#setting up variable directories using above defined location vectors
setwd(paste0("/media/kulbushan/_hdd2/Camera Trap Data/Camera trap database",location[4]))
setwd(paste0(getwd(),location.year[7]))

#reading .csv files within subfolders and assigning them variable names
files<-list.files(pattern = "trap_info_kinnaur.csv")
ct_dat<-read.csv(files, header=T)
View(ct_dat)
#Tabo images have been renamed with Station names
#Tabo date time has been fixed

fixDateTimeOriginal(inDir = getwd(), recursive = TRUE) 

##############################Species and Individual Identification Using DigiKam###############################################

checkSpeciesIdentification(inDir = getwd(), 
                           IDfrom = "metadata", 
                           hasCameraFolders = FALSE,
                           metadataSpeciesTag = "Species",
                           metadataSpeciesTagToCompare =,
                           metadataHierarchyDelimitor = "|",
                           maxDeltaTime = 20, 
                           writecsv = FALSE)

#8 Create a species record table from camera trap images

species.kinnaur.2017<-recordTable(inDir= getwd(),
                              IDfrom = "metadata",
                              cameraID = "filename",
                              camerasIndependent = TRUE,
                              exclude = c("Unidentified","Chukar","Snowcock","SLK","Pika","Other Birds","Rodent","People"),
                              minDeltaTime = 60,
                              deltaTimeComparedTo = "lastIndependentRecord",
                              timeZone = "Asia/Kolkata",
                              stationCol = "Station",
                              writecsv = TRUE,
                              outDir = getwd(),
                              metadataHierarchyDelimitor = "|",
                              metadataSpeciesTag = "Species",
                              removeDuplicateRecords = TRUE)

View(species.kinnaur.2017)

#################################### Data Exploration and Visualization ##############################################

#9 Exploratory Analysis, Data Visualization

detectionMaps(CTtable = ct_dat, recordTable = species.kinnaur.2017, Xcol = "utm_x", Ycol = "utm_y", stationCol = "Station",
              speciesCol = "Species", richnessPlot = TRUE, speciesPlots = TRUE, addLegend = TRUE,  printLabels = FALSE, plotR = TRUE,
              writePNG = FALSE, plotDirectory = getwd())

View(ct_dat)
View(species.kinnaur.2017)
length(unique(ct_dat$Station))
length(unique(species.kinnaur.2017$Station))

any(!unique(species.kinnaur.2017$Station %in% ct_dat$Station))    # is any value of data$Station NOT in ct_dat$Station?
which(!(species.kinnaur.2017$Station %in% ct_dat$Station)  # which value of data$Station is NOT in ct_dat$Station?
      
      
      activityHistogram(recordTable=species.kinnaur.2017, 
                        allSpecies = TRUE, 
                        speciesCol = "Species", 
                        recordDateTimeCol = "DateTimeOriginal", 
                        recordDateTimeFormat = "%Y-%m-%d %H:%M:%S")
      
      
      Report<-surveyReport(recordTable = species.kinnaur.2017, CTtable =  ct_dat, speciesCol = "Species", stationCol = "Station", setupCol = "Setup_Date",
                           retrievalCol = "Retrival_Date", CTDateFormat = "%Y-%m-%d",  CTHasProblems = FALSE, recordDateTimeCol = "DateTimeOriginal",
                           recordDateTimeFormat = "%Y-%m-%d %H:%M:%S", Xcol = "utm_x", Ycol = "utm_y", sinkpath = "/media/kulbushan/_hdd2/Camera Trap Data/Analysis Files/Upper Kinnaur_2017", makezip = TRUE)
      
      Report
      ######################
      ################################# Prepairing Input for Occupancy Analysis ##############################################
      
      # Create Camera Opretability Index
      
      # Trap_info Contains StationID, utm_y, utm_x, Setup and Retrival dates
      
      CamMatrix<-cameraOperation(CTtable = ct_dat, stationCol = "Station", setupCol = "Setup_Date", retrievalCol = "Retrival_Date", 
                                 byCamera = F, allCamsOn = TRUE, dateFormat = "%Y-%m-%d", hasProblems = FALSE, writecsv = TRUE, 
                                 outDir = getwd())
      
      View(CamMatrix)
      View(ct_dat)
      # Species detection histories for occupcancy analysis 
      
      ######################################################################################
      
      length(species.kinnaur.2017$Station[which(!species.kinnaur.2017$Station %in% ct_dat$Station)])  # show that value in data
      length(ct_dat$Station[which(!ct_dat$Station %in% species.kinnaur.2017$Station)])  # show that value in ct_dat
      
      Vulpes.DH<-detectionHistory(recordTable = species.kinnaur.2017,
                                  species = "Red Fox",
                                  camOp = CamMatrix,
                                  stationCol = "Station",
                                  speciesCol = "Species",
                                  recordDateTimeCol = "DateTimeOriginal",
                                  recordDateTimeFormat = "%Y-%m-%d %H:%M:%S",
                                  occasionLength = 1,
                                  day1 = "2017-03-17",
                                  maxNumberDays = 55,
                                  includeEffort = TRUE,
                                  scaleEffort = FALSE,
                                  occasionStartTime = 12,
                                  datesAsOccasionNames = FALSE,
                                  timeZone = "Asia/Kolkata",
                                  writecsv = FALSE)
      write.csv(Vulpes.DH, file = "Vulpes.DH.csv")
      
      #7  For Capture-recapture analysis export all pictures belonging to a single species to a species folder . eg "Snow Leopard"
      
      #8 Create Individual Record Table using Folder "Snow Leopard" with all metataged images of snow leopards. 
      
      SL.kinnaur.2017<-recordTableIndividual(inDir = "/media/kulbushan/_hdd2/Camera Trap Data/Camera trap database/Upper Kinnaur/Snow leopard",
                                         minDeltaTime = 60, deltaTimeComparedTo = "lastIndependentRecord", 
                                         hasStationFolders = FALSE, IDfrom = "metadata", writecsv = TRUE, 
                                         outDir = "/media/kulbushan/_hdd2/Camera Trap Data/Camera trap database/Upper Kinnaur", 
                                         metadataHierarchyDelimitor = "|", metadataIDTag = "Individuals", 
                                         timeZone = "Asia/Kolkata", removeDuplicateRecords =  TRUE)
      
      View(SL.kinnaur.2017)
      
      #################################### Prepaiting Input for Spatially-Explicit Capture-Recapture Analysis##################################
      
      #10 Creating Individual Capture History
      
      input_secr<-spatialDetectionHistory(recordTableIndividual =  SL.kinnaur.2017, species = "Snow leopard", camOp = CamMatrix, CTtable = ct_dat, output = "binary",
                                          stationCol = "Station", speciesCol = "Species", Xcol = "utm_x", Ycol = "utm_y", individualCol = "Individual", 
                                          recordDateTimeCol = "DateTimeOriginal", recordDateTimeFormat = "%Y-%m-%d %H:%M:%S", occasionLength = 1, occasionStartTime = 12, 
                                          day1 = "survey", includeEffort = TRUE, binaryEffort = TRUE, timeZone = "Asia/Kolkata")
      
      
      RmarkInp<-RMarkInput(input_secr, covariates = FALSE)
      ?write.captures()
      # Secr Analysis
      
      # Create Model With Traping Effort Specified
      #library(secr)
      
      summary(input_secr, terse = TRUE)
      
      mask <- make.mask(traps(input_secr), buffer = 28000, type = 'trapbuffer')
      fit<-secr.fit(input_secr, mask = mask, model = g0~1, start = list(D=.00002, g0= 0.02, sigma= 25000), trace = FALSE)
      predict(fit)
      print(fit)
      
      # 
      confint(fit, parm = "D", level = .95)#################################### Data Exploration and Visualization ##############################################
      
      #9 Exploratory Analysis, Data Visualization
      
      
      # Simulation 
      
      traplayout<-traps(input_secr)
      View(traplayout)
      estimates<-c(.00010,.000059,4234)
      estimates<-c(1,1,4234)
      params<-estimates
      
      ####################################################################################################
      ####################################################################################################
      ####################################################################################################
      ####################################################################################################
      ####################################################################################################
      ####################################################################################################
      ####################################################################################################
      ####################################################################################################
      
      detections <- sim.capthist(traplayout, noccasions = 69,
                                 popn = list(D = params[1], buffer = 28000),
                                 detectpar = list(g0 = params[2], sigma = params[3]),binomN=0)
      
      #estimates of parameters D= density/ha, g0, sigma
      #used to get initial values for SECR
      beta<-log(estimates)
      #estimate parameters with constant parameter model
      possum.est <- secr.fit(detections,start=beta,mask=mask)
      predict(possum.est)
      
      #########################################################################################################
      
      # SECR with Habitat Mask _ Creating Habitat Mask
      
      library('maptools')
      
      # Correct Projection of Non-Habitat Layer
      
      Kinnaur_NH <-readShapePoly("QGIS/Non-Habitat.shp", delete_null_obj = TRUE, proj4string=CRS("+proj=longlat"))
      proj4string(Kinnaur_NH)
      Kinnaur_NH_utm<-spTransform(Spiti_NH, CRS("+proj=utm +zone=43 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"))
      plot(Spiti_NH_utm)
      
      # Create habitat Mask
      
      habitat_mask <- make.mask(traps(input_secr), buffer = 28000, type = "trapbuffer", poly = Spiti_NH_utm, poly.habitat = FALSE)
      plot (habitat_mask, ppoly = FALSE, col = "grey", pch = 16, cex = 0.6, add = T)
      plot(traps(input_secr), add = T)
      
      # SECR Analysis
      
      fit2<-secr.fit(input_secr, mask = habitat_mask, model = g0~1, trace = FALSE)
      predict(fit2)
      print(fit2)
      
      
      # density Surface
      
      
      dsurf<-fx.total(fit)
      par(mar=c(1,5,1,8))
      plot(dsurf, covariate = 'D.fx', poly = TRUE, breaks = seq(0,.0018, .0003 ))
      plot(traps(input_secr), add = T)
      fx.total()
      