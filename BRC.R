## Everything in this file and any files in the R directory are sourced during `simInit()`;
## all functions and objects are put into the `simList`.
## To use objects, use `sim$xxx` (they are globally available to all modules).
## Functions can be used inside any function that was sourced in this module;
## they are namespaced to the module, just like functions in R packages.
## If exact location is required, functions will be: `sim$.mods$<moduleName>$FunctionName`.
defineModule(sim, list(
  name = "BRC",
  description = "This module is designed to calculate the mean and variance among bootstrap replicates of the Boreal Avian Modelling Project's National Models of bird density",
  keywords = "",
  authors = structure(list(list(given = c("R", "Isolde"), family = "Lane-Shaw", role = c("aut", "cre"), email = "r.i.lane.shaw@gmail.com", comment = NULL)), class = "person"),
  childModules = character(0),
  version = list(BRC = "0.0.0.9000"),
  timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = "year",
  citation = list("citation.bib"),
  documentation = list("README.md", "BRC.Rmd"), ## same file
  reqdPkgs = list("SpaDES.core", "raster", "data.table", "rgdal", "sf",
                  "LandR", "googledrive", "plotrix", "ggpubr", "diptest", "nortest", "dplyr", "ggplot2", "tidyverse", "terra", "reshape2", "RColorBrewer", "rasterVis"),
  parameters = bindrows(
    #defineParameter("paramName", "paramClass", value, min, max, "parameter description"),
    defineParameter(".plots", "character", "screen", NA, NA,
                    "Used by Plots function, which can be optionally used here"),
    defineParameter(".plotInitialTime", "numeric", start(sim), NA, NA,
                    "Describes the simulation time at which the first plot event should occur."),
    defineParameter(".plotInterval", "numeric", NA, NA, NA,
                    "Describes the simulation time interval between plot events."),
    defineParameter(".saveInitialTime", "numeric", NA, NA, NA,
                    "Describes the simulation time at which the first save event should occur."),
    defineParameter(".saveInterval", "numeric", NA, NA, NA,
                    "This describes the simulation time interval between save events."),
    defineParameter(".studyAreaName", "character", NA, NA, NA,
                    "Human-readable name for the study area used - e.g., a hash of the study",
                          "area obtained using `reproducible::studyAreaName()`"),
    defineParameter("archiveStudyArea", "character", NA, NA, NA,
                    "zip file that the study area shapefile is found in",
                    "area obtained using `reproducible::studyAreaName()`"),
    defineParameter("fromDrive", "logical", TRUE, NA, NA,
                    "Should the rasterToMatch, studyArea and bird Rasters be found on Google Drive or a similar online source? If false, they should already be on your local computer."),
    defineParameter("birdList", "character", NA, NA, NA,
                    "a list of bird species in the format of 4-letter bird codes"),
    defineParameter("rasterToMatchLocation", "character", NA, NA, NA,
                    "the file location of the rasterToMatch"),
    defineParameter("rasterToMatchName", "character", NA, NA, NA,
                    "the name of the rasterToMatch file"),
    defineParameter("studyAreaLocation", "character", NA, NA, NA,
                    "the file location of the studyArea"),
    defineParameter("nameBCR", "character", NA, NA, NA,
                    "the BAM regional model BCR region that the studyArea is located in"),
    defineParameter("bootRastersLocation", "character", NA, NA, NA,
                    "the file location of the BAM Regional Model Bootstraps"),
    defineParameter("outputBirdRastersFolder", "character", NA, NA, NA,
                    "the file location of the studyArea"),
    defineParameter("writeMeanRas", "logical", TRUE, NA, NA,
                    "should a raster of the mean value at each pixel location be created"),
    defineParameter("writeMedianRas", "logical", FALSE, NA, NA,
                    "should a raster of the median value at each pixel location be created"),
    defineParameter("writeSERas", "logical", FALSE, NA, NA,
                    "should a raster of the Standard Error at each pixel location be created"),
    defineParameter("writeSDRas", "logical", FALSE, NA, NA,
                    "should a raster of the standard deviation at each pixel location be created"),
    ## .seed is optional: `list('init' = 123)` will `set.seed(123)` for the `init` event only.
    defineParameter(".seed", "list", list(), NA, NA,
                    "Named list of seeds to use for each event (names)."),
    defineParameter(".useCache", "logical", FALSE, NA, NA,
                    "Should caching of events or module be used?")
  ),
  inputObjects = bindrows(
    #expectsInput("objectName", "objectClass", "input object description", sourceURL, ...),
    expectsInput(objectName = NA, objectClass = NA, desc = NA, sourceURL = NA),
    expectsInput("rasterToMatch", "SpatRaster", desc = "A raster used to determine projection and resolution of other spatial objects. Must cover all of the region covered by the studyArea"),
    expectsInput("studyArea", "SpatVector", desc = "Polygon to use as the study area.")
    #expectsInput(objectName = rasterToMatch, objectClass = "raster", desc = NA, sourceURL = NA)
  ),
  outputObjects = bindrows(
    #createsOutput("objectName", "objectClass", "output object description", ...),
    createsOutput(objectName = NA, objectClass = NA, desc = NA)
  )
))

## event types
#   - type `init` is required for initialization

doEvent.BRC = function(sim, eventTime, eventType) {
  switch(
    eventType,
    init = {
      ### check for more detailed object dependencies:
      ### (use `checkObject` or similar)

      # do stuff for this event
      sim <- Init(sim)
      
      # schedule future event(s)
      sim <- scheduleEvent(sim, P(sim)$.plotInitialTime, "BRC", "plot")
      sim <- scheduleEvent(sim, P(sim)$.saveInitialTime, "BRC", "save")
    },
    plot = {
      # ! ----- EDIT BELOW ----- ! #
      # do stuff for this event

      plotFun(sim) # example of a plotting function
      # schedule future event(s)

      # e.g.,
      #sim <- scheduleEvent(sim, time(sim) + P(sim)$.plotInterval, "BRC", "plot")

      # ! ----- STOP EDITING ----- ! #
    },
    save = {
      # ! ----- EDIT BELOW ----- ! #
      # do stuff for this event

      # e.g., call your custom functions/methods here
      # you can define your own methods below this `doEvent` function

      # schedule future event(s)

      # e.g.,
      # sim <- scheduleEvent(sim, time(sim) + P(sim)$.saveInterval, "BRC", "save")

      # ! ----- STOP EDITING ----- ! #
    },
    event1 = {
      # ! ----- EDIT BELOW ----- ! #
      # do stuff for this event

      # e.g., call your custom functions/methods here
      # you can define your own methods below this `doEvent` function

      # schedule future event(s)

      # e.g.,
      # sim <- scheduleEvent(sim, time(sim) + increment, "BRC", "templateEvent")

      # ! ----- STOP EDITING ----- ! #
    },
    event2 = {
      # ! ----- EDIT BELOW ----- ! #
      # do stuff for this event

      # e.g., call your custom functions/methods here
      # you can define your own methods below this `doEvent` function

      # schedule future event(s)

      # e.g.,
      # sim <- scheduleEvent(sim, time(sim) + increment, "BRC", "templateEvent")

      # ! ----- STOP EDITING ----- ! #
    },
    warning(paste("Undefined event type: \'", current(sim)[1, "eventType", with = FALSE],
                  "\' in module \'", current(sim)[1, "moduleName", with = FALSE], "\'", sep = ""))
  )
  return(invisible(sim))
}

## event functions
#   - keep event functions short and clean, modularize by calling subroutines from section below.

### template initialization
Init <- function(sim) {
  # # ! ----- EDIT BELOW ----- ! #

  # ! ----- STOP EDITING ----- ! #

  return(invisible(sim))
}

### template for save events
Save <- function(sim) {
  # ! ----- EDIT BELOW ----- ! #
  # do stuff for this event
  sim <- saveFiles(sim)

  # ! ----- STOP EDITING ----- ! #
  return(invisible(sim))
}

### template for plot events
plotFun <- function(sim) {
  # ! ----- EDIT BELOW ----- ! #
  # do stuff for this event
  sampleData <- data.frame("TheSample" = sample(1:10, replace = TRUE))
  Plots(sampleData, fn = ggplotFn)

  # ! ----- STOP EDITING ----- ! #
  return(invisible(sim))
}

### template for your event1
Event1 <- function(sim) {
  # ! ----- EDIT BELOW ----- ! #
  # THE NEXT TWO LINES ARE FOR DUMMY UNIT TESTS; CHANGE OR DELETE THEM.
  # sim$event1Test1 <- " this is test for event 1. " # for dummy unit test
  # sim$event1Test2 <- 999 # for dummy unit test

  # ! ----- STOP EDITING ----- ! #
  return(invisible(sim))
}

### template for your event2
Event2 <- function(sim) {
  # ! ----- EDIT BELOW ----- ! #
  # THE NEXT TWO LINES ARE FOR DUMMY UNIT TESTS; CHANGE OR DELETE THEM.
  # sim$event2Test1 <- " this is test for event 2. " # for dummy unit test
  # sim$event2Test2 <- 777  # for dummy unit test

  # ! ----- STOP EDITING ----- ! #
  return(invisible(sim))
}

.inputObjects <- function(sim) {
  # Any code written here will be run during the simInit for the purpose of creating
  # any objects required by this module and identified in the inputObjects element of defineModule.
  # This is useful if there is something required before simulation to produce the module
  # object dependencies, including such things as downloading default datasets, e.g.,
  # downloadData("LCC2005", modulePath(sim)).
  # Nothing should be created here that does not create a named object in inputObjects.
  # Any other initiation procedures should be put in "init" eventType of the doEvent function.
  # Note: the module developer can check if an object is 'suppliedElsewhere' to
  # selectively skip unnecessary steps because the user has provided those inputObjects in the
  # simInit call, or another module will supply or has supplied it. e.g.,
  # if (!suppliedElsewhere('defaultColor', sim)) {
  #   sim$map <- Cache(prepInputs, extractURL('map')) # download, extract, load file from url in sourceURL
  # }

  #cacheTags <- c(currentModule(sim), "function:.inputObjects") ## uncomment this if Cache is being used

  dPath <- asPath(getOption("reproducible.destinationPath", dataPath(sim)), 1)
  message(currentModule(sim), ": using dataPath '", dPath, "'.")

  # ! ----- EDIT BELOW ----- ! #
  
 sim$areaName <- P(sim)$.studyAreaName
 P(sim)$.studyAreaName <- paste(.studyAreaName, ".shp", sep = "")
  
  P(sim)$birdList <- sort(P(sim)$birdList)
   if (P(sim)$fromDrive == TRUE) {
   
#     ### GET FILES FROM GOOGLE DRIVE ###
# print("get rasterToMatch")
#        #get RasterToMatch
#    sim$rasterToMatch <-  googledrive::drive_download(
#                                 file = P(sim)$rasterToMatchLocation,
#                                 overwrite = TRUE,
#                                 verbose = TRUE,
#                                 path = file.path(inputsDir, "LCC2005_V1_4a.tif"))
#    sim$rasterToMatch <- terra::rast(file.path(inputsDir, "LCC2005_V1_4a.tif"))
#     
#  # get studyArea shapefile
#    print("get studyArea")
#      sim$studyArea <- prepInputs(targetFile = P(sim)$.studyAreaName,
#                            url = P(sim)$studyAreaLocation,
#                            archive = P(sim)$archiveStudyArea,
#                            alsoExtract = "similar", #Extract other files with similar names
#                            destinationPath = downloadFolderArea, #folder to download to
#                            #fun = "raster::shapefile", #use the function shapefile
#                            useTerra = TRUE,
#                            targetCRS = crs(sim$rasterToMatch), #make crs same as rasterToMatch
#                            overwrite = TRUE,
#                            verbose = TRUE)
# 
#    #crop and mask rasterTomatch to studyArea
#      sim$rasterToMatch <- terra::mask(terra::crop(sim$rasterToMatch, sim$studyArea), sim$studyArea)
#      names(sim$rasterToMatch) <- "rasterToMatch"
#    
#    # get boot bird rasters and produce 
#    print("get boot bird rasters")
#    lapply(X = P(sim)$birdList,
#           FUN = function(bird) {
#           
#             print(bird) 
#           patternNameBirdRaster <- paste("pred250-", bird, "-BCR_", P(sim)$nameBCR, "-boot-", sep = "") #NOTE: ONLY WORKS WITH FILE NAME CONVENTION pred250-XXX-BCR_XX-boot-
#             
#             ## drive_ls function is used to list all the files it finds using the folder url with the given pattern
#             filesToDownload <- 
#               googledrive::drive_ls(path = as_id(bootRastersLocation),
#                                     pattern = patternNameBirdRaster)
#             filesToDownload <- filesToDownload[order(filesToDownload$name),]
#            print(filesToDownload)
#             ## download the rasters to downloadsBRC folder
#             lapply(X = filesToDownload$name,
#                    FUN = function(rasterFile) {
#                      tryCatch({
#                        googledrive::drive_download(
#                        file = as_id(filesToDownload[filesToDownload$name %in% rasterFile,]$id),
#                        overwrite = TRUE,
#                        verbose = TRUE,
#                        path = file.path(inputBootBirdRasters,rasterFile))
#                      }, error = function(e) return(NULL))
#                    }
#             )
#             
#          
#             
#             bootNames <- list.files(path = inputBootBirdRasters)
#            
#             #postprocess bootRasters (crop and make CRS of rasterToMatch)
#             bootRasters <- lapply(X = bootNames, FUN = function(ras){
#               tryCatch({ 
#                             print(ras)
#                             bootRaster <- terra::rast(paste(inputBootBirdRasters, "/", ras, sep = ""))
#                             bootRaster <- postProcess(bootRaster,
#                                         filename2 = file.path(paste(inputBootBirdRasters, "/", ras, sep = "")),
#                                         overwrite = TRUE,
#                                         useTerra = TRUE,
#                                         fun = "terra::rast",
#                                         rasterToMatch = sim$rasterToMatch,
#                                         #studyArea = sim$studyArea,
#                                         verbose = TRUE
#                                         
#               )
#               
#               return(bootRaster)
#             }, error = function(e) return(NULL))
#             })
#             
#             names(bootRasters) <- bootNames
#             bootRasters[sapply(bootRasters, is.null)] <- NULL
#            
#             #find out number of boot Rasters
#             noRasters <- length(bootRasters)
#             print(paste(noRasters, " rasters for ", bird, sep = ""))
#             birdSp <- bird
#             noBootRasPerSp <- cbind(birdSp, noRasters)
#             
#             if(noRasters > 1){
#             
#                bootRasters <- terra::rast(bootRasters)
#             #   print("write mean and se raster")
#             # # write mean rasters
#             # meanRaster <- terra::app(bootRasters, fun= mean, verbose = TRUE)
#             # meanRasterName <- paste(bird, "-meanBoot", sep = "")
#             # names(meanRaster) <- meanRasterName
#             # terra::writeRaster(x = meanRaster, filename = file.path(paste(outputMeanBirdRasters,"/", bird, "-meanBoot_BCR-", P(sim)$nameBCR, "_", sim$areaName, sep = "")),
#             #             filetype= "GTiff",
#             #             gdal="COMPRESS=NONE",
#             #             overwrite = TRUE)
#             
#             print("write median and se raster")
#             # write mean rasters
#             medianRaster <- terra::app(bootRasters, fun= median, verbose = TRUE)
#             medianRasterName <- paste(bird, "-medianBoot", sep = "")
#             names(medianRaster) <- medianRasterName
#             terra::writeRaster(x = medianRaster, filename = file.path(paste(outputMedianBirdRasters,"/", bird, "-medianBoot_BCR-", P(sim)$nameBCR, "_", sim$areaName, sep = "")),
#                                filetype= "GTiff",
#                                gdal="COMPRESS=NONE",
#                                overwrite = TRUE)
#             
#             # write standard error rasters
#             seRaster <- terra::app(bootRasters, fun = std.error)
#             seRasterName <- paste(bird, "-seBoot", sep = "")
#             names(seRaster) <- seRasterName
#             terra::writeRaster(x = seRaster, filename = file.path(paste(outputMeanBirdRasters,"/", bird, "-seBoot_BCR-", P(sim)$nameBCR, "_", sim$areaName, sep = "")),
#                                filetype= "GTiff",
#                                gdal="COMPRESS=NONE",
#                         overwrite = TRUE)
#             } else {
#               
#               
#               bootRaster <- unlist(bootRasters, use.names = FALSE)
#               bootRaster <- bootRaster[[1]]
#               # meanRasterName <- paste(bird, "-meanBoot", sep = "")
#               # names(bootRaster) <- meanRasterName
#               # terra::writeRaster(x = bootRaster, filename = file.path(paste(outputMeanBirdRasters,"/", bird, "-meanBoot_BCR-", P(sim)$nameBCR, "_", sim$areaName, sep = "")),
#               #             filetype= "GTiff",
#               #             gdal="COMPRESS=NONE",
#               #             overwrite = TRUE)
#               
#               medianRasterName <- paste(bird, "-medianBoot", sep = "")
#               names(bootRaster) <- medianRasterName
#               terra::writeRaster(x = bootRaster, filename = file.path(paste(outputMedianBirdRasters,"/", bird, "-medianBoot_BCR-", P(sim)$nameBCR, "_", sim$areaName, sep = "")),
#                                  filetype= "GTiff",
#                                  gdal="COMPRESS=NONE",
#                                  overwrite = TRUE)
#             }
#             
#             # # ### DANGER!!!
#             # # get all files in the directories, recursively
#             # toDelete <- list.files(inputBootBirdRasters, include.dirs = F, full.names = T, recursive = T)
#             # # remove the files
#             # file.remove(toDelete)
# 
#             return(noBootRasPerSp) 
#           }
#           
#    )
   
  } else {
    
  ### GET FILES FROM LOCAL LOCATION ###
  print("get files from local folder")
    #browser()
   
    #get rasterToMatch
    if (!suppliedElsewhere("rasterToMatch", sim)) {
      print("get rasterToMatch")
      sim$rasterToMatch <- terra::rast(file.path(P(sim)$rasterToMatchLocation, P(sim)$rasterToMatchName))
    }
   
   
  
  
  #get StudyArea shapefile
   if (!suppliedElsewhere("studyArea", sim)) {
    print("get studyArea")
    sim$studyArea <- terra::vect(file.path(P(sim)$studyAreaLocation, P(sim)$.studyAreaName ))
  }
    sim$studyAreaFolder <- checkPath(file.path(Paths$inputPath, "studyArea"), create = TRUE)
   
    #postProcess studyArea
    sim$studyArea <- reproducible::postProcess(sim$studyArea,
                                               destinationPath = sim$studyAreaFolder,
                                               #filename2 = "studyArea",            
                                               fun = "terra::vect", #use the function vect
                                               useTerra = TRUE,
                                               targetCRS = crs(sim$rasterToMatch), #make crs same as rasterToMatch
                                               overwrite = TRUE,
                                               verbose = TRUE)
    
  #crop and mask rasterTomatch to studyArea
  sim$rasterToMatch <- terra::mask(terra::crop(sim$rasterToMatch, sim$studyArea), sim$studyArea)
  names(sim$rasterToMatch) <- "rasterToMatch"
  
  
  
  #produce mean rasters using getMeanRastersFunction
  print("get combined bird rasters")
  
  BRCombine <- function(bird) {
  #tryCatch({
    
    print(bird)
    
    patternNameBirdRaster <- paste("pred250-", bird, "-BCR_", P(sim)$nameBCR, "-boot-", sep = "") 
    bootNames <- list.files(path = P(sim)$bootRastersLocation, pattern = patternNameBirdRaster)
    
    
    bootRasters <- lapply(X = bootNames, FUN = function(ras){
      tryCatch({
        
        print(ras)
        bootRaster <- terra::rast(paste(P(sim)$bootRastersLocation, "/", ras, sep = ""))
        bootRaster <- postProcessTerra(from = bootRaster,
                                       to = sim$rasterToMatch,
                                       overwrite = TRUE,
                                       verbose = TRUE)
        
       print("raster postprocessed")
        return(bootRaster)
      }, error = function(e) return(NULL))
    })
    
    
  
    #find out number of boot Rasters
    noRasters <- length(bootRasters)
    print(paste(noRasters, " rasters for ", bird, sep = ""))
    birdSp <- bird
    noBootRasPerSp <- as.data.frame(cbind(birdSp, noRasters))
    
    print("write combined rasters")
    if(noRasters > 1){
      tryCatch({
      bootRasters <- terra::rast(bootRasters)
    
      if (P(sim)$writeMeanRas == TRUE) {
      # write mean rasters
      meanRaster <- terra::app(bootRasters, fun= mean, verbose = TRUE)
      meanRasterName <- paste(bird, "-meanBoot_", P(sim)$nameBCR, sep = "")
      names(meanRaster) <- meanRasterName
      #Plot(meanRaster, main = meanRasterName)
      terra::writeRaster(x = meanRaster, filename = file.path(paste(P(sim)$outputBirdRastersFolder,"/", bird, "-meanBoot_BCR-", P(sim)$nameBCR, "_", sim$areaName, sep = "")),
                         filetype= "GTiff",
                         gdal="COMPRESS=NONE",
                         overwrite = TRUE)
      }
      
      if (P(sim)$writeMedianRas == TRUE) {
        # write mean rasters
        medianRaster <- terra::app(bootRasters, fun= median, verbose = TRUE)
        medianRasterName <- paste(bird, "-medianBoot_", P(sim)$nameBCR, sep = "")
        names(medianRaster) <- medianRasterName
        #Plot(medianRaster, main = medianRasterName)
        terra::writeRaster(x = medianRaster, filename = file.path(paste(P(sim)$outputBirdRastersFolder,"/", bird, "-medianBoot_BCR-", P(sim)$nameBCR, "_", sim$areaName, sep = "")),
                           filetype= "GTiff",
                           gdal="COMPRESS=NONE",
                           overwrite = TRUE)
      }
      
      if (P(sim)$writeSERas == TRUE) {
      # write standard error rasters
      seRaster <- terra::app(bootRasters, fun = std.error)
      seRasterName <- paste(bird, "-seBoot_", P(sim)$nameBCR, sep = "")
      names(seRaster) <- seRasterName
      #Plot(seRaster, main = seRasterName)
      terra::writeRaster(x = seRaster, filename = file.path(paste(P(sim)$outputBirdRastersFolder,"/", bird, "-seBoot_BCR-", P(sim)$nameBCR, "_", sim$areaName, sep = "")),
                         filetype= "GTiff",
                         gdal="COMPRESS=NONE",
                         overwrite = TRUE)
      }
      
      if (P(sim)$writeSDRas == TRUE) {
        # write mean rasters
        sdRaster <- terra::app(bootRasters, fun= sd, verbose = TRUE)
        sdRasterName <- paste(bird, "-sdBoot_", P(sim)$nameBCR, sep = "")
        names(sdRaster) <- sdRasterName
        #Plot(seRaster, main = seRasterName)
        terra::writeRaster(x = sdRaster, filename = file.path(paste(P(sim)$outputBirdRastersFolder,"/", bird, "-sdBoot_BCR-", P(sim)$nameBCR, "_", sim$areaName, sep = "")),
                           filetype= "GTiff",
                           gdal="COMPRESS=NONE",
                           overwrite = TRUE)
      }
      
      print("combined rasters written")
      
      }, error = function(e) return(NULL))
    } else {
      tryCatch({
      bootRaster <- unlist(bootRasters, use.names = FALSE)
      bootRaster <- bootRaster[[1]]
      
      if (P(sim)$writeMeanRas == TRUE) {
      meanRasterName <- paste(bird, "-meanBoot", sep = "")
      names(bootRaster) <- meanRasterName
      #Plot(meanRaster, main = meanRasterName)
      terra::writeRaster(x = bootRaster, filename = file.path(paste(P(sim)$outputBirdRastersFolder,"/", bird, "-meanBoot_BCR-", P(sim)$nameBCR, "_", sim$areaName, sep = "")),
                         filetype= "GTiff",
                         gdal="COMPRESS=NONE",
                         overwrite = TRUE)
      }
      
      if (P(sim)$writeMedianRas == TRUE) {
        medianRasterName <- paste(bird, "-medianBoot", sep = "")
        names(bootRaster) <- medianRasterName
        #Plot(medianRaster, main = medianRasterName)
        terra::writeRaster(x = bootRaster, filename = file.path(paste(P(sim)$outputBirdRastersFolder,"/", bird, "-medianBoot_BCR-", P(sim)$nameBCR, "_", sim$areaName, sep = "")),
                           filetype= "GTiff",
                           gdal="COMPRESS=NONE",
                           overwrite = TRUE)
      }
    }, error = function(e) return(NULL))
    }
    
    return(noBootRasPerSp)
  #}, error = function(e) return(NULL))  
}

  print("do BRCombine and make summaryBRCTab")
  summaryBRC <- lapply(X = P(sim)$birdList,
                  FUN = BRCombine)
 
  sim$summaryBRC <- rbindlist(summaryBRC)
  write.csv(sim$summaryBRC, file =  file.path(P(sim)$outputBirdRastersFolder, paste("summaryBRC_",  P(sim)$nameBCR, ".csv", sep = "")))

  }
  # ! ----- STOP EDITING ----- ! #
  return(invisible(sim))
}

ggplotFn <- function(data, ...) {
  ggplot(data, aes(TheSample)) +
    geom_histogram(...)
}

### add additional events as needed by copy/pasting from above
