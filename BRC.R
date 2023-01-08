## Everything in this file and any files in the R directory are sourced during `simInit()`;
## all functions and objects are put into the `simList`.
## To use objects, use `sim$xxx` (they are globally available to all modules).
## Functions can be used inside any function that was sourced in this module;
## they are namespaced to the module, just like functions in R packages.
## If exact location is required, functions will be: `sim$.mods$<moduleName>$FunctionName`.
defineModule(sim, list(
  name = "BRC",
  description = "This module is designed to calculate the mean and standard error among bootstrap replicates of the Boreal Avian Modelling Project's National Models of bird density",
  keywords = "",
  authors = structure(list(list(given = c("R", "Isolde"), family = "Lane Shaw", role = c("aut", "cre"), email = "r.i.lane.shaw@gmail.com", comment = NULL)), class = "person"),
  childModules = character(0),
  version = list(BRC = "0.0.0.9000"),
  timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = "year",
  citation = list("citation.bib"),
  documentation = list("README.md", "BRC.Rmd"), ## same file
  reqdPkgs = list("PredictiveEcology/SpaDES.core@development (>= 1.1.0.9017)", "ggplot2", "raster", "data.table", "rgdal", "sf",
                  "LandR", "googledrive", "plotrix", "ggpubr", "diptest", "nortest", "dplyr", "tidyverse", "terra", "reshape2", "RColorBrewer", "rasterVis"),
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
    defineParameter("studyAreaLocation", "character", NA, NA, NA,
                    "the file location of the studyArea"),
    defineParameter("nameBCR", "character", NA, NA, NA,
                    "the BAM regional model BCR region that the studyArea is located in"),
    defineParameter("bootRastersLocation", "character", NA, NA, NA,
                    "the file location of the BAM Regional Model Bootstraps"),
    ## .seed is optional: `list('init' = 123)` will `set.seed(123)` for the `init` event only.
    defineParameter(".seed", "list", list(), NA, NA,
                    "Named list of seeds to use for each event (names)."),
    defineParameter(".useCache", "logical", FALSE, NA, NA,
                    "Should caching of events or module be used?")
  ),
  inputObjects = bindrows(
    #expectsInput("objectName", "objectClass", "input object description", sourceURL, ...),
    expectsInput(objectName = NA, objectClass = NA, desc = NA, sourceURL = NA),
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
  
   if (P(sim)$fromDrive == TRUE) {
   
    ### GET FILES FROM GOOGLE DRIVE ###

       #get RasterToMatch
   sim$rasterToMatch <-  googledrive::drive_download(
                                file = P(sim)$rasterToMatchLocation,
                                overwrite = TRUE,
                                verbose = TRUE,
                                path = file.path(inputsDir, "LCC2005_V1_4a.tif"))
   sim$rasterToMatch <- raster::raster(file.path(inputsDir, "LCC2005_V1_4a.tif"))
    
 # get studyArea shapefile
     sim$studyArea <- prepInputs(targetFile = .studyAreaName,
                           url = P(sim)$studyAreaLocation,
                           archive = P(sim)$archiveStudyArea,
                           alsoExtract = "similar", #Extract other files with similar names
                           destinationPath = downloadFolderArea, #folder to download to
                           fun = "raster::shapefile", #use the function shapefile
                           targetCRS = crs(sim$rasterToMatch), #make crs same as rasterToMatch
                           overwrite = TRUE,
                           verbose = TRUE)

   #crop and mask rasterTomatch to studyArea
   sim$rasterToMatch <- raster::crop(sim$rasterToMatch, sim$studyArea)
   sim$rasterToMatch <- raster::mask(sim$rasterToMatch, sim$studyArea)
   
   # get boot bird rasters and produce 
   lapply(X = P(sim)$birdList,
          FUN = function(bird) {
          
            print(bird) 
          patternNameBirdRaster <- paste("pred250-", bird, "-BCR_", P(sim)$nameBCR, "-boot-", sep = "") #NOTE: ONLY WORKS WITH FILE NAME CONVENTION pred250-XXX-BCR_XX-boot-
            
            ## drive_ls function is used to list all the files it finds using the folder url with the given pattern
            filesToDownload <- 
              googledrive::drive_ls(path = as_id(bootRastersLocation),
                                    pattern = patternNameBirdRaster)
           
            
            ## download the rasters to downloadsBRC folder
            lapply(X = filesToDownload$name,
                   FUN = function(rasterFile) {
                     googledrive::drive_download(
                       file = as_id(filesToDownload[filesToDownload$name %in% rasterFile,]$id),
                       overwrite = TRUE,
                       verbose = TRUE,
                       path = file.path(inputBootBirdRasters,rasterFile))
                   }
            )
            
         
            
            bootNames <- list.files(path = inputBootBirdRasters)
           
            #postprocess bootRasters (crop and make CRS of rasterToMatch)
            bootRasters <- lapply(X = bootNames, FUN = function(ras){
                            bootRaster <- raster(paste(inputBootBirdRasters, "/", ras, sep = ""))
                            bootRaster <- postProcess(bootRaster,
                                        filename2 = file.path(paste(inputBootBirdRasters, "/", ras, sep = "")),
                                        overwrite = TRUE,
                                        rasterToMatch = sim$rasterToMatch,
                                        verbose = TRUE,
                                        studyArea = sim$studyArea,
              )
              
              return(bootRaster)
              
            })

            names(bootRasters) <- bootNames
            
           
            #find out number of boot Rasters
            noRasters <- length(bootRasters)
            print(paste(noRasters, " rasters for ", bird, sep = ""))
            
            bootRastersStack <- raster::stack(bootRasters, verbose = TRUE)
            
            # write mean rasters
            meanRaster <- raster::overlay(bootRastersStack, fun= mean, verbose = TRUE)
            meanRasterName <- paste(bird, "-meanBoot", sep = "")
            names(meanRaster) <- meanRasterName
            writeRaster(x = meanRaster, filename = file.path(paste(outputMeanBirdRasters,"/", bird, "-meanBoot", sep = "")),
                        format = "GTiff",
                        overwrite = TRUE)
            
            # write standard error rasters
            seRaster <- raster::overlay(bootRastersStack, fun = std.error)
            seRasterName <- paste(bird, "-seBoot", sep = "")
            names(seRaster) <- seRasterName
            writeRaster(x = seRaster, filename = file.path(paste(outputMeanBirdRasters,"/", bird, "-seBoot", sep = "")),
                        format = "GTiff",
                        overwrite = TRUE)
            
            
            # ### DANGER!!!
            # get all files in the directories, recursively
            toDelete <- list.files(inputBootBirdRasters, include.dirs = F, full.names = T, recursive = T)
            # remove the files
            file.remove(toDelete)

            
          }
   )
   
  } else {
    
  ### GET FILES FROM LOCAL LOCATION ###
  
  #get rasterToMatch
  sim$rasterToMatch <- raster(file.path(P(sim)$rasterToMatchLocation))
  
  
  #get StudyArea shapefile
  sim$studyArea <- st_read(file.path(P(sim)$studyAreaLocation))
  
  #postProcess studyArea
  sim$studyArea <- reproducible::postProcess(sim$studyArea,
                                         destinationPath = downloadFolderArea,
                                         filename2 = "studyArea",            
                                         fun = "raster::shapefile", #use the function shapefile
                                         targetCRS = crs(sim$rasterToMatch), #make crs same as rasterToMatch
                                         overwrite = TRUE,
                                         verbose = TRUE)
  
  #crop and mask rasterToMatch
  sim$rasterToMatch <- raster::crop(sim$rasterToMatch, sim$studyArea) 
  sim$rasterToMatch <- raster::mask(sim$rasterToMatch, sim$studyArea) 
  
  browser()
  #produce mean rasters using getMeanRastersFunction
  getMeanRasters <- function(bird) {
  tryCatch({
    
    print(bird)
    
    patternNameBirdRaster <- paste("pred250-", bird, "-BCR_", P(sim)$nameBCR, "-boot-", sep = "")
    bootNames <- list.files(path = P(sim)$bootRastersLocation, pattern = patternNameBirdRaster)
    
    
    bootRasters <- lapply(X = bootNames, FUN = function(ras){
      tryCatch({
        print(ras)
        bootRaster <- raster(paste(P(sim)$bootRastersLocation, "/", ras, sep = ""))
        
        bootRaster <- postProcess(bootRaster,
                                  #filename2 = file.path(paste(downloadsBRC, "/", ras, sep = "")),
                                  #overwrite = TRUE,
                                  rasterToMatch = sim$rasterToMatch,
                                  verbose = TRUE)
        #                           studyArea = studyArea,
        # )
       
        return(bootRaster)
      }, error = function(e) return(NULL))
    })
    
    #find out number of boot Rasters
    noRasters <- length(bootRasters)
    print(paste(noRasters, " rasters for ", bird, sep = ""))
    
    bootRastersStack <- raster::stack(bootRasters, verbose = TRUE)
    
    # write mean rasters
    meanRaster <- raster::overlay(bootRastersStack, fun= mean, verbose = TRUE)
    meanRasterName <- paste(bird, "-meanBoot", sep = "")
    names(meanRaster) <- meanRasterName
    writeRaster(x = meanRaster, filename = file.path(paste(outputMeanBirdRasters,"/", bird, "-meanBoot", sep = "")),
                format = "GTiff",
                overwrite = TRUE)
    
    # write standard error rasters
    seRaster <- raster::overlay(bootRastersStack, fun = std.error)
    seRasterName <- paste(bird, "-seBoot", sep = "")
    names(seRaster) <- seRasterName
    writeRaster(x = seRaster, filename = file.path(paste(outputMeanBirdRasters,"/", bird, "-seBoot", sep = "")),
                format = "GTiff",
                overwrite = TRUE)
   
  }, error = function(e) return(NULL))  
}

  
lapply(X = P(sim)$birdList,
       FUN = getMeanRasters)

  }
  # ! ----- STOP EDITING ----- ! #
  return(invisible(sim))
}

ggplotFn <- function(data, ...) {
  ggplot(data, aes(TheSample)) +
    geom_histogram(...)
}

### add additional events as needed by copy/pasting from above
