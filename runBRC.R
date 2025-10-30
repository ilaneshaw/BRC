library("conflicted")
library("SpaDES.core")
# library("googledrive")


## make a list of directory paths
setPaths(
  modulePath = file.path("../../modules"),
  cachePath = file.path("../../cache"),
  scratchPath = file.path("../../scratch"),
  inputPath = file.path("../../inputs"),
  outputPath = file.path("../../outputs/birdRange")
)
simPaths <- getPaths()

# specify where inputs come from
locationRasterToMatch <- Paths$inputPath
rasterToMatchName <- "ALFL-meanBoot_BCR-60_studyArea_AB_BCR6"

studyAreaLocation <- checkPath(file.path(Paths$inputPath, "studyArea/studyArea_AB_BCR6"), create = TRUE)
.studyAreaName <- "studyArea_AB_BCR6.shp"

bootRastersLocation <- file.path(Paths$inputPath, "bootBirdRasters") # give the location where the bootstrapped bird rasters can be found
outputBirdRastersFolder <- checkPath(file.path(Paths$outputPath, "meanBirdRasters"), create = TRUE) # say where to write the output rasters to

nameBCR <- "60" # give the name of the BCR you want to create rasters for in the format: "##"

# birdList <- sort(c("CAWA", "OVEN")) #give a list of the 4-letter bird codes for the bird species you wish to create rasters for.
birdList <- sort(unique(c(
  "ALFL", "AMCR", "AMGO", "AMPI", "AMRE", "AMRO", "ATSP",
  "ATTW", "BANS", "BAOR", "BARS", "BAWW", "BBCU", "BBMA",
  "BBWA", "BBWO", "BCCH", "BEKI", "BHCO", "BHVI", "BLBW",
  "BLJA", "BLPW", "BOBO", "BOCH", "BOWA", "BRBL", "BRCR",
  "BRTH", "BTNW", "BWWA", "CAWA", "CCSP", "CEDW", "CHSP",
  "CLSW", "CMWA", "COGR", "CONW", "CORA", "COYE", "CSWA",
  "DEJU", "DOWO", "DUFL", "EABL", "EAKI", "EAPH", "EATO",
  "EAWP", "EUST", "EVGR", "FOSP", "GCFL", "GCKI", "GCSP",
  "GCTH", "GRAJ", "GRCA", "GRSP", "GRYE", "GWWA", "HAFL",
  "HAWO", "HETH", "HOLA", "HOSP", "HOWR", "INBU", "KILL",
  "LCSP", "LEFL", "LEYE", "MOWA", "NESP", "OSFL", "OVEN",
  "PAWA", "PHVI", "RBNU", "RCKI", "REVI", "RUBL", "SWTH",
  "TEWA", "WCSP", "WETA", "WIWR", "YRWA", "ALFL", "AMCR",
  "AMGO", "AMPI", "AMRE", "AMRO", "ATSP", "ATTW", "BANS",
  "BAOR", "BARS", "BAWW", "BBCU", "BBMA", "BBWA", "BBWO",
  "BCCH", "BEKI", "BGGN", "BHCO", "BHVI", "BLBW", "BLJA",
  "BLPW", "BOBO", "BOCH", "BOWA", "BRBL", "BRCR", "BRTH",
  "BTBW", "BTNW", "BWWA", "CAWA", "CCSP", "CEDW", "CHSP",
  "CLSW", "CMWA", "COGR", "CONW", "CORA", "COYE", "CSWA",
  "DEJU", "DOWO", "DUFL", "DUNL", "EABL", "EAKI", "EAPH",
  "EATO", "EAWP", "EUST", "EVGR", "FISP", "FOSP", "GCFL",
  "GCKI", "GCSP", "GCTH", "GRAJ", "GRCA", "GRSP", "GRYE",
  "GWWA", "HAFL", "HAWO", "HETH", "HOLA", "HOSP", "HOWR",
  "INBU", "KILL", "LALO", "LCSP", "LEFL", "LEYE", "LISP",
  "MAWA", "MAWR", "MOBL", "MODO", "MOWA", "NAWA", "NESP",
  "NOCA", "NOFL", "NOPA", "NOWA", "OCWA", "OSFL", "OVEN",
  "PAWA", "PHVI", "PIGR", "PISI", "PIWA", "PIWO", "PUFI",
  "RBGR", "RBNU", "RBWO", "RCKI", "RECR", "REVI", "RHWO",
  "ROPI", "RTHU", "RUBL", "RUGR", "RWBL", "SAVS", "SCTA",
  "SEWR", "SOGR", "SOSA", "SOSP", "SPGR", "SPSA", "SWSP",
  "SWTH", "TEWA", "TOSO", "TOWA", "TRES", "UPSA", "VATH",
  "VEER", "VESP", "WAVI", "WBNU", "WCSP", "WETA", "WEWP",
  "WIPT", "WISN", "WITU", "WIWA", "WIWR", "WOTH", "WTSP",
  "WWCR", "YBCU", "YBFL", "YBSA", "YEWA", "YHBL", "YRWA"
)))

print("set module and simulation params")
simModules <- list("BRC")

## Set simulation and module parameters
simTimes <- list(start = 1, end = 10, timeunit = "year")
simParams <- list(
  BRC = list(
    simulationTimeStep = 10,
    .plotInitialTime = 1,
    fromDrive = FALSE,
    writeMeanRas = FALSE, # T/F do you want to create a raster of mean of the bootstrap replicates?
    writeMedianRas = TRUE, # T/F do you want to create a raster of median of the bootstrap replicates?
    writeSERas = FALSE, # T/F do you want to create a raster of standard error between the bootstrap replicates?
    writeSDRas = FALSE, # T/F do you want to create a raster of standard deviation between the bootstrap replicates?
    rasterToMatchLocation = rasterToMatchLocation,
    rasterToMatchName = rasterToMatchName,
    studyAreaLocation = studyAreaLocation,
    .studyAreaName = .studyAreaName,
    outputBirdRastersFolder = outputBirdRastersFolder,
    bootRastersLocation = bootRastersLocation,
    nameBCR = nameBCR,
    birdList = birdList
  )
)


print("set up sim")
## Simulation setup
mySim <- simInit(
  times = simTimes, params = simParams,
  modules = simModules, paths = simPaths
)

test <- spades(mySim)
