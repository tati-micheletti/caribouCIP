defineModule(sim, list(
  name = "caribouCIP",
  description = paste0("SpaDES compatible module for caribou co-informed predictions using ",
                       "resource selection function and habitat suitability index models"),
  keywords = c("Caribou", "population", "Indigenous knowledge"),
  authors = c(person("Tati", "Micheletti", email = "tati.micheletti@gmail.com", role = c("aut", "cre")),
              person("Eliot", "McIntire", email = "Eliot.McIntire@canada.ca", role = c("aut", "cre")),
              person("Frances", "Stewart", email = "frances.stewart@canada.ca", role = c("aut", "cre"))),
  childModules = character(0),
  version = list(SpaDES.core = "1.0.0.9004", caribouCIP = "0.0.0.9000"),
  timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = "year",
  citation = list("citation.bib"),
  documentation = deparse(list("README.txt", "caribouCIP.Rmd")),
  reqdPkgs = list("raster", "PredictiveEcology/pemisc@development", "data.table", "matrixStats"),
  parameters = rbind(
    defineParameter(".plotInitialTime", "numeric", NA, NA, NA,
                    "CURRENTLY NOT WORKING. Describes the simulation time at which the first plot event should occur."),
    defineParameter(".plotInterval", "numeric", NA, NA, NA,
                    "Describes the simulation time interval between plot events."),
    defineParameter(".saveInitialTime", "numeric", NA, NA, NA,
                    "Describes the simulation time at which the first save event should occur."),
    defineParameter(".saveInterval", "numeric", NA, NA, NA,
                    "This describes the simulation time interval between save events."),
    defineParameter(".useCache", "logical", FALSE, NA, NA,
                    paste("Should this entire module be run with caching activated?",
                          "This is generally intended for data-type modules, where stochasticity",
                          "and time are not relevant")),
    defineParameter("predictionInterval", "numeric", 1, NA, NA,
                    "This describes the prediction interval"),
    defineParameter("includeLastYear", "logical", TRUE, NA, NA,
                    paste0("If the ratio between last-first years devided by the ",
                           "predictionInterval is not an integer, should predict for the last year?"))
  ),
  inputObjects = bindrows(
    expectsInput(objectName = "habitatSuitabilityIndex", objectClass = "list", 
                 desc = "List of YearXXXX of caribou HSI predictions", 
                 sourceURL = NA), 
    expectsInput(objectName = "predictedPresenceProbability", objectClass = "list", 
                 desc = "List of rasters per year, indicating the probability of presence of Caribous", 
                 sourceURL = NA)
  ),
  outputObjects = bindrows(
    createsOutput(objectName = "caribouCoInformedPredictions", objectClass = "list", 
                  desc = paste0("List of years (YearXXXX) of a unified caribou co-informed ",
                                "prediction map based on a resource selection (collar based) and ",
                                "a habitat suitability index (Indigenous Knowledge informed) models"))
  )
))

doEvent.caribouCIP = function(sim, eventTime, eventType) {
  switch(
    eventType,
    init = {

      # schedule future event(s)
      sim <- scheduleEvent(sim, start(sim), "caribouCIP", "checkingData", eventPriority = .last())
      sim <- scheduleEvent(sim, start(sim), "caribouCIP", "harmonizingPredictions", eventPriority = .last())
      sim <- scheduleEvent(sim, P(sim)$.plotInitialTime, "caribouCIP", "plot", eventPriority = .last())
    },
    checkingData = {
      # Check the rasters align on their own 
      # habitatSuitabilityIndex
      tryCatch({
        HSI <-    stack(sim$habitatSuitabilityIndex)
      }, error = function(e){
        stop("The habitatSuitabilityIndex rasters do not align accross years. Please debug.")
      })
      # predictedPresenceProbability
      tryCatch({
        RSF <-  stack(sim$predictedPresenceProbability)
      }, error = function(e){
        stop("The predictedPresenceProbability rasters do not align accross years. Please debug.")
      })
      
      # Check the rasters align together
      # predictedPresenceProbability
      tryCatch({
        CIP <-  stack(RSF, HSI)
      }, error = function(e){
        stop(paste0("The predictedPresenceProbability and habitatSuitabilityIndex rasters do not ",
                    "align accross years. Please debug."))
      })
      # schedule future event(s)
      sim <- scheduleEvent(sim, time(sim) + P(sim)$predictionInterval, "caribouCIP", "checkingData")
      if (P(sim)$includeLastYear){
        if (all(time(sim) == start(sim), (end(sim)-start(sim)) != 0))
          sim <- scheduleEvent(sim, end(sim), "caribouCIP", "checkingData")
      }
    },
    harmonizingPredictions = {
      sim$caribouCoInformedPredictions[[paste0("Year", time(sim))]] <- harmonizeRasters(stack(sim$habitatSuitabilityIndex[[paste0("Year", time(sim))]],
                                                                                              sim$predictedPresenceProbability[[paste0("Year", time(sim))]]),
                                                                                        currentTime = time(sim))
      # schedule future event(s)
      sim <- scheduleEvent(sim, time(sim) + P(sim)$predictionInterval, "caribouCIP", "harmonizingPredictions")
      if (P(sim)$includeLastYear){
        if (all(time(sim) == start(sim), (end(sim)-start(sim)) != 0))
          sim <- scheduleEvent(sim, end(sim), "caribouCIP", "harmonizingPredictions")
      }
      
      },
    plot = {
      # TODO fix Plot(). Not working for second year
      # caribouCIP_Ave <- sim$caribouCoInformedPredictions[[paste0("Year", time(sim))]][["average"]]
      # Plot(caribouCIP_Ave,
      #      title = paste0("Caribou co-informed average predictions: ", time(sim)))
      # 
      # caribouCIP_Std <- sim$caribouCoInformedPredictions[[paste0("Year", time(sim))]][["stdDev"]]
      # Plot(caribouCIP_Std,
      #      title = paste0("Caribou co-informed standard deviation predictions: ", time(sim)))
      
      # schedule future event(s)
      # sim <- scheduleEvent(sim, time(sim) + P(sim)$predictionInterval, "caribouCIP", "plot")
      # if (P(sim)$includeLastYear){
      #   if (all(time(sim) == start(sim), (end(sim)-start(sim)) != 0))
      #     sim <- scheduleEvent(sim, end(sim), "caribouCIP", "plot")
      # }
      
    },
    warning(paste("Undefined event type: \'", current(sim)[1, "eventType", with = FALSE],
                  "\' in module \'", current(sim)[1, "moduleName", with = FALSE], "\'", sep = ""))
  )
  return(invisible(sim))
}

.inputObjects <- function(sim) {
  #cacheTags <- c(currentModule(sim), "function:.inputObjects") ## uncomment this if Cache is being used
  dPath <- asPath(getOption("reproducible.destinationPath", dataPath(sim)), 1)
  message(currentModule(sim), ": using dataPath '", dPath, "'.")
  
  if (all(!suppliedElsewhere("habitatSuitabilityIndex", sim = sim, where = "sim"),
          !suppliedElsewhere("predictedPresenceProbability", sim = sim, where = "sim"))){
    message(crayon::yellow(paste0("Neither habitatSuitabilityIndex nor predictedPresenceProbability ",
                               "were provided. Using dummy data to show module functionality")))
    sequenceYears <- seq(start(sim), end(sim), by = P(sim)$predictionInterval)
    if (P(sim)$includeLastYear){
      sequenceYears <- c(sequenceYears, end(sim))
    }
    sequenceYears <- paste0("Year", sequenceYears)
    sim$habitatSuitabilityIndex <- lapply(eval(parse(text = paste0("list(", 
                                                                   paste(rep("raster()", 
                                                                             times = length(sequenceYears)), 
                                                                         collapse = ", "),")"))), 
                                          setValues, values = runif(64800))
    sim$predictedPresenceProbability <- lapply(eval(parse(text = paste0("list(", 
                                                                   paste(rep("raster()", 
                                                                             times = length(sequenceYears)), 
                                                                         collapse = ", "),")"))), 
                                          setValues, values = runif(64800))
    names(sim$habitatSuitabilityIndex) <- names(sim$predictedPresenceProbability) <- sequenceYears
  } else {
    if (any(!suppliedElsewhere("habitatSuitabilityIndex", sim = sim, where = "sim"),
        !suppliedElsewhere("predictedPresenceProbability", sim = sim, where = "sim")))
    stop(paste0("Either habitatSuitabilityIndex or predictedPresenceProbability ",
                               "was not provided. Please provide both, or none for a dummy example ", 
                               "of how this module works"))
  }

  return(invisible(sim))
}
