specSummary <- function(dir, output= "csv", fname= "", pngplot= F, spect= "meanspec"){
  require(tuneR)
  require(seewave)

  specs <- dir(dir, pattern = spect, full.names= F)

  # Functions
  specw <- function(spec, f){
    sum(spec[,2] * (spec[,1] - f) ^ 2) / sum(spec[,2])
  }

  propsL <- as.list(specs)
  for (i in 1:length(specs)){
    spec <- readRDS(file.path(dir, specs[[i]]))

    # Get seewave specprop statistics
    props <- specprop(spec)

   # Spectral width around peak frequency
    mode_w <- specw(spec, props$mode / 1000)

    # Incorporate additional measures
    propsL[[i]] <- do.call("c", props)
    propsL[[i]] <- c(propsL[[i]],
                     width_aroundPeak= mode_w)

    # Save plot showing important landmarks
    if (pngplot){
      png(filename = file.path("./plots/descriptive/Ochimb_20180307/",
                               sub(pattern = "_meanspec", replacement = "_meanspec.png", specs[[i]])))
      plot(spec, type= "l", col= "gray")
      abline(v= c(propsL[[i]]["mode"], propsL[[i]]["Q25"],
                  propsL[[i]]["Q75"]) / 1000,
             col= "red")
      text(x= c(propsL[[i]]["mode"], propsL[[i]]["Q25"],
                propsL[[i]]["Q75"]) / 1000,
           y = 1:3 * diff(range(spec[,2])) / 4,
           labels = c("Peak f.", "Q25", "Q75"),
           col= "red", pos= 4)
      dev.off()
    }
  }

  propsL <- do.call("rbind", propsL)
  propsL <- data.frame(file= specs, propsL)

  if (output == "csv"){
    write.csv(propsL, file.path("./cache", fname))
  }

  return(propsL)
}
