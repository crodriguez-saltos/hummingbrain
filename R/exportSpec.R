#' exportSpec
#'@export
exportSpec <- function(dir, spect= "meanspec", outputDir){
  require(tuneR)
  require(seewave)

  wavfiles <- dir(dir, pattern = ".wav", full.names= F)

  for (i in 1:length(wavfiles)){
    audio <- readWave(file.path(dir, wavfiles[[i]]))
    if(spect== "meanspec"){
      spec <- meanspec(audio, plot= F)
    }else{
      spec <- spec(audio, plot= F)
    }

    saveRDS(spec,
            file = file.path(outputDir, sub(".wav", paste0("_", spect), wavfiles[[i]])))
  }
}
