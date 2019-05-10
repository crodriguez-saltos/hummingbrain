batchBandpass <- function(inputdir, outputdir = "#DEFAULT#", 
                          pass, normalize){
  # Bandpass-filter a batch of recordings.
  
  # Arguments
  # inputdir      Folder containing the input recordings
  # outputdir     Folder where bandpass-filtered recordings will be stored.
  #               Alternatively, a flag specifying how to create the output 
  #               folder
  # pass          Vector of 2 elements, corresponding to minimum and maximum
  #               frequency of filter (in Hz), respectively.
  # normalize     Audio. Whether to normalize audio.
  #
  # Details
  # When outputdir is "#DEFAULT#", a directory will be created called 
  # "bandpass-filtered" and located in the parent directory of inputdir. Within
  # "bandpass-filtered", a directory named after inputdir will be created and
  # bandpass-filtered recordings will be saved there.
  
  # Libraries and sources
  require(tuneR)
  require(seewave)
  
  # Set output folder
  if (outputdir == "#DEFAULT#"){
    basedir <- dirname(inputdir)
    filterdir <- file.path(basedir, "bandpass-filtered")
    if (!dir.exists(filterdir)){
      dir.create(filterdir)
    }
    outd <- file.path(filterdir, basename(inputdir))
    if (!dir.exists(outd)){
      dir.create(outd)
    }
  }
  
  # Read file names
  c <- dir(inputdir)
  nrecs <- length(c)
  
  # Bandpass filter
  print(paste("Beginning bandpass-filtering in folder", inputdir))
  for (i in 1:nrecs){
    print(paste0("Bandpass-filtering ", i, " out of ", nrecs, " recordings."))
    outname <- file.path(outd, sub(
      pattern = ".wav", 
      replacement= paste0(
        "_bandpass-filtered-", pass[1] / 1000, "KHz-", pass[2] / 1000, "KHz.wav"
      ),
      x = c[i] 
    ))
    
    if (!file.exists(outname)){
      d = readWave(file.path(inputdir, c[i]))
      e = ffilter(d, from = pass[1], to = pass[2], bandpass = T, output = "Wave")
      if (normalize){
        e = normalize(e, unit = as.character(d@bit))
      }
      writeWave(object= e, filename = outname)
    }
  }
  print("Bandpass-filtering has finished.")
}