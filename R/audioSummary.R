audioSummary <- function(dir, output= "csv", fname= "",
                         pngplot= F, ffplot= F, thresh,
                         dfplot= F){
  # Obtain summary statistics based on power spectra for a batch of *.wav files.
  require(tuneR)
  require(seewave)

  wavfiles <- dir(dir, pattern = ".wav", full.names= F)

  # Functions
  specw <- function(spec, f){
    sum(spec[,2] * (spec[,1] - f) ^ 2) / sum(spec[,2])
  }

  propsL <- as.list(wavfiles)
  for (i in 1:length(wavfiles)){
    print(paste(
      "Getting stats for file", i, "out of", length(wavfiles), ":", wavfiles[i]
    ))
    audio <- readWave(file.path(dir, wavfiles[[i]]))
    # meanspec was used instead of spec because of problems with integer overflow.
    # Changed on 2018/03/07
    # spec <- spec(wave= audio, f= audio@samp.rate, plot= T)
    spec <- meanspec(wave= audio, f= audio@samp.rate, plot= F)

    # Get seewave specprop statistics
    props <- specprop(spec)

    # Get mean fundamental frequency and, if queried, plot fundamental frequencies
    if (ffplot){
      ffdir <- file.path("./plots/ff", basename(dir))
      if (!dir.exists(ffdir)){
        dir.create(ffdir)
      }
      png(filename = file.path(ffdir,
                               sub(pattern = ".wav", replacement = ".png", wavfiles[[i]])))

      funds <- fund(audio, plot= T)
      mfund <- mean(funds[,2], na.rm= T)
      sd_fund <- sd(funds, na.rm= T)

      abline(h= mfund, col= "red")
      legend(x = "topright",legend = "mean", col = "red", lty = 1)
      axis(2, at = round(mfund, digits= 2),
           col = "red", col.axis= "red", las= 1, cex.axis= 0.85)
      dev.off()
    }else{
      funds <- fund(audio, plot= F, threshold = thresh)
      mfund <- mean(funds[,2], na.rm= T)
      sd_fund <- sd(funds, na.rm= T)
    }

    # Get peak frequencies----
    if (dfplot){
      ffdir <- file.path("./plots/df", basename(dir))
      if (!dir.exists(ffdir)){
        dir.create(ffdir)
      }
      png(filename = file.path(
        ffdir,
        sub(pattern = ".wav", replacement = ".png", wavfiles[[i]]))
      )

      dfreqs <- dfreq(wave = audio, plot= T, threshold = thresh)
      df_mean <- mean(dfreqs[,2], na.rm= T)
      sd_dfreqs <- sd(dfreqs[,2], na.rm= T)

      abline(h= df_mean, col= "red")
      legend(x = "topright",legend = "mean", col = "red", lty = 1)
      axis(2, at = round(df_mean, digits= 2),
           col = "red", col.axis= "red", las= 1, cex.axis= 0.85)
      dev.off()
    }else{
      dfreqs <- dfreq(wave = audio, plot= T, threshold = thresh)
      df_mean <- mean(dfreqs[,2])
      sd_dfreqs <- sd(dfreqs[,2], na.rm= T)
    }

    # Spectral width around fundamental frequency----
    fund_w <- specw(spec, mfund)

    # Spectral width around peak frequency
    mode_w <- specw(spec, props$mode / 1000)

    # Incorporate the additional measures
    propsL[[i]] <- do.call("c", props)
    propsL[[i]] <- c(fund_mean= mfund, fund_sd = sd_fund,
                     width_aroundFund= fund_w,
                     dfreq_mean= df_mean, dfreq_sd= sd_dfreqs,
                     propsL[[i]],
                     width_aroundPeak= mode_w)

    # Save plot showing important landmarks
    if (pngplot){
      if (!dir.exists(file.path("./plots/descriptive", basename(dir)))){
        dir.create(file.path("./plots/descriptive", basename(dir)))
      }
      png(filename = file.path("./plots/descriptive", basename(dir),
                               sub(pattern = ".wav", replacement = ".png", wavfiles[[i]])))
      plot(spec, type= "l", col= "gray")
      abline(v= c(propsL[[i]]["fund_mean"] * 1000, propsL[[i]]["mode"], propsL[[i]]["Q25"],
                  propsL[[i]]["Q75"]) / 1000,
             col= "red")
      text(x= c(propsL[[i]]["fund_mean"] * 1000, propsL[[i]]["mode"], propsL[[i]]["Q25"],
                propsL[[i]]["Q75"]) / 1000,
           y = 1:4 * diff(range(spec[,2])) / 5,
           labels = c("Fundamental f.", "Peak f.", "Q25", "Q75"),
           col= "red", pos= 4)
      dev.off()
    }
  }

  propsL <- do.call("rbind", propsL)
  propsL <- data.frame(file= wavfiles, propsL)

  if (output == "csv"){
    write.csv(propsL, file.path("./cache", fname))
  }

  return(propsL)
}
