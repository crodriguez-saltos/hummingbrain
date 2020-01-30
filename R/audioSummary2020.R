#' Get a series of summary statistics for a single file
#'
#' @param wave Wave object. It may be imported using `tuneR`
#' @param threshold Amplitude threshold for separating signal from noise, in %.
#' @param window_length Length, in number of samples, of each spectral window.
#' @param overalp Percentage of overlap between spectral windows.
#' @param plot Should acoustic features be plotted?
#'
#' @export

audioSummary2020 <- function(sound, threshold, window_length, overlap, plot = F){
  # Trajectories
  ## Sound as input
  ### Dominant frequency
  dfreqs <- seewave::dfreq(
    wave = sound,
    wl = window_length,
    ovlp = overlap,
    threshold = threshold,
    plot= plot
  )

  ### Fundamental frequency
  funds <- seewave::fund(
    wave = sound,
    wl= window_length,
    ovlp = overlap,
    threshold= threshold,
    plot= plot
  )

  ## Spectrogram as input
  ### Getting the spectrogram
  spectrog <- misound::signal_spectro(
    sound, plot= plot,
    segment_params = list(
      wl= window_length, ovlp= overlap, threshold= threshold
    )
  )

  ### Specprops, from seewave
  specprops <- apply(
    X = spectrog$amp,
    MARGIN = 2,
    FUN = function(x){
      if (!all(is.na(x))){
        seewave::specprop(cbind(spectrog$freq, x))
      }else{
        NA
      }
    })

  # Coalesce numbers into a data frame
  NApos <- sapply(specprops, function(x) all(is.na(x)))
  specpr <- plyr::ldply(.data = specprops[!NApos],
                  .fun = function(x) as.data.frame(x))
  specpr$pos <- which(!NApos)
  addnas <- matrix(nrow = length(which(NApos)), ncol = ncol(specpr) - 1)
  addnas <- cbind(addnas, which(NApos))
  addnas <- as.data.frame(addnas)
  names(addnas) <- names(specpr)
  specpr <- rbind(specpr, addnas)
  specpr <- specpr[order(specpr$pos),]

  rm(addnas, specprops, NApos)

  ### IQR
  iqrs <- specpr$IQR

  if (plot){
    plot(spectrog$time, iqrs)
  }

  ### Mean frequency
  meanfreqs <- specpr$mean

  if (plot){
    plot(spectrog$time, meanfreqs)
  }

  # Min and max frequency
  amplitudes <- na.omit(as.numeric(spectrog$amp))
  ampt <- (threshold * (max(amplitudes) - min(amplitudes)) / 100) + min(amplitudes)

  if (plot){
    plotly::plot_ly(z= spectrog$amp)%>%
      plotly::add_surface()%>%
      plotly::add_surface(
        z= matrix(ampt,
                  ncol= ncol(spectrog$amp),
                  nrow= nrow(spectrog$amp))
      )
  }

  signalamp <- spectrog$amp > ampt
  signalamp <- apply(signalamp, 2, function(x){
    which(x)
  })
  maxfreqs <- sapply(signalamp, function(x){
    if (length(x) == 0){
      NA
    }else{
      spectrog$freq[max(x)]
    }
  })
  minfreqs <- sapply(signalamp, function(x){
    if (length(x) == 0){
      NA
    }else{
      spectrog$freq[min(x)]
    }
  })

  maxmin <- data.frame(
    time= spectrog$time,
    minfreq= minfreqs,
    maxfreq= maxfreqs
  )
  maxmin <- reshape2::melt(maxmin, id.vars= "time", variable.name = "limit")

  if (plot){
    p <- ggplot2::ggplot(
      data = maxmin,
      mapping = ggplot2::aes(x= time, y= value,
                    color= limit)
      ) +
      ggplot2::geom_point()
    print(p)
  }

  ### Frequency range
  freqrange <- maxfreqs - minfreqs

  if (plot){
    plot(spectrog$time, freqrange)
  }

  ### Entropy
  entropies <- specpr$sh

  if (plot){
    plot(spectrog$time, entropies)
  }

  ### Median frequency
  medians <- specpr$median

  if (plot){
    plot(spectrog$time, medians)
  }

  ### Q25
  q25 <- specpr$Q25

  if (plot){
    plot(spectrog$time, q25)
  }

  #### Q75
  q75 <- specpr$Q75

  if (plot){
    plot(spectrog$time, q75)
  }

  # Summary stats across entire sound
  ## Sound as input
  ### Number of elements
  elements <- misound::detect_events(
    wave= sound,
    msmooth= c(window_length,overlap),
    threshold= threshold
  )
  nele <- nrow(elements)

  ### Audio duration
  songdur <- elements$end[nrow(elements)] - elements$start[1]

  ### Frequency range
  fr <- range(maxmin$value, na.rm= T)

  ### Fundamental frequency
  ff <- summary(na.omit(funds[,2]))

  ## Mean Power spectra as input
  ### Getting the mean power spectra
  meansp <- meanspec(sound, wl= window_length, ovlp = overlap, plot = plot)

  ### Getting properties of the power spectra
  specpr.spec <- specprop(meansp)

  # Output
  return(
    list(
      trajectories = data.frame(
        time = spectrog$time,
        dominant_frequency = dfreqs[,2],
        fundamental_frequency = funds[,2],
        iqr = iqrs,
        mean_frequency = meanfreqs,
        mininum_frequency = minfreqs,
        maximum_frequency = maxfreqs,
        frequency_range = freqrange,
        entropy = entropies,
        median_frequency = medians,
        first_quartile = q25,
        third_quartile = q75
      ),
      summary_stats= c(
        number_elements = nele,
        song_duration = songdur,
        frequency_range = fr,
        fundamental_frequency = ff,
        dominant_frequency = specpr.spec$mode,
        iqr = specpr.spec$IQR,
        average_frequency= specpr.spec$mean,
        median_frequency= specpr.spec$median,
        entropy = specpr.spec$sh,
        q25 = specpr.spec$Q25,
        q75 = specpr.spec$Q75
      )
    )
  )
}
