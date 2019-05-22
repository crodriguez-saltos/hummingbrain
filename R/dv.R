#' Ratio of amplitudes of pulses and interpulse intervals
#'
#' @param sound Wave object.
#' @param ... Further arguments passed to `seewave::timer()`.
#'
#' @details This script calculates the ratio of amplitudes of pulses and
#' interpulse intervals according to Ryan & Sullivan (1989). The root mean
#' squares for each pulse and interpulse interval are calculated. Then the
#' rms of each interpulse interval is divided by the rms of the preceding
#' pulse, and the result is substracted from 1. The last pulse is not
#' considered in the calculation because by definition it does not precede
#' any other interpulse interval.
#'
#' @return A vector with the ratio of amplitude of each interpulse interval
#' and its preceding pulse.
#' @export

dv <- function(sound, ...){
  # Convert to mono----
  if (sound@stereo){
    print("Sound is stereo, only the left channel will be used")
    sound <- sound@left
  }

  # Get the pulses and pulse intervals with `seewave::timer()`----
  p.tms <- seewave::timer(sound, ...)

  # Modify output of `seewave::timer()`----
  p.tms <- p.tms[c("s.start", "s.end", "first")]

  # Give the beginning of the file as the start of the first pulse, if
  # silence is not present at the start. when silence is not present
  # at the beginning, `seewave::timer()` considers the first signal to
  # not have beginning.
  if (p.tms$first != "pause") {
    p.tms$s.start <- c(0, p.tms$s.start)
  }

  # If no silence is at the end, give the duration of the sound file as the
  # end of the last signal. When silence does not occur at the end,
  # `seewave::timer` considers the last signal to have no end.
  if (p.tms$s.end[length(p.tms$s.end)] < p.tms$s.start[length(p.tms$s.start)]){
    p.tms$s.end <- c(p.tms$s.end, duration(sound))
  }

  # Arrange timestamps into dataframes----
  p.tms$p <- data.frame(start= p.tms$s.start, end= p.tms$s.end)
  p.tms$ipi <- data.frame(start= p.tms$p$end[1:(nrow(p.tms$p) - 1)],
                          end= p.tms$p$start[2:nrow(p.tms$p)])

  # Obtain rms for the pulses and pulse invervals----
  rms.p <-apply(p.tms$p, 1, function(x){
    rms <- cutw(wave = sound, from = x["start"], to= x["end"])
    rms <- rms(rms)
    return(rms)
  })

  rms.ipi <- apply(p.tms$ipi, 1, function(x){
    rms <- cutw(wave = sound, from = x["start"], to= x["end"])
    rms <- rms(rms)
    return(rms)
  })

  dv <- 1 - rms.ipi / rms.p[-length(rms.p)]

  return(dv)
}

