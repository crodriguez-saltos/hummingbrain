#' Durations of syllables and intersyllable intervals
#'
#' @param sound Wave object.
#' @param labels File containing the timestamps of the syllables.
#' @param ... Further arguments passed to `seewave::timer()`.
#'
#' @export

intervals <- function(sound, labels= NULL, ...){
  # Convert to mono----
  if (sound@stereo){
    print("Sound is stereo, only the left channel will be used")
    sound <- sound@left
  }

  # Import label file, if provided by the user.----
  if (!is.null(labels)){
    label.df <- read.table(labels)
    p.tms <- list(
      s.start= label.df$V1,
      s.end= label.df$V2,
      label= label.df$V3,
      first= ifelse(label.df$V1[1] > 0, yes = "pause", no = "signal")
    )
  }else{
    # Get the pulses and pulse intervals with `seewave::timer()`----
    # This function is invoked if no label file has been provided by the user.
    p.tms <- seewave::timer(sound, ...)

    # Modify output of `seewave::timer()`----
    p.tms <- p.tms[c("s.start", "s.end", "first")]
    p.tms$label <- rep("s", length(p.tms$s.start))
  }

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
  p.tms$p <- data.frame(start= p.tms$s.start, end= p.tms$s.end, label= p.tms$label)
  p.tms$ipi <- data.frame(start= p.tms$p$end[1:(nrow(p.tms$p) - 1)],
                          end= p.tms$p$start[2:nrow(p.tms$p)],
                          label= p.tms$label[1:(nrow(p.tms$p) - 1)])

  # Obtain durations----
  p.tms$p$duration <- p.tms$p$end - p.tms$p$start
  p.tms$ipi$duration <- p.tms$ipi$end - p.tms$ipi$start
  durations <- list(syllables= p.tms$p, intersyllable_intervals= p.tms$ipi)

  return(durations)
}

