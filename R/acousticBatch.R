#' Extract acoustic features in batch for multiple files in a directory
#'
#' @param inputdir Folder containing wave files.
#' @param outputdir Folder where to write results.
#' @param ... Arguments to be passed to `audioSummary2020`.
#' @export

acousticBatch <- function(inputdir, outputdir, ...){
  # Get wave file list
  wavefs <- dir(inputdir, pattern= ".wav", full.names= T)

  processed <- 0
  nfiles <- length(wavefs)
  acoustic_summary <- as.list(1:nfiles)

  for (w in wavefs){
    processed <- processed + 1
    print(paste(
      "Processing file", processed, "of", nfiles
    ))
    features <- audioSummary2020(
      sound= tuneR::readWave(w),
      ...
    )

    # Export trajectory
    write.table(
      x = features$trajectories,
      file = file.path(
        outputdir,
        sub(
          pattern = ".wav",
          replacement = "_trajectories.txt",
          x = basename(w)
        )
      )
    )

    # Store acoustic summary
    acoustic_summary[[processed]] <- as.data.frame(t(features$summary_stats))
  }

  # Export acoustic summary
  acoustic_summary <- do.call("rbind", acoustic_summary)
  acoustic_summary <- data.frame(
    file= basename(wavefs),
    acoustic_summary
  )
  write.table(
    x = acoustic_summary,
    file= file.path(outputdir, "acoustic_summary.txt")
  )
}
