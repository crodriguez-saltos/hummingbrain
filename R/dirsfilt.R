dirsfilt <- function() {
  dirs2 <- grep(pattern = "best_for_analysis", x = dirs)
  dirs2 <- dirs[dirs2]
  return(dirs2)
}
