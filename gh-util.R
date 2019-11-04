
gh_all <- function(q) {
  message("Retrieving ", q, " .", appendLF = FALSE)
  all <- gh::gh(q)
  page <- all
  more <- TRUE
  total <- 1
  
  tryCatch({
    while(more) {
      page <- gh::gh_next(page)
      all <- append(all, page)
      message(".", appendLF = FALSE)
      total <- total + 1
    }
  }, error = function(e) {
    # Swallow errors here since we expect to reach an error when
    # there's no next page of results
    more <<- FALSE
  })
  
  message(" OK (", total, " pages)")
  
  all
}

