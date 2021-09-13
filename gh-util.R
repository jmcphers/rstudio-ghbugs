
# Retrieves all GitHub results for a given query, using pagination.
gh_all <- function(q, ...) {
  message("Retrieving ", q, " .", appendLF = FALSE)
  all <- gh::gh(q, ...)
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

# Extracts a date field from a Github issue
extract_gh_date <- function(issue, field) {
  date <- issue[[field]]
  if (is.null(date))
    NA
  else
    date
}

extract_gh_pr <- function(issue, field) {
  pr <- issue[[field]]
  if (is.null(pr))
    NA
  else 
    pr$url 
}

extract_gh_type <- function(issue, field) {
  labels <- unlist(lapply(issue[[field]], `[[`, "name"))
  if ("bug" %in% labels)
    "bug"
  else if ("enhancement" %in% labels)
    "enhancement"
  else
    "other"
}
