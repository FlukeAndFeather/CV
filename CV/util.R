format_name <- function(author) {
  components <- strsplit(author, " ")[[1]]
  first_name <- components[1]
  first_initial <- str_sub(first_name, 1, 1)
  middle_initial <- str_match(components[2], "([A-Z])[.]")[, 2]
  last_name <- if (is.na(middle_initial)) {
    components[-1]
  } else {
    components[-c(1:2)]
  }

  initials <- na.omit(c(first_initial, middle_initial))
  last_name <- paste(last_name, collapse = " ")
  sprintf("%s, %s.", last_name, paste(initials, collapse = "."))
}

combine_authors <- function(authors) {
  formatted <- map_chr(authors, format_name)
  czapanskiy_loc <- which(str_detect(formatted, "Czapanskiy"))
  formatted[czapanskiy_loc] <- "\\textbf{Czapanskiy, M.F.}"
  max_authors <- 6
  if (length(formatted) > max_authors) {
    if (czapanskiy_loc > max_authors - 1) {
      paste(c(formatted[1:(max_authors - 2)],
              "...",
              formatted[czapanskiy_loc],
              "...",
              formatted[length(formatted)]),
            collapse = ", ")
    } else {
      paste(c(formatted[1:(max_authors - 1)],
              "...",
              formatted[length(formatted)]),
            collapse = ", ")
    }
  } else {
    paste(formatted, collapse = ", ")
  }
}
