get_reviewers <- function(content){
  # Get a list of reviewers
  content$object$workflows$data$reviewer |>
    purrr::discard(is.na)
}
