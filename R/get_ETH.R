get_ETH <- function(content){
  # Get application ID
  content$object$attributes$`hres:attribute:application-id`$value |>
    unlist()
}
