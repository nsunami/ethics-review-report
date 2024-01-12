format_reviews <- function(reviews){
  # Formatting for reporting
  summary_table <- reviews |>
    mutate(month = format(last_modified_month, "%B %Y")) |>
    pivot_wider(
      id_cols = c(committee, reviewer_name),
      names_from = month,
      values_from = n)
  summary_table |>  
    arrange(committee, reviewer_name)
}