count_chair_reviews <- function(ethics_applications){
  # Count Chair Reviews ====
  ethics_applications |>
    count(last_modified_month, chair_name) |>
    drop_na() |>
    mutate(month = format(last_modified_month, "%B, %Y")) |>
    pivot_wider(id_cols = chair_name,
                names_from = month,
                values_from = n)
}