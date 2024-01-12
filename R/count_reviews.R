count_reviews <- function(
    cleaned_applications
){

  reviews_df <- cleaned_applications |> 
    select(
      committee,
      app_ref, 
      application_id, 
      first_applicant,
      created_at_month, 
      last_modified,
      last_modified_month,
      reviewers
    ) |>
    unnest(cols = "reviewers") |>
    rename("reviewer" = "reviewers") # Renaming to highlight that the column is a single value of reviewer
  
  ## Label reviewers 
  reviewers_unique <- reviews_df |>
    select(reviewer) |>
    distinct() |>
    mutate(reviewer_name = map_chr(reviewer, add_faculty_info))
  reviews_df <- reviews_df |>
    left_join(reviewers_unique, by = "reviewer")
  
  # Only get the one review per application per reviewer
  # (reviewing multiple times for one application counts as one review)
  reviews_df_no_repeat <- reviews_df |>
    group_by(application_id, reviewer) |>
    slice_head() |>
    ungroup()
  
  reviews_by_date <- reviews_df_no_repeat |>
    count(last_modified_month, committee, reviewer_name)
  # filter(last_modified_month > from) |>
  # filter(last_modified_month < to)

  reviews_by_date |>
    arrange(committee, last_modified_month)
}

