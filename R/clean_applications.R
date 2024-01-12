# Clean the downloaded applications

library(tidyverse)
library(here)
library(hellohaplo)


source(here("R/get_ETH.R"))
source(here("R/get_reviewers.R"))


clean_applications <- function(
    raw_applications,
    drop_raw = TRUE
){
  
  # Pluck the applicants for each application
  ethics_applications <- raw_applications |>
    mutate(applicants = map(
      content,
      pluck_applicant
    ))
  
  ## Get the first applicant for each application
  ethics_applications <- ethics_applications |>
    mutate(first_applicant = map_chr(
      applicants,
      ~ .[1]
    ))
  
  # Get the dates  ============================================================
  ## Created At & Last Modified
  get_creation_date <- function(content){
    content$object$creationDate
  }
  get_last_modified_date <- function(content){
    content$object$lastModificationDate
  }
  ethics_applications <- ethics_applications |> 
    mutate(created_at = map_chr(content, get_creation_date),
           created_at = parse_datetime(created_at)) |>
    mutate(last_modified = map_chr(content, get_last_modified_date),
           last_modified = parse_datetime(last_modified))
  # Create date bins
  ethics_applications <- ethics_applications |> 
    mutate(
      across(
        c(created_at, last_modified),
        list(day = ~floor_date(., unit = "day"),
             week = ~floor_date(., unit = "week"),
             month = ~floor_date(., unit = "month"),
             year = ~floor_date(., unit = "year")),
        .names = "{.col}_{.fn}"
      )
    )
  
  
  ## First review on
  get_first_review_date <- function(content){
    
    is_reviewed <- "hres_peer_review:peer_review" %in% content[["object"]][["workflows"]][["workType"]]
    
    if(!is_reviewed){ return(NA) }
    
    workflows_tibble <- content$object$workflows |>
      as_tibble()
    
    workflows_tibble |>
      filter(workType == "hres_peer_review:peer_review") |>
      arrange(createdAt) |>
      slice_head() |>
      pull(createdAt)
  }
  
  ethics_applications <- ethics_applications |>
    mutate(
      first_review_date = map_chr(
        content, 
        get_first_review_date
      )
    )
  
  
  # Get ethics workflow  ============================================================
  get_ethics_workflows <- function(content){
    content$object$workflows
  }
  # Get the workflows
  ethics_applications <- ethics_applications |>
    mutate(workflows = map(content, get_ethics_workflows))
  
  # Pluck the ethics workflow
  pluck_ethics_workflow <- function(workflows){
    workflows <- as.list(workflows)
    workflows[workflows$workType == "hres_ethics:eth"]
  }
  ethics_applications <- ethics_applications |>
    mutate(ethics_workflow = map(workflows, pluck_ethics_workflow))
  
  
  # Convert the workflow as tibble
  pluck_ethics_workflow_tibble <- function(workflows){
    workflows |> as.data.frame() |> as_tibble()
  }
  ethics_applications <- ethics_applications |>
    mutate(workflows_tibble = map(
      workflows,
      pluck_ethics_workflow_tibble
    ))
  
  
  # Filter the hres_ethics row 
  pluck_hres_ethics <- function(workflow_tibble){
    if(is.null(workflow_tibble$workType)){ return(NA) }
    workflow_tibble |> 
      filter(workType == "hres_ethics:eth") 
  }
  ethics_applications <- ethics_applications |>
    mutate(workflow_hres_ethics = map(
      workflows_tibble, pluck_hres_ethics
    ))
  
  
  # Get the ethics state based on hres_ethics
  get_ethics_state <- function(hres_ethics){
    if(!is_tibble(hres_ethics)){ return(NA) }
    hres_ethics |>
      pull(state)
  }
  ethics_applications <- ethics_applications |>
    mutate(state_tibble = map_chr(workflow_hres_ethics, get_ethics_state))
  
  # Tag committees ========================================================
  tag_committee <- function(workflow_hres_ethics){
    if(!is_tibble(workflow_hres_ethics)){ return(NA) }
    target <- workflow_hres_ethics$target 
    if(is.null(target)){ return(NA) }
    target
  }
  ethics_applications <- ethics_applications |> 
    mutate(committee_code = map_chr(
      workflow_hres_ethics, tag_committee
    ))
  # Create friendly labels for committees
  committees_key <- read_csv2(here("data/committees.csv")) |>
    select(2, 1) |> deframe()
  ethics_applications <- ethics_applications |>
    mutate(committee = fct_recode(committee_code, !!!committees_key))
  
  # Get committee codes ======
  get_committee_code <- function(content){
    code <- content$object$attributes$`hres:attribute:ethics-approver`$ref
    if(!is.null(code)) { code } else { NA }
  }
  ethics_applications <- ethics_applications |>
    mutate(committee_ref = map(content, get_committee_code))
  ## Get the first committee - some applications has two or more committees associated
  ethics_applications <- ethics_applications |>
    mutate(committee_ref_1st = map_chr(committee_ref, ~.[[1]]))
  
  
  # Tag chairs based on committees ===================
  chairs_raw <- ethics_applications |> 
    select(committee_ref) |>
    unnest(committee_ref) |>
    distinct(committee_ref) |>
    drop_na()
  chairs_raw <- chairs_raw |>
    mutate(content = map(committee_ref, 
                         ~get_content(get_object_info(.)))) 
  chairs_raw <- chairs_raw |>
    mutate(
      chair = map(
        content, 
        ~.[["object"]][["attributes"]][["haplo:attribute:chair"]]
      )
    )
  chairs <- chairs_raw |>
    select(committee_ref, chair) |>
    unnest(chair) |>
    select(committee_ref,
           chair_ref = ref,
           chair_name = title)
  # Join the tables to add chair info 
  ethics_applications <- ethics_applications |>
    left_join(chairs, by = c("committee_ref_1st" = "committee_ref"))
  
  
  # Tag the states  ===================
  tag_state <- function(ethics_workflow){
    if(!is_tibble(ethics_workflow)){ return(NA) }
    state <- ethics_workflow$state
    if(is.null(state)){ return(NA) }
    state[!is.na(state)]
  }
  ethics_applications <- ethics_applications |>
    mutate(state = map_chr(workflow_hres_ethics, tag_state))
  
  # Get the state of ethics approval workflow
  # Get notifications table
  get_notifications <- function(ethics_workflow){
    if(!is_tibble(ethics_workflow)){ return(NA) }
    notifications <- ethics_workflow$notifications
    notifications |> as_tibble()
  }
  ethics_applications <- ethics_applications |>
    mutate(notifications = map(workflow_hres_ethics, get_notifications))
  
  # Determine when the approval letter is sent 
  get_approval_letter_date <- function(notifications){
    if(!is_tibble(notifications) || ncol(notifications) == 0){ return(NA) }
    manual_approval_date <- notifications$decision$sentAt
    if(is.null(manual_approval_date)){
      auto_approval_date <- notifications$eur1approvalLetter$sentAt
      if(is.null(auto_approval_date)){ return(NA) }
      return(auto_approval_date)
    }
    manual_approval_date
  }
  ethics_applications <- ethics_applications |> 
    mutate(approval_letter_on = map_chr(notifications, get_approval_letter_date),
           approval_letter_on = ymd_hms(approval_letter_on))
  
  # Determine the decision was made
  tag_done_application <- function(notifications){
    if(ncol(notifications) == 0 || nrow(notifications) == 0 || is.na(notifications)){ return(NA) }
    pending_auto <- notifications$decision$pending
    if(is.null(pending_auto)){
      pending_manual <- notifications$eur1approvalLetter$pending
      if(is.null(pending_manual)){ return(NA) }
      return(!pending_manual)
    }
    !pending_auto
  }
  ethics_applications <- ethics_applications |>
    mutate(decision_made = map_lgl(notifications, tag_done_application))
  
  # Calculate the duration to the approval letter
  ethics_applications <- ethics_applications |>
    mutate(decision_period = interval(last_modified, approval_letter_on),
           decision_days = decision_period / ddays(1))
  
  # Tag auto-approvals  ============================================================
  # Figure out if the auto-approval was made - this was not accurate and thus removing this
  # Applications such as https://ethicsmonitor.eur.nl/842q7 was marked as automatically approved
  tag_auto_approval <- function(hres_tibble){
    if(is.null(hres_tibble$documents$form$consentToAutoApproval)){ return(NA) }
    hres_tibble$documents$form$consentToAutoApproval
  }
  ethics_applications <- ethics_applications |>
    mutate(auto_approval_consented = map_chr(ethics_workflow, tag_auto_approval))
  
  # Approval consent to logical
  ethics_applications <- ethics_applications |>
    mutate(auto_approval_consented_lgl = case_match(
      auto_approval_consented,
      "TRUE" ~ TRUE,
      "FALSE" ~ FALSE,
      "yes" ~ TRUE,
      "no" ~ FALSE
    ))
  
  # Label the auto approve consent 
  ethics_applications <- ethics_applications |>
    mutate(auto_approval_consented_label = case_when(
      auto_approval_consented_lgl == TRUE ~ "auto-approved",
      auto_approval_consented_lgl == FALSE ~ "declined"
    ))
  # Convert to the binary auto approved or not
  ethics_applications <- ethics_applications |>
    mutate(auto_approved_lgl = case_when(
      auto_approval_consented == TRUE ~ TRUE,
      auto_approval_consented == FALSE ~ TRUE,
      auto_approval_consented == "yes" ~ TRUE,
      auto_approval_consented == "no" ~ FALSE,
      is.na(auto_approval_consented) ~ FALSE
    ))
  
  
  # Tag DMP (for RSM)  ============================================================
  tag_dmp <- function(ethics_workflow){
    # Return "yes", "no", or NA
    if(!is_tibble(ethics_workflow)){ return(NA) }
    already_with_DMP <- ethics_workflow$documents$form$alreadyHaveADataManagementPlan
    if(is.null(already_with_DMP)){ return(NA) }
    if(is.logical(already_with_DMP)){
      return(
        case_when(already_with_DMP == TRUE ~ "yes",
                  already_with_DMP == FALSE ~ "no")
      )
    }
    already_with_DMP
  }
  ethics_applications <- ethics_applications |>
    mutate(already_with_DMP = map_chr(workflow_hres_ethics, tag_dmp))
  
  # Get the ETH codes
  ethics_applications <- ethics_applications |>
    mutate(application_id = map_chr(content, get_ETH))
  
  # Get the reviewers
  ethics_applications <- ethics_applications |>
    mutate(reviewers = map(content, get_reviewers))
  
  if(drop_raw){
    ethics_applications <- ethics_applications |>
      select(
        -content,
        -starts_with("workflow"), 
        -ethics_workflow,
        -notifications
      )
  }
  
  # Return the data frame of cleaned applications
  ethics_applications
}
