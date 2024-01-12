# Reporting app
library(shiny)
library(here)
library(hellohaplo)
library(bslib)
library(waiter)
library(shinyauthr)

source(here("R/clean_applications.R"))
source(here("R/count_reviews.R"))
source(here("R/format_reviews.R"))

applications_raw_tmp <- tempfile()

# Login
SKIP_LOGIN <- Sys.getenv("SKIP_LOGIN") %||% FALSE

## Login credentials
users <- tribble(
  ~username,                    ~password,                    ~permission, ~name,
  Sys.getenv("ADMIN_USERNAME"), Sys.getenv("ADMIN_PASSWORD"), "admin",     "Admin",
)

# Prepare a tab for login
login_tab <- tabPanel(
  title = icon("lock"), 
  value = "login", 
  shinyauthr::loginUI("login")
)


ui <- page_navbar(
  id = "tabs",
  header = waiter::use_waiter(),
  theme = bs_theme(
    fg = "#002328",
    bg = "white",
    primary = "#0c8066",
    secondary = "#e3dad8",
    "dark" = "#002328",
    "navbar-bg" = "#0c8066",
    base_font = font_google("Raleway", local = TRUE)
  ), 
  login_tab,
)

home_tab <- nav_panel(
  title = "Home",
  value = "home",
  page_fluid(
    h1("Ethics Applications Reviewed"),
    dataTableOutput("preview"),
    downloadButton("download", "Download formatted"),
    downloadButton("download_reviews", "Download reviews"),
    card(
      "Note: The figures refer to the number of ethics applications reviewed. If a reviewer reviwed one application multiple times, it's counted as one."
    )
  )
)

server <- function(input, output, session) {
  # Run the app only user logs in successfully
  observeEvent(credentials()$user_auth, {
    # if user logs in successfully
    if(credentials()$user_auth) { 
      # Show progress bar while cleaning data
      waiter <- waiter::Waiter$new(
        html = tagList(
          waiter::spin_3(),
          h4("Loading data... This may take ~5 minutes.")
        )
      )
      waiter$show()
      on.exit(waiter$hide())
      
      download_ethics_applications(
        file = applications_raw_tmp
      )
      
      applications_raw <- read_rds(applications_raw_tmp)
      
      applications_cleaned <- applications_raw |>
        clean_applications()
      
      reviews <- applications_cleaned |>
        count_reviews()
      
      reviews_formatted <- reviews |>
        format_reviews()
      
      output$preview <- renderDataTable({
        reviews_formatted
      })
      
      
      output$download <- downloadHandler(
        filename = function(){
          paste0("output.tsv")
        },
        content = function(file){
          write_tsv(reviews_formatted, file)
        })
      
      output$download_reviews <- downloadHandler(
        filename = "reviews.tsv",
        content = function(file){
          write_tsv(reviews, file)
        }
      )
      
      
    }})
  
  
  # Login-logout server actions
  credentials <- shinyauthr::loginServer(
    id = "login",
    data = users,
    user_col = username,
    pwd_col = password,
    log_out = reactive(logout_init())
  )
  
  logout_init <- shinyauthr::logoutServer(
    id = "logout",
    active = reactive(credentials()$user_auth)
  )
  # Take care the post-login behavior
  observeEvent(credentials()$user_auth, {
    # if user logs in successfully
    if(credentials()$user_auth) { 
      # remove the login tab
      removeTab("tabs", "login")
      # add home tab 
      appendTab("tabs", home_tab, select = TRUE)
    }
  })
  
}

shinyApp(ui, server)