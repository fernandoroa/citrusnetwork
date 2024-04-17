ui <- dashboardPage(
  title = "Citrus genealogy",
  dashboardHeader(title = p(em("Citrus"), "genealogy")),
  dashboardSidebar(
    width = 0
  ),
  dashboardBody(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "app.css"),
      tags$style(HTML("
        .nav-tabs-custom > .nav-tabs > li > a {
           background-color: #f4b943;
           color: #FFF;
        }
        .tab-content tab-pane {
        }
        .nav-tabs-custom > .nav-tabs > li[class=active] > a {
           background-color: green;
           color:white
        }")),
      tags$style(HTML(".box {min-width: 840px;}")),
      tags$style(HTML("
                                /* logo */
                                .skin-blue .main-header .logo {
                                background-color: #f4b943;
                                }

                                /* logo when hovered */
                                .skin-blue .main-header .logo:hover {
                                background-color: #f4b943;
                                }

                                /* navbar (rest of the header) */
                                .skin-blue .main-header .navbar {
                                background-color: #f4b943;
                                }

                                /* main sidebar */
                                .skin-blue .main-sidebar {
                                background-color: #f4b943;
                                }

                                /* active selected tab in the sidebarmenu */
                                .skin-blue .main-sidebar .sidebar .sidebar-menu .active a{
                                background-color: #ff0000;
                                }

                                /* other links in the sidebarmenu */
                                .skin-blue .main-sidebar .sidebar .sidebar-menu a{
                                background-color: #00ff00;
                                color: #000000;
                                }

                                /* other links in the sidebarmenu when hovered */
                                .skin-blue .main-sidebar .sidebar .sidebar-menu a:hover{
                                background-color: #ff69b4;
                                }
                                /* toggle button when hovered  */
                                .skin-blue .main-header .navbar .sidebar-toggle:hover{
                                background-color: #ff69b4;
                                }

                                /* body */
                                .content-wrapper, .right-side {
                                background-color: #7da2d1;
                                }

                                "))
    ), fluidRow(
      column(
        width = 4,
        passwordInput("password", "Password", placeholder = "passw. needed during review - contact authors!")
      )
    ),

    #
    #     down big boxes
    #
    fluidRow(
      conditionalPanel(
        condition = "input.password==''",
        tabBox(
          title = "Figure S2. Parental network and admixture",
          id = "tabsetLeft",
          selected = "k=4",
          tabPanel(
            "k=4",
            div(
              style = "text-align: center;",
              checkboxInput("checkboxk4", "Show names of molec. study",
                value = FALSE, width = "100%"
              )
            ),
            div(
              style = "overflow-y:auto; overflow-x:auto",
              uiOutput("k4image")
            ),
            div(
              style = "text-align: left;",
              uiOutput("k4Legend")
            )
          ),
          tabPanel(
            "k=3",
            div(
              style = "text-align: center;",
              checkboxInput("checkboxk3", "Show names of molec. study",
                value = FALSE, width = "100%"
              )
            ),
            div(
              style = "overflow-y:auto;overflow-x:auto",
              uiOutput("k3image")
            ),
            div(
              style = "text-align: left;",
              uiOutput("k3Legend")
            )
          )
        ),
        uiOutput("mytabBox")
      )
    )
  )
)
