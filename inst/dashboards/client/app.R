# dashboard for client

# library(shinydashboard)
library(shiny)
library(restbench)



client_app <- function(module_id = 'client'){

  ui <- function(module_id, label = "Task Viewer (Client)"){
    ns <- shiny::NS(module_id)
    shiny::tagList(
      dipsaus::use_shiny_dipsaus(),
      fluidRow(
        shinydashboard::box(
          width = 12L,
          title = shiny::span(shiny::icon("tasks"), "Task summary"),
          fluidRow(
            shiny::div(class = "col-xs-3",
                       fluidRow(shinydashboard::infoBoxOutput(ns("summary_task_latest"), width = 12L))),
            shiny::div(class = "col-xs-3",
                       fluidRow(shinydashboard::infoBoxOutput(ns("summary_task_init"), width = 12L))),
            shiny::div(class = "col-xs-3",
                       fluidRow(shinydashboard::infoBoxOutput(ns("summary_task_running"), width = 12L))),
            shiny::div(class = "col-xs-3",
                       fluidRow(shinydashboard::infoBoxOutput(ns("summary_task_finished"), width = 12L)))
          ),
          fluidRow(
            shiny::div(class = "col-xs-3", numericInput(ns("last_days"), "Display days", value = 10, min = 0)),
            shiny::div(class = "col-xs-3", shiny::checkboxGroupInput(
              ns('auto_refresh'), "Auto-update", inline = TRUE,
              choices = "On",
              selected = character(0)
            ))
          ),
          dipsaus::actionButtonStyled(ns('manual_update'), "Refresh Table", type = 'success')
        ),
        shinydashboard::box(
          width = 6L, title = "Query Results",
          style = 'height: 40vh; overflow-y: scroll;',
          DT::DTOutput(ns("task_table"))
        ),
        shinydashboard::box(
          width = 6L, title = "Task Details",
          style = 'height: 40vh; overflow-y: scroll;',
          shiny::uiOutput(ns('task_details'))
          # shiny::tags$pre(
          #   id = ns('task_details'),
          #   class = "shiny-text-output",
          #   style = "min-height: 50vh",
          #   "Click on a task to show the details"
          # )
        ),
        shiny::hr(),
        shiny::column(
          width = 12L,
          shiny::tags$pre(id = ns("console"), class = "shiny-text-output noplaceholder",
                          collapse = " ", style = "max-height: 40vh; overflow-y: scroll;")
        )
      )

    )
  }
  server <- function(module_id){
    ns <- shiny::NS(module_id)
    shiny::moduleServer(module_id, function(input, output, session) {

      # ---- data
      local_map <- dipsaus::list_to_fastmap2(list(
        refresh_interval = 5
      ))
      local_data <- shiny::reactiveValues(
        task_table = NULL,
        widths = c(0,0,0),
        expire = 864000,
        summary = list(
          total = NA, # total
          running = 0,
          finished = 0,
          init = 0,
          nonexpired = 0,
          valid = 0
        )
      )

      # ---- static functions
      # list all tasks on the client side
      refresh_table <- function(assign = TRUE){
        # get local database on tasks
        try({
          raw <- restbench::list_tasks(status = 'all', order = TRUE, expire = local_data$expire)
          task_table <- data.frame(
            `Readable Name` = stringr::str_remove_all(raw$name, sprintf("(^%s[_]+)|(__[a-zA-Z0-9]{16})", raw$userid)),
            Submitted = ifelse(raw$submitted > 0, sprintf("%s:%s", raw$serverip, raw$serverport), "No"),
            Finished = ifelse(raw$collected > 0, "Yes", "No"),
            `Time Created` = strftime(as.POSIXct(raw$time_added, origin="1970-01-01"), "%m-%d %H:%M", usetz = FALSE),
            stringsAsFactors = FALSE, check.names = FALSE
          )
          local_data$task_table <- task_table
          local_data$raw_table <- raw

          # calculate tasks
          running <- raw$submitted & !raw$collected
          notsubmit <- !raw$submitted & !raw$collected
          finished <- raw$collected
          task_counts <- list(
            total = nrow(raw), # total
            running = sum(running), # last 10 days
            finished = sum(finished), # last 10 days
            init = sum(notsubmit), # last 10 days
            valid = sum(!raw$removed) # total
          )
          local_data$summary <- task_counts

          # ----- total task, ??? tasks in the past 10 days, and not removed (valid)
          # init, running, finished
          return(task_table)
        })
        return(NULL)
      }

      # ---- reactive observes

      observe({
        if("On" %in% input$auto_refresh){
          shiny::invalidateLater(local_map$refresh_interval * 1000)
          refresh_table()
        }
      })

      observeEvent(input$last_days, {
        local_data$expire <- input$last_days * 86400
        if(!"On" %in% input$auto_refresh){
          refresh_table()
        }
      })

      output$console <- renderPrint({
        if(!length(local_data$console)){
          return(invisible())
        }
        if(is.character(local_data$console)){
          cat(local_data$console, sep = '\n')
        } else {
          print(local_data$console)
        }

      })

      output$task_table <- DT::renderDT({
        DT::datatable(
          local_data$task_table,
          options = list(
            autoWidth = FALSE,
            columnDefs = list(
              list(width = "20%", target = 0),
              list(width = "20%", target = 0),
              list(width = "10%", target = 0),
              list(width = "50%", target = 0)
            )
          ), rownames = FALSE, width = '100%', autoHideNavigation = TRUE,
          selection = list(mode = 'single', target = 'row'), editable = FALSE
        )

      })

      output$task_details <- shiny::renderUI({

        if(!length(input$task_table_rows_selected)){
          return(shiny::p("Click on a task to show the details"))
        }

        raw <- shiny::isolate({
          local_data$raw_table
        })
        if(!is.data.frame(raw)){return(shiny::p("Initializing..."))}

        shiny::isolate({
          task_name <- raw$name[[input$task_table_rows_selected[[1]]]]
          task <- restore_task2(task_name)

          local_data$task <- task

          if(is.null(task)){
            return("Cannot restore the task from local drive.")
          }

          res <- capture.output({print(task)})

          actions <- NULL
          if(dir.exists(task$task_dir) && task$submitted){
            resolved <- tryCatch({
              task$resolved()
            }, error = function(e){ FALSE })
            if(resolved){
              actions <- shiny::tagList(
                dipsaus::actionButtonStyled(ns("view_results"), label = 'View results', type = 'primary'),
                dipsaus::actionButtonStyled(ns("check_server"), label = "Check server (slow)", type = 'default')
              )
            } else {
              actions <- shiny::tagList(
                dipsaus::actionButtonStyled(ns("check_server"), label = "Check server (slow)", type = 'primary')
              )
            }
          } else if(!task$submitted){
            actions <- shiny::tagList(
              fluidRow(
                column(4L, textInput(ns("submit_protocol"), "Protocol", value = default_protocol())),
                column(4L, textInput(ns("submit_host"), "Host", value = default_host())),
                column(4L, numericInput(ns("submit_port"), "Port", value = default_port()))
              ),
              dipsaus::actionButtonStyled(ns("submit_task"), label = "Submit task", type = 'primary')
            )
          } else {
            actions = shiny::span(style = "color:red;", "Cannot find task files. The task was removed from your hard drive.")
          }


          shiny::column(
            12,
            shiny::p(
              shiny::HTML(paste(res[1:4], '<br />')),
              shiny::tags$pre(
                paste(res[-c(1:4,length(res))], collapse = '\n'),
                class = 'shiny-text-output noplaceholder'
              ),
              actions
            )
          )
        })


      })

      observeEvent(input$view_results, {
        task <- local_data$task
        if(!is.null(task)){
          local_data$console <- task$collect()
        } else {
          local_data$console <- NULL
        }

      })

      observeEvent(input$check_server, {
        task <- local_data$task
        if(!is.null(task)){
          s <- capture.output({
            tryCatch({
              if(task$submitted){
                newly_started <- ensure_server(host = task$submitted_to$host, port = task$submitted_to$port,
                                               make_default = FALSE, validate = TRUE)
                if(isTRUE(newly_started)){
                  on.exit({
                    try({server_kill(host = task$submitted_to$host, port = task$submitted_to$port)}, silent = TRUE)
                  })
                }
                status <- as.data.frame(task$server_status())
                cat("\nCheck server status:\n")
                print(status)

                if(nrow(status) && status$status == 'finish'){
                  cat("\nServer finished. Requesting for results...\n")
                  waited <- 1
                  while(!task$resolved()){
                    waited <- waited + 1
                    if(waited > 10){
                      stop("Cannot retrieve results from the server.")
                    }
                  }
                  print(task$collect())
                }
              }
            }, error = function(e){
              cat("\nError found!\n")
              print(e)
            })
          })
          local_data$console <- s
        } else {
          local_data$console <- NULL
        }
      })

      observeEvent(input$submit_task, {
        task <- local_data$task
        if(is.null(task)){
          local_data$console <- "Cannot submit to the server. This is not a valid task!"
          return()
        }

        local_data$console <- capture.output({
          tryCatch({
            task$protocol <- input$submit_protocol
            task$host <- input$submit_host
            task$port <- input$submit_port

            showNotification(p("Submitting to server... This may take a while"), duration = NULL,
                             closeButton = TRUE, type = 'message', id = ns('notif'))
            cat("Checking server is running...\n")
            ensure_server(host = task$host, port = task$port, protocol = task$protocol,
                          make_default = TRUE, validate = TRUE)

            cat(sprintf("Submitting task [%s] ...\n", task$task_name))
            task$submit(force = TRUE)

            cat("\nChecking server status...\n\n")
            print(as.data.frame(task$server_status()))

            showNotification(p("Submitted! Please refresh the task table."), duration = 10,
                             closeButton = TRUE, type = 'message', id = ns('notif'))
            invisible()


          }, error = function(e){
            showNotification(p("Error while submitting to server. See the console below."), duration = 10,
                             closeButton = TRUE, type = 'error', id = ns('notif'))
            cat("Error found!")
            print(e)
          })
        })


      })

      observeEvent(input$manual_update, {
        refresh_table()
      })


      # output$summary_task_total <- shinydashboard::renderInfoBox({
      #   shinydashboard::infoBox(subtitle = "Total tasks", value = local_data$summary$total,
      #                            color = 'light-blue', icon = shiny::icon("globe"), fill = FALSE)
      # })
      # output$summary_task_valid <- shinydashboard::renderValueBox({
      #   shinydashboard::valueBox(subtitle = "Local caches", value = local_data$summary$valid,
      #                            color = 'green', icon = shiny::icon("hdd-o"))
      # })
      output$summary_task_latest <- shinydashboard::renderInfoBox({
        shinydashboard::infoBox(
          title = "Total tasks",
          subtitle = shiny::tags$small(sprintf("Last %.0f days", local_data$expire / 86400)),
          value = local_data$summary$total,
          color = 'light-blue',
          icon = shiny::icon("globe"),
          fill = TRUE
        )
      })

      output$summary_task_init <- shinydashboard::renderInfoBox({
        shinydashboard::infoBox(
          title = "Initialized",
          value = local_data$summary$init,
          subtitle = shiny::tags$small("To be submitted"),
          color = 'red',
          width = local_data$widths[[1]],
          icon = shiny::icon("stop-circle"),
          fill = TRUE
        )
      })
      output$summary_task_running <- shinydashboard::renderInfoBox({
        shinydashboard::infoBox(
          title = "Running",
          value = local_data$summary$running,
          subtitle = shiny::tags$small("Play hard"),
          color = 'orange',
          width = local_data$widths[[2]],
          icon = shiny::icon("clock-o"),
          fill = TRUE
        )
      })
      output$summary_task_finished <- shinydashboard::renderInfoBox({
        shinydashboard::infoBox(
          title = "Finished",
          value = local_data$summary$finished,
          subtitle = shiny::tags$small("Yay!"),
          color = 'green',
          width = local_data$widths[[3]],
          icon = shiny::icon("hdd-o"),
          fill = TRUE
        )
      })


      # ----- initialization
      shiny::isolate({
        refresh_table()
      })

    })
  }
  list(ui = ui, server = server)
}


app <- client_app("restbench-client")
shiny::shinyApp(
  ui = shinydashboard::dashboardPage(
    skin = 'purple', title = "Restbench Viewer",
    shinydashboard::dashboardHeader(title = "Task Viewer (Client)"),
    shinydashboard::dashboardSidebar(disable = TRUE),
    shinydashboard::dashboardBody(app$ui("client-dashboard"))
  ),
  server = function(input, output, session) {
    app$server("client-dashboard")
  }, options = list(launch.browser = .rs.invokeShinyWindowViewer)
)

