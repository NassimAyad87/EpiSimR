library(shiny)
library(shinythemes)
library(dplyr)
library(deSolve)
library(DT)
library(openxlsx)



ui <- fluidPage(
    theme = shinytheme("cerulean"),
    titlePanel("A Shiny App to Simulate the Dynamics of Spread of Epidemic and Endemic Diseases"),
    tags$footer(
      style = "
      position: fixed;
      bottom: 0;
      width: 100%;
      text-align: right;
      padding-right: 20px;
      padding-top: 1px;
      padding-bottom: 2px;
      background-color: rgba(248, 249, 250, 0.1);
      border-top: 1px solid rgba(221, 221, 221, 0.1);
      z-index: 1000;",
      HTML("<strong> \u00A9 2025 Nassim AYAD - Licensed under GPL-3.0</strong>")
    ),
    sidebarLayout(
      sidebarPanel(
        h3("Parameter setting"),
        sliderInput("population", "Initial population size (N0):",
                    min = 1000, max = 10000, value = 5000, step = 1000),
        sliderInput("d", "Infectious period (days):",
                    min = 3, max = 20, value = 7, step = 1),
        sliderInput("r0", "Basic reproduction number (R0):",
                    min = 1, max = 12, value = 2, step = 0.1),
        sliderInput("time_simulation", "Time of simulation (days):",
                    min = 1, max = 3000, value = 1000, step = 1),
        selectInput(
          "model_type",
          "Choose the model:",
          choices = c("With latency period (SEIR model)" = "SEIR", "Without latency period (SIR model)" = "SIR"),
          selected = "SIR"
        ),
        conditionalPanel(
          condition = "input.model_type == 'SEIR'",
          sliderInput("l", "Latency period:",
                      min = 1, max = 14, value = 5, step = 1)
        ),
        selectInput(
          "demographics",
          "Include demographic changes (birth/death rates)?",
          choices = c("Yes" = "yes", "No" = "no"),
          selected = "no"
        ),
        conditionalPanel(
          condition = "input.demographics == 'yes'",
          sliderInput("birth_rate", "Birth rate (per day):",
                      min = 0, max = 0.01, value = 0.005, step = 0.001),
          sliderInput("death_rate", "Death rate (per day):",
                      min = 0, max = 0.01, value = 0.005, step = 0.001)
        ),
        selectInput(
          "immunity",
          "Does the disease induce lifelong (permanent) immunity?",
          choices = c("Yes" = "yes", "No" = "no"),
          selected = "yes"
        ),
        conditionalPanel(
          condition = "input.immunity == 'no'",
          sliderInput("m", "Immunity duration (days):",
                      min = 30, max = 240, value = 90, step = 5)
        ),
        selectInput(
          "vaccination",
          "Will the initial population be vaccinated?",
          choices = c("Yes" = "yes", "No" = "no"),
          selected = "no"
        ),
        conditionalPanel(
          condition = "input.vaccination == 'yes'",
          sliderInput("p", "Vaccination coverage of initial population (%):",
                      min = 0, max = 100, value = 30, step = 1),
          sliderInput("unit_price_vaccine", "Vaccine unit price (DZD):",
                      min = 0, max = 8000, value = 1400, step = 100)
        ),
        selectInput(
          "isolation",
          "Will the infectious individuals be isolated?",
          choices = c("Yes" = "yes", "No" = "no"),
          selected = "no"
        ),
        conditionalPanel(
          condition = "input.isolation == 'yes'",
          sliderInput("q", "Time from infectiousness to isolation (Hours):",
                      min = 1, max = 70, value = 48, step = 1),
          sliderInput("u", "Proportion of isolated infectious individuals (%):",
                      min = 0, max = 100, value = 30, step = 5)
        ),
        selectInput(
          "dynamics_choice",
          "Choose curves to display in the graphic:",
          choices = c("All" = "all", "Infections" = "infection"),
          selected = "all"
        )
      ),
      mainPanel(
        tabsetPanel(
          tabPanel(
            "Graphic",
            div(
              style = "display: flex; flex-direction: column; align-items: flex-end;",
              downloadButton(
                outputId = "download_plot",
                label = "Download plot",
                class = "btn-primary",
                style = "margin-bottom: 10px;"
              )
            ),
            plotOutput("dynamics", height = "600px")
          ),
          tabPanel(
            "Key outputs",
            tags$div(
              style = "font-size: 18px; line-height: 1.8; color: #444; padding: 10px; background-color: #f9f9f9; border: 1px solid #ddd; border-radius: 5px;",
              htmlOutput("outputs")
            )
          ),
          tabPanel(
            "Complete Outputs table",
            div(
              style = "display: flex; flex-direction: column; align-items: flex-end;",
              downloadButton(
                outputId = "download_data",
                label = "Download data table",
                class = "btn-primary",
                style = "margin-bottom: 10px;"
              ),
              dataTableOutput("table_output")
          )
        )
      )
    )
  )
)


server <- function(input, output) {
  # Simulation
  simulation <- reactive({
    # Parameters
    N0 <- input$population
    p <- input$p
    r0 <- input$r0
    d <- input$d
    l <- if (input$model_type == "SEIR") input$l else NA
    V0 <- if (input$vaccination == "yes") (N0 * (input$p / 100)) else 0
    I0 <- 1
    Q0 <- 0
    R0 <- 0
    S0 <- N0 - V0 - I0
    m <- if (input$immunity == "no") input$m else Inf
    q <- if (input$isolation == "yes") input$q/24 else Inf
    u <- if (input$isolation == "yes") input$u/100 else 0
    birth_rate <- if (input$demographics == "yes") input$birth_rate else 0
    death_rate <- if (input$demographics == "yes") input$death_rate else 0

    # Initial conditions
    if (input$model_type == "SEIR") {
      state <- c(N = N0, S = S0, E = 0, I = I0, Q = Q0, R = R0, V = V0)
    } else if (input$model_type == "SIR") {
      state <- c(N = N0, S = S0, I = I0, Q = Q0, R = R0, V = V0)
    }

    parameters <- c(r0 = r0, d = d, l = l, m = m, q = q, u = u, birth_rate = birth_rate, death_rate = death_rate)
    time <- seq(0, input$time_simulation, by = 1)

    # Models
    model_function <- if (input$model_type == "SEIR") {
      function(time, state, parameters) {
        with(as.list(c(state, parameters)), {
          dS <- birth_rate * N - death_rate * S - (r0 * (((1 / l) + death_rate) / (1 / l)) * ((1 / d) + death_rate) * S * I / N) + (1 / m) * R
          dE <- (r0 * (((1 / l) + death_rate) / (1 / l)) * ((1 / d) + death_rate) * S * I / N) - (1 / l) * E - death_rate * E
          dI <- (1 / l) * E - (1 / d) * (1 - u) * I - death_rate * I - (1 / q) * u * I
          dQ <- (1 / q) * u * I - (1 / (d - q)) * Q
          dR <- (1 / d) * (1 - u) * I - death_rate * R - (1 / m) * R + (1 / (d - q)) * Q
          dV <- - death_rate * V - (1 / m) * V
          dN <- dS + dE + dI + dQ + dR +dV
          new_infections <- r0 * (((1 / l) + death_rate) / (1 / l)) * ((1 / d) + death_rate) * S * I / N
          new_susceptibles <- birth_rate * N + (1 / m) * R + (1 / m) * V
          list(c(dN, dS, dE, dI, dQ, dR, dV), new_infections = new_infections, new_susceptibles = new_susceptibles)
        })
      }
    } else {
      function(time, state, parameters) {
        with(as.list(c(state, parameters)), {
          dS <- birth_rate * N - death_rate * S - (r0 * ((1 / d) + death_rate) * S * I / N) + (1 / m) * R
          dI <- (r0 * ((1 / d) + death_rate) * S * I / N) - (1 / d) * (1 - u) * I - death_rate * I - (1 / q) * u * I
          dQ <- (1 / q) * u * I - (1 / (d - q)) * Q
          dR <- (1 / d) * (1 - u) * I - death_rate * R - (1 / m) * R + (1 / (d - q)) * Q
          dV <- - death_rate * V - (1 / m) * V
          dN <- dS + dI + dQ + dR + dV
          new_infections <- r0 * ((1 / d) + death_rate) * S * I / N
          new_susceptibles <- birth_rate * N + (1 / m) * R + (1 / m) * V
          list(c(dN, dS, dI, dQ, dR, dV), new_infections = new_infections, new_susceptibles = new_susceptibles)
        })
      }
    }

    # Solve the model
    ode(y = state, times = time, func = model_function, parms = parameters) %>% as.data.frame()
  })

  # Summary of results
  output$outputs <- renderUI({
    out <- simulation()
    time_to_peak <- out$time[which.max(out$I)]
    peak_size <- round(max(out$I), digits = 0)
    total_infections <- round(sum(out$new_infections), digits = 0)
    attack_rate <- if (input$demographics == "yes" | input$immunity == "no")
      round((total_infections / (input$population + sum(out$new_susceptibles))) * 100, digits = 2)
    else round((total_infections / input$population) * 100, digits = 2)
    last_state <- if (input$model_type == "SEIR")
      round(out[dim(out)[1],5], digits = 0)
    else round(out[dim(out)[1],4], digits = 0)
    cost_vaccination <- round(input$p * input$population * input$unit_price_vaccine / 100)


    HTML(sprintf(
      "
      <ul>
        <li><b>Time to peak:</b> %d days</li>
        <li><b>Peak size:</b> %d individuals</li>
        <li><b>Total infections:</b> %d individuals</li>
        <li><b>Attack rate:</b> %.2f %%</li>
        <li><b>Last state of the epidemic/endemic:</b> %d infected individuals</li>
        <li><b>Vaccination cost:</b> %d DZD</li>
      </ul>",
      time_to_peak, peak_size, total_infections, attack_rate, last_state, cost_vaccination
    ))
  })

  # Graphic
  output$dynamics <- renderPlot({
    out <- simulation()
    if (input$dynamics_choice == "all") {
      cols <- if (input$model_type == "SEIR") c("S", "E", "I", "R", "new_infections") else c("S", "I", "R", "new_infections")
      labels <- if (input$model_type == "SEIR") c("Susceptible", "Exposed (Infected)", "Infectious", "Removed", "Daily new-infected") else c("Susceptible", "Infectious", "Removed", "Daily new-infected")
      colors <- if (input$model_type == "SEIR") c("blue", "orange", "red", "green", "brown") else c("blue", "red", "green", "brown")
      type <- if (input$model_type == "SEIR") c(1, 1, 1, 1, 3) else c(1,1,1,3)
      matplot(out$time, out[, cols], type = "l", lty = type, lwd = 2, col = colors,
              xlab = "Time (days)", ylab = "Individuals", main = if(input$demographics == "yes" | input$immunity == "no") paste(input$model_type, "Dynamics of endemic spread") else(paste(input$model_type, "Dynamics of epidemic spread")))
      legend("topright", legend = labels, col = colors, lty = type, lwd = 2)
    } else {
      colss <- if (input$model_type == "SEIR") c("E", "I", "new_infections") else c("I", "new_infections")
      labelss <- if (input$model_type == "SEIR") c("Exposed (Infected)", "Infectious", "Daily new-infected") else c("Infectious", "Daily new-infected")
      colorss <- if (input$model_type == "SEIR") c("orange", "red", "brown") else c("red", "brown")
      types <- if (input$model_type == "SEIR") c(1, 1, 3) else c(1,3)
      matplot(out$time, out[, colss], type = "l", lty = types, lwd = 2, col = colorss,
              xlab = "Time (days)", ylab = "Individuals", main = if(input$demographics == "yes" | input$immunity == "no") paste(input$model_type, "Dynamics of endemic spread") else(paste(input$model_type, "Dynamics of epidemic spread")))
      legend("topright", legend = labelss, col = colorss, lty = types, lwd = 2)

    }
  })

  # Download handler for exporting the graphic
  output$download_plot <- downloadHandler(
    filename = function() {
      paste("simulation_plot", Sys.Date(), ".png", sep = "")
    },
    content = function(file) {
      # Open PNG device
      png(file, width = 800, height = 600) # Adjust dimensions as needed

      # Plot logic
      out <- simulation()
      if (input$dynamics_choice == "all") {
        cols <- if (input$model_type == "SEIR") c("S", "E", "I", "R", "new_infections") else c("S", "I", "R", "new_infections")
        labels <- if (input$model_type == "SEIR") c("Susceptible", "Exposed (Infected)", "Infectious", "Removed", "Daily new-infected") else c("Susceptible", "Infectious", "Removed", "Daily new-infected")
        colors <- if (input$model_type == "SEIR") c("blue", "orange", "red", "green", "brown") else c("blue", "red", "green", "brown")
        type <- if (input$model_type == "SEIR") c(1, 1, 1, 1, 3) else c(1, 1, 1, 3)
        matplot(out$time, out[, cols], type = "l", lty = type, lwd = 2, col = colors,
                xlab = "Time (days)", ylab = "Individuals", main = if(input$demographics == "yes" | input$immunity == "no") paste(input$model_type, "Dynamics of endemic spread") else(paste(input$model_type, "Dynamics of epidemic spread")))
        legend("topright", legend = labels, col = colors, lty = type, lwd = 2)
      } else {
        colss <- if (input$model_type == "SEIR") c("E", "I", "new_infections") else c("I", "new_infections")
        labelss <- if (input$model_type == "SEIR") c("Exposed (Infected)", "Infectious", "Daily new-infected") else c("Infectious", "Daily new-infected")
        colorss <- if (input$model_type == "SEIR") c("orange", "red", "brown") else c("red", "brown")
        types <- if (input$model_type == "SEIR") c(1, 1, 3) else c(1, 3)
        matplot(out$time, out[, colss], type = "l", lty = types, lwd = 2, col = colorss,
                xlab = "Time (days)", ylab = "Individuals", main = if(input$demographics == "yes" | input$immunity == "no") paste(input$model_type, "Dynamics of endemic spread") else(paste(input$model_type, "Dynamics of epidemic spread")))
        legend("topright", legend = labelss, col = colorss, lty = types, lwd = 2)
      }

      # Close the PNG device
      dev.off()
    }
  )

  # Tidy table of outputs
  output$table_output <- renderDataTable({
    out <- simulation()

    # Determine columns to display based on the selected model type
    if (input$model_type == "SEIR") {
      cols <- c("time", "S", "E", "I", "R", "new_infections")
    } else if (input$model_type == "SIR") {
      cols <- c("time", "S", "I", "R", "new_infections")
    }

    # Select only the relevant columns for the table
    out %>%
      select(all_of(cols)) %>%
      mutate(across(where(is.numeric), ~ round(.x, 4))) %>%
      datatable(
        rownames = FALSE,
        options = list(
          searching = FALSE,
          paging = TRUE,
          scrollX = TRUE
        )
      )
  })
  # Download handler for exporting the table
  output$download_data <- downloadHandler(
    filename = function() {
      paste("simulation_output", Sys.Date(), ".xlsx", sep = "")
    },
    content = function(file) {
      out <- simulation()

      # Select relevant columns based on the model type
      if (input$model_type == "SEIR") {
        cols <- c("time", "S", "E", "I", "R", "new_infections")
      } else if (input$model_type == "SIR") {
        cols <- c("time", "S", "I", "R", "new_infections")
      }

      # Export the filtered and rounded table to CSV
      out %>%
        select(all_of(cols)) %>%
        mutate(across(where(is.numeric), ~ round(.x, 4))) %>%
        write.xlsx(file, row.Names = FALSE)
    }
  )
}

# Launch the app
shinyApp(ui = ui, server = server)
