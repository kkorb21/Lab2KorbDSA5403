#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(Lab2KorbDSA5403)

ui <- fluidPage(
  titlePanel("Bayesian Coin Model: Prior, Likelihood, Posterior"),

  sidebarLayout(
    sidebarPanel(
      sliderInput("gridlen", "Theta grid length:",
                  min = 11, max = 501, value = 101, step = 10),

      selectInput("priorType", "Prior distribution:",
                  choices = c("Uniform", "Triangular")),

      sliderInput("alpha", "Alpha (for equal-tail interval):",
                  min = 0.01, max = 0.20, value = 0.05, step = 0.01),

      numericInput("n", "Number of trials (n):", value = 20, min = 1),
      numericInput("z", "Number of successes (z):", value = 12, min = 0)
    ),

    mainPanel(
      plotOutput("triPlot", height = "400px"),
      tableOutput("intervalTable")
    )
  )
)

server <- function(input, output, session) {

  output$triPlot <- renderPlot({

    theta <- seq(0, 1, length.out = input$gridlen)

    # Prior selection
    prior <- switch(input$priorType,
                    "Uniform" = rep(1/length(theta), length(theta)),
                    "Triangular" = {
                      p <- pmin(theta, 1 - theta)
                      p / sum(p)
                    })

    # Call coin() from your package
    res <- coin(theta, prior, n = input$n, z = input$z, alpha = input$alpha)

    posterior <- res$posterior
    likelihood <- dbinom(input$z, size = input$n, prob = theta)

    plot(theta, prior, type = "l", lwd = 2, col = "blue",
         ylim = c(0, max(c(prior, likelihood, posterior))),
         ylab = "Density / Probability", xlab = expression(theta),
         main = "Prior, Likelihood, Posterior")

    lines(theta, likelihood / max(likelihood) * max(prior),
          col = "darkgreen", lwd = 2, lty = 2)

    lines(theta, posterior, col = "red", lwd = 2)

    legend("topright",
           legend = c("Prior", "Likelihood (scaled)", "Posterior"),
           col = c("blue", "darkgreen", "red"),
           lwd = c(2,2,2), lty = c(1,2,1))
  })

  output$intervalTable <- renderTable({

    theta <- seq(0, 1, length.out = input$gridlen)

    prior <- switch(input$priorType,
                    "Uniform" = rep(1/length(theta), length(theta)),
                    "Triangular" = {
                      p <- pmin(theta, 1 - theta)
                      p / sum(p)
                    })

    res <- coin(theta, prior, n = input$n, z = input$z, alpha = input$alpha)

    data.frame(
      Alpha = input$alpha,
      L = round(res$interval["L"], 4),
      U = round(res$interval["U"], 4)
    )
  })
}

shinyApp(ui, server)
