library(shiny)
library(dplyr)
library(tidyr)
library(ggplot2)

ui <- fluidPage(
  
  # Application title
  titlePanel("BNB Chain Relay Revenue - 30 days"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      numericInput("base_usd", "BNB/USD Price:", 271.79, min = .0001, max = 100000),
      numericInput("avg_gas_price_gwei", "Average Gas price Gwei:",  7, min = 1, max = 100000),
      numericInput("dao_premium", "DAO Premium:", .0075, min = .0001, max = 1),
      numericInput("relayer_premium", "Relayer Premium:", .15, min = .0001, max = 1),
      numericInput("gwei_cap", "Premium Cap Gwei:", 15, min = 1, max = 1000),
      numericInput("daily_tx_baseline", "Daily Tx Baseline:", 5000, min = 2000, 50000000)
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("allPlot"),
      plotOutput("relayPlot")
    )
  ),
  p("Stats found here: https://dune.com/trebor_yatska/evm-gas-markets",
    id = "dune_stats",
    style = "font-weight:bold;"),
  p("Average gas units consumed per tx: 121,933",
    id = "tx_vol",
    style = "font-weight:bold;"),
  p("Standard deviation of average daily tx volume: 20.8%",
    id = "tx_vol_sd",
    style = "font-weight:bold;"),
  p("Standard deviation of average daily gas price (gwei): 0.42",
    id = "gas_price_sd",
    style = "font-weight:bold;"),
  p("Standard deviation of average gas units consumed per tx: 6,092",
    id = "tx_vol_sd",
    style = "font-weight:bold;")
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  # model params
  # note, we take averages and variance of daily averages over the last 30 days. 
  # This makes sense since for each day, we are assuming our txs are all executed at the averages (gas price and units). 
  sd_daily_txs_constant <- .2084
  min_daily_txs <- 2000
  # sd of avg daily gas price
  sd_gas_price_gwei <- 0.42
  # sd of avg gas consumed per tx
  avg_gas_units_consumed <- 121933
  sd_gas_units_consumed <- 6092
  min_gas_price_gwei_polygon <- 1
  min_gas_units_consumed_polygon <- 21000
  # launch period in days
  n_beta <- 30
  # number of simulations per round
  p <- 500
  t <- 1:p
  
  relayer_beta <- function (t, n_beta, mean_daily_txs, sd_daily_txs, avg_gas_price_gwei_polygon, avg_gas_units_consumed_polygon, sd_gas_price_gwei_polygon, sd_gas_units_consumed, min_gas_price_gwei_polygon, min_gas_units_consumed_polygon, relayer_premium_gwei_price, dao_premium_gwei_price, gwei_premium_cap) {
    # create vectors to hold calcs
    daily_txs <- vector("list",n_beta)
    gas_units_consumed <- vector("list",n_beta)
    gas_price_gwei <- vector("list",n_beta)
    user_tx_cost <- vector("list",n_beta)
    relayer_revenue <- vector("list",n_beta)
    dao_revenue <- vector("list",n_beta)
    
    # simulate keeping tx count constant - beta period
    for (tx_day in 1:n_beta) {
      # variable path dependent gas price.
      if (tx_day == 1) {
        gas_price_gwei[[tx_day]] <- max(min_gas_price_gwei_polygon,  rnorm(1, avg_gas_price_gwei_polygon, sd_gas_price_gwei_polygon))
      } else {
        gas_price_gwei[[tx_day]] <- max(min_gas_price_gwei_polygon,  rnorm(1, gas_price_gwei[[tx_day-1]], sd_gas_price_gwei_polygon))
      }
      #now calculate path independent variables
      daily_txs[[tx_day]] <- max(min_daily_txs, rnorm(1, mean_daily_txs, sd_daily_txs))
      gas_units_consumed[[tx_day]] <- max(min_gas_units_consumed_polygon, rnorm(1, avg_gas_units_consumed_polygon, sd_gas_units_consumed)) * daily_txs[[tx_day]]
      user_tx_cost[[tx_day]] <- (gas_units_consumed[[tx_day]] * gas_price_gwei[[tx_day]])/1e9
      # relayer revenue
      relayer_revenue[[tx_day]] <- (gas_units_consumed[[tx_day]] * min((gas_price_gwei[[tx_day]] * relayer_premium_gwei_price),gwei_premium_cap))/1e9
      # dao revenue
      dao_revenue[[tx_day]] <- (gas_units_consumed[[tx_day]] * min((gas_price_gwei[[tx_day]] * dao_premium_gwei_price),gwei_premium_cap))/1e9
      
    }
    
    # combine lists
    beta_period_results <- c(rep(t,n_beta),rep(mean_daily_txs,n_beta),daily_txs,gas_units_consumed,gas_price_gwei,user_tx_cost,relayer_revenue,dao_revenue)
    dim(beta_period_results) <- c(n_beta,8)
    
    # return combined list
    return(beta_period_results)
  }
  
  get_plot_data <- reactive({
    #calculate sd of daily txs
    tx_volume = input$daily_tx_baseline
    sd_daily_txs <- tx_volume * sd_daily_txs_constant
    
    # generate bins based on input$bins from ui.R
    beta_relayer_earnings <- lapply(t, relayer_beta, mean_daily_txs=tx_volume, sd_daily_txs=sd_daily_txs,n_beta=n_beta, 
                                    avg_gas_price_gwei_polygon=input$avg_gas_price_gwei, 
                                    avg_gas_units_consumed_polygon=avg_gas_units_consumed, 
                                    sd_gas_price_gwei_polygon=sd_gas_price_gwei, 
                                    sd_gas_units_consumed=sd_gas_units_consumed,
                                    min_gas_price_gwei_polygon=min_gas_price_gwei_polygon,
                                    min_gas_units_consumed_polygon=min_gas_units_consumed_polygon,
                                    relayer_premium_gwei_price=input$relayer_premium,
                                    dao_premium_gwei_price=input$dao_premium,
                                    gwei_premium_cap = input$gwei_cap
    )
    
    # # convert list to df
    df <- as.data.frame(array(unlist(do.call(rbind,beta_relayer_earnings)),dim=c(p*n_beta,8)))
    colnames(df) <- c("sim_run","baseline_tx_count","daily_tx_count","daily_gas_units_consumed", "daily_gas_price_gwei_constant", "total_daily_user_tx_cost_native", 
                      "total_daily_relayer_revenue_native", "total_daily_dao_revenue_native")
    
    # # add user tx cost usd column and convert revenue columns to usd from native
    df <- df %>%
      mutate(total_daily_dao_revenue_usd = total_daily_dao_revenue_native * input$base_usd,
             total_daily_infra_charge_usd = (total_daily_relayer_revenue_native * input$base_usd) + total_daily_dao_revenue_usd)
    
    # aggregate by sim run
    df_sim_run <- df %>% 
      group_by(sim_run) %>%
      summarise(avg_dly_gas_price = mean(daily_gas_price_gwei_constant),
                totl_dao_rev_usd = sum(total_daily_dao_revenue_usd),
                totl_infra_rev_usd = sum(total_daily_infra_charge_usd))
    # return df
    df_sim_run
  })
  
  # total relay revenue plot
  output$allPlot <- renderPlot({
    
    # plot for all revenue
    ggplot(get_plot_data(), aes(x=totl_infra_rev_usd, y=avg_dly_gas_price)) +
      geom_point() +
      scale_x_continuous(labels=scales::dollar_format()) +
      labs(title = paste0("Total Tx Revenue - ", n_beta," days"), x = "total revenue usd", y = " avg daily gas price gwei")
    
  })
  
  # DAO revenue plot
  output$relayPlot <- renderPlot({
    
    # plot for DAO revenue
    ggplot(get_plot_data(), aes(x=totl_dao_rev_usd, y=avg_dly_gas_price)) +
      geom_point() +
      scale_x_continuous(labels=scales::dollar_format()) +
      labs(title = paste0("DAO Tx Revenue - ", n_beta," days"), x = "DAO revenue usd", y = " avg daily gas price gwei")
    
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
