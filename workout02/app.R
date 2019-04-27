
library(shiny)
library(ggplot2)

ui <- fluidPage(
   titlePanel("Saving Investing Modalities"),
   sidebarLayout(
      fluidRow(
        column(4,sliderInput("ini_amo",
                     "Initial Amount",
                     min = 0,
                     max = 100000,
                     value = 1000)),
        column(4,sliderInput("ret_rat",
                             "Return Rate(in %)",
                             min = 0,
                             max = 20,
                             value = 5)),
        column(4,sliderInput("yea",
                     "Years",
                     min = 0,
                     max = 50,
                     value = 20)),
        column(4,sliderInput("ann_con",
                    "Annual Contribution",
                    min = 0,
                    max = 50000,
                    value = 2000)),
        column(4,sliderInput("gro_rat",
                    "Growth Rate(in %)",
                    min = 0,
                    max = 20,
                    value = 2)),
        column(4,selectInput("fac",
                    "Facet?",
                    c("No","Yes")
        ))
        )
      ,
      mainPanel(
        h4("Timelines"),
        plotOutput("plot"),
        h4("Balances"),
        dataTableOutput("sheet")
      )
   )
)

server <- function(input, output) {
  
   output$sheet <- renderDataTable({
       future_value <- function(amount = 100, rate = 0.01, years=10){
         return(amount * ((1 + rate) ^ years))
       }
       
       annuity <- function(contrib = 200, rate = 0.05, years = 6){
         return(contrib * (((1 + rate) ^ years - 1) / rate) )
       }
       
       growing_annuity <- function(contrib = 200, rate = 0.05, growth = 0.03, years = 1){
         return(contrib * (((1 + rate) ^ years - (1 + growth) ^ years) / (rate - growth)))
       }
       rate = 0.01 * input$ret_rat
       growth = 0.01 * input$gro_rat
       initial_investment = input$ini_amo
       annuity_amount = input$ann_con
       year = 0 : input$yea
       no_contrib = rep(0,length(year))
       fixed_contrib = rep(0,length(year))
       growing_contrib = rep(0,length(year))
       for(i in 1:length(year)){
         no_contrib[i] = future_value(initial_investment, rate, i-1)
       }
       for(i in 1:length(year)){
         fixed_contrib[i] =future_value(initial_investment, rate, i-1) + annuity(annuity_amount, rate, i-1)
       }
       for(i in 1:length(year)){
         growing_contrib[i] =future_value(initial_investment, rate, i-1) + growing_annuity(annuity_amount, rate, growth,  i-1)
       }
       modalities = as.data.frame(cbind(year, no_contrib, fixed_contrib,growing_contrib))
       modalities
     })

   output$plot <- renderPlot({
     future_value <- function(amount = 100, rate = 0.01, years=10){
       return(amount * ((1 + rate) ^ years))
     }
     
     annuity <- function(contrib = 200, rate = 0.05, years = 6){
       return(contrib * (((1 + rate) ^ years - 1) / rate) )
     }
     
     growing_annuity <- function(contrib = 200, rate = 0.05, growth = 0.03, years = 1){
       return(contrib * (((1 + rate) ^ years - (1 + growth) ^ years) / (rate - growth)))
     }
       rate = 0.01 * input$ret_rat
       growth = 0.01 * input$gro_rat
       initial_investment = input$ini_amo
       annuity_amount = input$ann_con
       year = 0 : input$yea
       no_contrib = rep(0,length(year))
       fixed_contrib = rep(0,length(year))
       growing_contrib = rep(0,length(year))
       for(i in 1:length(year)){
         no_contrib[i] = future_value(initial_investment, rate, i-1)
       }
       for(i in 1:length(year)){
         fixed_contrib[i] =future_value(initial_investment, rate, i-1) + annuity(annuity_amount, rate, i-1)
       }
       for(i in 1:length(year)){
         growing_contrib[i] =future_value(initial_investment, rate, i-1) + growing_annuity(annuity_amount, rate, growth,  i-1)
       }
       modalities = as.data.frame(cbind(year, no_contrib, fixed_contrib,growing_contrib))
       d = modalities
       d1 = as.data.frame(rep(year,3))
       d2 = as.data.frame(c(d[,2],d[,3],d[,4]))
       d2
       d3 = as.data.frame(c(rep("no_con",length(year)),rep("fix_con",length(year)),rep("gro_con",length(year))))
       d3
       d <- cbind(d1,d2,d3)
       colnames(d) <- c("year","money","mode")
       if(input$fac == "Yes"){
       ggplot(d, aes(x=year, y = money, color = factor(mode)), type = "l") + geom_line() + geom_point() + facet_wrap(~mode) + labs(title = "nine different savings scenarios") + theme_bw()  + theme(legend.title = element_blank()) }
       else{ggplot(d, aes(x=year, y = money, color = factor(mode)), type = "l") + geom_line() + geom_point() + labs(title = "Three models of investing") + theme_bw()  + theme(legend.title = element_blank()) }
     })
}


shinyApp(ui = ui, server = server)

