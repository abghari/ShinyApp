#Server code - Backend side of data

##Load Libs
library(shiny)
library(tidyverse)
library(ggplot2)
library(jsonlite)
library(plotly)

###Play with data
####Load Data
json_file <- read_json("orders5.2.2020",simplifyVector = TRUE)
json13 <- read_json("orders202052.json",simplifyVector = TRUE)
json14 <- read_json("orders202053.json",simplifyVector = TRUE)
json15 <- read_json("orders202054.json",simplifyVector = TRUE)
df <- as.data.frame(json_file)
df13 <- as.data.frame(json13)
df14 <- as.data.frame(json14)
df15 <- as.data.frame(json15)
df <- rbind_pages(list(df,df13,df14,df15))
#df <- dplyr::distinct(df)
df <- df[!duplicated(df$data.orders.order_guid),]
rm(df13,df14,df15,json_file,json13,json14,json15)

####Data Preparation
df1 <- do.call(cbind,df)
df1 <- df1[,-c(27)]
df1 <- do.call(cbind,df1)

#####convert list data of item columns into dataframe
items <- do.call(rbind,
                 lapply(lapply(1:nrow(df), 
                               function(i,x,y,z) cbind(x[i], y[i],do.call(rbind,z[i])), 
                               x = df$data.orders.delivery_info$recipient_name, 
                               y = df$data.orders.payment_time,
                               z = df$data.orders.basket$items),
                        as.data.frame))

#####Delievery date
df1$del_date <- as.Date(df1$data.orders.delivery_time.delivery_time_id)
#####Basket creation time
df1$basket_date <- as.POSIXct(df1$data.orders.basket.create_time,origin="1970-01-01")
#####Payment time
df1$pay_date <- as.POSIXct(df1$data.orders.payment_time,origin="1970-01-01")

####Extract KPI
#####Orders
orders <- as.data.frame(tapply(df1$data.orders.status,as.Date(df1$pay_date),length))
colnames(orders) <- c("orders")
orders$date <- as.Date(row.names(orders))
last_order <- as.integer(orders$orders[orders$date == max(orders$date)])

#####Avg price
df1$basket_value <- df1$data.orders.final_amount - df1$data.orders.delivery_type.amount
avg_basket_val <- as.data.frame(tapply(df1$basket_value,as.Date(df1$pay_date),mean,na.rm=T))
colnames(avg_basket_val) <- c("avg_pay")
avg_basket_val$date <- as.Date(row.names(avg_basket_val))
last_avg <- as.integer(avg_basket_val$avg_pay[avg_basket_val$date == max(avg_basket_val$date)])

#####New Users
new_users <- as.data.frame(tapply(as.character(as.Date(df1$pay_date)),df1$data.orders.delivery_info.recipient_mobile,min))
new_users <- data.frame(table(new_users$`tapply(as.character(as.Date(df1$pay_date)), df1$data.orders.delivery_info.recipient_mobile, `))
colnames(new_users) <- c("date","users")
new_users$date <- as.Date(new_users$date) 
last_new <- new_users$users[new_users$date == max(new_users$date)]

#####Orders amount from each stations
station_orders <- data.frame(table(df1$data.orders.basket.provider_title[df1$del_date == max(df1$del_date[!is.na(df1$del_date)])]))
station_orders <- station_orders[station_orders$Var1 != "تره بار" , ]
station_orders <- station_orders[order(station_orders$Freq),]

#####Delivery interval
df1 <- cbind(df1,str_split_fixed(as.character(df1$data.orders.delivery_time.delivery_time_id), "-", 2))
del_int <- data.frame(table(as.character(df1$`2`[df1$del_date == max(df1$del_date[!is.na(df1$del_date)])])))



##Make Server Function
server <- function(input, output){
  ##Outputs
  
  ###Last date record
  output$date <- renderText({
    as.character(max(as.Date(df1$pay_date)))
  })
  
  ###demands
  output$last_demand <- renderText({
    last_order
  })
  output$demand <- renderPlotly({
    f <- list(
      family = "Courier New, monospace",
      size = 18,
      color = "#7f7f7f"
    )
    x <- list(
      title = "Date",
      titlefont = f
    )
    y <- list(
      title = "Orders",
      titlefont = f
    )
    fig <- plot_ly(data = orders, x = ~date, y = ~orders, type = "scatter", mode = "lines")
    fig <- fig %>% layout(xaxis = x, yaxis = y)
    fig
  })
  
  ###Avg prices
  output$last_avg_pay <- renderText({
    last_avg
  })
  output$avg_pay <- renderPlotly({
    f <- list(
      family = "Courier New, monospace",
      size = 18,
      color = "#7f7f7f"
    )
    x <- list(
      title = "Date",
      titlefont = f
    )
    y <- list(
      title = "Payment (Toman)",
      titlefont = f
    )
    fig <- plot_ly(data = avg_basket_val, x = ~date, y = ~avg_pay, type = "scatter", mode = "lines")
    fig <- fig %>% layout(xaxis = x, yaxis = y)
    fig
  })
  
  ###New users
  output$last_new_users <- renderText({
    last_new
  })
  output$new_users <- renderPlotly({
    f <- list(
      family = "Courier New, monospace",
      size = 18,
      color = "#7f7f7f"
    )
    x <- list(
      title = "Date",
      titlefont = f
    )
    y <- list(
      title = "New Customers",
      titlefont = f
    )
    fig <- plot_ly(data = new_users, x = ~date, y = ~users, type = "scatter", mode = "lines")
    fig <- fig %>% layout(xaxis = x, yaxis = y)
    fig
  })
  
  ###Number of baskets which should be delivered by each stations
  output$station_orders <- renderPlotly({
    f <- list(
      family = "Courier New, monospace",
      size = 18,
      color = "#7f7f7f"
    )
    x <- list(
      title = "Frequency",
      titlefont = f
    )
    y <- list(
      title = "",
      titlefont = f,
      categoryarray = ~c(station_orders$Var1)
    )
    fig <- plot_ly(data = station_orders, x = ~Freq, y = ~Var1, type = "bar", orientation = "h")
    fig <- fig %>% layout(xaxis = x, yaxis = y)
    fig
  })
  
  ###Py chart of delivery interval
  output$del_int <- renderPlotly({
    fig <- plot_ly(data = del_int, labels = ~Var1, values = ~Freq, type = "pie")
    #fig <- fig %>% add_pie(hole = 0.6)
    fig <- fig %>% layout(xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                          yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    fig
  })
  
  #dataInput <- reactive({
  #  data <- data.frame(tapply(items$count[as.Date(as.POSIXct(items$`y[i]`,origin="1970-01-01")) == max(as.Date(as.POSIXct(items$`y[i]`,origin="1970-01-01")))],
  #                             items$title[as.Date(as.POSIXct(items$`y[i]`,origin="1970-01-01")) == max(as.Date(as.POSIXct(items$`y[i]`,origin="1970-01-01")))],
  #                             sum))
  #  data
  #})
}


