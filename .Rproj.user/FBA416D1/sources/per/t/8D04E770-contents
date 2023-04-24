library(shiny)
library(plotrix)
library(ggplot2)
library(cluster)

# Define the UI
ui <- fluidPage(
  # Add a title to the page
  titlePanel("Customer Segmentation Visualization"),

  # Add tabs for each type of plot
  tabsetPanel(
    tabPanel("Gender Visualization",
             plotOutput("barplot1"),
             plotOutput("piechart1")),

    tabPanel("Visualization of Age Distribution",
             # sidebarPanel(
             #   sliderInput("bins",
             #               "Number of bins:",
             #               min = 1,
             #               max = 50,
             #               value = 30)
             # ),
             plotOutput("histogram1"),
             plotOutput("boxplot1"),
    ),

    tabPanel("Analysis of Annual Income",
             # sidebarPanel(
             #   sliderInput("bins",
             #               "Number of bins:",
             #               min = 1,
             #               max = 50,
             #               value = 30)
             # ),
             plotOutput("histogram2"),
             plotOutput("boxplot3"),
             plotOutput("histogram4")
    ),
    tabPanel("Analysis of Spending Score",
             # sidebarPanel(
             #   sliderInput("bins",
             #               "Number of bins:",
             #               min = 1,
             #               max = 50,
             #               value = 30)
             # ),
             plotOutput("histogram5"),
             plotOutput("boxplot2"),
             plotOutput("histogram3"),

    ),

    tabPanel("Elbow Method",
             plotOutput("barplot2")),

    tabPanel("Segmentation",
             plotOutput("scatterplot1"),
             plotOutput("scatterplot2")
             # plotOutput("scatterplot3")
    ),
  )
)

# Define the server
server <- function(input, output) {
  # Generate the scatterplots
  output$scatterplot1 <- renderPlot({
    ggplot(customer_data, aes(x =Annual.Income..k.., y = Spending.Score..1.100.)) +
      geom_point(stat = "identity", aes(color = as.factor(k6$cluster))) +
      scale_color_discrete(name=" ",
                           breaks=c("1", "2", "3", "4", "5", "6"),
                           labels=c("Cluster 1", "Cluster 2", "Cluster 3", "Cluster 4", "Cluster 5","Cluster 6")) +
      ggtitle("Segments of Mall Customers", subtitle = "Using K-means Clustering")
  })
  output$scatterplot2 <- renderPlot({
    ggplot(customer_data, aes(x =Spending.Score..1.100., y =Age)) +
      geom_point(stat = "identity", aes(color = as.factor(k6$cluster))) +
      scale_color_discrete(name=" ",
                           breaks=c("1", "2", "3", "4", "5","6"),
                           labels=c("Cluster 1", "Cluster 2", "Cluster 3", "Cluster 4", "Cluster 5","Cluster 6")) +
      ggtitle("Segments of Mall Customers", subtitle = "Using K-means Clustering")

  })
  # output$scatterplot3 <- renderPlot({
  #   clusplot(customer_data[, c("Spending.Score..1.100.", "Age")],
  #            y_kmeans,
  #            lines = 0,
  #            shade = TRUE,
  #            color = TRUE,
  #            labels = 2,
  #            plotchar = FALSE,
  #            span = TRUE,
  #            main = paste("Mall Customers"),
  #            xlab = 'Spending.Score..1.100.',
  #            ylab = 'Age')
  # })

  # Generate the boxplots
  output$boxplot1 <- renderPlot({
    boxplot(customer_data$Age,
            col="#ff0066",
            horizontal = TRUE,
            main="Boxplot for Descriptive Analysis of Age")
  })
  output$boxplot2 <- renderPlot({

    boxplot(customer_data$Spending.Score..1.100.,
            horizontal=TRUE,
            col="#990000",
            main="BoxPlot for Descriptive Analysis of Spending Score")

  })
  output$boxplot3 <- renderPlot({
    boxplot(customer_data$Annual.Income..k..,
            horizontal=TRUE,
            col="#990000",
            main="BoxPlot for Descriptive Analysis of Annual Income")
  })

  # Generate the histograms
  output$histogram1 <- renderPlot({
    hist(customer_data$Age[customer_data$Gender == "Male"],
         main = "Age Distribution",
         xlab = "Age", ylab = "Count", col = "blue",labels = TRUE,ylim = c(0,50))
    hist(customer_data$Age[customer_data$Gender == "Female"],
         add = TRUE, col = "#F45050",labels = TRUE,ylim = c(0,50))
    # legend("topright", legend = c("Male", "Female"), fill = c("blue", "pink"))
  })
  output$histogram2 <- renderPlot({
    hist(customer_data$Annual.Income..k..,
         col="#660033",
         main="Histogram for Annual Income",
         xlab="Annual Income Class",
         ylab="Frequency",
         labels=TRUE,
         ylim = c(0,80))
  })
  output$histogram3 <- renderPlot({
    # hist(customer_data$Spending.Score..1.100.,
    #      main="HistoGram for Spending Score",
    #      xlab="Spending Score Class",
    #      ylab="Frequency",
    #      col="#6600cc",
    #      labels=TRUE)

    hist(customer_data$Spending.Score..1.100.[customer_data$Gender == "Male"],
         main = "Histogram Spending Score By Gender",
         xlab = "Spending Score", ylab = "Count", col = "blue",ylim = c(0,60),labels = TRUE)
    hist(customer_data$Spending.Score..1.100.[customer_data$Gender == "Female"],
         add = TRUE, col = "pink",ylim=c(0,60),labels = TRUE)
    legend("topright", legend = c("Male", "Female"), fill = c("blue", "pink"))
  })

  output$histogram4 <- renderPlot({
    # hist(customer_data$Spending.Score..1.100.,
    #      main="HistoGram for Spending Score",
    #      xlab="Spending Score Class",
    #      ylab="Frequency",
    #      col="#6600cc",
    #      labels=TRUE)

    hist(customer_data$Annual.Income..k..[customer_data$Gender == "Male"],
         main = "Histogram Annual Income By Gender",
         xlab = "Annual Income", ylab = "Count", col = "blue",ylim = c(0,40),labels = TRUE)
    hist(customer_data$Annual.Income..k..[customer_data$Gender == "Female"],
         add = TRUE, col = "pink",ylim=c(0,40),labels = TRUE)
    legend("topright", legend = c("Male", "Female"), fill = c("blue", "pink"))
  })

  output$histogram5 <- renderPlot({
    hist(customer_data$Spending.Score..1.100.,
         col="#660033",
         main="Histogram for Spending Score",
         xlab="Annual Income Class",
         ylab="Frequency",
         labels=TRUE,
         ylim = c(0,110))
  })


  # Generate the barplots
  output$barplot1 <- renderPlot({
    barplot(a,main="Using BarPlot to display Gender Comparision",
            ylab="Count",
            xlab="Gender",
            ylim = c(0,250),
            col=c("#B2A4FF","#F45050"),
            width = c(10,10),
            legend=rownames(a))
  })
  output$barplot2 <- renderPlot({
    plot(k.values, iss_values,
         type="b", pch = 19, frame = FALSE,
         xlab="Number of clusters K",
         ylab="Total intra-clusters sum of squares")
  })


  # Generate the piecharts
  output$piechart1 <- renderPlot({
    pie3D(a,labels=lbs,
          main="Pie Chart Depicting Ratio of Female and Male")

  })
  # output$piechart2 <- renderPlot({
  #   pie(table(mtcars$carb))
  # })
  # output$piechart3 <- renderPlot({
  #   pie(table(mtcars$cyl))
  # })
}

# Run the app
shinyApp(ui = ui, server = server)

# File_2
