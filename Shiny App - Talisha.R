library(shiny)
library(ggplot2)
library(dplyr)
library(readr)
library(tidytext)
library(tidyverse)
library(wordcloud)
library(tm)

# Read the dataset
df_airbnb <- read.csv("~/Desktop/Business Unstructured Data/listingsAndReviews.csv")

# Define a vector of common English stop words
stopwords_vector <- c("i", "me", "de", "e", "my", "myself", "we", "our", "ours", "ourselves", "you", "your", "yours", "yourself", "yourselves", 
                      "he", "him", "his", "himself", "she", "her", "hers", "herself", "it", "its", "itself", "they", "them", 
                      "their", "theirs", "themselves", "what", "which", "who", "whom", "this", "that", "these", "those", 
                      "am", "is", "are", "was", "were", "be", "been", "being", "have", "has", "had", "having", "do", "does", 
                      "did", "doing", "a", "an", "the", "and", "but", "if", "or", "because", "as", "until", "while", "of", 
                      "at", "by", "for", "with", "about", "against", "between", "into", "through", "during", "before", "after", 
                      "above", "below", "to", "from", "up", "down", "in", "out", "on", "off", "over", "under", "again", 
                      "further", "then", "once", "here", "there", "when", "where", "why", "how", "all", "any", "both", "each", 
                      "few", "more", "most", "other", "some", "such", "no", "nor", "not", "only", "own", "same", "so", 
                      "than", "too", "very", "s", "t", "can", "will", "just", "don", "should", "now")

# Define column names (adjust these as per your actual dataset columns)
roomTypeColumn <- "room_type"
bedroomsColumn <- "bedrooms"
priceColumn <- "price"
reviewsColumn <- "number_of_reviews"

# Define UI
ui <- navbarPage("Airbnb Data Analysis Dashboard",
                 tabPanel("Data Analysis",
                          sidebarLayout(
                            sidebarPanel(
                              selectInput("roomTypeInput", "Select Room Type", 
                                          choices = unique(df_airbnb[[roomTypeColumn]]), 
                                          selected = unique(df_airbnb[[roomTypeColumn]])[1]),
                              sliderInput("bedroomsInput", "Number of Bedrooms",
                                          min = min(df_airbnb[[bedroomsColumn]], na.rm = TRUE), 
                                          max = max(df_airbnb[[bedroomsColumn]], na.rm = TRUE), 
                                          value = c(min(df_airbnb[[bedroomsColumn]], na.rm = TRUE), max(df_airbnb[[bedroomsColumn]], na.rm = TRUE))),
                              numericInput("minPriceInput", "Minimum Price", 
                                           min = 45, value = 50, step = 15),
                              actionButton("submit", "Apply Filters")
                            ),
                            mainPanel(
                              plotOutput("priceReviewPlot"),
                              plotOutput("avgPriceRoomTypePlot")
                            )
                          )
                 ),
                 tabPanel("Description Analysis",
                          sidebarLayout(
                            sidebarPanel(
                              sliderInput("numWords", 
                                          "Number of top words:", 
                                          min = 5, 
                                          max = 20, 
                                          value = 10),
                              selectInput("roomTypeDesc", 
                                          "Select Room Type:", 
                                          choices = c("All", unique(df_airbnb$room_type))),
                              actionButton("update", "Update")
                            ),
                            mainPanel(
                              plotOutput("descPlot")
                            )
                          )
                 ),
                 tabPanel("Sentiment Analysis",
                          sidebarLayout(
                            sidebarPanel(
                              selectInput("lexicon_choice", "Choose Lexicon", choices = c("afinn", "nrc", "bing")),
                              selectInput("text_type", "Choose Text Type", choices = c("Descriptions", "Reviews"))
                            ),
                            mainPanel(
                              plotOutput("sentiment_plot")
                            )
                          )
                 ),
                 tabPanel("Word Cloud",
                          sidebarLayout(
                            sidebarPanel(
                              selectInput("selection", "Choose Data Type:", 
                                          choices = c("Descriptions", "Reviews"))
                            ),
                            mainPanel(
                              plotOutput("wordcloudPlot")
                            )
                          )
                 ),
                 tabPanel("Interactive Visualization",
                          sidebarLayout(
                            sidebarPanel(
                              selectInput("roomTypeInteractive", "Select Room Type:", 
                                          choices = unique(df_airbnb$room_type), selected = "Entire home/apt"),
                              sliderInput("priceRange", "Price Range:", 
                                          min = min(df_airbnb$price, na.rm = TRUE), max = max(df_airbnb$price, na.rm = TRUE), 
                                          value = c(min(df_airbnb$price, na.rm = TRUE), max(df_airbnb$price, na.rm = TRUE))),
                              sliderInput("reviewRange", "Number of Reviews Range:", 
                                          min = min(df_airbnb$number_of_reviews, na.rm = TRUE), max = max(df_airbnb$number_of_reviews, na.rm = TRUE), 
                                          value = c(min(df_airbnb$number_of_reviews, na.rm = TRUE), max(df_airbnb$number_of_reviews, na.rm = TRUE))),
                              numericInput("binsInput", "Number of Bins for Histogram:", 
                                           value = 30, min = 1, max = 100)
                            ),
                            mainPanel(
                              plotOutput("histPlot")
                            )
                          )
                 )
)

# Define server logic
server <- function(input, output) {
  
  # For Data Analysis Tab
  # Reactive expression to filter data based on input
  filteredData <- reactive({
    req(input$submit) # Require an action button click to trigger the filtering
    df_airbnb %>%
      filter(df_airbnb[[roomTypeColumn]] == input$roomTypeInput,
             df_airbnb[[bedroomsColumn]] >= input$bedroomsInput[1],
             df_airbnb[[bedroomsColumn]] <= input$bedroomsInput[2],
             df_airbnb[[priceColumn]] >= input$minPriceInput)
  })
  
  # Render the scatter plot
  output$priceReviewPlot <- renderPlot({
    filtered_data <- filteredData()
    ggplot(filtered_data, aes_string(x = priceColumn, y = reviewsColumn)) +
      geom_point(aes(color = filtered_data[[reviewsColumn]])) +
      labs(title = "Price vs Number of Reviews", x = "Price", y = "Number of Reviews") +
      theme_minimal()
  })
  
  # Render the bar plot for average price per room type
  output$avgPriceRoomTypePlot <- renderPlot({
    filtered_data <- filteredData()
    avg_price_data <- filtered_data %>%
      group_by(filtered_data[[roomTypeColumn]]) %>%
      summarize(avg_price = mean(filtered_data[[priceColumn]], na.rm = TRUE))
    ggplot(avg_price_data, aes_string(x = roomTypeColumn, y = "avg_price", fill = roomTypeColumn)) +
      geom_bar(stat = "identity") +
      labs(title = "Average Price per Room Type", x = "Room Type", y = "Average Price") +
      theme_minimal()
  })
  
  # For Description Analysis Tab
  output$descPlot <- renderPlot({
    req(input$update) # Require an action button click to update the plot
    filtered_data <- if (input$roomTypeDesc == "All") {
      df_airbnb
    } else {
      df_airbnb %>% filter(room_type == input$roomTypeDesc)
    }
    
    tidy_descriptions <- filtered_data %>%
      select(description) %>%
      unnest_tokens(word, description) %>%
      filter(!word %in% stopwords_vector) %>%
      count(word, sort = TRUE) %>%
      top_n(input$numWords)
    
    ggplot(tidy_descriptions, aes(x = reorder(word, n), y = n)) +
      geom_bar(stat = "identity") +
      coord_flip() +
      labs(title = paste("Top", input$numWords, "Frequent Words in Airbnb Descriptions"),
           x = "Words", y = "Frequency") +
      theme_minimal()
  })
  
  # For Sentiment Analysis Tab
  # Load the sentiment lexicons
  afinn <- get_sentiments("afinn")
  nrc <- get_sentiments("nrc")
  bing <- get_sentiments("bing")
  
  # Combine the sentiment lexicons
  sentiments <- bind_rows(mutate(afinn, lexicon="afinn"),
                          mutate(nrc, lexicon="nrc"),
                          mutate(bing, lexicon="bing"))
  
  output$sentiment_plot <- renderPlot({
    selected_text <- if(input$text_type == "Reviews") {
      unnest_tokens(df_airbnb, word, reviews)
    } else {
      unnest_tokens(df_airbnb, word, description)
    }
    
    tidy_text <- selected_text %>%
      inner_join(sentiments, by = "word") %>%
      filter(lexicon == input$lexicon_choice)
    
    if(nrow(tidy_text) > 0 && length(unique(tidy_text$sentiment)) > 1) {
      ggplot(tidy_text, aes(x = sentiment, fill = sentiment)) +
        geom_bar() +
        labs(title = paste("Sentiment Distribution for", input$lexicon_choice, "Lexicon in", input$text_type),
             x = "Sentiment", y = "Count") +
        theme_minimal()
    } else {
      ggplot() + 
        labs(title = paste("No data available for", input$lexicon_choice, "Lexicon in", input$text_type),
             x = "", y = "") +
        theme_void()
    }
  })
  
  # For Word Cloud Tab
  output$wordcloudPlot <- renderPlot({
    req(df_airbnb)
    data <- if (input$selection == "Descriptions") {
      df_airbnb$description
    } else {
      df_airbnb$reviews
    }
    
    corpus <- Corpus(VectorSource(data))
    corpus <- tm_map(corpus, content_transformer(tolower))
    corpus <- tm_map(corpus, removePunctuation)
    corpus <- tm_map(corpus, removeNumbers)
    corpus <- tm_map(corpus, removeWords, stopwords("en"))
    
    dtm <- TermDocumentMatrix(corpus)
    m <- as.matrix(dtm)
    v <- sort(rowSums(m), decreasing=TRUE)
    d <- data.frame(word = names(v), freq = v)
    
    set.seed(1234)
    wordcloud(words = d$word, freq = d$freq, min.freq = 1,
              max.words = 100, random.order = FALSE, 
              rot.per = 0.35, colors = brewer.pal(8, "Dark2"))
  })
  
  # For Interactive Visualization Tab
  output$histPlot <- renderPlot({
    filtered_data <- df_airbnb %>%
      filter(room_type == input$roomTypeInteractive,
             price >= input$priceRange[1], price <= input$priceRange[2],
             number_of_reviews >= input$reviewRange[1], number_of_reviews <= input$reviewRange[2])
    
    ggplot(filtered_data, aes(x = price, fill = ..count..)) +
      geom_histogram(bins = input$binsInput, color = "black", alpha = 0.7) +
      scale_fill_gradient(low = "blue", high = "red") +
      labs(title = paste("Histogram of Prices for", input$roomTypeInteractive, "Listings"),
           x = "Price", y = "Count") +
      theme_minimal()
  })
  
}

# Run the application
shinyApp(ui = ui, server = server)
