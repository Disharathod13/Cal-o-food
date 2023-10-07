library(shiny)
library(dplyr)
library(ggplot2)
library(plotly)
library(shinyWidgets)
library(Calofood)

data <- fastfoodcalories

# Creating the UI of the application
ui <- fluidPage(
  setBackgroundColor(
    color = c("#F7FBFF", "#2171B5"),
    gradient = "linear",
    direction = "bottom"
  ),
  titlePanel("Cal-O-food"),
  tabsetPanel(
    # Tab 1: Health goal and Restaurant Selection
    tabPanel("Your Health Goal & Restaurant",
             fluidRow(
               sidebarPanel(
                 radioButtons("goal_type", "Select Health Goal:",
                              choices = c("Heart Health", "Sugar Control", "Preventing High Blood Pressure")),
                 selectInput("restaurant", "Select Restaurant:",
                             choices = c("All Restaurants", unique(data$restaurant)))
               ),
               mainPanel(
                 fluidRow(
                   plotOutput("recommendation_plot"),
                   verbatimTextOutput("description")
                 )
               )
             ),
             fluidRow(
               column(
                 width = 12,
                 align = "left",
                 style = "padding: -35px 20px;",
                 imageOutput("image_output")
               )
             )
    ),

    # Tab 2: Gaining Weight
    tabPanel("Gaining Weight",
             fluidRow(
               sidebarPanel(
                 selectInput("gain_weight_restaurant", "Select Restaurant:",
                             choices = c("All Restaurants", unique(data$restaurant))),
               ),
               mainPanel(
                 plotOutput("gain_weight_plot"),
                 verbatimTextOutput("description2")
               )
             ),
             fluidRow(
               column(
                 width = 12,
                 align = "left",
                 style = "padding: 35px 20px;",
                 imageOutput("image_output2")
               ))


    ),

    # Tab 3: Losing Weight
    tabPanel("Losing Weight",
             fluidRow(
               sidebarPanel(
                 selectInput("lose_weight_restaurant", "Select Restaurant:",
                             choices = c("All Restaurants", unique(data$restaurant))),
               ),
               mainPanel(
                 plotOutput("lose_weight_plot"),
                 verbatimTextOutput("description3")
               )
             ),
             fluidRow(
               column(
                 width = 12,
                 align = "left",
                 style = "padding: 35px 20px;",
                 imageOutput("image_output3")
               ))


    ),

    # Tab 4: Vitamin Rich Diet
    tabPanel("Vitamin Rich Diet",
             fluidRow(
               sidebarPanel(
                 selectInput("vitamin_diet_restaurant", "Select Restaurant:",
                             choices = c("All Restaurants", unique(data$restaurant))),
               ),
               mainPanel(
                 plotOutput("vitamin_diet_plot"),
                 verbatimTextOutput("description4")
               )
             ),
             fluidRow(
               column(
                 width = 12,
                 align = "left",
                 style = "padding: 50px 20px;",
                 imageOutput("image_output4")
               ))


    ),



    # The About tab
    tabPanel("About",
             fluidRow(
               column(
                 width = 10,
                 mainPanel(
                   h2("About the Cal-O-food App"),
                   p("This app was created by Disha Rathod to provide recommendations for different diet needs and restaurant choices based on a dataset of fast food items."),
                   p("Source Data:", a("Link to the source data", href = "https://github.com/rfordatascience/tidytuesday/tree/master/data/2018/2018-09-04")),
                   p("Purpose: The Cal-O-food app is designed to assist people who want to make informed food choices when dining at fast food restaurants. By analyzing a comprehensive dataset of fast food items, the app provides personalized recommendations tailored to specific dietary needs, such as heart health, monitoring your sugar control, gaining weight, losing weight and having a vitamin rich diet. This user-friendly tool aims to simplify the decision-making process and promote healthier eating habits, ensuring that individuals can enjoy fast food options that satisfy their preferences and their unique eating goals. One of the other benefit is that users won't have to enter the restaurant chain and check the calorie consumption, with Cal-o-food you can click and check as per your needs."),
                   p("Motivation: Being a teenager with weight-gain issues, always forced me to just drink water or   eat a side salad while my friends used to enjoy their scrumptious meal every-time. However, this issue peaked when we used to hang-out at leading fast-food joints. So, this idea of Cal-O-food crossed my mind, where we took the nutrients data of leading fast food chains and made a few suggestions based on a few very common dietary requirements that people tend to be on. Although, these options are not specefically termed as Healthy, but when it's necessary or urgent or even when your heart craves for your favourite food chain, you can take some of the recommendations of food items from our app and use it to your advantage and indulge in them by feeling good about it.")
                 )

               )
             ),
             fluidRow(
               mainPanel(
                 h3("Session Information:"),
                 verbatimTextOutput("session_info_output")
               )
             )

    )
  )
)

server <- function(input, output) {
  # Tab 1: Health goal & Restaurant Recommendations
  filtered_data_tab1 <- reactive({
    if (input$restaurant == "All Restaurants") {
      data %>%
        filter(
          (input$goal_type == "Heart Health" & sat_fat_percentage <= 6 & cholesterol <= 50 & calories >= 500 & calories <= 800) |
            (input$goal_type == "Sugar Control" & sugar_percent <= 2 & total_carb <= 45 & calories >= 500 & calories <= 700) |
            (input$goal_type == "Preventing High Blood Pressure" & sodium <= 700 & calories >=400 & calories <= 800)
        ) %>%
        arrange(calories) %>%
        head(10)
    } else {
      data %>% filter(restaurant == input$restaurant) %>%
        filter(
          (input$goal_type == "Heart Health" & sat_fat_percentage <= 6 & cholesterol <= 50 & calories >= 450) |
            (input$goal_type == "Sugar Control" & sugar_percent <= 2 & total_carb <= 45 & calories >= 500 & calories <= 800) |
            (input$goal_type == "Preventing High Blood Pressure" & sodium <= 900 & calories >= 350 & calories <= 800 )
        ) %>%
        arrange(calories) %>%
        head(10)
    }
  })

  output$recommendation_plot <- renderPlot({
    top_10_items <- filtered_data_tab1() %>%
      arrange(desc(calories)) %>%
      head(10)

    if (input$restaurant == "All Restaurants") {
      plot <- ggplot(top_10_items, aes(x = calories, y = reorder(item, calories), fill = restaurant)) +
        geom_bar(stat = "identity") +
        labs(x = "Cal-O-food recommends", y = "Food Item") +
        scale_fill_discrete(name = "Restaurants") +
        theme_minimal() +
        theme(
          plot.title = element_text(size = 16, face = "bold"),
          axis.title = element_text(size = 14),
          axis.text = element_text(size = 12),
          legend.title = element_text(size = 14),
          legend.text = element_text(size = 12),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          panel.border = element_blank()
        ) +
        ggtitle("Recommended Items for your health goal from all the restaurants combined.")
    } else {
      plot <- ggplot(top_10_items,
                     aes(x = calories, y = reorder(item, calories))) +
        geom_bar(stat = "identity", fill = "skyblue") +
        labs(x = "Cal-O-food recommends", y = "Food Item") +
        theme_minimal() +
        theme(
          plot.title = element_text(size = 16, face = "bold"),
          axis.title = element_text(size = 14),
          axis.text = element_text(size = 12),
          legend.title = element_text(size = 14),
          legend.text = element_text(size = 12),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          panel.border = element_blank()
        ) +
        ggtitle("Recommended Items for your health goal")
    }

    plot
  })

  output$description <- renderText({
    if (input$restaurant == "All Restaurants") {
      "Recommended Items for your health goal from a cumulative list of all fast food restaurants.
      This recommendations are based between the calorie range of 400 to 800 per meal.
      Heart Health : Low saturated fat percentage and cholestrol.
      Controlling Sugar: Low sugar percentage and total number of carbs.
      High Blood pressure: Low sodium levels.

      Note: These foods are still not categorized as healthy.
      They are just relatively better than others."
    } else {
      "Recommended Items for your health goal This recommendations are based between the calorie range of 400 to 800 per meal.
      Heart Health : Low saturated fat percentage and cholestrol.
      Controlling Sugar: Low sugar percentage and total number of carbs.
      High Blood pressure: Low sodium levels.

      Note: These foods are still not categorized as healthy.
      They are just relatively better than others."
    }
  })


  # Tab 2: Gaining Weight Recommendations
  filtered_data_gain <- reactive({
    if (input$gain_weight_restaurant == "All Restaurants") {
      data %>%   filter(calories >= 850, calories <= 1400)
    } else {
      data %>% filter(restaurant == input$gain_weight_restaurant, calories >= 800, calories <= 1400)
    }
  })

  output$gain_weight_plot <- renderPlot({
    top_10_items <- filtered_data_gain() %>%
      arrange(desc(protein_percent + carb_percent)) %>%
      head(10)

    if (input$gain_weight_restaurant == "All Restaurants") {
      plot <- ggplot(top_10_items, aes(x = protein_percent + carb_percent, y = reorder(item, protein_percent + carb_percent), fill = restaurant)) +
        geom_bar(stat = "identity") +
        labs(x = "Cal-O-food recommends", y = "Food Item") +
        scale_fill_discrete(name = "Restaurants") +
        theme_minimal() +
        theme(
          plot.title = element_text(size = 16, face = "bold"),
          axis.title = element_text(size = 14),
          axis.text = element_text(size = 12),
          legend.title = element_text(size = 14),
          legend.text = element_text(size = 12),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          panel.border = element_blank()
        ) +
        ggtitle("Recommended for Gaining Weight")
    } else {
      plot <- ggplot(top_10_items, aes(x = protein_percent + carb_percent, y = reorder(item, protein_percent + carb_percent))) +
        geom_bar(stat = "identity", fill = "skyblue") +
        labs(x = "Cal-O-food recommends", y = "Food Item") +
        theme_minimal() +
        theme(
          plot.title = element_text(size = 16, face = "bold"),
          axis.title = element_text(size = 14),
          axis.text = element_text(size = 12),
          legend.title = element_text(size = 14),
          legend.text = element_text(size = 12),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          panel.border = element_blank()
        ) +
        ggtitle("Recommended for Gaining Weight")
    }

    plot
  })
  output$description2 <- renderText({
    if (input$restaurant == "All Restaurants") {
      "Recommended Items for your health goal from a cumulative list of all fast food restaurants.
      This recommendations are based between the calorie range of 800 to 1400 per meal.
      Food items have : High protein and carbs.

      Note: These foods are still not categorized as healthy.
      They are just relatively better than others."
    } else {
      "Recommended Items for your health goal.
       This recommendations are based between the calorie range of 800 to 1400 per meal.
      Food items have : High protein and carbs.

      Note: These foods are still not categorized as healthy.
      They are just relatively better than others."
    }
  })

  # Tab 3: Losing Weight Recommendations
  filtered_data_lose <- reactive({
    if (input$lose_weight_restaurant == "All Restaurants") {
      data %>% filter(calories >= 250, calories <= 500)
    } else {
      data %>% filter(restaurant == input$lose_weight_restaurant, calories >= 250, calories <= 500)
    }
  })

  output$lose_weight_plot <- renderPlot({
    top_10_items <- filtered_data_lose() %>%
      arrange(desc(fiber_percent + protein_percent)) %>%
      head(10)

    if (input$lose_weight_restaurant == "All Restaurants") {
      lweight_plot <- ggplot(top_10_items, aes(x = fiber_percent + protein_percent, y = reorder(item, fiber_percent + protein_percent), fill = restaurant)) +
        geom_bar(stat = "identity") +
        labs(x = "Cal-O-food recommends", y = "Food Item") +
        scale_fill_discrete(name = "Restaurants") +
        theme_minimal() +
        theme(
          plot.title = element_text(size = 16, face = "bold"),
          axis.title = element_text(size = 14),
          axis.text = element_text(size = 12),
          legend.title = element_text(size = 14),
          legend.text = element_text(size = 12),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          panel.border = element_blank()
        ) +
        ggtitle("Recommended for Losing Weight")
    } else {
      lweight_plot <- ggplot(top_10_items, aes(x = fiber_percent + protein_percent, y = reorder(item, fiber_percent + protein_percent))) +
        geom_bar(stat = "identity", fill = "skyblue") +
        labs(x = "Cal-O-food recommends", y = "Food Item") +
        theme_minimal() +
        theme(
          plot.title = element_text(size = 16, face = "bold"),
          axis.title = element_text(size = 14),
          axis.text = element_text(size = 12),
          legend.title = element_text(size = 14),
          legend.text = element_text(size = 12),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          panel.border = element_blank()
        ) +
        ggtitle("Recommended for Losing Weight")
    }

    lweight_plot
  })

  output$description3 <- renderText({
    if (input$restaurant == "All Restaurants") {
      "Recommended Items for your health goal from a cumulative list of all fast food restaurants.
      This recommendations are based between the calorie range of 250 to 500 per meal.
      Food items have : High protein and fiber.

      Note: These foods are still not categorized as healthy.
      They are just relatively better than others."
    } else {
      "Recommended Items for your health goal.
       This recommendations are based between the calorie range of 250 to 500 per meal.
      Food items have : High protein and fiber.

      Note: These foods are still not categorized as healthy.
      They are just relatively better than others."
    }
  })

  # Tab 4: Vitamin Rich Diet Recommendations
  filtered_data_vitamin <- reactive({
    if (input$vitamin_diet_restaurant == "All Restaurants") {
      data
    } else {
      data %>% filter(restaurant == input$vitamin_diet_restaurant)
    }
  })

  output$vitamin_diet_plot <- renderPlot({
    top_10_items <- filtered_data_vitamin() %>%
      arrange(desc(vit_a_percent + vit_c_percent + calcium_percent)) %>%
      head(10)

    if (input$vitamin_diet_restaurant == "All Restaurants") {
      plot <- ggplot(top_10_items, aes(x = vit_a_percent + vit_c_percent + calcium_percent, y = reorder(item, vit_a_percent + vit_c_percent + calcium_percent), fill = restaurant)) +
        geom_bar(stat = "identity") +
        labs(x = "Cal-O-food recommends", y = "Food Item") +
        scale_fill_discrete(name = "Restaurants") +
        theme_minimal() +
        theme(
          plot.title = element_text(size = 16, face = "bold"),
          axis.title = element_text(size = 14),
          axis.text = element_text(size = 12),
          legend.title = element_text(size = 14),
          legend.text = element_text(size = 12),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          panel.border = element_blank()
        ) +
        ggtitle("Recommended for a Vitamin Rich Diet")
    } else {
      plot <- ggplot(top_10_items, aes(x = vit_a_percent + vit_c_percent + calcium_percent, y = reorder(item, vit_a_percent + vit_c_percent + calcium_percent))) +
        geom_bar(stat = "identity", fill = "skyblue") +
        labs(x = "Cal-O-food recommends", y = "Food tem") +
        theme_minimal() +
        theme(
          plot.title = element_text(size = 16, face = "bold"),
          axis.title = element_text(size = 14),
          axis.text = element_text(size = 12),
          legend.title = element_text(size = 14),
          legend.text = element_text(size = 12),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          panel.border = element_blank()
        ) +
        ggtitle("Recommended for a Vitamin Rich Diet")
    }

    plot
  })

  output$description4 <- renderText({
    if (input$restaurant == "All Restaurants") {
      "Recommended Items for your health goal from a cumulative list of all fast food restaurants.
      Food items have : High vitamin A, C and calcium.

      Note: These foods are still not categorized as healthy.
      They are just relatively better than others."
    } else {
      "Recommended Items for your health goal.
      Food items have : High vitamin A, C and calcium.


      Note: These foods are still not categorized as healthy.
      They are just relatively better than others."
    }
  })



  output$image_output <- renderImage({
    list(src = "logo.jpg", height = "200px")
  }, deleteFile = FALSE)

  output$image_output2 <- renderImage({
    list(src = "logo.jpg", height = "200px")
  }, deleteFile = FALSE)

  output$image_output3 <- renderImage({
    list(src = "logo.jpg", height = "200px")
  }, deleteFile = FALSE)

  output$image_output4 <- renderImage({
    list(src = "logo.jpg", height = "200px")
  }, deleteFile = FALSE)

  session_info <- capture.output(sessionInfo())
  output$session_info_output <- renderPrint({
    cat(paste(session_info, collapse = "\n"))
  })
}

shinyApp(ui = ui, server = server)
