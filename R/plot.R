#' Bar Plot for A Vitamin Rich Diet
#'
#' @description
#' This function helps you to decide the food item that is best suited for you to have
#' a vitamin-rich diet. The input for this function can be one of the leading fast-food
#' joints as follows:
#' - Mcdonalds
#' - Chick Fil-A
#' - Sonic
#' - Arbys
#' - Burger King
#' - Dairy Queen
#' - Subway
#' - Taco Bell
#' - All Restaurants (this is when you can't decide which restaurant you want to
#'   eat at and want a combined response from all the restaurants listed above)
#'
#' @param resto The name of the restaurant/restaurants
#'
#' @return A bar plot of the containing the food items for the restaurant entered
#'
#' @examples
#' foodplot("Mcdonalds")
#' foodplot("All Restaurants")
#'
#' @import ggplot2
#'
#'@export
foodplot <- function(resto){
  if (resto == "All Restaurants") {
    plot <- fastfoodcalories %>%
      dplyr::arrange(desc(vit_a_percent + vit_c_percent + calcium_percent)) %>%
      head(10) %>%
      ggplot(aes(x = vit_a_percent + vit_c_percent + calcium_percent, y = reorder(item, vit_a_percent + vit_c_percent + calcium_percent), fill = restaurant)) +
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
    filtered <- fastfoodcalories %>%
      dplyr::filter(restaurant == resto) %>%
      dplyr::arrange(desc(vit_a_percent + vit_c_percent + calcium_percent)) %>%
      head(10)
    if (nrow(filtered) == 0){
      plot <- "No healthy food available for your choice of restaurant"
    }
    else{
      plot <- filtered %>% ggplot( aes(x = vit_a_percent + vit_c_percent + calcium_percent, y = reorder(item, vit_a_percent + vit_c_percent + calcium_percent))) +
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

  }

  return(plot)
}


