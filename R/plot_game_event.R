# Script to animate NBA game events using SportVU data

# Load libraries
library(jsonlite)
library(ggplot2)
library(gganimate)
library(dplyr)
library(png)
library(grid)

# Function to animate game event ------------------------------------------

animate_nba_event <- function(game_data, event_index = 1) {
  # Load the court image
  court_img <- readPNG("court.png")
  
  # Extract the event correctly
  event_data <- game_data$events[[event_index]]  # Use [[ ]] for lists
  
  # Ensure "moments" exist
  if (!"moments" %in% names(event_data) || is.null(event_data$moments)) {
    stop("No moment data found for this event.")
  }
  
  # Extract all moments
  moments <- event_data$moments
  
  # Convert moments into a data frame
  all_positions <- do.call(rbind, lapply(seq_along(moments), function(i) {
    moment <- moments[[i]]
    
    # Extract player and ball positions (inside moment[[6]])
    positions <- do.call(rbind, lapply(moment[[6]], function(player) {
      data.frame(
        team_id = player[[1]],
        player_id = player[[2]],
        x = player[[3]],
        y = player[[4]],
        radius = player[[5]],
        frame = i,  # Frame for animation
        type = ifelse(player[[1]] == -1, "Ball", "Player")  # Identify ball vs player
      )
    }))
    
    return(positions)
  }))
  
  # Plot animated event with court overlay
  anim <- ggplot(all_positions, aes(x = x, y = y, color = type, group = player_id)) +
    # Overlay the court image
    annotation_raster(court_img, xmin = 0, xmax = 100, ymin = 0, ymax = 50) +
    
    # Plot player and ball movements
    geom_point(aes(size = ifelse(type == "Ball", 5, 3))) +
    xlim(0, 100) + ylim(0, 50) +
    labs(title = "NBA Game Event Animation",
         subtitle = "Frame: {frame}",
         x = "Court X Position", y = "Court Y Position") +
    scale_color_manual(values = c("Player" = "blue", "Ball" = "orange")) +
    theme_minimal() +
    transition_time(frame) +
    ease_aes("linear")
  
  # Render animation
  animate(anim, fps = 10, duration = 5, width = 800, height = 400)
}

# Example usage -----------------------------------------------------------

# 12.11.2015.GSW.at.BOS.7z
# Extract the data
game_data <- read_json("data/2016.NBA.Raw.SportVU.Game.Logs/0021500336.json")

# Run animation
animate_nba_event(game_data, event_index = 1)


