library(tidyverse)
library(rlist)

tmlist <- c("SEA", "DEN", "DAL", "TB", "NYG", "TEN", "MIN", "GB", "LAC", "LV",
            "KC", "ARI", "JAX", "WAS", "NYJ", "BAL", "NE", "MIA", "IND", "HOU",
            "DET", "PHI", "PIT", "CIN", "CHI", "SF", "CAR", "CLE", "ATL", "NO",
            "BUF", "LA")

dflist <- list()
for(tm in tmlist){
  setwd("C:/Users/Aaron Friedlander/Desktop/Big Data Bowl/Defensive Personnel Data")
  filename <- paste0(tm, "personnel.csv")
  mydf <- suppressMessages(read_csv(filename) |> select(-1))
  dflist <- list.append(dflist, mydf)
  print(tm)
}

dflist[[23]] <- dflist[[23]] |> select(-route_6)

basefile <- dflist[[1]]
for(n in 2:length(dflist)){
  basefile <- rbind(basefile, dflist[[n]])
  print(n)
}

write_csv(basefile, "AllDefensivePersonnel.csv")


dflist <- list()
for(tm in tmlist){
  setwd("C:/Users/Aaron Friedlander/Desktop/Big Data Bowl/Offensive Personnel Data")
  filename <- paste0(tm, "personnel.csv")
  mydf <- suppressMessages(read_csv(filename) |> select(-1))
  dflist <- list.append(dflist, mydf)
  print(tm)
}

dflist[[15]] <- dflist[[15]] |> select(-route_6)

basefile <- dflist[[1]]
for(n in 2:length(dflist)){
  basefile <- rbind(basefile, dflist[[n]])
  print(n)
}

write_csv(basefile, "AllOffensivePersonnel.csv")





































############# OLD CODE TO MAKE A STUPID GRAPH


# Define field dimensions
field_length <- 120 # 100 yards + 2 end zones (10 yards each)
field_width <- 53.3 # width of the field in yards

# Create data for yard lines
yard_lines <- data.frame(
  x = rep(seq(0, field_length, by = 10), each = 2),
  y = c(0, field_width)
)

# Create data for hash marks
hash_marks <- data.frame(
  x = rep(seq(10, 110, by = 1), each = 2), # between the 10 and 110-yard marks
  y = c(rep(17.8, 101), rep(35.5, 101)) # NFL hash mark positions in yards
)



# Plot the football field
ggplot() +
  # Add field boundary
  geom_rect(aes(xmin = 0, xmax = field_length, ymin = 0, ymax = field_width),
            fill = "green", color = "white") +
  # Add end zones
  geom_rect(aes(xmin = 0, xmax = 10, ymin = 0, ymax = field_width),
            fill = "blue", alpha = 0.5) +
  geom_rect(aes(xmin = 110, xmax = 120, ymin = 0, ymax = field_width),
            fill = "red", alpha = 0.5) +
  # Add yard lines
  geom_segment(data = yard_lines, aes(x = x, xend = x, y = 0, yend = field_width),
               color = "white", linewidth = 0.5) +
  geom_point(aes(x = 90.58, y = 30.43)) +
  # Adjust plot limits and aspect ratio
  coord_fixed() +
  theme_void() # Remove background and axis
