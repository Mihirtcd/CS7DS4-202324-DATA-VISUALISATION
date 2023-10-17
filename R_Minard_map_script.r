# List of required packages
required_packages <- c("xlsx", "ggrepel", "gridExtra", "tidyverse")

# Install the required packages
install.packages(required_packages)

# Importing required libraries
library(xlsx)
library(ggrepel)
library(gridExtra)
library(tidyverse)

# Read the data from the dataset
dataFull <- read.xlsx("/Users/mihirshekhar/Downloads/minard-data.xlsx", sheetName = "Sheet1", na.rm = TRUE)

# Selecting city data and filtering out NA values
cities <- dataFull %>% select(1:3) %>% na.omit()

# Selecting army data and filtering out NA values
army <- dataFull %>% select(9:13) %>% na.omit()

# Selecting temperature data and filtering out NA values
temperature <- dataFull %>% select(4:8) %>% na.omit()

# Creating the base plot
plot <- ggplot()

# Defining breaks for the survivor palette
breaks <- c(1, 2, 3) * 10^5

# Creating the army path on the map
armyPath <- plot + 
  geom_path(data = army, aes(x = LONP, y = LATP, group = DIV,
    color = DIR, size = SURV), lineend = "square") + 
  labs(x = NULL, y = NULL) + 
  scale_y_continuous(expand = c(0, 0.2))

# Adding cities to the army path
armyPlusCities <- armyPath +
  geom_point(data = cities, aes(x = LONC, y = LATC))

# Adding city labels
armyPlusCitiesWithLabel <- armyPlusCities + 
  geom_text_repel(data = cities, aes(x = LONC, y = LATC, label = CITY), color = "#ffffff")

# Creating the survivor palette
variationSized = armyPlusCitiesWithLabel +   
  scale_size("Survivors", range = c(0.7, 5), breaks = breaks, labels = scales::comma(breaks))

# Customizing colors and themes
colouredPlot <- variationSized +
  scale_colour_manual(values = c("#D76500", "#252523")) + 
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = '#FFD284', colour = 'black'),
    legend.position = "top",
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank()
  )

# Formatting temperature data with month
formatedTemperature <- temperature %>% mutate(toDisplay = paste0(TEMP, "°C (", MON, ")"))

# Creating the temperature plot with better aesthetics
temperaturePlot <- ggplot(data = formatedTemperature, aes(x = LONT, y = TEMP)) +
  geom_line(color = "red", size = 1) + 
  geom_text(aes(label = toDisplay), vjust = 1, hjust = -0.1, size = 3, color = "black") + 
  labs(x = NULL, y = NULL) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Combining the colored plot and temperature plot
combinedPlot <- grid.arrange(colouredPlot, temperaturePlot, heights = c(10, 4))

# Saving the combined plot as an image
ggsave(file = "minardsMap.png", combinedPlot, width = 10, height = 5, limitsize = FALSE)
