###################################################################################
## Bibliometric intertidal topography artificial intelligence models              #
## Mario Luiz Mascagni 09/06/2024                                                 #
###################################################################################
# Load necessary libraries
library(ggplot2)

# Read the data from CSV
dataset <- read.csv("path_to_your_file.csv", stringsAsFactors = FALSE)

# Rename columns if necessary (ensure these match the actual column names)
colnames(dataset) <- c("id", "Model_Abbreviation", "Recurrence_of_use_in_literature")

# Convert 'id' to a factor to preserve order in the bar plot
dataset$id <- factor(dataset$id, levels = dataset$id)

# Define the color scheme: black for item 10, dark gray for subitems (10.1 to 10.7), and light gray for others
dataset$color <- ifelse(dataset$id == 10, "black", 
                        ifelse(grepl("^10\\.", dataset$id), "darkgray", "lightgray"))

# Plot the bar chart
ggplot(dataset, aes(x = Model_Abbreviation, y = Recurrence_of_use_in_literature, fill = color)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  scale_fill_identity() +  # Use the specified colors
  geom_text(aes(label = Recurrence_of_use_in_literature), vjust = -0.5) +  # Add labels on bars
  scale_y_continuous(breaks = seq(0, max(dataset$Recurrence_of_use_in_literature), by = 1)) +  # Y-axis increments of 1
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +  # Rotate x-axis labels
  labs(x = "Artificial Intelligence Models", y = "Recurrence in Literature", 
       title = "Artificial Intelligence Models Used in Literature")

png(file="ias_itertidal_models_r02.png", width=300, height=300, units="mm", res=300)
