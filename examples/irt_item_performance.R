# Load necessary packages
library(tidyverse)
library(mirt)   # for IRT
library(psych)  # for the BFI data

# Load BFI data
data(bfi, package = "psych")

# Add a participant_id column to the data
bfi$participant_id <- 1:nrow(bfi)

bfi[, c(1:25, 29)] -> bfi

# Convert from wide to long format
bfi_long <- bfi %>%
  pivot_longer(cols = -participant_id, 
               names_to = "item", 
               values_to = "response")

# Extract construct from item and remove ":" from item
bfi_long <- bfi_long %>%
  mutate(construct = str_extract(item, "^[A-Z]"),
         item = str_replace(item, ":", ""))

# Convert response to numeric
bfi_long$response <- as.numeric(bfi_long$response)

# Filter out non-numeric responses (e.g., NA)
bfi_long <- bfi_long %>%
  filter(!is.na(response))

# Transform data from long to wide again, for the IRT model
bfi_wide <- bfi_long %>% 
  pivot_wider(names_from = item, values_from = response)

# Run IRT model
# Increase the number of iterations and monitor convergence
model <- mirt(data = select(bfi_wide, -participant_id, -construct), model = 1, iter.em = 1000, EMtol = 1e-6)

# Check if the model converged successfully
if (any(grepl("convergence|warning", summary(model)$output))) {
  stop("The IRT model did not converge. Please check the data and model specification.")
}

# Extract item parameters
item_params <- coef(model, simplify = TRUE)$items

# Extract the discrimination parameter (a) and item difficulty (b)
discrimination <- item_params[, 1]
item_difficulty <- item_params[, 2]

# Create a tibble for plotting
plot_data <- tibble(item = rownames(item_params),
                    discrimination = discrimination,
                    item_difficulty = item_difficulty,
                    construct = str_extract(item, "^[A-Z]"))

# Create the scatter plot
ggplot(plot_data, aes(x = item_difficulty, y = discrimination, color = construct)) +
  geom_point(size = 3) +
  geom_text(aes(label = item), vjust = -1) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 20),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        legend.position = "right",
        axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Item Discrimination by Item Difficulty",
       x = "Item Difficulty",
       y = "Discrimination",
       color = "Construct")
