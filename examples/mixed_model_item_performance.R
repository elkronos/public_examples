# Load necessary packages
library(tidyverse)
library(lme4)
library(broom)
library(psych)
library(broom.mixed)

# Load BFI data
data(bfi, package="psych")

# Add a participant_id column to the data
bfi$participant_id <- 1:nrow(bfi)

# Convert from wide to long format
bfi_long <- bfi %>%
  pivot_longer(cols = -participant_id, 
               names_to = "item", 
               values_to = "response")

# Extract construct from item and remove ":" from item
bfi_long <- bfi_long %>%
  mutate(construct = str_extract(item, "^[A-Z]"),
         item = str_replace(item, ":", ""))

# Run mixed effects model
model <- lmer(response ~ item + (1|participant_id) + (1|construct), data = bfi_long, REML = FALSE)

# Extract fixed effects
fixed_effects <- tidy(model)

# Separate term into construct and item
fixed_effects <- fixed_effects %>%
  mutate(construct = ifelse(str_detect(term, "^item"), str_sub(term, 5, 5), NA),
         item = ifelse(str_detect(term, "^item"), str_sub(term, 5), term))

# Filter out rows that are not actual items
fixed_effects <- fixed_effects %>%
  filter(!is.na(construct))


# Create the scatter plot
ggplot(fixed_effects, aes(x = std.error, y = estimate, color = construct)) +
  geom_point(size = 3) +
  geom_text(aes(label = item), vjust = -1) +  # Add item labels above points
  scale_color_brewer(palette="Set1") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, face = 'bold', size = 20),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        legend.position = "right",
        axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Item Performance",
       x = "Item",
       y = "Estimate",
       color = "Construct")
