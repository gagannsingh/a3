# Load libraries
library(ggplot2)
library(dplyr)

# Read data
df <- read.csv("C:/Users/gagan/OneDrive/Desktop/info201/2022-2023-All-Checkouts-SPL-Data.csv", stringsAsFactors = FALSE)

# Get top 5 material types based on number of checkouts
top_material_types <- df %>%
  group_by(MaterialType) %>%
  summarize(checkouts = sum(Checkouts)) %>%
  arrange(desc(checkouts)) %>%
  head(5) %>%
  select(MaterialType)

# Filter data for top 5 material types
df_top5 <- df %>%
  filter(MaterialType %in% top_material_types$MaterialType)

# Group the data by MaterialType and CheckoutMonth and calculate the sum of Checkouts
checkout_by_material_month <- df_top5 %>%
  group_by(MaterialType, CheckoutMonth) %>%
  summarize(total_checkouts = sum(Checkouts))


# Create stacked bar chart showing the monthly checkout trends for the top 5 material types
ggplot(checkout_by_material_month, aes(x = CheckoutMonth, y = total_checkouts, fill = MaterialType)) +
  geom_bar(stat = "identity") +
  scale_fill_brewer(palette = "Set2") +
  labs(title = "Monthly Checkout Trends for Top 5 Material Types in 2022",
       x = "Month", y = "Total Checkouts",
       fill = "Material Type") +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = "bottom") +
  scale_x_continuous(breaks = seq(1, 12, 1)) +
  scale_y_continuous(labels = scales::number_format(scale = 1e-3, suffix = "k"))




