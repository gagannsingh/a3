#Load Library
library(ggplot2)

# Read Data
df <- read.csv("C:/Users/gagan/OneDrive/Desktop/info201/2022-2023-All-Checkouts-SPL-Data.csv", stringsAsFactors = FALSE)

# Group the data by UsageClass and CheckoutMonth and calculate the sum of Checkouts
checkout_by_usage_month <- aggregate(Checkouts ~ UsageClass + CheckoutMonth, df, sum)

# Create a line chart showing the monthly checkout trends for different UsageClasses
ggplot(checkout_by_usage_month, aes(x=CheckoutMonth, y=Checkouts, color=UsageClass)) + 
  geom_line(size=1) + 
  scale_color_brewer(palette="Dark2") +
  labs(title="Monthly Checkout Trends by Usage Class in 2022",
       x="Month", y="Total Checkouts",
       color="Usage Class") +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position="bottom") +
  scale_x_continuous(breaks = seq(1, 12, 1)) +
  scale_y_continuous(labels = scales::number_format(scale = 1e-3, suffix = "k"))




