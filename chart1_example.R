#Load Library
library(ggplot2)

# Read Data
df <- read.csv("C:/Users/gagan/OneDrive/Desktop/info201/2022-2023-All-Checkouts-SPL-Data.csv", stringsAsFactors = FALSE)

# Filter data for book, ebook, and audiobook
df_subset <- subset(df, MaterialType %in% c("BOOK", "EBOOK", "AUDIOBOOK"))

# Group the data by MaterialType and CheckoutMonth and calculate the sum of Checkouts
checkout_by_material_month <- aggregate(Checkouts ~ MaterialType + CheckoutMonth, df_subset, sum)

# Create a line chart showing the monthly checkout trends for different MaterialTypes
ggplot(checkout_by_material_month, aes(x=CheckoutMonth, y=Checkouts, color=MaterialType)) + 
  geom_line(size=1) + 
  scale_color_brewer(palette="Dark2") +
  labs(title="Monthly Checkout Trends of Books, eBooks, and Audiobooks in 2022",
       x="Month", y="Total Checkouts",
       color="Material Type") +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position="bottom",
        legend.text = element_text(size = 10)) +
  scale_x_continuous(breaks = seq(1, 12, 1)) +
  scale_y_continuous(labels = scales::number_format(scale = 1e-3, suffix = "k")) +
  geom_text(data = subset(checkout_by_material_month, CheckoutMonth == 12),
            aes(x = CheckoutMonth, y = Checkouts, label = round(Checkouts/1000, 1)),
            size = 3, vjust = -1)

