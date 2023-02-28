# What is the month or year with the most/least checkouts for the book "The Years" by Annie Ernaux?
# Read the data into a data frame
df <- read.csv("C:/Users/gagan/OneDrive/Desktop/info201/2022-2023-All-Checkouts-SPL-Data.csv", stringsAsFactors = FALSE)

# Subset the data for the book you're interested in
book_data <- subset(df, df$Title == "The Years" & df$ISBN == "9781609807887" & df$Creator == "Annie Ernaux")

# Group by month to get the total checkouts per month
checkout_by_month <- aggregate(Checkouts ~ CheckoutMonth, book_data, sum)

# Month with the most checkouts
most_checkouts <- checkout_by_month$CheckoutMonth[which.max(checkout_by_month$Checkouts)]

# Month with the least checkouts
least_checkouts <- checkout_by_month$CheckoutMonth[which.min(checkout_by_month$Checkouts)]



# What is the month or year with the most/least checkouts for ebooks?
# Subset the data for ebooks
ebook_data <- subset(df, df$MaterialType == "EBOOK")

# Group by month to get the total checkouts per month
checkout_by_month <- aggregate(Checkouts ~ CheckoutMonth, ebook_data, sum)

# Month with the most checkouts
most_ebook_checkouts <- checkout_by_month$CheckoutMonth[which.max(checkout_by_month$Checkouts)]

# Month with the least checkouts
least_ebook_checkouts <- checkout_by_month$CheckoutMonth[which.min(checkout_by_month$Checkouts)]



#How has the number of print book checkouts changed over time? (one of the material types analyzed in this analysis)
library(ggplot2)

# Read Data
df <- read.csv("C:/Users/gagan/OneDrive/Desktop/info201/2022-2023-All-Checkouts-SPL-Data.csv", stringsAsFactors = FALSE)

# Filter data for print books
df_print <- subset(df, MaterialType == "BOOK" & CheckoutYear == 2022)

# Group the data by CheckoutMonth and calculate the sum of Checkouts
checkout_by_month <- aggregate(Checkouts ~ CheckoutMonth, df_print, sum)

# Create a line chart showing the monthly checkout trends for print books 
library(ggplot2)

# Read Data
df <- read.csv("C:/Users/gagan/OneDrive/Desktop/info201/2022-2023-All-Checkouts-SPL-Data.csv")

# Filter data for print books in 2022
df_print <- subset(df, MaterialType == "BOOK" & CheckoutYear == 2022)

# Group the data by CheckoutMonth and calculate the sum of Checkouts
checkout_by_month <- aggregate(Checkouts ~ CheckoutMonth, df_print, sum)

# Create a line chart showing the monthly checkout trends for print books
ggplot(checkout_by_month, aes(x=CheckoutMonth, y=Checkouts)) + 
  geom_line(size=1, color="darkblue") + 
  labs(title="Monthly Checkout Trends of Print Books in 2022",
       x="Month", y="Total Checkouts") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_continuous(breaks = 1:12) +
  scale_y_continuous(labels = scales::number_format(scale = 1e-3, suffix = "k")) +
  geom_text(data = subset(checkout_by_month, CheckoutMonth == 12),
            aes(label = paste(round(Checkouts/1000, 1), "k")),
            size = 3, vjust = -1)