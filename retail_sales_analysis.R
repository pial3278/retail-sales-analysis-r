# Retail Sales Analysis
# Author: Pial Rahman
# Description: Exploratory data analysis and business insights using retail sales data

############# PART 1

# Load tidyverse (includes dplyr, ggplot2, readr, and more)
library(tidyverse)
library(here)
library(lubridate)


# Load Data File

sales_data <- read.csv("retail_sales_dataset.csv")
sales_data

# See the structure of the dataset
glimpse(sales_data)

# Summary statistics for all variables
summary(sales_data)

################ PART 2

# Calculate total revenue
total_rev <- sum(sales_data$Total.Amount)
total_rev

# Total number of transactions
total_transactions <- n_distinct(sales_data$Transaction.ID)
total_transactions

# Average transaction Amount

average_transaction_amount <- mean(sales_data$Total.Amount)
average_transaction_amount

# Number of Unique customers
Unique_customers <- sum(n_distinct(sales_data$Customer.ID))
Unique_customers

# Revenue per customer
revenue_per_customer <- (total_rev/Unique_customers)
revenue_per_customer


# Overall Performance
# RetailCo generated total revenue of $456,000 in 2023 from 1,000 transactions, indicating steady overall sales performance supported by a solid customer base. The average transaction amount of $456 is high for retail, reflecting strong per-purchase value, while revenue per customer at the same level suggests limited repeat purchasing. Overall, the results show healthy revenue generation per transaction, with future growth opportunities tied to increasing customer frequency and lifetime value.

################### PART 3

# Category Performance 
category_transactions <- table(sales_data$Product.Category)
category_transactions

top_category_count <- max(category_transactions)
top_category_count

top_category_percentage <- (top_category_count/total_transactions)*100
top_category_percentage

# Revenue for Beauty Category
Revenue_beauty <- sum(sales_data$Total.Amount[sales_data$Product.Category == "Beauty"])
Revenue_beauty

# Revenue for Clothing Category
Revenue_clothing <- sum(sales_data$Total.Amount[sales_data$Product.Category == "Clothing"])
Revenue_clothing

# Revenue for Electronics Category
Revenue_electronics <- sum(sales_data$Total.Amount[sales_data$Product.Category == "Electronics"])
Revenue_electronics

# Finding: Even though Clothing category has the highest number of transactions of 351, Electronics category has the highest revenue of $156,905. RetailCo should use Clothing to bring customers in and Electronics to drive profitability, aligning promotional intensity and budget allocation with each category’s distinct role.

################# PART 4

# Average age of customers
Mean_age <- mean(sales_data$Age)
Mean_age

# Median age of customers
Median_age <- median(sales_data$Age)
Median_age

# Age range of customers
Age_range <- range(sales_data$Age)
Age_range

# Grouping customers by age
GenZ <- sum(sales_data$Age < 30)
GenZ

Millenials <- sum(sales_data$Age > 29 & sales_data$Age <50)
Millenials

Boomers <- sum(sales_data$Age > 49)
Boomers

# Millenials is the largest group with 391 customers.

# Gender Breakdown
Gender_counts <- table(sales_data$Gender)
Gender_counts

Total_customer_base <- sum(Gender_counts)
Gender_percentage <- (Gender_counts/Total_customer_base)*100
Gender_percentage

# Our typical customer is a gender-neutral customer, and is most likely a Millennial, as this group represents the largest segment of our customer base. This suggests prioritizing digital-first marketing channels such as social media and email, supported by clear, value-oriented and convenience-focused messaging that resonates with Millennial preferences while remaining inclusive across age groups.

############### PART 5

# Price Per Unit Analysis
price_per_unit <- summary(sales_data$Price.per.Unit)
price_per_unit

# Unique price point
Unique_price_point <- table(sales_data$Price.per.Unit)
Unique_price_point

# RetailCo is using a clearly tiered pricing strategy, with demand spread relatively evenly across both mid-range prices (25–50) and premium prices (300–500). This indicates that customers are not concentrated only at low price points; higher-priced items are selling at comparable volumes. So it can be concluded that, RetailCo is a mid-range retailer with strong premium appeal, rather than a discount-focused business.

# Quantity Analysis
quantity_breakdown <- summary(sales_data$Quantity)
quantity_breakdown

Quantity_distribution <- table(sales_data$Quantity)
Quantity_distribution

# Average quantity per transaction is approximately 2.5 units, suggesting customers typically buy multiple items per transaction rather than a single product.

# MY additional Question

#### 1. Which month had the highest revenue?

Month_wise_transaction <- table(month(sales_data$Date, label = TRUE))
Month_wise_transaction

Month_wise_revenue <- sales_data |>
  group_by(month = month(Date, label = TRUE)) |>
    summarize(
    total_revenue = sum(Total.Amount, na.rm = TRUE))
Month_wise_revenue

# Highest revenue was earned during May $53,150.

### 2. Do older customers spend more per transaction? 

GenZ_revenue <- sum(sales_data$Total.Amount[sales_data$Age <30])
GenZ_revenue
GenZ_transaction <- sum(sales_data$Age < 30)
GenZ_average_transaction_amount <- GenZ_revenue/GenZ_transaction
GenZ_average_transaction_amount

Millenial_revenue <- sum(sales_data$Total.Amount[sales_data$Age > 29 & sales_data$Age < 50])
Millenial_revenue
Millenial_transaction <- sum(sales_data$Age > 29 & sales_data$Age < 50)
Millenial_average_transaction_amount <- Millenial_revenue/Millenial_transaction
Millenial_average_transaction_amount

Boomer_revenue <- sum(sales_data$Total.Amount[sales_data$Age > 49])
Boomer_revenue
Boomer_transaction <- sum(sales_data$Age > 49)
Boomer_average_transaction_amount <- Boomer_revenue/Boomer_transaction
Boomer_average_transaction_amount

Highest_average_Transaction_Amount_per_Age_Group <- max(GenZ_average_transaction_amount, Millenial_average_transaction_amount, Boomer_average_transaction_amount)
Highest_average_Transaction_Amount_per_Age_Group

# GenZ have the highest average revenue per transaction of $490.

################## PART 6

# Recommendation 1: Use Clothing as an acquisition driver and Electronics as the primary profit engine.

# Supporting Data:
# Clothing accounts for the highest transaction volume (351 transactions; 35.1%).
# Electronics generates the highest revenue ($156,905) despite fewer transactions than Clothing.
# Average transaction value is materially higher in Electronics due to premium price points ($300–$500).

# Expected Impact:
#Positioning Clothing promotions to drive traffic will increase customer acquisition, while targeted up-sell and cross-sell into Electronics will maximize revenue per visit. This strategy aligns category roles with their demonstrated strengths, improving overall profitability without sacrificing transaction volume.

# Recommendation 2: Prioritize Gen Z–focused premium and bundle offers to maximize transaction value

# Supporting Data:
# Gen Z customers (<30) have the highest average transaction value: $490.66, outperforming Millennials ($459.30) and Boomers ($426.06).
# Gen Z represents 251 transactions (25.1% of total), indicating room for growth.
# Customers typically purchase 2.5 items per transaction, supporting bundle strategies.

#Expected Impact:
#Targeting Gen Z with premium bundles, limited-edition products, and digitally native campaigns can scale a segment that already spends the most per transaction. Increasing Gen Z transaction frequency would disproportionately lift total revenue and long-term customer lifetime value.

# Recommendation 3: Increase marketing and inventory emphasis during high-revenue months, especially May and Q4

# Supporting Data:
# May generated the highest monthly revenue: $53,150, despite not having the highest transaction count.
# Q4 months (Oct–Dec) collectively contribute strong revenue:
# October: $46,580, December: $44,690

# Expected Impact:
# Concentrating promotional spend, inventory availability, and premium product launches during proven high-revenue periods will amplify returns on marketing investment. Seasonal optimization can drive incremental revenue without increasing fixed costs, improving operational efficiency and margin performance.

############ EMAIL 

# Subject: 2023 Sales Analysis & 2024 Recommendations

# Sarah,

# I've completed the 2023 sales analysis. Here are the key findings:

# PERFORMANCE OVERVIEW:
# RetailCo generated $456,000 in total revenue from 1,000 transactions, with an average transaction value of $456, indicating strong per-purchase performance but limited repeat purchasing across the customer base.

# KEY INSIGHTS:

# Category performance: Clothing drove the highest transaction volume (351 transactions; 35.1%), while Electronics generated the most revenue ($156,905), reflecting higher-priced items driving profitability.
# Customer behavior: Gen Z customers have the highest average transaction value ($491), outperforming Millennials ($459) and Boomers ($426).
# Seasonality: May was the strongest revenue month ($53,150), with additional revenue strength in Q4, particularly October and December.

# 2024 RECOMMENDATIONS:

# 1. Use Clothing as a traffic driver while prioritizing Electronics for margin and revenue growth through upselling and targeted promotions.
# 2. Expand Gen Z–focused premium bundles and digital campaigns** to increase transaction frequency within the highest-spending segment.
# 3. Concentrate marketing spend and inventory planning in peak months, especially May and Q4, to maximize return on promotional investment.

# Happy to discuss further before the board meeting.

# Best regards,
# Pial Rahman

##############REFLECTION

# Most interesting finding from the analysis
# The most interesting finding was that Gen Z customers had the highest average transaction value ($490.66), exceeding both Millennials and Boomers. This is counterintuitive given Gen Z’s typically lower disposable income and suggests that when they do purchase, they are willing to spend on premium or bundled products.

# Next analysis I’d do
# With more time or data, I would analyze repeat purchase behavior and customer lifetime value (CLV). Specifically, I would want to understand how often customers return, which categories drive repeat purchases, and whether high-spending segments (such as Gen Z) also demonstrate long-term loyalty.


################BAR CHART

library(ggplot2)

# Count by category
category_counts <- table(sales_data$Product.Category)
category_df <- as.data.frame(category_counts)
names(category_df) <- c("Category", "Count")

# Create bar chart
ggplot(category_df, aes(x = Category, y = Count, fill = Category)) +
  geom_bar(stat = "identity") +
  labs(title = "Transactions by Product Category",
       x = "Product Category",
       y = "Number of Transactions") +
  theme_minimal()
