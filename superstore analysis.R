library(tidyverse)
library(datasets)
stdata <-  read_csv("superstore_final_dataset (1).csv",
                    col_types = cols(Sales = col_double(), .default = col_character())) %>% rename_all(tolower)
                   
stdata$order_date <- as.Date(stdata$order_date, format = "%d/%m/%y")
stdata$ship_date <- as.Date(stdata$ship_date, format = "%d/%m/%y")
head(`stdata`)
tail(stdata)
glimpse(stdata)

# Incomplete Observations from the datasets 
stdata %>% filter(!complete.cases(.)) %>% view()

# Number of incomplete Observations from the datasets
stdata %>% filter(!complete.cases(.)) %>% count() 


# Number of complete Observations from the datasets
stdata %>% filter(complete.cases(.)) %>% count()

# Complete Observations from the datasets
stdata %>% filter(complete.cases(.))  %>%  view()

#Datasets without post_code NA data
sdata <- select(stdata, -postal_code) 


#mean sales
mean_sales <- mean(sdata$sales)
mean_sales

min(sdata$sales)

max(sdata$sales)
range(sdata$sales)

#Minimum sales record
min_sale <- sdata %>% filter(sales == min(sales))
print(min_sale)


#Maximum sales record
max_sale <- sdata %>% filter(sales == max(sales))
max_sale


sdata %>% distinct(ship_mode)
sdata %>% distinct(category)
sdata %>% distinct(sub_category)


#Total number of Customers
total_customers <- sdata %>% distinct(customer_name) %>% count()
total_customers


#Top 5 performing customers by number of purchases made
top_5_customers <- sdata %>% select(customer_name) %>% count(customer_name) %>% arrange(desc(n)) %>% slice(1:5)
top_5_customers

# Top selling categories
top_selling_category <- sdata %>% select(category, sales) %>%  group_by(category) %>% summarise(sum_of_sales = sum(sales))%>% arrange(desc(sum_of_sales))
top_selling_category

#Total Sales by Category
ggplot(sdata, aes(x = category, y = sales, fill = category)) +
  geom_bar(stat = "identity") +
  labs(title = "Total Sales by Ship Mode",
       x = "category",
       y = "Sum of Sales") +
  theme_minimal()

# Top sales by ship mode
top_selling_ship_mode <- sdata %>% select(ship_mode, sales) %>%  group_by(ship_mode) %>% summarise(sum_of_sales = sum(sales))%>% arrange(desc(sum_of_sales))
top_selling_ship_mode

# Total sales by Ship mode
ggplot(sdata, aes(x = ship_mode, y = sales, fill = ship_mode)) +
  geom_bar(stat = "identity") +
  labs(title = "Total Sales by Ship Mode",
       x = "Ship Mode",
       y = "Sum of Sales") +
  theme_minimal()



# Top 10 sales by sub-categories
top_10_selling_sub_category <- sdata %>% select(sub_category, sales) %>%  group_by(sub_category) %>% summarise(sum_of_sales = sum(sales)) %>% arrange(desc(sum_of_sales)) %>% slice(1:10)
top_10_selling_sub_category


sdata %>% select(sub_category, sales) %>% 
  group_by(sub_category) %>%
  summarise(sum_of_sales = sum(sales)) %>% 
  arrange(desc(sum_of_sales)) %>%
  slice(1:10) %>%
  ggplot(aes(x = sub_category, y = sum_of_sales, fill = sub_category)) +
  geom_bar(stat = "identity", width = 0.5) +
  labs(title = "Total Sales by sub category",
       x = "sub category",
       y = "Sum of Sales") +
  theme_minimal()




# Top 10 sales by state
top_10_selling_state <- sdata %>% select(state, sales) %>%  group_by(state) %>% summarise(sum_of_sales = sum(sales)) %>% arrange(desc(sum_of_sales)) %>% slice(1:10)
top_10_selling_state

# Total sales By States
ggplot(sdata, aes(y = state, x = sales, fill = state)) +
  geom_bar(stat = "identity") +
  labs(title = "Total Sales by Ship Mode",
       x = "Sum of Sales",
       y = "State") +
  theme_void() +
  guides(fill = FALSE) + 
  theme_minimal()



# Top sales by region
top_selling_region <- sdata %>% select(region, sales) %>%  group_by(region) %>% summarise(sum_of_sales = sum(sales)) %>% arrange(desc(sum_of_sales))
top_selling_region

# Total sales by Region
ggplot(sdata, aes(x = region, y = sales, fill = region)) +
  geom_bar(stat = "identity") +
  labs(title = "Total Sales by Region",
       x = "Region",
       y = "Sum of Sales") +
  theme_minimal()



# Top 20 sales by City
top_selling_cities <- sdata %>% select(city, sales) %>%  group_by(city) %>% summarise(sum_of_sales = sum(sales)) %>% arrange(desc(sum_of_sales)) %>%  slice(1:20)
top_selling_cities


top_20_selling_cities <- sdata %>% select(city, sales) %>%  group_by(city) %>% summarise(sum_of_sales = sum(sales)) %>% arrange(desc(sum_of_sales)) %>%  slice(1:20) %>% ggplot(aes(y = city, x = sum_of_sales, fill = city)) +
  geom_bar(stat = "identity") +
  labs(title = "Total Sales by Top 20 Cities",
       y = "City",
       x = "Sum of Sales") +
  theme_minimal()

top_20_selling_cities

