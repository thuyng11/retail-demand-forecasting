library(tidyverse)
library(lubridate)
library(here)
library(arrow)
library(scales)
library(slider)

sales_complete <-
  read_parquet(
    here(
      "data",
      "processed",
      "sales_complete_ca1.parquet"
    )
  )

daily_sales <-
  read_csv(
    here(
      "data",
      "processed",
      "daily_category_sales_ca1.csv"
    )
  )

# Section 1 - Overall demand: How has total demand changed over time?

daily_total <-
  
  daily_sales %>%
  group_by(date) %>%
  summarise(
    units = sum(units_sold),
    revenue = sum(revenue),
    .groups = "drop"
  )

ggplot(
  daily_total,
  aes(date, units)
)+
  geom_line()+
  labs(
    title="Daily Demand",
    x=NULL,
    y="Units Sold"
  )+
  theme_minimal()

daily_total <-
  
  daily_total %>%
  arrange(date) %>%
  mutate(
    ma30=
      slider::slide_dbl(
        units,
        mean,
        .before=29,
        .complete=TRUE
      )
  )

ggplot(
  daily_total,
  aes(date,units)
)+
  
  geom_line(alpha=.3)+
  
  geom_line(
    aes(y=ma30),
    linewidth=1
  )+
  
  theme_minimal()


# Section 2: monthly seasonality

monthly_sales <-
  
  daily_sales %>%
  
  group_by(
    year,
    month
  )%>%
  
  summarise(
    units=sum(units_sold),
    .groups="drop"
  )

ggplot(
  monthly_sales,
  aes(month,units,
      group=year,
      color=factor(year))
)+
  
  geom_line()+
  geom_point()+
  
  scale_x_continuous(
    breaks=1:12
  )+
  
  theme_minimal()


# Section 3: Category Analysis - Which department sells the most?

category_summary <-
  
  daily_sales %>%
  
  group_by(cat_id)%>%
  
  summarise(
    
    total_sales=sum(units_sold),
    
    total_revenue=sum(revenue),
    
    avg_daily_sales=mean(units_sold)
    
  )

ggplot(
  category_summary,
  aes(
    reorder(cat_id,total_sales),
    total_sales
  )
)+
  
  geom_col()+
  coord_flip()+
  theme_minimal()


# Weekday Analysis

weekday_sales <-
  
  daily_sales %>%
  
  group_by(
    weekday,
    wday
  )%>%
  
  summarise(
    
    sales=sum(units_sold)
    
  )

ggplot(
  weekday_sales,
  aes(
    reorder(
      weekday,
      wday
    ),
    sales
  )
)+
  
  geom_col()


# Holiday Analysis

holiday_sales <-
  
  daily_sales %>%
  
  group_by(is_event)%>%
  
  summarise(
    
    sales=mean(units_sold)
    
  )

ggplot(
  holiday_sales,
  aes(
    factor(is_event),
    sales
  )
)+
  
  geom_col()

# SNAP analysis

snap_summary <-
  
  daily_sales %>%
  
  group_by(snap_ca)%>%
  
  summarise(
    
    sales=mean(units_sold)
    
  )

ggplot(
  snap_summary,
  aes(
    factor(snap_ca),
    sales
  )
)+
  
  geom_col()

# price analysis

price_sales <-
  sales_complete %>%
  filter(
    !is.na(sell_price)
  )

ggplot(
  price_sales,
  aes(
    sell_price,
    sales
  )
)+
  geom_point(
    alpha=.02
  )+
  geom_smooth()


# product analysis

top_products <-
  
  sales_complete %>%
  
  group_by(item_id)%>%
  
  summarise(
    
    total_sales=sum(sales)
    
  )%>%
  
  arrange(
    desc(total_sales)
  )


top_products %>%
  head(20)

top_products_plot <- top_products %>%
  slice_max(total_sales, n = 10) %>%
  ggplot(
    aes(
      x = reorder(item_id, total_sales),
      y = total_sales
    )
  ) +
  geom_col(fill = "#2C7FB8") +
  coord_flip() +
  scale_y_continuous(labels = scales::comma) +
  labs(
    title = "Top 10 Products by Total Unit Sales",
    subtitle = "Store CA_1",
    x = NULL,
    y = "Total units sold"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold"),
    panel.grid.major.y = element_blank()
  )

top_products_plot


# choose top 10 product in each category for forecasting

sales_complete %>%
  
  group_by(item_id,cat_id)%>%
  
  summarise(
    total_sales=sum(sales),
    .groups="drop"
  )%>%
  
  group_by(cat_id)%>%
  
  slice_max(
    total_sales,
    n=10
  )

# prepare dataset for shiny

product_summary <- sales_complete %>%
  group_by(item_id, dept_id, cat_id) %>%
  summarise(
    total_sales = sum(sales, na.rm = TRUE),
    average_daily_sales = mean(sales, na.rm = TRUE),
    median_daily_sales = median(sales, na.rm = TRUE),
    zero_sales_pct = mean(sales == 0) * 100,
    active_days = sum(sales > 0),
    average_price = mean(sell_price, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(total_sales))

write_csv(
  product_summary,
  here(
    "data",
    "processed",
    "product_summary_ca1.csv"
  )
)

selected_products <- product_summary %>%
  slice_max(
    total_sales,
    n = 100
  ) %>%
  select(item_id)

selected_product_sales <- sales_complete %>%
  semi_join(
    selected_products,
    by = "item_id"
  ) %>%
  select(
    date,
    item_id,
    dept_id,
    cat_id,
    sales,
    sell_price,
    is_event,
    is_weekend,
    snap_ca
  )

arrow::write_parquet(
  selected_product_sales,
  here(
    "data",
    "processed",
    "selected_product_sales_ca1.parquet"
  )
)