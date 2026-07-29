# Data Dictionary

## calendar.csv

| Variable | Description |
|-----------|-------------|
| date | Calendar date |
| wm_yr_wk | Walmart week identifier |
| weekday | Day of week |
| wday | Numerical day of week |
| month | Month |
| year | Year |
| event_name_1 | Primary holiday/event |
| event_type_1 | Holiday type |
| snap_CA | SNAP active in California |
| snap_TX | SNAP active in Texas |
| snap_WI | SNAP active in Wisconsin |

---

## sales_train_evaluation.csv

| Variable | Description |
|-----------|-------------|
| id | Product-store identifier |
| item_id | Product ID |
| dept_id | Department |
| cat_id | Product category |
| store_id | Store |
| state_id | State |
| d_1 ... d_1941 | Daily units sold |

---

## sell_prices.csv

| Variable | Description |
|-----------|-------------|
| store_id | Store |
| item_id | Product |
| wm_yr_wk | Walmart week |
| sell_price | Product selling price |