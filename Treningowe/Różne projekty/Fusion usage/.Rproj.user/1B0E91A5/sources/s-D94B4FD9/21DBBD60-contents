library(bigrquery)
library(tibble)
library(dplyr)

sess_list = c(1,2,3,4,5,10,20,30)

a = tibble(sess_list, )
ds <- 'bladebound-56279680'

for (i in seq_along(sess_list)){
  paste(
    "
  WITH users AS
  (
    SELECT user_id
    , MAX(sess_no) AS max_sess
    , SUM(actions.Fusion.amount) AS fusion_amount
    FROM `bladebound-56279680.dev.anal_591_events_by_day_and_sess_with_dimensions` 
    WHERE validation_flag IS NULL
    GROUP BY user_id
  )
  
  SELECT ROUND(COUNTIF(fusion_amount > 0)/COUNT(user_id) * 100, 2)
  FROM users
  WHERE max_sess >= ", sess_list[i] 
    
  ) %>% 
    bq_project_query(ds, .) %>%
    bq_table_download() -> temp
    
    a[i, 2] = temp[[1]]
}

capture.output(a ,file = "Output/fusion_usage.txt")

