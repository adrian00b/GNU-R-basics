library(bigrquery)

sql_v1 <- paste(
  "
  SELECT *
  FROM
  (
    SELECT 
    user_id
    , ROUND(SUM(IF(s_refund_timestamp IS NULL AND se_day_no <= 30, s_amount_pln, 0)), 0) AS revenue_d30
    FROM `tinydragons-983ab.analytics.sales_with_dimensions` 
    WHERE fr_first_open_date BETWEEN" , paste("", first_open_date_start, "", sep = "'")
    , "AND", paste("", first_open_date_end, "", sep = "'")
    , "AND fr_install_version IN (" , paste("", v1, "", sep = "'"), ") 
    GROUP BY user_id
  )
  WHERE revenue_d30 > 0 AND revenue_d30 IS NOT NULL
  "
  , sep = ""
)

sql_v2 <- paste(
  "
  SELECT *
  FROM
  (
    SELECT 
    user_id
    , ROUND(SUM(IF(s_refund_timestamp IS NULL AND se_day_no <= 30, s_amount_pln, 0)), 0) AS revenue_d30
    FROM `tinydragons-983ab.analytics.sales_with_dimensions` 
    WHERE fr_first_open_date BETWEEN" , paste("", first_open_date_start, "", sep = "'")
    , "AND", paste("", first_open_date_end, "", sep = "'")
    , "AND fr_install_version IN (" , paste("", v2, "", sep = "'"), ") 
    GROUP BY user_id
  )
  WHERE revenue_d30 > 0 AND revenue_d30 IS NOT NULL
  "
  , sep = ""
)

sql_v1_v2 <- paste(
  "
  SELECT *
  FROM
  (
    SELECT 
    user_id
    , fr_install_version
    , ROUND(SUM(IF(s_refund_timestamp IS NULL AND se_day_no <= 30, s_amount_pln, 0)), 0) AS revenue_d30
    FROM `tinydragons-983ab.analytics.sales_with_dimensions` 
    WHERE fr_first_open_date BETWEEN" , paste("", first_open_date_start, "", sep = "'")
    , "AND", paste("", first_open_date_end, "", sep = "'")
    , "AND fr_install_version IN (" , paste("", v1, "", sep = "'"), ",", paste("", v2, "", sep = "'"), ") 
    GROUP BY user_id, fr_install_version
  )
  WHERE revenue_d30 > 0 AND revenue_d30 IS NOT NULL
  "
  , sep = ""
)

sql_number_of_users_v1 <- paste(
  "
  SELECT COUNT(DISTINCT user_id)
  FROM `tinydragons-983ab.analytics.sales_with_dimensions` 
  WHERE fr_first_open_date BETWEEN" , paste("", first_open_date_start, "", sep = "'")
  , "AND", paste("", first_open_date_end, "", sep = "'")
  , "AND fr_install_version IN (" , paste("", v1, "", sep = "'"), ") 
  
  "
  , sep = ""
)

sql_number_of_users_v2 <- paste(
  "
  SELECT COUNT(DISTINCT user_id)
  FROM `tinydragons-983ab.analytics.sales_with_dimensions` 
  WHERE fr_first_open_date BETWEEN" , paste("", first_open_date_start, "", sep = "'")
  , "AND", paste("", first_open_date_end, "", sep = "'")
  , "AND fr_install_version IN (" , paste("", v2, "", sep = "'"), ") 
  
  "
  , sep = ""
)


ds <- 'tinydragons-983ab'
query_v1 <- bq_project_query(ds, sql_v1)
query_v2 <- bq_project_query(ds, sql_v2)
query_v1_v2 <- bq_project_query(ds, sql_v1_v2)
query_number_of_users_v1 <- bq_project_query(ds, sql_number_of_users_v1)
query_number_of_users_v2 <- bq_project_query(ds, sql_number_of_users_v2)

table_v1 <- bq_table_download(query_v1)
table_v2 <- bq_table_download(query_v2)
table_v1_v2 <- bq_table_download(query_v1_v2)
table_number_of_users_v1 <- bq_table_download(query_number_of_users_v1)
table_number_of_users_v2 <- bq_table_download(query_number_of_users_v2)
