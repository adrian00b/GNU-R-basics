library(dplyr)
library(openxlsx)
library(tibble)

start_date = "20170101"
end_date =  "20191231"

companies_list = list(
  abbreviation = c("alr", "bhw")
  , link = c()
  , data = list()
)

union_data = tibble(Nazwa = character(), Data = character(), Zamkniecie = numeric())
for (i in seq_along(companies_list$abbreviation)){
  companies_list$link[i] = paste("https://stooq.pl/q/d/l/?s=", companies_list$abbreviation[i]
                                 , "&d1=", start_date, "&d2=", end_date, "&i=d", sep = "")
  companies_list$data[[i]] = read.csv(companies_list$link[i])
  companies_list$data[[i]]$Nazwa = companies_list$abbreviation[i]
  companies_list$data[[i]] = select(companies_list$data[[i]], c("Nazwa", "Data", "Zamkniecie"))
  union_data = union(union_data, companies_list$data[[i]])
}

write.xlsx(companies_list$data, "stooq_data.xlsx")
write.xlsx(union_data, "stooq_union_data.xlsx")
S