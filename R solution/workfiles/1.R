# top10 etykiet Badge o najwiekszej ilosci wystąpień, z nazwą etykiety
# i najmniejszą klasą spośród etykiet jednego typu

# data
Badges <- read.csv("Badges.csv.gz")

# solution 0 - sql
sql1 <- function(Badges){
  res_sql <- sqldf::sqldf('SELECT Name,
  COUNT(*) AS Number,
  MIN(Class) AS BestClass
  FROM Badges
  GROUP BY Name
  ORDER BY Number DESC
  LIMIT 10')
}

# solution 1 - base functions
base1 <- function(Badges){
  res_base <-
      aggregate(Badges$Class, 
        Badges["Name"], 
        function(x) c(Number = length(x), BestClass = min(x)))
  res_base <- cbind.data.frame(Name = res_base[,1], res_base[,2])
  res_base <- res_base[order(-res_base$Number),]
  res_base <- head(res_base, 10)
}

# solution 2 - dplyr
dplyr1 <- function(Badges){
  library(dplyr)
  res_dplyr <- 
    Badges[, c("Id", "Name", "Class")] %>%
    group_by(Name) %>%
    summarise(
      Number = n(),
      BestClass = min(Class)) %>%
    arrange(desc(Number)) %>%
    slice_head(n=10)
}

# solution 3 - data.table 
data_table <- function(Bagdes){
  
}
# testing
res_base <- base1(Badges)
res_sql <- sql1(Badges)
res_dplyr <- dplyr1(Badges)

dplyr::all_equal(res_sql, res_base)
compare::compare(res_sql, res_base, allowAll = TRUE)
dplyr::all_equal(res_sql, res_dplyr)
compare::compare(res_sql, res_dplyr, allowAll = TRUE)
dplyr::all_equal(res_sql, res_dt)
compare::compare(res_sql, res_dt, allowAll = TRUE)

microbenchmark::microbenchmark(
  sqldf1 = sql1(Badges),
  base1 = base1(Badges),
  dplyr1 = dplyr1(Badges)
) -> res
summary(res)
