# liczba postów, zamieszczonych przez wszystkich uzytkownikow 
# pochodzących z danej lokacji

# data
Posts <- read.csv("Posts.csv.gz")
Users <- read.csv("Users.csv.gz")

# solution 0 - sqldf
sql <- function(Posts, Users){
  res_sql <- sqldf::sqldf(
  "SELECT Location, COUNT(*) AS Count
  FROM (
        SELECT Posts.OwnerUserId, Users.Id, Users.Location
        FROM Users
        JOIN Posts ON Users.Id = Posts.OwnerUserId
        )
  WHERE Location NOT IN ('')
  GROUP BY Location
  ORDER BY Count DESC
  LIMIT 10")
}

# solution 1 - base functions
base <- function(Posts, Users){
  res <- merge(Users, Posts,  by.x = "Id", by.y="OwnerUserId")
  res <- data.frame(OwnerUserId = res$Id, 
                    Id = res$Id,  
                    Location = res$Location)
  res <- res[res$Location != "", ]
  
  res <- aggregate(res[,1], by = res[c("Location")], length)
  colnames(res)[2] <- "Count"
  res <- res[order(-res$Count),]
  res <- head(res, 10)
}

# solution 2 - dplyr
dplyr <- function(Posts, Users){
  library(dplyr)
  res_dplyr <- 
    inner_join(Users,Posts,  by = c("Id" = "OwnerUserId")) %>%
    select(c(Id, Location)) %>%
    filter(Location != "") %>%
    group_by(Location) %>%
    summarise(
      Count = n()) %>%
    arrange(desc(Count)) %>%
    slice_head(n=10)
}

# solution 3 - data.table 
data_table <- function(Posts, Users){
  
}
# testing
res_base <- base(Posts, Users)
res_sql <- sql(Posts, Users)
res_dplyr <- dplyr(Posts, Users)
res_dt <- data_table(Posts, Users)

dplyr::all_equal(res_sql, res_base)
compare::compare(res_sql, res_base, allowAll = TRUE)
dplyr::all_equal(res_sql, res_dplyr)
compare::compare(res_sql, res_dplyr, allowAll = TRUE)
dplyr::all_equal(res_sql, res_dt)
compare::compare(res_sql, res_dt, allowAll = TRUE)

microbenchmark::microbenchmark(
  sqldf = sql(Posts, Users),
  base = base(Posts, Users),
  dplyr = dplyr(Posts, Users),
  data.table = data_table(Posts, Users)
) -> res
summary(res)


