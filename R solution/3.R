#TOP 10 postów, które uzyskały najwięcej odpowiedzi
# data
Posts <- read.csv("Posts.csv.gz")
Users <- read.csv("Users.csv.gz")

# solution 0 - sql
sql <- function(Posts, Users){
  res_sql <- sqldf::sqldf(
    "SELECT
        Users.AccountId,
        Users.DisplayName,
        Users.Location,
        AVG(PostAuth.AnswersCount) as AverageAnswersCount
    FROM
        (
        SELECT
        AnsCount.AnswersCount,
        Posts.Id,
        Posts.OwnerUserId
        FROM 
            (
            SELECT Posts.ParentId, COUNT(*) AS AnswersCount
            FROM Posts
            WHERE Posts.PostTypeId = 2
            GROUP BY Posts.ParentId
            ) AS AnsCount
        JOIN Posts ON Posts.Id = AnsCount.ParentId
        ) AS PostAuth
    JOIN Users ON Users.AccountId=PostAuth.OwnerUserId
    GROUP BY OwnerUserId
    ORDER BY AverageAnswersCount DESC
    LIMIT 10
")
}

# solution 1 - base functions
base <- function(Posts, Users){
  ansCount <- Posts[Posts$PostTypeId == 2, c("Id", "ParentId")]
  ansCount <- aggregate(ansCount[ , 1], by = ansCount[c("ParentId")], length)
  colnames(ansCount)[2] <- "AnswersCount"
  
  postAuth <- merge(Posts, ansCount, by.x = "Id", by.y = "ParentId")
  postAuth <- data.frame(AnswersCount = postAuth$AnswersCount, 
                         Id = postAuth$Id,  
                         OwnerUserId = postAuth$OwnerUserId)
  
  res <- merge(Users, postAuth, by.x = "AccountId", by.y = "OwnerUserId")
  res <- aggregate(res["AnswersCount"], 
                   by = res[c("AccountId",
                              "DisplayName",
                              "Location")], 
                   mean)
  
  colnames(res)[colnames(res) == "AnswersCount"] <- "AverageAnswersCount"
  res <- res[order(-res$AverageAnswersCount), ] #TO ZMIENIC
  res <- head(res, 10) 
  res %>% as.data.frame(row.names = 1:nrow(.))
}

# solution 2 - dplyr
dplyr <- function(Posts, Users){
  library(dplyr)
  res_dplyr <- Posts %>%
  filter(PostTypeId == 2) %>%
  select(Id, ParentId, PostTypeId) %>%
  group_by(ParentId) %>%
  summarise(AnswersCount = n()) %>%
  inner_join(x = Posts, y = ., by = c("Id" = "ParentId")) %>%
  select(AnswersCount, Id, OwnerUserId) %>%
  inner_join(x = Users, y = ., by = c("AccountId" = "OwnerUserId")) %>%
  select(AccountId, AnswersCount) %>%
  group_by(AccountId) %>%
  summarise(AverageAnswersCount = mean(AnswersCount)) %>%
  inner_join(x = Users, y = ., by = "AccountId") %>% #getting location and name back
  select(AccountId, DisplayName, Location, AverageAnswersCount) %>%
  arrange(desc(AverageAnswersCount)) %>%
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
compare::compare(res_base, res_dplyr, allowAll = TRUE)

microbenchmark::microbenchmark(
  sqldf = sql(Posts, Users),
  base = base(Posts, Users),
  dplyr = dplyr(Posts, Users),
  data.table = data_table(Posts, Users)
) -> res
summary(res)


