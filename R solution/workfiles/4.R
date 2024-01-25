# Post(typu PostTypeId=1), który w danym roku ma najwięcej UpVote-ów (typu VoteTypeId=2)
# Title - tytuł rekordowego posta
# Year - rok zamieszenia
# Count - ilość UpVote-ów (maksimum danego roku)


# data
Posts <- read.csv("Posts.csv.gz")
Votes <- read.csv("Votes.csv.gz")

# solution 0 - sqldf
sql <- function(Posts, Votes){
  res_sql <- sqldf::sqldf(
    "SELECT
        Posts.Title,
        UpVotesPerYear.Year,
        MAX(UpVotesPerYear.Count) AS Count
    FROM 
        (
        SELECT
            PostId,
            COUNT(*) AS Count,
            STRFTIME('%Y', Votes.CreationDate) AS Year
        FROM Votes
        WHERE VoteTypeId=2
        GROUP BY PostId, Year
        ) AS UpVotesPerYear
    JOIN Posts ON Posts.Id=UpVotesPerYear.PostId
    WHERE Posts.PostTypeId=1
    GROUP BY Year
    ORDER BY Year ASC

")
}

# solution 1 - base functions
base <- function(Posts, Votes){
  inner <- Votes[Votes$VoteTypeId == 2, 
                 c("PostId", "Id", "CreationDate")]
  inner["CreationDate"] <- lapply(inner["CreationDate"], 
                                  function(d) c(format(as.Date(d), "%Y")))
  colnames(inner)[colnames(inner) == "CreationDate"] <- "Year"
  inner <- aggregate(inner[ , 2], by = inner[c("PostId", "Year")], length)
  colnames(inner)[colnames(inner) == "x"] <- "Count"
  
  outer <- Posts[Posts$PostTypeId == 1, c("Id", "Title")]
  outer <- merge(outer, inner, by.x = "Id", by.y = "PostId")
  x <- outer 
  x_splitted <- split(x, x$Year)
  x_agg <- lapply(x_splitted, function(z) z[which.max(z$Count), ])
  outer <- data.frame(
                      Title = sapply(x_agg, function(z) z$Title),
                      Year = sapply(x_agg, function(z) z$Year),
                      Count = sapply(x_agg, function(z) z$Count)                      
                    )
  outer <- outer[order(outer$Year), ]
}

# solution 2 - dplyr
dplyr <- function(Posts, Votes){
  library(dplyr)
  res_dplyr <- Votes %>% 
    filter(VoteTypeId ==2) %>%
    select(PostId, Id, CreationDate) %>%
    mutate(Year = format(as.Date(CreationDate), "%Y")) %>%
    group_by(PostId, Year) %>%
    summarize(Count = n()) %>%
    inner_join(x = Posts %>%  filter(PostTypeId == 1) %>% select(Id, Title),
               y = ., by = c("Id" = "PostId")) %>%
    group_by(Year) %>%
    filter(Count == max(Count)) %>%
    select(Title, Year, Count) %>%
    arrange(Year) 
}

# solution 3 - data.table 
data_table <- function(Posts, Votes){
  
}
# testing
res_base <- base(Posts, Votes)
res_sql <- sql(Posts, Votes)
res_dplyr <- dplyr(Posts, Votes)
res_dt <- data_table(Posts, Votes)

dplyr::all_equal(res_sql, res_base)
compare::compare(res_sql, res_base, allowAll = TRUE)
dplyr::all_equal(res_sql, res_dplyr)
compare::compare(res_sql, res_dplyr, allowAll = TRUE)
dplyr::all_equal(res_sql, res_dt)
compare::compare(res_sql, res_dt, allowAll = TRUE)
compare::compare(res_base, res_dplyr, allowAll = TRUE)

microbenchmark::microbenchmark(
  sqldf = sql(Posts, Votes),
  base = base(Posts, Votes),
  dplyr = dplyr(Posts, Votes),
  data.table = data_table(Posts, Votes)
) -> res
summary(res)

