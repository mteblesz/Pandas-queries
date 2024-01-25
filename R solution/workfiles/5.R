

# data
Posts <- read.csv("Posts.csv.gz")
Votes <- read.csv("Votes.csv.gz")

# solution 0 - sqldf
sql5 <- function(Posts, Votes){
  res_sql <- sqldf::sqldf(
    "SELECT
    Posts.Title,
    VotesByAge2.OldVotes
    FROM Posts
    JOIN (
        SELECT
            PostId,
            MAX(CASE WHEN VoteDate = 'new' THEN Total ELSE 0 END) NewVotes,
            MAX(CASE WHEN VoteDate = 'old' THEN Total ELSE 0 END) OldVotes,
            SUM(Total) AS Votes
        FROM (
            SELECT
              PostId,
              CASE STRFTIME('%Y', CreationDate)
                WHEN '2021' THEN 'new'
                WHEN '2020' THEN 'new'
                ELSE 'old'
              END VoteDate,
              COUNT(*) AS Total
            FROM Votes
            WHERE VoteTypeId IN (1, 2, 5)
            GROUP BY PostId, VoteDate
            ) AS VotesByAge
        GROUP BY VotesByAge.PostId
        HAVING NewVotes=0
        ) AS VotesByAge2 ON VotesByAge2.PostId=Posts.ID
    WHERE Posts.PostTypeId=1
    ORDER BY VotesByAge2.OldVotes DESC
    LIMIT 10


")
}


# solution 2 - dplyr
dplyr5 <- function(Posts, Votes){
  library(dplyr)
  res_dplyr <- Votes %>%
    filter(VoteTypeId %in% c(1, 2, 5)) %>%
    mutate(Year = format(as.Date(CreationDate), "%Y")) %>%
    mutate(VoteDate = case_when(
      Year == 2021 ~ 'new',
      Year == 2020 ~ 'new',
      TRUE ~ 'old'
    )) %>%
    group_by(PostId, VoteDate) %>%
    summarize(Total = n()) %>% # end of VotesByAge  nowe/stare głosy na posty 
    mutate( 
      NewVotes = if_else(VoteDate == 'new', Total, 0)) %>%
    mutate( 
      OldVotes = if_else(VoteDate == 'old', Total, 0)) %>%
    group_by(PostId) %>%
    summarize(
      NewVotes = max(NewVotes),
      OldVotes = max(OldVotes),
      Votes = sum(Total)) %>%
    filter(NewVotes == 0) %>% #2 wiersze z rozną wartosia zmienione na kolumny
    inner_join(x = Posts %>%  filter(PostTypeId == 1), 
               y = ., by = c("Id" = "PostId")) %>%
    select(Title, OldVotes) %>%
    arrange(desc(OldVotes))%>%
    slice_head(n=10)
}


# testing
res_sql5 <- sql5(Posts, Votes)
res_dplyr5 <- dplyr5(Posts, Votes)

dplyr::all_equal(res_sql5, res_dplyr5)
compare::compare(res_sql5, res_dplyr5, allowAll = TRUE)



