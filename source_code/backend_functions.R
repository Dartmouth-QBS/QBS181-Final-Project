# install.packages(c("tidyverse", "plotly", "devtools", "wesanderson", "ggpubr",
#                    "RColorBrewer", "knitr", "magrittr", "kableExtra", "kable"))
suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(plotly))
suppressPackageStartupMessages(library(devtools))
suppressPackageStartupMessages(library(wesanderson))
suppressPackageStartupMessages(library(ggpubr))
suppressPackageStartupMessages(library(RColorBrewer))
suppressPackageStartupMessages(library(knitr))
suppressPackageStartupMessages(library(magrittr))
suppressPackageStartupMessages(library(kableExtra))

# renameNeighborhoods: df -> vector
# recode neighborhood names so that they are standardized with the
# official names
renameNeighborhoods = function(x) {
  x$Community.Area.Name = recode(x$Community.Area.Name, 
                                 "Humboldt park" = "Humboldt Park",
                                 "Loop" = "The Loop", 
                                 "Montclaire" = "Montclare")
  return(x$Community.Area.Name)
}

# pivotLonger: data.frame, numerical vector, string, string -> tibble
# transpose the given dataframe so that each row corresponds to some
# unique grouping of neighborhood and year
pivotLonger = function(x, cols, names, values) {
  x = x %>%
    pivot_longer(cols = cols,
                 names_to = names,
                 values_to = values) %>%
    mutate(year = substr(eval(parse(text = names)), 
                         nchar(eval(parse(text = names))) - 3, 
                         nchar(eval(parse(text = names)))))
  return(x)
}

# join_with_race: data.frame, data.frame -> data.frame
# inner join dataframe with dataframe of race distributions and
# create groups of neighborhoods based on the proprotion of the
# White, Non-Hispanic population in 2000
join_with_race = function(stat_df, race_df) {
  joined_df = stat_df %>%
    inner_join(race_df, by = c("Community.Area.Name" = "Neighborhood")) %>%
    mutate(diversity_score = case_when(
      White.Non.Hispanic_2000 >= 62.8 ~ "1",
      White.Non.Hispanic_2000 < 62.8 & White.Non.Hispanic_2000 >= 36.7 ~ "2",
      White.Non.Hispanic_2000 < 36.7 & White.Non.Hispanic_2000 >= 12.3 ~ "3",
      White.Non.Hispanic_2000 < 12.3 & White.Non.Hispanic_2000 >= 1.1 ~ "4",
      White.Non.Hispanic_2000 < 1 ~ "5"
    )) %>%
    mutate(community_group = recode(diversity_score,
                                    "1" = "Least Diverse",
                                    "2" = "Less Diverse",
                                    "3" = "Moderately Diverse",
                                    "4" = "More Diverse",
                                    "5" = "Most Diverse"))
  return(joined_df)
}









