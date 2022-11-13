source("source_code/stat_analysis/helper.R")

AnovaTukeyLBW <- function(year,data){
  model<-aov(PercentLBW~CommunityGroup,data=data %>% filter(Year==year))
  summary(model)
  tukey = TukeyHSD(model)
  return(tukey$CommunityGroup[,4])
}

AnovaTukeySigDiffs = function(data) {
  years<-c(2000:2009)
  
  p_vals = sapply(years, function(x) {
    AnovaTukeyLBW(x, data)
  }) #ANOVA applied to each year bt 2000-2009
  
  colnames(p_vals) = years
  
  tukey_results = apply(p_vals, MARGIN = 2, function(x) {
    ifelse(x < 0.001, "< 0.001", ifelse(round(x, 5) == 1, 0.99999, round(x, 5)))
  })
  
  return(tukey_results)
}




