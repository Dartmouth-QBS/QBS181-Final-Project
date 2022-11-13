source("source_code/stat_analysis/helper.R")

#ANOVA
births_and_fertility$community_group = as.factor(births_and_fertility$community_group)
levels(births_and_fertility$community_group) = c("Least", "Less", "Moderately", "More", "Most")

AnovaTukeyFertility<-function(year){
  model<-aov(fertility_rate_per_1000 ~ community_group, 
             data = births_and_fertility %>% filter(year == year))
  summary(model)
  tukey = TukeyHSD(model)
  return(tukey$community_group[,4])
}

years<-c(2000:2009)

p_vals = sapply(years, function(x) {
  # p_vals[x - 2001] = AnovaTukey(x)
  AnovaTukeyFertility(x)
}) #ANOVA applied to each year bt 2000-2009
colnames(p_vals) = years

tukey_results_fertility = apply(p_vals, MARGIN = 2, function(x) {
  ifelse(x < 0.001, "< 0.001", ifelse(round(x, 5) == 1, 0.99999, round(x, 5)))
})

tukey_results_fertility = format_kable(tukey_results_fertility)



