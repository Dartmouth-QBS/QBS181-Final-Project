# Add community group name as column
TeenMomsBRLongCommGroup<-TeenMomsBirthRatesLong %>% 
  mutate(CommunityGroup=as.factor(community_groups[DiversityScore]))

# returns matrix of Tukey results
AnovaTukeyTeenMoms<-function(year){
  model<-aov(BirthRate~CommunityGroup,data=TeenMomsBRLongCommGroup 
             %>% filter(Year==year))
  return(TukeyHSD(model)$CommunityGroup[,4])
}

years<-c(2000:2009)

p_vals = sapply(years, function(x) {
  AnovaTukeyTeenMoms(x)
}) #ANOVA applied to each year bt 2000-2009

colnames(p_vals) = years

tukey_results_tm = apply(p_vals, MARGIN = 2, function(x) {
  ifelse(x < 0.001, "< 0.001", ifelse(round(x, 5) == 1, 0.99999, round(x, 5)))
})

tukey_results_tm = format_kable(tukey_results_tm)