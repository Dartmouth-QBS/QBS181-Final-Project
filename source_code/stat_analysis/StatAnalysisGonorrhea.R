source("source_code/stat_analysis/helper.R")

#ANOVA
AnovaTukey<-function(x){
  model<-aov(avg~Comm_Group,data=gonorrhea_pivot %>% filter(year==x) %>%
               mutate(Comm_Group=as.factor(Comm_Group)))
  summary(model)
  tukey = TukeyHSD(model)
  return(tukey$Comm_Group[,4])
}

years<-c(2000:2009)

p_vals = sapply(years, function(x) {
  AnovaTukey(x)
}) #ANOVA applied to each year bt 2000-2009

colnames(p_vals) = years

tukey_results_gonr = apply(p_vals, MARGIN = 2, function(x) {
  ifelse(x < 0.001, "< 0.001", ifelse(round(x, 5) == 1, 0.99999, round(x, 5)))
})

tukey_results_gonr = format_kable(tukey_results_gonr)
