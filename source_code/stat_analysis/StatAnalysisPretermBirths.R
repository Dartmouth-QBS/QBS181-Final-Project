source("source_code/stat_analysis/helper.R")

preterm_anova = function() {
  #performing ANOVA & Tukey HSD analyses
  anova_results_preterm_births_2000 = aov(Percent.2000 ~ Group, data = preterm_births_all)
  anova_results_preterm_births_2000
  summary(anova_results_preterm_births_2000)
  
  tukey2000 = TukeyHSD(anova_results_preterm_births_2000)$Group[,4]
  
  anova_results_preterm_births_2001 = aov(Percent.2001 ~ Group, data = preterm_births_all)
  anova_results_preterm_births_2001
  summary(anova_results_preterm_births_2001)
  
  tukey2001 = TukeyHSD(anova_results_preterm_births_2001)$Group[,4]
  
  anova_results_preterm_births_2002 = aov(Percent.2002 ~ Group, data = preterm_births_all)
  anova_results_preterm_births_2002
  summary(anova_results_preterm_births_2002)
  
  tukey2002 = TukeyHSD(anova_results_preterm_births_2002)$Group[,4]
  
  anova_results_preterm_births_2003 = aov(Percent.2003 ~ Group, data = preterm_births_all)
  anova_results_preterm_births_2003
  summary(anova_results_preterm_births_2003)
  
  tukey2003 = TukeyHSD(anova_results_preterm_births_2003)$Group[,4]
  
  anova_results_preterm_births_2004 = aov(Percent.2004 ~ Group, data = preterm_births_all)
  anova_results_preterm_births_2004
  summary(anova_results_preterm_births_2004)
  
  tukey2004 = TukeyHSD(anova_results_preterm_births_2004)$Group[,4]
  
  anova_results_preterm_births_2005 = aov(Percent.2005 ~ Group, data = preterm_births_all)
  anova_results_preterm_births_2005
  summary(anova_results_preterm_births_2005)
  
  tukey2005 = TukeyHSD(anova_results_preterm_births_2005)$Group[,4]
  
  anova_results_preterm_births_2006 = aov(Percent.2006 ~ Group, data = preterm_births_all)
  anova_results_preterm_births_2006
  summary(anova_results_preterm_births_2006)
  
  tukey2006 = TukeyHSD(anova_results_preterm_births_2006)$Group[,4]
  
  anova_results_preterm_births_2007 = aov(Percent.2007 ~ Group, data = preterm_births_all)
  anova_results_preterm_births_2007
  summary(anova_results_preterm_births_2007)
  
  tukey2007 = TukeyHSD(anova_results_preterm_births_2007)$Group[,4]
  
  anova_results_preterm_births_2008 = aov(Percent.2008 ~ Group, data = preterm_births_all)
  anova_results_preterm_births_2008
  summary(anova_results_preterm_births_2008)
  
  tukey2008 = TukeyHSD(anova_results_preterm_births_2008)$Group[,4]
  
  anova_results_preterm_births_2009 = aov(Percent.2009 ~ Group, data = preterm_births_all)
  anova_results_preterm_births_2009
  summary(anova_results_preterm_births_2009)
  
  tukey2009 = TukeyHSD(anova_results_preterm_births_2009)$Group[,4]
  
  tukey_results = data.frame(tukey2000, tukey2001, tukey2002, tukey2003, tukey2004, 
                             tukey2005, tukey2006, tukey2007, tukey2008, tukey2009)
  
  names(tukey_results) = c(2000:2009)
  
  tukey_results = apply(tukey_results, MARGIN = 2, function(x) {
    ifelse(x < 0.001, "< 0.001", ifelse(round(x, 5) == 1, 0.99999, round(x, 5)))
  })
  
  tukey_results = format_kable(tukey_results)
  
  return(tukey_results)
}