source("source_code/stat_analysis/helper.R")

# ANOVA/Tukey TESTS FOR PRENATAL CARE ONLY
prenatal_tri1_anova = function() {
  anova_results_prenatal_care_triOne_2000 = aov(Percent.2000 ~ Group, data = prenatal_care_trimesterOne_all)
  anova_results_prenatal_care_triOne_2000
  summary(anova_results_prenatal_care_triOne_2000)
  
  tukey2000 = TukeyHSD(anova_results_prenatal_care_triOne_2000)$Group[,4]
  
  anova_results_prenatal_care_triOne_2001 = aov(Percent.2001 ~ Group, data = prenatal_care_trimesterOne_all)
  anova_results_prenatal_care_triOne_2001
  summary(anova_results_prenatal_care_triOne_2001)
  
  tukey2001 = TukeyHSD(anova_results_prenatal_care_triOne_2001)$Group[,4]
  
  anova_results_prenatal_care_triOne_2002 = aov(Percent.2002 ~ Group, data = prenatal_care_trimesterOne_all)
  anova_results_prenatal_care_triOne_2002
  summary(anova_results_prenatal_care_triOne_2002)
  
  tukey2002 = TukeyHSD(anova_results_prenatal_care_triOne_2002)$Group[,4]
  
  anova_results_prenatal_care_triOne_2003 = aov(Percent.2003 ~ Group, data = prenatal_care_trimesterOne_all)
  anova_results_prenatal_care_triOne_2003
  summary(anova_results_prenatal_care_triOne_2003)
  
  tukey2003 = TukeyHSD(anova_results_prenatal_care_triOne_2003)$Group[,4]
  
  anova_results_prenatal_care_triOne_2004 = aov(Percent.2004 ~ Group, data = prenatal_care_trimesterOne_all)
  anova_results_prenatal_care_triOne_2004
  summary(anova_results_prenatal_care_triOne_2004)
  
  tukey2004 = TukeyHSD(anova_results_prenatal_care_triOne_2004)$Group[,4]
  
  anova_results_prenatal_care_triOne_2005 = aov(Percent.2005 ~ Group, data = prenatal_care_trimesterOne_all)
  anova_results_prenatal_care_triOne_2005
  summary(anova_results_prenatal_care_triOne_2005)
  
  tukey2005 = TukeyHSD(anova_results_prenatal_care_triOne_2005)$Group[,4]
  
  anova_results_prenatal_care_triOne_2006 = aov(Percent.2006 ~ Group, data = prenatal_care_trimesterOne_all)
  anova_results_prenatal_care_triOne_2006
  summary(anova_results_prenatal_care_triOne_2006)
  
  tukey2006 = TukeyHSD(anova_results_prenatal_care_triOne_2006)$Group[,4]
  
  anova_results_prenatal_care_triOne_2007 = aov(Percent.2007 ~ Group, data = prenatal_care_trimesterOne_all)
  anova_results_prenatal_care_triOne_2007
  summary(anova_results_prenatal_care_triOne_2007)
  
  tukey2007 = TukeyHSD(anova_results_prenatal_care_triOne_2007)$Group[,4]
  
  anova_results_prenatal_care_triOne_2008 = aov(Percent.2008 ~ Group, data = prenatal_care_trimesterOne_all)
  anova_results_prenatal_care_triOne_2008
  summary(anova_results_prenatal_care_triOne_2008)
  
  tukey2008 = TukeyHSD(anova_results_prenatal_care_triOne_2008)$Group[,4]
  
  anova_results_prenatal_care_triOne_2009 = aov(Percent.2009 ~ Group, data = prenatal_care_trimesterOne_all)
  anova_results_prenatal_care_triOne_2009
  summary(anova_results_prenatal_care_triOne_2009)
  
  tukey2009 = TukeyHSD(anova_results_prenatal_care_triOne_2009)$Group[,4]
  
  tukey_results = data.frame(tukey2000, tukey2001, tukey2002, tukey2003, tukey2004, 
                             tukey2005, tukey2006, tukey2007, tukey2008, tukey2009)
  
  names(tukey_results) = c(2000:2009)
  
  tukey_results = apply(tukey_results, MARGIN = 2, function(x) {
    ifelse(x < 0.001, "< 0.001", ifelse(round(x, 5) == 1, 0.99999, round(x, 5)))
  })
  
  tukey_results = format_kable(tukey_results)
  
  return(tukey_results)
}

prenatal_tri2_anova = function() {
  #performing ANOVA & Tukey HSD analyses
  anova_results_prenatal_care_triTwo_2000 = 
    aov(Percent.2000 ~ Group, data = prenatal_care_trimesterTwo_all)
  anova_results_prenatal_care_triTwo_2000
  summary(anova_results_prenatal_care_triTwo_2000)
  
  tukey2000 = TukeyHSD(anova_results_prenatal_care_triTwo_2000)$Group[,4]
  
  anova_results_prenatal_care_triTwo_2001 = 
    aov(Percent.2001 ~ Group, data = prenatal_care_trimesterTwo_all)
  anova_results_prenatal_care_triTwo_2001
  summary(anova_results_prenatal_care_triTwo_2001)
  
  tukey2001 = TukeyHSD(anova_results_prenatal_care_triTwo_2001)$Group[,4]
  
  anova_results_prenatal_care_triTwo_2002 = 
    aov(Percent.2002 ~ Group, data = prenatal_care_trimesterTwo_all)
  anova_results_prenatal_care_triTwo_2002
  summary(anova_results_prenatal_care_triTwo_2002)
  
  tukey2002 = TukeyHSD(anova_results_prenatal_care_triTwo_2002)$Group[,4]
  
  anova_results_prenatal_care_triTwo_2003 = 
    aov(Percent.2003 ~ Group, data = prenatal_care_trimesterTwo_all)
  anova_results_prenatal_care_triTwo_2003
  summary(anova_results_prenatal_care_triTwo_2003)
  
  tukey2003 = TukeyHSD(anova_results_prenatal_care_triTwo_2003)$Group[,4]
  
  anova_results_prenatal_care_triTwo_2004 = 
    aov(Percent.2004 ~ Group, data = prenatal_care_trimesterTwo_all)
  anova_results_prenatal_care_triTwo_2004
  summary(anova_results_prenatal_care_triTwo_2004)
  
  tukey2004 = TukeyHSD(anova_results_prenatal_care_triTwo_2004)$Group[,4]
  
  anova_results_prenatal_care_triTwo_2005 = 
    aov(Percent.2005 ~ Group, data = prenatal_care_trimesterTwo_all)
  anova_results_prenatal_care_triTwo_2005
  summary(anova_results_prenatal_care_triTwo_2005)
  
  tukey2005 = TukeyHSD(anova_results_prenatal_care_triTwo_2005)$Group[,4]
  
  anova_results_prenatal_care_triTwo_2006 = 
    aov(Percent.2006 ~ Group, data = prenatal_care_trimesterTwo_all)
  anova_results_prenatal_care_triTwo_2006
  summary(anova_results_prenatal_care_triTwo_2006)
  
  tukey2006 = TukeyHSD(anova_results_prenatal_care_triTwo_2006)$Group[,4]
  
  anova_results_prenatal_care_triTwo_2007 = 
    aov(Percent.2007 ~ Group, data = prenatal_care_trimesterTwo_all)
  anova_results_prenatal_care_triTwo_2007
  summary(anova_results_prenatal_care_triTwo_2007)
  
  tukey2007 = TukeyHSD(anova_results_prenatal_care_triTwo_2007)$Group[,4]
  
  anova_results_prenatal_care_triTwo_2008 = 
    aov(Percent.2008 ~ Group, data = prenatal_care_trimesterTwo_all)
  anova_results_prenatal_care_triTwo_2008
  summary(anova_results_prenatal_care_triTwo_2008)
  
  tukey2008 = TukeyHSD(anova_results_prenatal_care_triTwo_2008)$Group[,4]
  
  anova_results_prenatal_care_triTwo_2009 = 
    aov(Percent.2009 ~ Group, data = prenatal_care_trimesterTwo_all)
  anova_results_prenatal_care_triTwo_2009
  summary(anova_results_prenatal_care_triTwo_2009)
  
  tukey2009 = TukeyHSD(anova_results_prenatal_care_triTwo_2009)$Group[,4]
  
  tukey_results = data.frame(tukey2000, tukey2001, tukey2002, tukey2003, tukey2004, 
                             tukey2005, tukey2006, tukey2007, tukey2008, tukey2009)
  
  names(tukey_results) = c(2000:2009)
  
  tukey_results = apply(tukey_results, MARGIN = 2, function(x) {
    ifelse(x < 0.001, "< 0.001", ifelse(round(x, 5) == 1, 0.99999, round(x, 5)))
  })
  
  tukey_results = format_kable(tukey_results)
  
  return(tukey_results)
}

prenatal_tri3_anova = function() {
  #performing ANOVA & Tukey HSD analyses
  anova_results_prenatal_care_triThree_2000 = 
    aov(Percent.2000 ~ Group, data = prenatal_care_trimesterThree_all)
  anova_results_prenatal_care_triThree_2000
  summary(anova_results_prenatal_care_triThree_2000)
  
  tukey2000 = TukeyHSD(anova_results_prenatal_care_triThree_2000)$Group[,4]
  
  anova_results_prenatal_care_triThree_2001 = 
    aov(Percent.2001 ~ Group, data = prenatal_care_trimesterThree_all)
  anova_results_prenatal_care_triThree_2001
  summary(anova_results_prenatal_care_triThree_2001)
  
  tukey2001 = TukeyHSD(anova_results_prenatal_care_triThree_2001)$Group[,4]
  
  anova_results_prenatal_care_triThree_2002 = 
    aov(Percent.2002 ~ Group, data = prenatal_care_trimesterThree_all)
  anova_results_prenatal_care_triThree_2002
  summary(anova_results_prenatal_care_triThree_2002)
  
  tukey2002 = TukeyHSD(anova_results_prenatal_care_triThree_2002)$Group[,4]
  
  anova_results_prenatal_care_triThree_2003 = 
    aov(Percent.2003 ~ Group, data = prenatal_care_trimesterThree_all)
  anova_results_prenatal_care_triThree_2003
  summary(anova_results_prenatal_care_triThree_2003)
  
  tukey2003 = TukeyHSD(anova_results_prenatal_care_triThree_2003)$Group[,4]
  
  anova_results_prenatal_care_triThree_2004 = 
    aov(Percent.2004 ~ Group, data = prenatal_care_trimesterThree_all)
  anova_results_prenatal_care_triThree_2004
  summary(anova_results_prenatal_care_triThree_2004)
  
  tukey2004 = TukeyHSD(anova_results_prenatal_care_triThree_2004)$Group[,4]
  
  anova_results_prenatal_care_triThree_2005 = 
    aov(Percent.2005 ~ Group, data = prenatal_care_trimesterThree_all)
  anova_results_prenatal_care_triThree_2005
  summary(anova_results_prenatal_care_triThree_2005)
  
  tukey2005 = TukeyHSD(anova_results_prenatal_care_triThree_2005)$Group[,4]
  
  anova_results_prenatal_care_triThree_2006 = 
    aov(Percent.2006 ~ Group, data = prenatal_care_trimesterThree_all)
  anova_results_prenatal_care_triThree_2006
  summary(anova_results_prenatal_care_triThree_2006)
  
  tukey2006 = TukeyHSD(anova_results_prenatal_care_triThree_2006)$Group[,4]
  
  anova_results_prenatal_care_triThree_2007 = 
    aov(Percent.2007 ~ Group, data = prenatal_care_trimesterThree_all)
  anova_results_prenatal_care_triThree_2007
  summary(anova_results_prenatal_care_triThree_2007)
  
  tukey2007 = TukeyHSD(anova_results_prenatal_care_triThree_2007)$Group[,4]
  
  anova_results_prenatal_care_triThree_2008 = 
    aov(Percent.2008 ~ Group, data = prenatal_care_trimesterThree_all)
  anova_results_prenatal_care_triThree_2008
  summary(anova_results_prenatal_care_triThree_2008)
  
  tukey2008 = TukeyHSD(anova_results_prenatal_care_triThree_2008)$Group[,4]
  
  anova_results_prenatal_care_triThree_2009 = 
    aov(Percent.2009 ~ Group, data = prenatal_care_trimesterThree_all)
  anova_results_prenatal_care_triThree_2009
  summary(anova_results_prenatal_care_triThree_2009)
  
  tukey2009 = TukeyHSD(anova_results_prenatal_care_triThree_2009)$Group[,4]
  
  tukey_results = data.frame(tukey2000, tukey2001, tukey2002, tukey2003, tukey2004, 
                             tukey2005, tukey2006, tukey2007, tukey2008, tukey2009)
  
  names(tukey_results) = c(2000:2009)
  
  tukey_results = apply(tukey_results, MARGIN = 2, function(x) {
    ifelse(x < 0.001, "< 0.001", ifelse(round(x, 5) == 1, 0.99999, round(x, 5)))
  })
  
  tukey_results = format_kable(tukey_results)
  
  return(tukey_results)
}

prenatal_none_anova = function() {
  #performing ANOVA & Tukey HSD analyses
  anova_results_prenatal_care_none_2000 = 
    aov(Percent.2000 ~ Group, data = prenatal_care_none_all)
  anova_results_prenatal_care_none_2000
  summary(anova_results_prenatal_care_none_2000)
  
  tukey2000 = TukeyHSD(anova_results_prenatal_care_none_2000)$Group[,4]
  
  anova_results_prenatal_care_none_2001 = 
    aov(Percent.2001 ~ Group, data = prenatal_care_none_all)
  anova_results_prenatal_care_none_2001
  summary(anova_results_prenatal_care_none_2001)
  
  tukey2001 = TukeyHSD(anova_results_prenatal_care_none_2001)$Group[,4]
  
  anova_results_prenatal_care_none_2002 = 
    aov(Percent.2002 ~ Group, data = prenatal_care_none_all)
  anova_results_prenatal_care_none_2002
  summary(anova_results_prenatal_care_none_2002)
  
  tukey2002 = TukeyHSD(anova_results_prenatal_care_none_2002)$Group[,4]
  
  anova_results_prenatal_care_none_2003 = 
    aov(Percent.2003 ~ Group, data = prenatal_care_none_all)
  anova_results_prenatal_care_none_2003
  summary(anova_results_prenatal_care_none_2003)
  
  tukey2003 = TukeyHSD(anova_results_prenatal_care_none_2003)$Group[,4]
  
  anova_results_prenatal_care_none_2004 = 
    aov(Percent.2004 ~ Group, data = prenatal_care_none_all)
  anova_results_prenatal_care_none_2004
  summary(anova_results_prenatal_care_none_2004)
  
  tukey2004 = TukeyHSD(anova_results_prenatal_care_none_2004)$Group[,4]
  
  anova_results_prenatal_care_none_2005 = 
    aov(Percent.2005 ~ Group, data = prenatal_care_none_all)
  anova_results_prenatal_care_none_2005
  summary(anova_results_prenatal_care_none_2005)
  
  tukey2005 = TukeyHSD(anova_results_prenatal_care_none_2005)$Group[,4]
  
  anova_results_prenatal_care_none_2006 = 
    aov(Percent.2006 ~ Group, data = prenatal_care_none_all)
  anova_results_prenatal_care_none_2006
  summary(anova_results_prenatal_care_none_2006)
  
  tukey2006 = TukeyHSD(anova_results_prenatal_care_none_2006)$Group[,4]
  
  anova_results_prenatal_care_none_2007 = 
    aov(Percent.2007 ~ Group, data = prenatal_care_none_all)
  anova_results_prenatal_care_none_2007
  summary(anova_results_prenatal_care_none_2007)
  
  tukey2007 = TukeyHSD(anova_results_prenatal_care_none_2007)$Group[,4]
  
  anova_results_prenatal_care_none_2008 = 
    aov(Percent.2008 ~ Group, data = prenatal_care_none_all)
  anova_results_prenatal_care_none_2008
  summary(anova_results_prenatal_care_none_2008)
  
  tukey2008 = TukeyHSD(anova_results_prenatal_care_none_2008)$Group[,4]
  
  anova_results_prenatal_care_none_2009 =
    aov(Percent.2009 ~ Group, data = prenatal_care_none_all)
  anova_results_prenatal_care_none_2009
  summary(anova_results_prenatal_care_none_2009)
  
  tukey2009 = TukeyHSD(anova_results_prenatal_care_none_2009)$Group[,4]
  
  tukey_results = data.frame(tukey2000, tukey2001, tukey2002, tukey2003, tukey2004, 
                             tukey2005, tukey2006, tukey2007, tukey2008, tukey2009)
  
  names(tukey_results) = c(2000:2009)
  
  tukey_results = apply(tukey_results, MARGIN = 2, function(x) {
    ifelse(x < 0.001, "< 0.001", ifelse(round(x, 5) == 1, 0.99999, round(x, 5)))
  })
  
  tukey_results = format_kable(tukey_results)
  
  return(tukey_results)
}