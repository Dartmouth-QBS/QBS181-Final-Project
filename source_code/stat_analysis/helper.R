format_kable = function(df) {
  rows = rownames(df)
  df = data.frame(df)
  
  df = data.frame(lapply(df, function(x) {
    cell_spec(x, bold = ifelse((as.numeric(x) < 0.05) | (x == "< 0.001"), T, F))
  }))
  
  rownames(df) = rows
  names(df) = c("2000", "2001", "2002", "2003", "2004", "2005", "2006",
                "2007", "2008", "2009")
  return(df)
}