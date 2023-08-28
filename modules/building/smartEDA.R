# install.packages("SmartEDA")
library(SmartEDA)


# similarly, with dplyr syntax: df %>% ExpReport(...)

ExpReport(
  df,
  Target="cardio",
  label=NULL,
  op_file="Report.html",
  op_dir=getwd())
