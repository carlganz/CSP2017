library(dplyr)

database <- data.frame(
  stringsAsFactors = FALSE,
  varname = c("HEALTH", "SRAGE", "POVLL", "SRSEX"),
  label = c("Health Status", "Age",
            "Poverty Level", "Gender"),
  universe = "All Adults",
  vartype = c(1, 2, 2, 1),
  identify = c(4, 3, 3, 4),
  sensitive = c(3, 4, 3, 4),
  PUF = c("Y", "N", "Y", "Y"),
  ddsection = c("B","A","A", "A"),
  ddorder = 4:1,
  notes = c(NA,NA, "Based on 2010 Poverty Line", NA),
  year = c(rep(2011, 4),
           rep(2012, 4),
           rep(2013, 4),
           rep(2014, 4)),
  age = c(rep("ADULT",16),
          rep("TEEN",16),
          rep("CHILD",16))
)

my_db <- src_sqlite("metadata.sqlite3", create = TRUE)

copy_to(my_db, database, "metadata",
        temporary = FALSE)