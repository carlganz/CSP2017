
responses <- data.frame(stringsAsFactors = FALSE,
  respondent = c(1847291,1823441,
                 1932184,1832521),
  answer = c("Persia", "US",
             "Romainia",
             "Guiana")
)

saveRDS(responses,
        "responses.rds")

pastUpcodes <- data.frame(stringsAsFactors=FALSE,
  answer = c(
    "USA",
    "Romania",
    "Guyana",
    "French Guiana"
  ),
  upcode = c(
    1, 4, 2, 2
  )
)

saveRDS(pastUpcodes,"pastUpcodes.rds")