ethnicity_recode <- c(
  `1` = "Akan (Bono, Ashanti etc)",
  `2` = "MO",
  `3` = "Kusasi",
  `4` = "Frafra",
  `5` = "Dagarti",
  `6` = "Chokosi",
  `7` = "Bimoba",
  `8` = "Fulani",
  `9` = "Basare",
  `10` = "Kokomba",
  `11` = "Wala",
  `12` = "Sisala",
  `13` = "Zambrama",
  `14` = "Ga Adangbe",
  `15` = "Mamprusi",
  `16` = "Dagomba",
  `17` = "Gonja",
  `18` = "Ewe",
  `20` = "Kasem",
  `21` = "Nankam",
  `22` = "Buli",
  `19` = "Other" # Keep "Other specify" as is
)

# Recode the ethnicity column
data$ethnicity_recoded <- ethnicity_recode[as.character(data$ethnicity)]

# Convert the recoded column to a factor
data$ethnicity_recoded <- as.factor(data$ethnicity_recoded)