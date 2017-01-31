
install.packages("readr")
install.packages("tidyr")
install.packages("dplyr")
install.packages("stringr")
library(tidyr)
library(dplyr)
library(readr)
library(stringr)

# load refine_orignal.csv
electronics_df <- read_csv("refine_original.csv")

# convert company names to lower case 
electronics_df$company <- with( electronics_df, tolower(company))

# remove excess spaces 
# with stringr
electronics_df$company <- str_replace_all(electronics_df$company, pattern=" ", repl="")

company_spelling <- c("philips", "akzo", "van houten", "unilever") 


for (i in 1:nrow(electronics_df)) {
  sample <- agrep (electronics_df[ i, "company"], x = company_spelling, ignore.case = TRUE, value = TRUE, max.distance = 2)
  electronics_df[i,"company"] <- sample
}



##########################################################################################

# split product code and number into separate columns 
# with dplyr
electronics_df <- separate(electronics_df,"Product code / number", c("product_code", "product_number"), sep= "-", remove = TRUE)


# create new field product_category and fill 
# with match from vector category_names when = prodcut_code value 
product_names <- c(p= "Smartphone", v = "TV", x = "Laptop", q = "Tablet")
electronics_df <- mutate(electronics_df, product_category =  product_names[product_code] )

# concatonate addresses into new column
# with dplyr
electronics_df <- unite(electronics_df, full_address, address, city, country, sep = ", ", remove = FALSE)


###########################################################################################



# create binary columns for each company
electronics_df <- electronics_df %>%
  mutate(company_unilever = ifelse(company == "unilever",1,0)) %>%
  mutate(company_akzo = ifelse(company == "akzo",1,0)) %>%
  mutate(company_philips = ifelse(company == "philips",1,0)) %>%
  mutate(company_van_houten = ifelse(company == "van houten",1,0)) 

# create binary columns for each procduct category
electronics_df <- electronics_df %>%
  mutate(product_smartphone = ifelse(product_category == "Smartphone",1,0)) %>%
  mutate(product_tv = ifelse(product_category == "TV",1,0)) %>%
  mutate(product_laptop = ifelse(product_category == "Laptop",1,0)) %>%
  mutate(product_tablet = ifelse(product_category == "Tablet",1,0)) 


write.csv(electronics_df, file = "refine_clean.csv")
