# What: Software Carpentry Workshop
# When: Octorber 18th, 2016
# Who: Ning Wang
# Where: Hatcher Library, UM

# Packages Necessary for this analysis

install.packages('ggplot2')
install.packages('RSQLite')
install.packages('dplyr')

library(RSQLite)

conn <- dbConnect(SQLite(), dbname='/Users/ningwang/Desktop/survey.sqlite')

tables <- dbListTables(conn)
tables


class(tables)

surveys <- dbGetQuery(conn, 'Select * from surveys')
head(surveys)
summary(surveys)
names(surveys)

surveys <- dbGetQuery(conn, 'Select * from surveys
      JOIN species ON surveys.species_id = species.species_id
      JOIN plots ON surveys.plot_id = plots.plot_id;')
names(surveys)

surveys <- read.csv('/Users/ningwang/Desktop/ecology.csv')
head(surveys)
class(surveys)

x1 <- c(1, 2, 3)
class(x1)
typeof(x1)
x2 <- c('a', 'b', 'c')
class(x2)
typeof(x2)

df <- data.frame(
  x1 = c(TRUE, FALSE, TRUE), # c means concatenate, treat all as characters
  x2 = c(1, 'red', 2))
df
list(99, TRUE, 'balloons') # list can contain different vectors and define them as numeric or characters.
list(1:10, c(TRUE, FALSE))

str(surveys)
class(df$x1)
typeof(df)
typeof(df$x2)

class(surveys$year) # Gives as a vector 'integer'
head(surveys$year)

class(surveys['year']) # Give data frame
head(surveys['year'])

class(surveys[,'year'])

class(surveys[['year']])

head(surveys$sex)
levels(surveys$sex)
nlevels(surveys$sex)
levels(surveys$genus)

spice <- factor(c('low', 'medium', 'high'))
?factor
level(spice)
levels(spice)

spice <- factor(c('low', 'medium', 'high'), levels = c('low', 'medium', 'high'), ordered = TRUE)
max(spice)

spice <- ordered(spice, levels = c('high', 'medium', "low"))
max(spice)

my_taxa <- ordered(surveys$taxa, levels = c('Rodent', 'Bird', 'Rabbit', 'Reptile'))

tabulation <- table(surveys$taxa)
tabulation
barplot(table(my_taxa))

table(surveys$year, surveys$taxa)
with(surveys, table(year, taxa))

order(surveys$weight) # order the indices
surveys[order(surveys$weight),]
sort(surveys$weight)  # values in order 


?order
?table

dbDisconnect(conn)
rm(conn)

surveys$taxa == 'Rodent'
length(surveys$taxa == 'Rodent')
dim(surveys) # ?dim
surveys[surveys$taxa == 'Rodent', 'taxa']
year1 <- surveys$year >= 1980
year2 <- surveys$year <= 1990
seq.int(1980, 1990)
surveys[surveys$year %in% seq.int(1980,1990) & surveys$taxa == 'Rodent',]
surveys[year1 | year2 & surveys$taxa == 'Rodent',] # | means or


library(dplyr)
output <- select(surveys, year, taxa, weight)
head(output)
filter(surveys, taxa == 'Rodent')
dplyr::filter(surveys, taxa == 'Rodent')
?install.packages
?filter
# using pipes to redirect
filter(select(surveys, year, taxa, weight), taxa == 'Rodent')
surveys %>% filter(taxa == 'Rodent') %>% select(year, taxa, weight)

# put the filter results to an output, just assign them to a name
rodent_surveys <- surveys %>% filter(taxa == 'Rodent') %>% select(year, taxa, weight)

rodent_surveys2 <- surveys %>% filter(taxa == 'Rodent' & year == seq.int(1980, 1990)) # %in% find years in 1980-1990

all.equal(rodent_surveyes, rodent_surveys2) # check if two command results are equal

surveys %>%
  mutate(weight_kg = weight / 1000) %>%
  tail()

# Split, apply, combine
surveys %>%
  filter(!is.na(weight), taxa == 'Rodent', year %in% 1980:1990) %>%  # filter(weight != 'NA')
  group_by(species_id) %>%
  summarize(median(weight)) %>%
  print(n=25)


surveys_complete <- surveys %>%
  filter(!is.na(weight),
         species_id != '',
         !is.na(hindfoot_length),
         sex != '',
         taxa == 'Rodent') # %>%
 # summary()
head(surveys_complete)

common_species <- surveys_complete %>% 
  group_by(species_id) %>%
  tally()  %>%
   filter(n >= 50) %>%
  select(species_id)
head(common_species)

common_surveys <- surveys_complete %>%
   filter(species_id %in% common_species$species_id)

write.csv(common_surveys, file = '~/Desktop/surveys_complete.csv', row.names = FALSE)

library(ggplot2)
ggplot(data = common_surveys,
       aes(x = weight, y = hindfoot_length, color = species_id)) +
  geom_point()
