## 4/17: Collaboration mini project II: data manipulation

### PART I ###

rm(list = ls()) # Clear the workspace

### Installation ###

# Many packages! Plus some explanation:

library(dplyr)  # Data wrangling, glimpse() and tbl_df()
library(fs)  # File management functions
library(lubridate)  # Dates and times
library(magrittr)  # Pipes %>%, %T>% and equals(), extract()'
library(maps)  # Maps for US cities
library(mosaic)  # favstats and other summary functions
library(ncdf4)  # NetCDF manipulation
library(stringi)  # More strings
library(stringr)  # String operations
library(tibble)  # Convert row names into a column
library(tidyr)  # Prepare a tidy dataset, use gather()
library(tidytext)  # Tidying text data for analysis
library(tidyverse)  # All tidyverse packages

# Hint: use install.packages(c('pkg1', 'pkg2')) etc. to install missing ones.

### PART II ###

## Load the data

Abc7 <- read.csv("abc7ny.csv")
Kcra <- read.csv("kcra.csv")

## Inspect the data 

?glimpse()  # In the tibble library
Abc7 %>% glimpse()
# You can also just use glimpse(Abc7), but getting comfy with pipes is good!

# Now inspect your Kcra data. 

# Doublecheck that the column names are identical between the files
base::identical(names(Abc7), names(Kcra))

?dim
?class()  # You may want to use these functions sometimes

## Combine the datasets

?dplyr::bind_rows()  # Top-to-bottom merge of two data frames 
?dplyr::bind_cols()  # Side-by-side merge of two data frames

# Note that the :: format above means package::function.
# You can equivalently write: 

?bind_rows()
?bind_cols()

# What makes sense to merge these files? 

Merge <- dplyr::bind_cols(Abc7, Kcra, .id = "data_id") 

Merge <- dplyr::bind_rows(Abc7, Kcra, .id = "data_id") 
# Warnings are ok, don't worry

# Check the dimensions
Merge %>% 
  dplyr::count(data_id) # A tibble
head(Merge)  # head(tibble) is a bit different from head(df)
Merge %>% 
  glimpse()

# The somewhat dramatic definition of a tibble is on your worksheet

## Manipulating strings

# I'm taking the first 5 lines of column 'headline'
# I'll put that data into an object (headlines_var):

headlines_var <- Merge %>% 
  dplyr::select(headline) %>% 
  Matrix::head(5) 

# {base} applies arguments to characters
# We need to ensure we are working with characters:

is.character(headlines_var)  # Not a character...
typeof(headlines_var)

# We can convert a list to a character vector via unlist():

headlines_var <- unlist(headlines_var)
is.character(headlines_var)  # Worked!

# Examine the vector:

headlines_var %>% utils::str()

## Remove characters

sub(pattern = "-", replacement = " ", x = headlines_var)

# Here, base::sub() only finds the first instance of - and replaces it with a blank space " ". 
# If I want to remove ALL instances of -, I use gsub().

gsub(pattern = "-", replacement = " ", x = headlines_var)

## Combine characters

# To collapse strings or "stick" them together, use paste():

?paste()
paste(headlines_var, collapse = "; ")

# Or cat(), short for "concatenate":

?cat()
cat(headlines_var, sep = ", ")

# Or noquote():
noquote(headlines_var)

## {stringr}

# Hadley Wickham, a chief programmer for R Studio, developed this handy package.
# {stringr} is a package I have relied on a lot lately, so this is only a preview.

# Change cases

Merge %>% 
  mutate(headline_low = str_to_lower(headline)) %>% 
  select(headline, headline_low) %>% 
  head()

# You can also use {stringr} to search for word occurrences:
?dplyr::mutate()
Merge %>% 
  dplyr::mutate(teaser_3_words = stringr::word(Merge$teaser, 1, 3)) %>% 
  count(teaser_3_words, sort = TRUE) %>%   # Again, note the package::library format
  head(10)

# Joining new data

# We've explored cbind(), rbind(), and merge(). Here's some advanced joining.

UsCity <- maps::us.cities # Using built-in data from package {maps}
UsCity %>% glimpse()

UsCity %>% dplyr::distinct(name) %>% nrow()  # What is this telling us?

# Next up, we are touching on "regular expressions", defined as:
# "pattern[s] which specifies a set of strings of characters"
# They are commonly also defined as "a nightmare". Ha. 

UsCity <- UsCity %>%  # Reassign our mutated UsCity df to an identically named df
  mutate(
    city_id = stringr::str_replace_all(  # This is my favorite {stringr} function
      string = UsCity$name,
      pattern = UsCity$country.etc,
      replacement = ""
    ),
    city_id = stringr::str_trim(city_id),  # Trim whitespace
    city_id = stringr::str_to_lower(city_id)  # Change to lowercase
  )
UsCity %>% glimpse()

# Make a vector of IDs parsed by |

city_id_vec <- unlist(UsCity$city_id)
city_id_vec <- paste(city_id_vec, sep = "", collapse = " | ")

# Turn everything to lowercase:

Merge <- Merge %>% 
  mutate(headline = stringr::str_to_lower(headline))
Merge %$% head(headline, 1)  # This pipe differs- it makes use of a regular expression argument
?"%$%"

# Now we want to see which headlines in Merge contain cities from "city_id_vec"
# We ask stringr's str_detect() to operate inside dplyr's filter()
# These functions check for matches according to the pattern = argument.

MapMerge <- Merge %>%
  filter(stringr::str_detect(string = headline,
                             pattern = city_id_vec)) %>%
  dplyr::select(headline,
                dplyr::everything())
MapMerge %>% dplyr::glimpse()

# We now have 2322 matching, filtered observations.
# To join to UsCity, we create a city_id variable in MapMerge using str_extract()

MapMerge <- MapMerge %>%
  mutate(
    city_id = str_extract(
      string = MapMerge$headline,
      pattern = paste(city_id_vec, collapse = "|")
    ),
    city_id = stringr::str_trim(city_id)
  )
MapMerge %>%
  count(city_id, sort = TRUE) %>%
  head() # Hello, tibble

# Using inner_join() from dplyr, I join the two dfs:

?inner_join()
MapMerge <- inner_join(MapMerge, UsCity, by = "city_id")

# This is the end of this tutorial. Save your edits and return to the worksheet.

### PART III ###

## Dates and times

# Getting R to recognize date/time entry as I intended has taken MONTHS of my life.
# Save yourself those months with this tutorial.
# First up, package {lubridate}, another magical contribution by Hadley Wickham:

ymd('20180417')
mdy('04-18-2018')
dmy('18/04/2018')

# {lubridate} nicely handles varius formats and separators.

arrive <- ymd_hms("2018-04-18 12:00:00", tz = "Pacific/Honolulu")
arrive

leave <- ymd_hms("2018-04-18 14:00:00", tz = "Pacific/Honolulu")
leave

## Setting and extracting

second(arrive)
second(arrive) <- 25 #  Change the arrival time
arrive
second(arrive) <- 0 #  Reset it

wday(arrive)  # returns the day of the week as a number or ordered factor if label is TRUE
wday(arrive, label = TRUE)

## Time Zones

defense <- ymd_hms("2018-05-04 15:30:00", tz = "Pacific/Honolulu")
with_tz(defense, "America/New_York")  # What time is the defense in NYC?

## Time Intervals

hnl <- interval(arrive, leave) #  How long will we be in Honolulu?
hnl 

hnl <- arrive %--% leave #  Another pipe type!
?"%--%"
hnl

# How long was the visit to Honolulu?
hnl / ddays(1)
hnl / dhours(1)
hnl / dminutes(1)
hnl / dminutes(2)  # How many minutes in the second half of the visit?

## Handling NetCDF libraries

# Note that I use "libraries" here to refer to the *software* type, not an R library.
# NetCDF = Network Common Data Form
# Do you ever use NOAA or UCAR data? You need to know how to examine a NetCDF file.
 
nc = nc_open('yourpathway/file.nc')  # Hint: Style tip #8
str(nc)  # Examine nc file structure
# Okay, we see there are 4 dimensions (labeled: $ dimid: int [1:4(1d)] 0 1 2 3)
# There are other variables here too- coordinate info, units, variable of interest, units...
v1 = nc$var[[1]] # Identifies which of the variables that you'll extract time from
v1

# Now you'll need to index into the dimensions of v1. Try this:
v1$dim[[1]]  # Doesn't look like time. What about...
v1$dim[[2]]  # Keep going...
v1$dim[[3]]  # Here is our depth variable, identified with $name up top...
v1$dim[[4]]  # Here's time.

?as.POSIXlt()
dates<-as.POSIXlt(v1$dim[[4]]$vals, origin = "1970-01-01", tz = "UTC") # Hint: Style tip #5 
dates  # Verify the date

dat <- ncvar_get(nc; v1) # Hint: Style tip #9
dim(dat)
# 139 rows (lon) and 38 col (lat)
lon <- v1$dim[[1]]$vals # dim[[1]] is lon, verified above with v1$dim[[1]] 
lat = v1$dim[[2]]$vals # dim[[2]] is lat; also, edit this line, delete this comment
# Repeat each lat value 139 times (to match with lon)
latrep <- rep(lat, each = 139)
latrepdf <- as.data.frame(latrep)
# Repeat lon values 38 times
lonrep <- rep(lon, 38)
lonrepdf <- as.data.frame(lonrep)
coords <- cbind(lonrepdf, latrepdf)
# Pair values to appropriate coords
valsApr18 <- c(dat)  # Combine Values into a vector
datvals <- as.data.frame(valsApr18)
tempdat <- cbind(datvals,coords) # Hint: Style tip #5 
summary(tempdat)

# Now these data are helpfully organized in a way that allows for further manipulation

# End of this Part III exercise- return to the worksheet.