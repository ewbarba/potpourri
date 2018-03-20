## 3/20: Integrating R and GitHub- Lindsay Veazey
## {taxa} tutorial adapted from: https://cran.r-project.org/web/packages/taxa/vignettes/taxa-vignette.html 
## Package developed by: Scott Chamberlain and Zachary Foster (2017). taxa: Taxonomic Classes. 
## R package version 0.2.0. https://CRAN.R-project.org/package=taxa

##########################################################################################################

# This is an in-development version on the GH page: https://github.com/ropensci/taxa 
devtools::install_github("ropensci/taxa") # This download is a bit slow for me as of now- may be my home network..
# install.packages('taxa') # Try only if ^this fails
library(taxa)
## Classes in {taxa}

# {taxa} defines some basic taxonomic classes and functions to manipulate them using other packages in R. 
# There are two types of classes:
  # 1. The classes concerned only with *taxonomic* information: taxon, taxonomy, hierarchy.
  # 2. The taxmap class is concerned with combining taxonomic data with *user-defined data* of any type (molecular sequences, abundance counts etc.)

# Taxonomic data usually comes from a database. 
# A common example is the NCBI Taxonomy Database 
# The database class stores the name of the database and associated information:
  
(ncbi <- taxon_database(
    name = "ncbi",
    url = 'http://www.ncbi.nlm.nih.gov/taxonomy',
    description = 'NCBI Taxonomy Database',
    id_regex = '*'))

ncbi$name
ncbi$url

# The authors provide a built-in selection of frequently used databases:

database_list 

taxon_rank(name = 'species', database = 'ncbi')

# Add below how to call "name = 'species' to the Barcode of Life database.

taxon_rank(name = 'species', database = 'bold')

# The taxon class combines the classes containing the name, rank, and ID for the taxon. 

(x <- taxon(
  name = taxon_name('Poa annua'),
  rank = taxon_rank('species'),
  id = taxon_id(93036),
  authority = 'Linnaeus'
))

(x <- taxon(
  name = 'Poa annua',
  rank = 'species',
  id = 93036,
  authority = 'Linnaeus'
))

# ^ What did I forget here? Add it in.

# Taxonomic classifications are ordered, ranked taxa.
# The hierarchy class stores a list of taxon classes like taxa in a correctly ordered classification.

x <- taxon(
  name = taxon_name("Poaceae"),
  rank = taxon_rank("family"),
  id = taxon_id(4479)
)

y <- taxon(
  name = taxon_name("Poa"),
  rank = taxon_rank("genus"),
  id = taxon_id(4544)
)

z <- taxon(
  name = taxon_name("Poa annua"),
  rank = taxon_rank("species"),
  id = taxon_id(93036)
)

(poaHier <- hierarchy(z, y, x)) # Edit this.

# Multiple hierarchy classes are stored in the hierarchies class.

a <- taxon(
  name = taxon_name("Felidae"),
  rank = taxon_rank("family"),
  id = taxon_id(9681)
)

b <- taxon(
  name = taxon_name("Puma"),
  rank = taxon_rank("genus"),
  id = taxon_id(146712)
)

c <- taxon(
  name = taxon_name("Puma concolor"),
  rank = taxon_rank("species"),
  id = taxon_id(9696)
)
(pumaHier <- hierarchy(c, b, a))

# Call both recently created hierarchies with the following format: hierarchies(hier1, hier2). Type below:

hierarchies(poaHier, pumaHier)

# The taxonomy class stores unique taxon objects in a tree structure. 
# Usually this kind of complex information would be the output of a file parsing function, 
# but the code below shows how to construct a taxonomy object from scratch.

mammalia <- taxon(name = "Mammalia", rank = "class", id = 9681)
felidae <- taxon(name = "Felidae", rank = "family", id = 9681)
felis <- taxon(name = "Felis", rank = "genus", id = 9682)
catus <- taxon(name = "catus", rank = "species", id = 9685)
panthera <- taxon(name = "Panthera", rank = "genus", id = 146712)
tigris <- taxon(name = "tigris", rank = "species", id = 9696)
plantae <- taxon(name = "Plantae", rank = "kingdom", id = 33090)
solanaceae <- taxon(name = "Solanaceae", rank = "family", id = 4070)
solanum <- taxon(name = "Solanum", rank = "genus", id = 4107)
lycopersicum <- taxon(name = "lycopersicum", rank = "species", id = 49274)

# Define hierarchies:
tiger <- hierarchy(mammalia, felidae, panthera, tigris)
cat <- hierarchy(mammalia, felidae, felis, catus)
# Define the tomato hierarchy below:
tomato <- hierarchy(plantae, solanaceae, solanum, lycopersicum)

tax <- taxonomy(tiger, cat, tomato) # <- Note that you have to define tomato to get this to work.

# roots: taxa that have no supertaxa.
roots(tax, value = "taxon_names")

# leaves: taxa that have no subtaxa.
leaves(tax, value = "taxon_names")

# pop() - Pop out taxa, that is, drop them: 

pop(pumaHier, ranks("family"))

# Keep all taxa greater than or equal to a taxonomic level using span().
span(pumaHier, ranks(">= genus"))

### Cool- here's the end of this brief tutorial on {taxa}. 

# Now, because you all may issue identical pull requests, here is a unique simple edit you can make:

class.list <- as.list(c('evan')) # Add your name, and I can accept and merge these unique additions.

## Return to worksheet 9.
