#
# Checks for new UFR data and runs acquisition script if new data is found
#

# Load relevant libraries
library(rvest)

# Get the urls for recently tagged mgoblog articles
ufr_html  <- read_html("http://mgoblog.com/category/post-type/upon-further-review")
new_links <- ufr_html %>% html_nodes("a") %>% xml_attr("href")
new_links <- new_links[grepl("upon-further-review-20",new_links)]

# Remove urls for links to comments section of articles already listed
new_links <- new_links[!grepl("#",new_links)]

# Reverse the order of new_links to ensure it processes oldest first
new_links <- new_links[length(new_links):1]

# Check to see if any links are new
are_new_links <- !sapply(new_links,function(x,y){x %in% y},y=links)
# If any of the links are new, update the UFR databases
if(any(are_new_links)) source("ufr_acquire_new.R")
