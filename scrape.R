######################
### scrape.R
### 1) download all coffees from Trade Coffee
### 2) minor data cleanup
### 3) add map coordinate data from region names

require(tidyverse)
require(httr)
require(rvest)
require(RSelenium)

######################
### Getting page data: load full coffee list by clicking "more" 
# Create a Selenium driver (it wants a version I don't have by default)
rd <- rsDriver(browser = "chrome", chromever = "87.0.4280.88") 

# Set the client browser
rem_dr <- rd[["client"]]

# Go to the page
all_coffee_url <- "https://www.drinktrade.com/coffee/all-coffee"
rem_dr$navigate(all_coffee_url)

# Wait 8 seconds, then escape the popup
Sys.sleep(time = 8)
rem_dr$findElement(using = "css","body")$sendKeysToElement(list(key="escape"))

# Click "more" a bunch of times
more_button <- rem_dr$findElement(using = 'css selector', ".load-more-btn")

keep_loading <- function(fn) {
  fn
  Sys.sleep(time = 1)
}

replicate(n = 27, expr = keep_loading(more_button$clickElement()))

# Download the page
full_page <- read_html(rem_dr$getPageSource()[[1]])

# Stop the selenium server
rd[["server"]]$stop()

### Work with the downloaded page
# Get the children of the top level body node
body_nodes <- full_page %>% 
  html_node("body") %>% 
  html_children()

body_nodes

### Get coffee info
# Grab the coffee URL nodes
link_nodes <- body_nodes %>% html_nodes(".product-link")

# Isolate product link URLs
product_links <- html_attr(link_nodes, "href")

# Download coffee pages
download_from_suffix <- function(suffix) { 
  full_url <- paste0("https://www.drinktrade.com", suffix)
  html_output <- read_html(full_url)
  
  return(list(html_output, full_url))
}

coffee_pages <- lapply(product_links, download_from_suffix)

### Combine coffee info
# Function to pull relevant information into a dataframe
extract_info <- function(page) {
  ### ### Store URL separately
  url <- page[[2]]
  
  ### ### Format page (into a list)
  coffee_nodes <- page[[1]] %>% html_node("body") %>% html_children()
  
  ### ### Get coffee attributes
  ### Used Selector Gadget to pull XPaths/specific elements directly
  roaster_name <- coffee_nodes %>% html_nodes(".roaster-name") %>% html_text()
  product_name <- coffee_nodes %>% html_nodes(".product-name") %>% html_text()
  price <- coffee_nodes %>% html_nodes(".product-price") %>% html_text()
  roaster_taste_notes <- coffee_nodes %>% html_nodes(".roaster-notes-body") %>% html_text()
  trade_taste_notes <- coffee_nodes %>% html_nodes(".description-container") %>% html_text()
  taste_profile <- coffee_nodes %>% html_nodes(".taste-features li") %>% html_text()
  image <- coffee_nodes %>% html_nodes(".product-image-container") %>% html_children() %>% html_attr("src")
  roaster_location <- coffee_nodes %>% html_nodes(".location .body") %>% html_text()
  # taste_profile_category <- coffee_nodes %>% html_nodes(".spec-section-type .spec-title") %>% html_text()
  # roast_day <- coffee_nodes %>% html_nodes(".spec-section-taste .text-display-3-desktop") %>% html_text()
  
  ### There are unnamed attributes, so I have to get the names with the values
  # Initialize character vectors
  titles <- vector("character")
  values  <- vector("character")
  
  # Make vectors of titles and values
  for(i in 1:5) {
    title <- coffee_nodes %>% html_nodes(paste0("li:nth-child(", i, ") .list-", "title")) %>% html_text()
    value  <- coffee_nodes %>% html_nodes(paste0("li:nth-child(", i, ") .list-", "value")) %>% html_text()
    
    titles <- titles %>% append(title)
    values <- values %>% append(value)
  }
  
  # Combine into a df for lookup
  attributes_df <- as_tibble(cbind(titles, values))
  
  # Look up values based on title names (could use `==` instead of grep)
  producer <- attributes_df[grep(pattern = "Producer", x=attributes_df$titles),"values"] %>% 
    as.character()
  process <- attributes_df[grep(pattern = "Process", x=attributes_df$titles),"values"] %>% 
    as.character()
  sub_region <- attributes_df[grep(pattern = "Sub Region", x=attributes_df$titles),"values"] %>% 
    as.character()
  variety <- attributes_df[grep(pattern = "Variety", x=attributes_df$titles),"values"] %>% 
    as.character()
  elevation <- attributes_df[grep(pattern = "Elevation", x=attributes_df$titles),"values"] %>% 
    as.character()
  
  ### ### Return all the values
  return(as_tibble(
                cbind(
                  roaster_name,product_name,price,roaster_taste_notes,trade_taste_notes,producer,
                  taste_profile,process,sub_region,elevation,variety,roaster_location,image,url
                     )
                 )
        )
}



### Combine elements (within coffee, then across) into a dataframe
coffees <- lapply(coffee_pages, FUN = extract_info) %>% bind_rows() 


# Clean the dataframe
coffees <- coffees %>% 
  # Remove 5 lb bags
  filter(!grepl(product_name, pattern="5 lb")) %>% 
  # Label decaf
  mutate(decaf = grepl(product_name, pattern = "decaf", ignore.case = TRUE)) %>% 
  # Remove duplicates
  distinct()

### Optionally, add map coordinates
### register for API key at console.cloud.google.com (https://developers.google.com/maps/documentation/maps-static/get-api-key)
### enable API: https://stackoverflow.com/questions/32994634/this-api-project-is-not-authorized-to-use-this-api-please-ensure-that-this-api
### enable Billing (!)
# register_google(key = "key string", write=TRUE)
coffees <- coffees %>% 
  mutate(source_gps_coordinates = ifelse(sub_region == "character(0)", ### ignore all the blank strings;######## not actually working
                                          ############ change the encoding here
                                         NA, 
                                         ggmap::geocode(sub_region, output = "latlon")
                                         ) 
         ) %>% 
  mutate(source_gps_string =      ifelse(is.na(source_gps_coordinates$lat),
                                         NA,
                                         paste(source_gps_coordinates$lat, source_gps_coordinates$lon, sep=",")
                                         )
  )



# Save it. Make a copy because it's small (an alias would be better)
save(coffees, file = "./Data/coffees.Rdata")

current_time <- Sys.time() %>% format("%Y%m%d_%H%M")
save(coffees, file = paste0("./Data/coffees_", current_time, ".Rdata"))

                                                      
