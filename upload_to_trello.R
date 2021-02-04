require(trelloR)

### get key & secret from https://trello.com/app-key ### use personal Trello account
# Only do this once https://github.com/jchrom/trelloR/issues/32
my_token = get_token(appname = "Trade board project",
                     key = "30278732032ff952a83401244dd94968",
                     secret = NULL, # should read from cache
                     scope = c("read", "write"),
                     expiration = "never"
                     )

### Load most recent data
load("./Data/coffees.Rdata")

############ ############ ############ 
############ Set up       ########### 
############ ############ ############ 

### Navigated in browser to 
# create the board
# delete the lists I don't want,
# create a list I do want ("Trade coffee list)
# get the board ID 
# create custom fields

###################### Make card function #########################
### <final> Create one card for each coffee, with names a concat of roaster/product names
# For each...
# Make card name
# Make card description
# Create card
# Store card ID in the same ? dataframe (new_card$id)
##################################################################

##### ##### ##### ##### 
##### Prep  ##### ##### 
##### ##### ##### ##### 

### Global variables
# Store board ID
board_id <- "5fe8d603949d7e3f69354b90"

# Target list
list_ids <- get_board_lists(board_id)
coffee_list_id <- list_ids[list_ids$name=="Trade coffee list","id"] %>% as.character()

# Get card custom fields' IDs
board_custom_fields <- get_board_cards(board_id)[1,1] %>% get_card_fields()

# Store field names
cfs <- c("Price", "Producer", "Sub_region", "Process", "Variety", "Elevation", "URL")

# Function for attachments
add_attachments <- function(card_id, coffee) {
  
  # global variables
  token <- my_token$credentials$oauth_token
  key <- "30278732032ff952a83401244dd94968" # obviously not global, but it could be
  
  # defined by inputs
  put_url <- paste0("https://api.trello.com/1/cards/", card_id, "/attachments/")
  image_payload <- list(name = "PNG", url = coffee$image, mimeType = "image/png")
  url_payload <- coffee$url
  
  httr::POST(url = paste0(put_url, '?key=', key, '&token=', token),
             body = image_payload,  
             encode = "json"
  )  
  
  httr::POST(url = paste0(put_url, '?key=', key, '&token=', token),
             body = url_payload,  
             encode = "json"
  )    
  
}


##### ##### ##### ##### 
##### Make a card ##### 
##### ##### ##### ##### 
make_coffee_card <- function(coffee, map = FALSE) {
  # Description function
  source("./populate_card_description.R")
  
  # Card title
  roaster <- coffee$roaster_name
  product_name <- coffee$product_name
  
  # Description
  roaster_taste_notes <- coffee$roaster_taste_notes
  trade_taste_notes <- coffee$trade_taste_notes
  taste_profile <- coffee$taste_profile
  
  description <- populate_card_description(roaster_taste_notes, trade_taste_notes, taste_profile)
  
  # if(map == TRUE) {gps_coordinates <- coffee$source_gps_string}
  gps_coordinates <- coffee$source_gps_string # not sure how to handle NAs; trying it out
  
  # Card list, name, description, and map coordindates to send with request
  card_details <- list(name = paste0(product_name, " - ", roaster),
                       desc = description, 
                       coordinates = gps_coordinates, ### this is where the map location goes
                       pos = "bottom")
  
  # Create card & store details (so you can call the ID)
  new_card <- add_card(list = coffee_list_id, body = card_details, token = my_token)
  
  # Update custom fields
  for(cf in cfs) {
    column_name <- tolower(cf)
    
    field_id <- board_custom_fields[board_custom_fields$name==cf,"id"]%>% as.character()
    field_key <- "text" 
    raw_field_value <- coffee[1,column_name] %>% 
      # coerce tibble to character
      as.character() %>% 
      # handle empty result
      ifelse(.=="character(0)", "", .)
    # 
    field_value <- ifelse(raw_field_value=="character(0)", "", raw_field_value)
    
    
    # # this is deprecated, but I don't see how to use update_resource()
    update_card_field(card = new_card$id, 
                      field = field_id,
                      key   = field_key,
                      value = field_value,
                      token = my_token)
    }
  
  
  # Attach image and URL (* need to learn how to add attachment)
  add_attachments(card_id = new_card$id, coffee = coffee)    
    
  # Add decaf label (*need to learn)
  if(coffee$decaf==TRUE) {
    add_label(card = new_card$id, color = "yellow", name = "Decaf")
  }
  
 return(new_card$id) 
}

### Upload from all rows in the df
for(i in 1:nrow(coffees)) {
  df <- tibble(coffees[i,])
  make_coffee_card(df, map = TRUE)
}
