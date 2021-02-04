update_card_description <- function(roaster_taste_notes, trade_taste_notes, taste_profile) {
  
description <- paste0(
"Verdict
-----
<buy it again?>

Taste
-----

Brew notes
-----

Description
-----

** Roaster**: _", 
roaster_taste_notes, 
"_

**Trade**: _",
trade_taste_notes,
"_

**Taste profile**: _",
taste_profile,
"_"
)
  
  return(description)
}


