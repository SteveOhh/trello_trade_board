# trello_trade_board
Create a list of Trello cards corresponding to coffee currently for sale on drinktrade.com

## scrape.R
Trade conveniently has all their coffees listed on one page. Inconveniently, there’s a "load more" button, so I used RSelenium to send keypresses and clicks, until the page has all links to the individual coffee pages. The script downloads them all, then creates a dataframe with a row for each coffee. I used Selector Gadget to identify exactly which HTML elements I needed. 

After downloading, duplicates and 5-lb. bags are removed from the dataframe. Then, the location information needs to be augmented in order to use Trello's Maps Power-Up. After creating a Google Cloud project and API key, you can use their API to look up latitude and longitude coordinates from the text field.

## upload_to_trello.R
After setting up your Trello API key, there is some manual board set up. Once a board is in place as described in the file, the `make_coffee_card()` function adds a card with the right title, description, label, attachments, and custom fields. 

## update_card_description.R
This was a long function, so I pulled it into a separate file. It generates the text string which makes up the card description.
