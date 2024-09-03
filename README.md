
# Goodreader <a href="https://github.com/chaoliu-cl/Goodreader"><img src="man/figures/Goodreader.png" alt="Goodreader website" align="right" height="138"/></a> 

Goodreader is a comprehensive toolkit for scraping and analyzing book
data from Goodreads.

## Installation

From CRAN:

``` r
install.packages("Goodreader") 
```

## Use Goodreader

``` r
#load the package
library(Goodreader)

#Search for books
AI_df <- search_goodreads(search_term = "artificial intelligence", search_in = "title", num_books = 10, sort_by = "ratings")

#Retrieve Book IDs and save them into a text file
get_book_ids(input_data = AI_df, file_name = "AI_books.txt")

#Get book-related information
scrape_books(book_ids_path = "AI_books.txt")

#Scrape book reviews
scrape_reviews(book_ids_path = "AI_books.txt", num_reviews = 10)
```
