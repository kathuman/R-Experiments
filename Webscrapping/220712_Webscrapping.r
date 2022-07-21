install.packages('rvest')

library(rvest)

webpage = read_html("https://www.geeksforgeeks.org/data-structures-in-r-programming")
heading = html_node(webpage, '.entry-title')

text = html_text(heading)
print(text)
