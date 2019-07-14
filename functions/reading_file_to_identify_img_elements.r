

setwd("/Users/Gaston/Documents/gastonstat.github.io/_posts")
files = system("ls", intern=TRUE)
head(files)

# read first post
content = readLines(files[1])
num_lines = length(content)

for (i in 1L:num_lines) 
{
  
  # extract first word
  first_word = str_extract(content[i], "\\w+")
  # if first word is an image:
  if (first_word == "img") {
    split_img = strsplit(content[i], split=" ")
    img_pieces = length(split_img)
    img_source = split_img[img_pieces]
    
  }
}

library(stringr)


content = readLines("2014-02-26-My-year-in-Oniris-Nantes.md", warn=FALSE)
first_word = str_extract(content[15], "\\w+")
split_img = unlist(strsplit(content[15], split=" "))
img_pieces = length(split_img)
img_source = split_img[img_pieces]
img_file = substr(x=img_source, start=6, stop=nchar(img_source))
file_starts_with = substr(img_file, start=1, stop=4)
if (file_starts_with != "/ima") {
  
}
cat(split_content)


string = '<img class="centered" src="/images/rstudio_keyshortcut.png">'
str_extract(string, "\\w+")


?str_extract


