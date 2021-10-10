
# https://bookdown.org/rdpeng/rprogdatascience/regular-expressions.html




# the Stringr package comes with fruit, words and sentences which later was known.






# REGULAR EXPRESSIONS 

# search for matches/ patterns
# grep()    | grepl()

# search a vector for matches, returns index of string
# regexpr() | gregexpr() 

# search a vector for matches & replaces it
# sub()     | gsub()

# search a vector returns location of any expression
# regexec()

# df = readLines("fileName.txt)

# w = grep("rainbow", df)
# w = grep("rainbow | rainbows", df)

# w = grep("Community: [2S]LGBTQ", df)

#======== grep words with context !
# p = grep("Community: [2S]LGBTQ")
# s = grep("[Ss]unday", df) # compare
# z = grep("[Cc]ummunirt", df)
# find difference
# setdiff(s, z)
# setdiff(z,s) # returns x

# df[x]




library(tidyverse)
df = starwars 

grep("^Darth", df$name) # return index
grep("^Darth", df$name, value = T) # returns strings




r = regexpr("[Hh]uman*", df$species[1:10])
regmatches(df$species[1:10], r)






x = df$name[3]
d = gsub("R2-D2", "R2-Dx",x)
d



# ============= stringr pkg
# df, "expression", option

l = str_subset(df$name, "Luke")
length(l)

str_detect(df$name, "Darth")

str_extract(df$name, "Darth")

darth= str_match(df$name, "Darth")
darth[4]
darth[42]





# ========= stringr documentation

x = c("Saturday","cannabis","relaxing",'Jazz','cool','CLIMATE', 'EMERG','loveR')

str_length(x) # counts length of each word  8 8 8 4 4 7 5 5

str_c(x, collapse = "-") # "Saturday-cannabis-relaxing-Jazz-cool"

str_sub(x, start = 1, end = 3) # "Sat" "can" "rel" "Jaz" "coo"

# regular expressions
str_subset(x, "[aeiou]")
str_subset(x, "[a]x") # "relaxing"

str_count(x, "[aeiou]") # 3 3 3 1 2 0 0 2


#--- 7 main verbs for patterns

# 1
str_detect(x, "[aeiou]") # TRUE  TRUE  TRUE  TRUE  TRUE FALSE FALSE  TRUE

# 2
str_count(x, "[ae]") # 2 2 2 1 0 0 0 1

# 3
str_subset(x, "[em]") # "relaxing" "loveR"  

# 4
str_locate(x, "[aeiou]") # returns a matrix

# 5
str_extract(x, "[aeiou]") # "a" "a" "e" "a" "o" NA  NA  "o"

# 6
str_match(x, "(.)[aeiou](.)") # returns matrix vowels removed

# 7
str_replace(x, "[aeiou]", "?")
# "S?turday" "c?nnabis" "r?laxing" "J?zz" "c?ol" "CLIMATE"  "EMERG"    "l?veR"  

# -----------------

str_to_upper(x)

str_to_lower(x)

str_to_title(x)
str_to_sentence(x)

str_c("Letter",letters, sep = ": ")

fruit =c("apple","cherry","Mango",'pineapple','pear','bananas','grapes')
str_count(fruit, 'p')
str_detect(fruit, "^a")
str_detect(fruit, "a$")



str_dup(fruit, 2) # "appleapple" "cherrycherry" ...
str_dup(fruit, 1:7) # ... "MangoMangoMango" ...


str_extract(fruit, "[a-z]") # a" "c" "a" "p" "p" "b" "g"
str_extract(fruit, "[a-z]{1,9}")
str_extract(fruit, "[A-Z]{1,9}")



str_flatten(fruit, collapse = "-")


species = "Humanoid"
stardate = Sys.Date()
str_glue(
  "you are a {species} ",
  "on this date: {stardate}"
)



str_length(c("I",'love','R'))


str_locate(fruit,"[Mm]ango")
#      start end
# [1,]    NA  NA
# [2,]    NA  NA
# [3,]     1   5

str_locate(fruit, '$')
str_locate(fruit, 'a')
str_locate(fruit, 'e')
str_locate_all(fruit, c('a','b','p','p'))



str_match(fruit, "berry")

phonebook = c('304-222-9080','908-132-3322','482 897 6547','239-923-8771','$1000','Fax: 303.405.7799')

phone = "([2-9][0-9]{2}[- .]([0-9]{3})[- .]([0-9]{4}))"
str_extract(phonebook, phone)
str_extract_all(phonebook, phone) 




str_order(letters)
str_sort(letters)

x = c("100",'321','90')
str_sort(x, numeric = T, decreasing = F)




str_pad("rainbow", width = 30, side = 'left', pad = "-")
# "-----------------------rainbow"

str_pad("rainbow", width = 30, side = 'right', pad = "-")
# "rainbow-----------------------"

str_pad("rainbow", width = 30, side = 'both', pad = "-")
# "-----------rainbow------------"






bag = c('2 apples','4 milks','6 bananas')

str_remove(bag, "[aeiou]") # "2 pples"  "4 mlks"   "6 bnanas"
str_remove_all(bag, "[aeiou]") # "2 ppls" "4 mlks" "6 bnns"



str_replace(bag, pattern = "[aeiou]", replacement = "*")
str_replace_all(bag, pattern = "[aeiou]", replacement = "*")
str_replace(bag, pattern = "[aeiou]", replacement = toupper )
str_replace_all(bag, pattern = "[aeiou]", replacement = "_" )


str_split(bag, " and ")
str_split(fruit, " and ", simplify = T)



str_starts(fruit, pattern = "p", negate = F)
fruit[55:65]


words = "Rainbows & Unicorns"
str_sub(words, start = 3, end = 13)
str_sub(words, start = c(1:5), end = c(7:11))

x <- "BBCDEF"
str_sub(x, 1, 1) <- "A"; x
str_sub(x, -1, -1) <- "K"; x
str_sub(x, -2, -2) <- "GHIJ"; x
str_sub(x, 2, -2) <- ""; x



str_subset(fruit, "a")
str_which(fruit, 'a') # return integers 

# Returns elements that do NOT match
str_subset(fruit, "^p", negate = TRUE)
str_which(fruit, "^p", negate = T)




whitespace = "    hey, we got   lots of s p a c e    here"
str_trim(whitespace, side = 'both')
str_squish(whitespace) # simpler and better 


str_trunc(whitespace, width = 20, side = 'right')



str_view( c('abc','fgh','jkl'), "[aeiou]" )



sentence_ = c("Jane saw how wonderful the Chimpanzees were","Jane documented all of her work","Alexa was not available")

word(sentence_ , start = 1) # "Jane"  "Jane"  "Alexa"
word(sentence_ , start = 2, end = -2)





  
  
  
  
  




