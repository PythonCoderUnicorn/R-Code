
#========= STRINGR DOCUMENTATION



library(stringr)

s = "bavdh eeuydh TEA $45 grams SHIPPING EXTRA -- no warranty !! \n halloween candy is a bad idea as it is TOO MUCH SUGAR SUGAR SUGAR  "
t = "extra extra don't Stringy it !!  \ya know it.\ don't wait. a\\b. Price $45"
v = c(s, t)


str_length(s) 

str_sub(s, 3, 3)
str_sub(s, 2, 2) <- 'XX'


str_order(v, decreasing = T)
str_sort(v)


phoneNums = c("apple", '232 122 6676', '899 090 3332', 'work: 344-788-4544', 'home: 564-677-7823')


str_detect(phones)

phone <- "([2-9][0-9]{2})[- .]([0-9]{3})[- .]([0-9]{4})"
str_subset(phoneNums, phone)

str_count(phoneNums, phone)




str_extract_all(phoneNums, phone, simplify = T)
str_match(phoneNums, phone)
str_replace(phoneNums, phone, "000-222-9999")


str_extract(v, '.t.') # .t. == letter< t >letter


# == REGULAR EXPRESSION
dot = "\\."

str_extract( c("abc", "a.c","bef"), "a\\.c"  )

s.1 = c("ab.cd.ef","ghi")
startsWith(s.1, "ab.")



# ===========================
(text.1 = "Some \t badly\n\t\tspaced \f text")

str_replace_all(text.1, "\\s+", " ")


(quotes.1 = c('"double double"', "<inside brace>","fancy dbl") )
str_replace_all(quotes.1, "\\p{quotation mark}", "'")


# ========================
str_extract_all(v, "\\w+"[[1]])
str_split(v, "\\W+"[[1]])


str_replace_all("the underscores this point", "\\b", '_')
str_replace_all("the underscores this point", "\\B", '_')



# ============= ******************** =============
match_abc = "[abc]" # a, b or c
match_a_z = "[a-z]" # every char between a : z
match_except = "[^abc]" # match all but a, b or c
# match_ticks = "[\^\-]" # match ^ or -

# ==== built-in classes, use inside [ ]
punct = "[[:punct:]]"
alphas = "[[:alpha:]]"
lowers = "[[:lower:]]" # lowercase letters
uppers = "[[:upper:]]"
digits = "[[:digit:]]" # NOT DIGITS !!!! 
hex_digits = "[[:xdigit:]]" # hex digits
alpha_nums = "[[:alnum:]]" # letters and numbers
ctrl_chars = "[[:cntrl:]]" # control characters
print_all = "[[:print:]]" # letters, numbers, punct, whitespace
blanks = "[[:blank:]]" # space and tab

# =========== !! Ahoy Anchors !! 
# ^  = matches the start of string 
# $  = matches the end of string

s.3= c("Ford-150",'Honda','Hyundai')
str_extract(s.3, "ai$")
str_extract(s.3, "^H")
str_extract(s.3, "\ai$") # not the same


# ---------- multiline 
# \A  = matches the start of the input.
# \z  = matches the end of the input.
# \Z  = matches the end of the input, but before the final line terminator, if it exists.

m = "line 1\n line 2\n\n line 3\n"
str_extract_all(m, "^line", simplify = T)[[1]] # returns just 'line'
str_extract_all(m, "^line..", simplify = T)[[1]]

str_extract_all(m, regex("\\Aline..", multiline = T))[[1]]



# ======== REPETITION
# ?   = 0 or 1
# +   = 1 or more
# *   = 0 or more

roman = "1888 is the longest MDCCCLXXXVIII"
str_extract(roman, "CC?") # CC
str_extract(roman, "CC+") #CCC
str_extract(roman, "CC*") # CCC
str_extract(roman, "C[LX]+") # "CLXXX"


# ======== number of matches 
str_extract(roman, "C{2}") # 2 matches 
str_extract(roman, "C{2,}") # 2+
str_extract(roman, "C{2,4}") #between x and y


# ========== GREEDY MATCHES
# ??  0 or 1   prefers 0
# +?  1+       fewer times
# *?  0+       fewer times

str_extract(roman, c("C{2,3}", "C{2,3}?") ) # "CCC" "CC" 
str_extract(roman, c("C[LX]+", "C[LX]+?") ) # "CLXXX" "CL" 

# ==== atomic matches
str_detect("ABC", "(?>A|.B)C") # false, matches A then skips B
str_detect("ABC", "(?:A|.B)C") # true, matches A, C no match, back to B


# ======= look arounds
look_ahead = "(?=...)" # matches if ... @ current input
look_back_neg = "(?!...)" # does not match @ current input
look_back_pos = "(?<=...)" # matches if text b4 current position w/ last char of match (no * or +)
look_back_neg_assrt = "(?<!...)" # does not match text b4 current position (no * or +)


# ============ patterns ===================
p = c('$3 cars free','$3 free payments','$6 months free','$2 weeks free')
str_extract(p, "(?<=\\$)\\d+")



phone <- regex("
  \\(?       # optional opening parens
  (\\d{3})   # area code
  \\)?       # optional closing parens
  (?:-|\\ )? # optional dash or space
  (\\d{3})   # another three numbers
  (?:-|\\ )? # optional dash or space
  (\\d{3})   # three more numbers
  ", comments = TRUE)

str_match(c("514-791-8141", "(514) 791 8141"), phone)




