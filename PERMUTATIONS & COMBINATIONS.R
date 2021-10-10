
# combinations & permutations


# permutations
# nPr = n! / (n - r)!

# combinations
# nCr = n! / (n - r)! r!

# probability
# p = (probability / success) / total outcomes



library(combinat)


# ============ PERMUTATIONS
permutes = permn(3)
permutes
# [1] 1 2 3
# [1] 1 3 2
# [1] 3 1 2
# [1] 3 2 1
# [1] 2 3 1
# [1] 2 1 3

permutes_count = length(permutes)
permutes_count # = 6










#======= COMBINATIONS
combinx = combinat::combn(3, 2) # matrix: row, columns
combinx

#       [,1] [,2] [,3]
# [1,]    1    1    2
# [2,]    2    3    3

combinx_count = ncol(combinx)
combinx_count # = 3



# =================== FACTORIAL
# how many ways to arrange letters of word?
word = "pepper"

word_length = stringr::str_count(word)
word_length

number_of_arrangements = factorial(word_length)
number_of_arrangements

# the letter repeats 
n_repeat = 3
factorial(word_length)/ factorial(n_repeat)


# how many ways can you arrange 2 
# of the letters in the word?

# permutations
# nPr = n! / (n - r)!

# 4 options for 1st letter
# 3 options for 2nd letter == 4 * 3 = 12
n= 4
r= 2

numerator = factorial(n)
denominator = factorial(n - r)

numerator/denominator


#  number of distinct ways to arrange letters
word = "geometry"
stringr::str_count(word) # 8

# e happens twice
letters_arranges = factorial(8)/ factorial(2)
letters_arranges




# 8 people are in a race for Gold, Silver, Bronze medals
# how many ways can medal be awarded?

# order matters

n= 8
r= 3 # medals

numerator = factorial(n)
denominator = factorial(n - r)

outcomes = numerator/denominator
outcomes




# 20 basketball players than play on 5 teams
# order doesn't matter

# combinations
# nCr = n! / (n - r)! r!

n = 20
r = 5

numerator = factorial(n)
demoninator = factorial(n - r) * factorial(r)

outcomes = numerator/demoninator
outcomes






# letters A B C T U V from a hat, find the 
# probability of finding exactly 3 letters that spell "cat"

# combinations
# nCr = n! / (n - r)! r!

n = 6
r = 3

numerator = factorial(n)
demoninator = factorial(n - r) * factorial(r)

outcomes = numerator/demoninator
outcomes
# =  1 in 20
probability = 1/outcomes * 100
paste(probability,"%")

# at a party w/ 30 people, each person shakes hand with every person
# how many total handshakes took place?

# order doesn't matter

# combinations
# nCr = n! / (n - r)! r!

n = 30
r = 2 # A + B shake hands

numerator = factorial(n)
demoninator = factorial(n - r) * factorial(r)

outcomes = numerator/demoninator
outcomes



# you and 3 friends running for committee
# 100 people are also running.
# what is the probability that you and friends win 4 seats?

# order doesn't matter

n = 100
r = 4

numerator = factorial(n)
demoninator = factorial(n - r) * factorial(r)

outcomes = numerator/demoninator
outcomes

probability = 1/outcomes * 100
paste(probability,"%")





# probability of 5 cards being all diamonds from 52 card deck?

n = 52
r = 5

numerator = factorial(n)
demoninator = factorial(n - r) * factorial(r)

outcomes = numerator/demoninator
outcomes



# how many 4 digit numbers < 7000 can be formed and is odd?
#  _ _ _ _ 

n1 = 6 # 6 is < 7
n2 = 10
n3 = 10
n4 =  5 # odd 1,3,5,7,9

number_of_arrangements = n1*n2*n3*n4
number_of_arrangements


# 5 people stand in a circle, how many ways?

numerator = factorial(5)
n= 5

numerator/n





letters[1:10]
letters[11:20]
uz = letters[21:26]

permn(uz)

w = c("w",'a','v','b','e','r')
length(w)
combn(w, 5)





library(combinat)
# get the list of all permutations
word.scramble <- combinat::permn(c("b","a","b","o","o","n"))

# convert the list to a matrix
my_word_matrix <- do.call(rbind,word.scramble)

#take the unique rows
my_matrix_ <- unique(my_word_matrix)

head(my_matrix_)











