# PatternToNumber
'create the variable pattern_input and assign it 
 the pattern we want to convert as a string'
pattern_input <- "AGT"

'define the function patternToNumber using the R function "function". 
The function takes at input the string of the pattern called pattern_input and 
returns the decimal number assigned to that pattern
https://www.rdocumentation.org/packages/base/versions/3.3.0/topics/function'
patternToNumber <- function(pattern_input){
  'convert the character string pattern_input into a vector using strsplit
  https://www.rdocumentation.org/packages/base/versions/3.6.0/topics/strsplit'
  pattern <- strsplit(pattern_input, "")[[1]]
  'create an empty set called numberfour'
  numberfour <- {}
  'create the variable numberDecimal and assign it the value zero'
  numberDecimal <- 0
  
'for-loop: going backwards through the pattern (starting at the last character).
  Determine the length of the pattern stored in pattern_input using nchar
  https://www.rdocumentation.org/packages/base/versions/3.6.0/topics/nchar'
  for (i in (nchar(pattern_input):1)){
    'if statements: check what letter is at the "i"th position of the pattern,
    create the variable called letter and assign it a value 
    between 0 and 3 according to the letter found'
    if (pattern[i] == "A")
      letter <- 0
    # Adenine gets the number 0
    if (pattern[i] == "C")
      letter <- 1
    # Cytosine becomes a 1 and so forth
    if (pattern[i] == "G")
      letter <- 2
    if (pattern[i] == "T")
      letter <- 3
    'add the number stored in letter to the list called numberfour using append
    (which now holds the pattern converted to a number with the basis 4,
    but as a vector (whose entries are the single numbers) and backwards
    https://www.rdocumentation.org/packages/base/versions/3.6.0/topics/append'
    numberfour <- append(numberfour, letter)
  }
'for loop transforms the vector numberfour into a decimal number.
  iterating through the entries in numberfour'
  for (i in (1:nchar(pattern_input))){
    'the decimal number called numberDecimal is altered In every iteration
    of the loop the value of the entry of the "i"th position in numberfour times
    4 (the basis) to the power of i-1 (convert the basis from 4 to 10) is added.'
    numberDecimal <- numberDecimal + (numberfour[i] * 4^(i-1))
  }
  'the function patternToNumber returns the created decimal number'
return(numberDecimal)
}

'execute the function patternToNumber with the test patterns:'
patternToNumber("AGT")
patternToNumber("CTTCTCACGTACAACAAAATC")



# numberToPattern

number <- 45
index <- 4

'create the function numberToPattern. Input: the number to be converted
and the index which is the length of the pattern'
numberToPattern <- function(number, index){
  'create the empty sets called "numberBackwards" and "pattern"'
  numberBackwards <- {}
  pattern <- {}
  'for-loop: iterating from 1 to the length of the pattern.
  It converts the decimal number "number" into a number to the basis 4, which
  is stored in the vector numberBackwards, with every digit as a single entry
  and in reverse order'
  for (i in (1:index)){
    'computes the modulo of the number divided by 4'
    modulo <- number %% 4
    'adds the modulo to the vector numberBackwards. According to the method to
    convert a number to basis 4 that was presented in the lecture, the modulo
    computed first represents the last digit in the converted number. Therefore,
    the number to the basis 4 is created backwards'
    numberBackwards <- append(numberBackwards, modulo)
    'reassign "number" with the number divided by 4 (and rounded down to the 
    next integer; using integer division %/%) to be used in the next iteration
    of the loop'
    number <- (number %/% 4)
  }
  'reverse the elements of numberBackwards to get the correct number with the 
  basis 4, which gets stored in the vector numberfour, using the function rev
  https://www.rdocumentation.org/packages/base/versions/3.6.0/topics/rev'
  numberfour <- rev(numberBackwards)
  'for loop: iterating from 1 to the index'
  for (i in 1:index){
    'converting the entries of numberfour to the DNA bases (as every digit in
    the number with the basis 4 represents one base in the pattern):
    0 becomes an Adenine, 1 a cytosine and so forth'
    if (numberfour[i] == 0)
      letter <- "A"
    if (numberfour[i] == 1)
      letter <- "C"
    if (numberfour[i] == 2)
      letter <- "G"
    if (numberfour[i] == 3)
      letter <- "T"
    'store the sequence of the pattern in the vector called pattern by appending
    the letter created from numberfour at every iteration of the loop'
    pattern <- append(pattern, letter)
  }
  'merge the entries of the vector "pattern" to a single character string 
  using paste
  https://www.rdocumentation.org/packages/base/versions/3.6.0/topics/paste'
  pattern <- paste(pattern, collapse="")
  'return the pattern as a single string'
  return(pattern)
  
}


'execute the function numberToPattern with the test numbers and indices'
numberToPattern(45,4)
numberToPattern(5353,7)


#Frequency Array
'define the function computingFrequencies. Input: text: character string that
holds the sequence. index: integer, length of the pattern'
computingFrequencies <- function(text, index){
  'create empty set'
  frequencyArray <- {}
  'for loop: iterating from 0 to all possible combinations with the length of 
  index, meaning the number of all possible patterns. creates a vector of
  that length with every entry being zero'
  for (i in 0:(4^index)){
    frequencyArray[i] <- 0
  }
  'for loop: going through the text, looking at the pattern of length index
  at every step'
  for (i in 0:(nchar(text)-index)){
    'extracting the pattern with length index at the position i in text using substr
    https://www.rdocumentation.org/packages/base/versions/3.6.0/topics/substr'
    pattern <-  substr(text, i+1, i + index)
    'create a variable position using the previously defined function patternToNumber'
    position <- patternToNumber(pattern)
    'use position to address the entry of frequencyArray which represent the current
    pattern and increase its value (corresponding the count number) by 1'
    frequencyArray[position +1] <- (frequencyArray[position+1]) + 1
  }
'return the frequency array called frequencyArray'
  return(frequencyArray)
}

'execute with data'
text <- "ACGCGGCTCTGAAA"
index <- 2

test <- "ACTTCGCCTAAGTCATTTATCCCGTGGTACGACGCTCCCTTACAGTCTTATATCCCGGTATATACGCAGAAATGCCTACGTCCCCTCGTCCCACACACCAGGGAAGCTGAAATCGCTCATCTACTATGCGTGTACTTCCGGACGAAATCGTCGTCGGCTTCTGTCTGGCGCTGGAGATCCGGGCTTCTTGAGGGACACACCCATTATGACCGTTACAGGACTTACAACTACTCTGAGCAATGATGGTGCTCTGTAACGAACAAACGCACTCACCTCTGTTTCCTGTATGACATCCTCAAATGGATCGACCGTGATGTACTGAGCGAATAAGTGCGGATTACATTTATAGTCAGCTACATTTATTCGCCGCTCGGAGCAGAGTATAATGAATTTATACCACTTGTTAGACTCCTTCTCGCATTTAGCCCCTACCGCAAGTCGGAGCGTTGGGGTGCAATAGAGTTTTCAGTATCTACGTACCGTTAAGTCTCTCGCGTTCTTTCAGCAGGCATCAATATGTTGCTTGCTGTGGGGTCGGGTGGGGCGGAGAGCCAATAAAGTGCATCGGAATTGGCTGCCCTCCTACGAATCCGCAAGATGCGGTGATGCTACGTGATTATGACTACTAGCTTAGTCCC"
computingFrequencies(text, index)
computingFrequencies(test, 6)


# Faster frequent words

'define the function. Input: text(where we are looking for the pattern) and 
index k: length of pattern. it returns the most frequent patterns of lenth k
as character strings'
fasterFrequentWords <- function(text, k){
  'create empty set'
  frequent_patterns <- {}
  'compute frequency array called frequency_array by using the previously defined
  function computingFrequencies'
  frequency_array <- computingFrequencies(text, k)
  'create variable called max_count that holds the value of the maximum count in
  frequency_array: the number of times the most frequent patterns occur in text.
  https://www.rdocumentation.org/packages/whitebox/versions/0.1.0/topics/max'
  max_count <- max(frequency_array)
'for loop: loopint over the entries of the frequency array'
  for (i in 0:(4^k -1)){
    'looking for the entries with the maximum count (checking at every position)'
    if (frequency_array[i+1] == max_count)
      'extract the pattern which corresponds to the highest frequeny
    by using the previously defined function numberToPattern'
      pattern <- numberToPattern(i, k)
      'add the pattern to the list frequent_patterns'
      frequent_patterns <- append(frequent_patterns, pattern)
  }
'return the most frequent patterns without duplicates
https://www.rdocumentation.org/packages/base/versions/3.6.0/topics/unique'
  return(unique(frequent_patterns))
}

fasterFrequentWords("AAGCAAAGGTGGG",2)


array <- computingFrequencies("AAGCAAAGGTGGG",2)

