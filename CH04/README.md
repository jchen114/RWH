# Chapter 4 Exercises

## IO.hs

safeHead' is an implementation of head such that it returns a Maybe data type.

printFirstWordOfLine is a function which takes a line and prints the first word of it

interactWith will apply the function passed in to the inputFile and write it to an outputFile

main function will receive arguments from the command prompt specifying the input and  output files and perform a specified function on the input file and write it to an output file

## Exercises.hs

splitWith will split a list into a list of lists based on a function passed in

transpose' takes a string with end of line characters and transposes them with a end of line character separating each transposed row

asInt_fold will turn a string into an integer. It works with strings with negative signs as well

asInt_either checks if the string is valid or not by checking the first element of the string and returns an Either data type

concat' is concat using a foldr

takeWhile' is takeWhile using explicit recursion

takeWhile'' is takeWhile using a foldr

groupBy' is groupBy using a foldl
