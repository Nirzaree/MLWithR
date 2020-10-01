# You have been given a stack of documents that have already been processed and some that have not. Your task is to classify these documents into one of eight categories: [1,2,3,...8]. You look at the specifications on what a document must contain and are baffled by the jargon. However, you notice that you already have a large amount of documents which have already been correctly categorize (training data). You decide to use Machine Learning on this data in order to categorize the uncategorized documents.
# 
# Training Data
# 
# In order to figure out what category each document should fall under you will base it on the categories of the documents in the "trainingdata.txt" file. This file will be included with your program at runtime and will be named "trainingdata.txt".
# 
# The file is formatted as follows:
#   
#   The first line contains the number of lines that will follow.
# 
# Each following line will contain a number (1-8), which is the category number. The number will be followed by a space then some space seperated words which is the processed document.
# 
# Input
# 
# The first line in the input file will contain T the number of documents. T lines will follow each containing a series of space seperated words which represents the processed document.
# 
# Output
# 
# For each document output a number between 1-8 which you believe this document should be categorized as.
# 
# Sample Input
# 
# 3
# This is a document
# this is another document
# documents are seperated by newlines
# 
# Sample Output
# 
# 1
# 4
# 8
# 
# Scoring
# 
# Your score for this challenge will be 100* (#correctly categorized - #incorrectly categorized)/(T).

dtTrainingData <- readLines("/home/nirzareevadgama/Downloads/document-classification-testcases/trainingdata.txt")
nlines <- as.numeric(dtTrainingData[1])
