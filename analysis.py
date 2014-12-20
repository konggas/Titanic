#https://www.kaggle.com/c/titanic-gettingStarted/details/getting-started-with-python

# The first thing to do is to import the relevant packages
# that I will need for my script, 
# these include the Numpy (for maths and arrays)
# and csv for reading and writing csv files
# If i want to use something from this I need to call 
# csv.[function] or np.[function] first

import csv as csv
import numpy as np
import platform
platform.system()

# Open up the csv file in to a Python object
if platform.system() == 'Darwin':
    csv_file_object = csv.reader(open('/Users/konggas/titanic/Titanic/train.csv', 'rb'))
else:
    csv_file_object = csv.reader(open('C:/Users/sekong/titanic/train.csv', 'rb'))
    
header = csv_file_object.next() # The next() command just skips the 
# first line which is a header
data=[] # Create a variable called 'data'.
for row in csv_file_object: # Run through each row in the csv file,
    data.append(row) # adding each row to the data variable
data = np.array(data) # Then convert from a list to an array
# Be aware that each item is currently
# a string in this format

# The size() function counts how many elements are in
# in the array and sum() (as you would expects) sums up
# the elements in the array.

number_passengers = np.size(data[0::,1].astype(np.float))
number_survived = np.sum(data[0::,1].astype(np.float))
proportion_survivors = number_survived / number_passengers

women_only_stats = data[0::,4] == 'female' # This finds where all 
                                           # the elements in the gender
                                           # column that equals female
men_only_stats = data[0::,4] != 'female'   # This finds where all the 
                                           # elements do not equal 
                                           # female (i.e. male)

