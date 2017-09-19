import random as random


## Probability of getting 2 ##
print("======= Experiment for getting Probability of getting 2 =========")
limit=1000001
number_of_2s = 0
for itr in list(range(1,limit)):
	remainder=(random.randint(1,100000) % 7)
	if remainder == 2:
		number_of_2s = number_of_2s + 1

print("number of 2s = %d and percentage = %f" %(number_of_2s,((number_of_2s/(limit-1))*100)) + "%")

print("======= Experiment for getting Probability of getting 2 or 3 =========")
## Probability of getting 2 or 3 ##
limit=1000001
number_of_2s = 0
number_of_3s = 0
for itr in list(range(1,limit)):
	remainder=(random.randint(1,100000) % 7)
	if remainder == 2:
		number_of_2s = number_of_2s + 1
	elif remainder == 3:
		number_of_3s = number_of_3s + 1

print("number of 2s = %d and percentage = %f" %(number_of_2s,((number_of_2s/(limit-1))*100)) + "%")
print("number of 3s = %d and percentage = %f" %(number_of_3s,((number_of_2s/(limit-1))*100)) + "%")
print("number of 2s or 3s= %d and percentage = %f" %(number_of_2s+number_of_3s,(((number_of_2s+number_of_3s)/(limit-1))*100)) + "%")


print("======= Experiment for getting Probability of getting an even number =========")
## Probability of getting an even number ##
limit=1000001
number_of_even = 0
for itr in list(range(1,limit)):
	remainder=(random.randint(1,100000) % 7)
	if remainder % 2 == 0:
		number_of_even = number_of_even + 1

print("number of even = %d and percentage = %f" %(number_of_even,((number_of_even/(limit-1))*100)) + "%")
