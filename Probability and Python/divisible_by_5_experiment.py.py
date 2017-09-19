import random as random
print("======= Experiment for getting Probability of getting an even number =========")
## Probability of getting an even number ##
limit=1000001
ref_list = [32,49,55,30,56,28,50,40,40,45,3,25]
number_of_divisible_5 = 0
for itr in list(range(1,limit)):
	sample = random.sample(ref_list,1)[0]
	if sample % 5 == 0:
		number_of_divisible_5 = number_of_divisible_5 + 1

print("number of divisible by 5 = %d and percentage = %f" %(number_of_divisible_5,((number_of_divisible_5/(limit-1))*100)) + "%")
print("Ideally the percentage should be = %f" %(len(list(filter(lambda x: x % 5 == 0,ref_list)))/len(ref_list)))