import random as random


limit=1000001
yellow_marble = 0
ref_list = ["Y","Y","Y","R","R","G","G","B"]
for itr in list(range(1,limit)):
	if (random.sample(ref_list,1))[0] == "Y":
		yellow_marble = yellow_marble + 1
print("Number of yellow marbles picked = %d and percentage = %f" %(yellow_marble,(yellow_marble/(limit-1))*100) + "%")
print("Ideally speaking it would be = %f" %(len(list(filter(lambda x : x == "Y",ref_list)))/len(ref_list)))
