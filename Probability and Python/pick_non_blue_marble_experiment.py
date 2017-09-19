import random as random


limit=1000001
non_blue_marble = 0
ref_list = ["R","R","R","R","R","R","R","R","R","B","B","G","G","G"]
for itr in list(range(1,limit)):
	if (random.sample(ref_list,1))[0] != "B":
		non_blue_marble = non_blue_marble + 1
print("Number of non blue marbles picked = %d and percentage = %f" %(non_blue_marble,(non_blue_marble/(limit-1))*100) + "%")
print("Ideally speaking it would be = %f" %(len(list(filter(lambda x : x != "B",ref_list)))/len(ref_list)))
