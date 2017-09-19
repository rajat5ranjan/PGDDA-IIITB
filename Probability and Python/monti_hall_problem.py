import random as random
import re
print("======= Experiment for getting Probability of Monty Hall Problem =========")
## Probability of getting an even number ##
limit=1000001
door_list_base=["Bad1","Good","Bad2"]
### Strategy = 0 : Do not switch ##
### Strategy = 1 : Always switch ##

for strategy in [0,1]:
	check_winning = 0
	for itr in list(range(1,limit)):
		## DO a deep copy of the door_list_base ##
		door_list = door_list_base[:]
		## Randomly select choice for this iteration ##
		selection1 = random.sample(door_list,1)[0]

		## If the strategy is to not change, then this would be the final selection, go no futher ##
		if strategy == 0:
			final_selection = selection1
		## If the strategy is to switch the selection, everytime, then following needs to be done ##
		else:
			## Remove the prior selection from the list, so that other 2 are left ##
			door_list.remove(selection1)
			
			## Get the Bad list after above removal, atleast 1 would be bad, because they are going to show 1 of the BAD ##
			bad_list = list(filter(lambda x : re.search("Bad*",x) != None,door_list))

			## <- If there is only 1 BAD here, then that will be added here, if there are 2, 1 will be selection
			selected_bad = random.sample(bad_list,1)[0]
			
			## Now if we remove the BAD, selection, there will be only 1 left, which will be the final selection of ours
			door_list.remove(selected_bad)
			final_selection = door_list[0]
		if final_selection == "Good":
			check_winning = check_winning + 1

	print("Total number of wins = %d, which percentage wise = %f" %(check_winning,(check_winning/(limit - 1))*100) + "%")
	if strategy == 0:
		print("strategy was to 'Do not switch', so ideally the winning probability = %f" %((1/3)*100) + "%")
	if strategy == 1:
		print("strategy was to 'Always switch', so ideally the winning probability = %f" %((2/3)*100) + "%")
