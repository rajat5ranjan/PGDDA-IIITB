import random as random



limit=1000001
head_count = 0
tail_count = 0
for itr in list(range(1,limit)):
	random_value=random.randint(1,100)
	if random_value % 2 == 0 :
		head_count = head_count + 1
	else:
		tail_count = tail_count + 1

print("Head count = %d and percentage = %f" %(head_count,((head_count/(limit-1))*100)) + "%")
print("Tail count = %d and percentage = %f" %(tail_count,((tail_count/(limit-1))*100)) + "%")