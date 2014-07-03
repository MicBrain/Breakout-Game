listed_lines = []
dataset = []
words = {}

lines = [line.strip() for line in open('Appache_Log_samples.txt')] # Reads the file line by line
for line in lines:
	listed_lines.append(line.split())

for line in listed_lines:
	for word in line:
		dataset.append(word) 

""" @@@ Analyzing Data @@@ """

def frequency(dictionary, item):
	"""Frequency of elements in dictionary."""
	counter = 0
	for value in dictionary.values():
		if value == item:
			counter += 1
	return counter

def infrequency(dictionary, item):
	counter = 0
	for value in dictionary.values():
		if value <= item:
			counter += 1
	return counter

def length():
	return len(dataset)

def item_count(dictionary):
	"""The number of unique items in the dictionary"""
	count = 0
	for _ in dictionary.values():
		count += 1
	return count

# Printing the results in file.txt
try:
	f = open("data_file.txt", "w")
	try:
		f.writelines(str(listed_lines[0]))
	finally:
		f.close()
except IOError:
	pass


def dense_minimum_regions_1():
	M = find_minimum_region()
	final_list = []
	Candidates = minimum_regions()[1]
	final_region = []
	for index, item in enumerate(region_1()):
		item.remove(item[len(item) - 1])
		line_data = [item, index]
		final_region.append(line_data)
	# 1st pass
	for data_index in range(0, len(final_region) - 1):
		line_x = final_region[data_index][0]
		index_x = final_region[data_index][1]
		temporary_list = []
		boolean = False
		if line_x != 0:
			for item_index in range(data_index + 1, len(final_region) - 1):
				calculator = 0
				line_y = final_region[item_index][0]
				index_y = final_region[item_index][1]
				for item_x in line_x:
					for item_y in line_y:
						if item_x[1] == item_y[1] and item_x[0] == item_y[0]:
							calculator += 1
				if calculator != M:
					index = index_y
					temporary_list.append(index)
					boolean = True
					final_region[index][0] = 0
					print(final_region[index])
					break
			if boolean == True:
				index =  index_x
				temporary_list.append(index)
				final_list.append(temporary_list)
				final_region[index][0] = 0
				print(final_region[index_x])
				break
			# return final_list


