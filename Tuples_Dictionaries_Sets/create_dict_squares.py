nums = input('''Please enter a series of numbers separated by '-'. \n Hit enter when you are done. ''')
l = nums.split('-')
for i in range(len(l)):
    l[i] = int(l[i])
result = {idx:idx**2 for idx in l}
print(result)
