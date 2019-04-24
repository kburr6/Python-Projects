nums = input('''Please enter a series of numbers separated by commas. \n Hit enter when you are done. ''')
s = nums.split(', ')
for i in range(len(s)):
    s[i] = int(s[i])
B = s[::2]
C = s[1::2]
result = list(zip(B,C))
print(result)
