# Input series of comma-separated strings
s = input('Please enter a series of comma-separated strings: ')
# Split on comma+space to create the list
l = s.split(', ')
odd = False
l2 = []
# Print the odd-indexed elements of list l
for idx, value in enumerate(l):
    if (idx%2 == 1):
        l2.append(value)
    

print(l2)
