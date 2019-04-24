# Input series of comma-separated strings
s = input('Please enter a series of comma-separated strings: ')
# Split on comma+space to create the list
l = s.split(', ')

l2 = l.copy()
l2.pop()
l2.reverse()
l = l + l2
print(l)
# Convert list l into a palindrome and then print the list
