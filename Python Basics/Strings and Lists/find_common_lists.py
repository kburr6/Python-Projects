l1 = []
l2 = []
for idx in range(7):
    i = input('Enter a number followed by enter for l1: ')
    l1.append(i)

for idx in range(7):
    i = input('Enter a number followed by enter for l2: ')
    l2.append(i)

common = [ idx for idx in l1 if idx in l2]
common.sort()
# Print the result
print(common)
