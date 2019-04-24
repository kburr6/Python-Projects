numbers1 = input('Enter numbers separated by commas: ')
numbers2 = input('Enter numbers separated by commas: ')

l1 = numbers1.split(',')
l2 = numbers2.split(',')

for idx in range(len(l1)):
    l1[idx] = int(l1[idx])
l1.sort()

for idx in range(len(l2)):
    l2[idx] = int(l2[idx])
l2.sort()

s1 = set(l1)
s2 = set(l2)

result = s1.intersection(s2)

if result == set():
    print('')
else:
    result = str(sorted((s1.intersection(s2))))
    result = result.strip('[]')
    result = result.strip(']')
    print(result)
