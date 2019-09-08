# Example input() statement
s = input('Please enter a string: ')

if len(s) < 6:
    print('String too short.')
else:
    print(s[5])

# Example input() statement
s = input('Please enter a string: ')

if len(s) < 5:
# Use this error message if the string is too short
    print('String too short.')
else:
    print(s[2:5])

# Example input() statement
s = input('Please enter a string: ')

if len(s) < 2:
# Use this error message if the string is too short
    print('String too short.')
else:
    print(s[1::2])

# Example input() statement
s = input('Please enter a string: ')

if len(s) < 2:
# Print the string when you are done
    print('String too short.')
else:
    print(s[len(s)-2])

# Example input() statement
s = input('Please enter a string: ')

index = 0
for i in s:
    if index == 0:
        print(i.upper())
    if index == 0:
        index+=1
    else:
        index-=1
