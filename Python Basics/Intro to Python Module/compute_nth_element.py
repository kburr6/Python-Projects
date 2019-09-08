# Example input() statement
number = int(input('Please enter a value for N: '))

result = 0
i = 0
while i <= number:
    if i == 0:
        result += 1
    else:
        result = 2*result+1
    i+=1

# Use print() to print the result, like this:
print(result)
