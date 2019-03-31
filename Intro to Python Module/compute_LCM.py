# Example input() statement
number1 = int(input('Please enter a number: '))
number2 = int(input('Please enter a number: '))

numerator = abs(number1*number2)
eval = 1
gcd = 0
while (eval <= number1) and (eval <= number2):
    if (number1%eval == 0) and (number2%eval == 0):
        gcd = eval
    eval+=1
denominator = gcd
result = int(numerator/denominator)
# Use print() to print the result, like this:
print(result)
