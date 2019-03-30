# Example input() statement
number1 = int(input('Please enter a number: '))
number2 = int(input('Please enter a number: '))
eval = 1
gcd = 0
while (eval <= number1) and (eval <= number2):
    if (number1%eval == 0) and (number2%eval == 0):
        gcd = eval
    eval+=1
# Use print() to print the result, like this:
print(gcd)
