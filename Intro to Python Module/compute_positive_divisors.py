# Example input() statement
number = int(input('Please enter a number: '))

divisor = 1
while divisor <= number:
    if number%divisor == 0:
        print(divisor)
    divisor+=1
# Remember to print out the results from lowest to highest
