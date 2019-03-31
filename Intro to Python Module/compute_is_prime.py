# Example input() statement
number = int(input('Please enter a number: '))
counter = 1
prime = True
while prime and counter<=number:
    if number%counter == 0:
        if counter in (1,number):
            prime = True
        else:
            prime = False
    counter+=1

# Here are the print statements that you should use.
if prime:
    print('The number you inputted is a prime number.')
else:
    print('The number you inputted is not a prime number.')
