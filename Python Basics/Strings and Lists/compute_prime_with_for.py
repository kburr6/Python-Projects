# Example input() statement
number = int(input('Please enter a number: '))
is_prime = False
# Here are the print statements that you should use.
for i in range(number):
    if number%(i+1) == 0:
        if (i+1) in (1, number):
            prime = True
        else:
            prime = False
            break

if prime:
    print('The number you inputted is a prime number.')
else:
    print('The number you inputted is not a prime number.')
