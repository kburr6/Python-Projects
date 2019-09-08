def factorial(n):
    '''
    Returns the factorial of the input integer:
        n * (n - 1) * (n - 2) * ... * 2 * 1
    Parameters
    ----------
    n: {int} number to compute factorial of (must be greater than 0)

    Returns
    -------
    n!: {int} factorial of n

    '''

    factorial = 1

    # check if the number is negative, positive or zero
    if n < 0:
       print("Sorry, factorial does not exist for negative numbers")
    elif n == 0:
       print("The factorial of 0 is 1")
    else:
       for i in range(1,n + 1):
           factorial = factorial*i

    return factorial
