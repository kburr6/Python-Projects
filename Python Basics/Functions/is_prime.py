def is_prime(n):
    '''
    Return True if the input is prime, False otherwise
    Parameters
    ----------
    n: {int} input integer

    Returns
    -------
    is_prime: {bool} whether n is prime
    '''

    prime = True
    for i in range(1,n+1):
        if ((n % i) == 0) and (i != 1) and (i != n):
            prime = False
            break

    return prime
