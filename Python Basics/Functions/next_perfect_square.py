from  math import sqrt

def is_perfect_square(number):
    if sqrt(number).is_integer():
        return True
    else:
        return False


def next_perfect_square(number):
    '''
    Returns the next perfect square of the input number, if the input number
    is not a perfect square, returns -1.
    Ex:
    next_perfect_square(10)
    >>> -1
    next_perfect_square(9)
    >>> 16
    next_perfect_square(25)
    >>> 36
    next_perfect_square(37)
    >>> -1

    Parameters
    ----------
    number: {int}

    Returns
    -------
    next_perfect: {int} the next perfect square, or -1 if number is not a
    perfect square
    '''
    if is_perfect_square(number):
        return int((sqrt(number)+1)**2)
    else:
        return -1
