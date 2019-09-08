def calculate_series(n):
    '''
    calculate the nth value in the series:
    a_i = 2 * a_(i-1) + 1
    and where the initial value of the series, a_0, is initialized to 1.

    Parameters
    ----------
    n: {int}

    Returns
    -------
    a_n: {int} the nth series value

    '''
    a=1
    for a in range(n):
        a = 2 * a + 1
    return a
