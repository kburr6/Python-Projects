import numpy as np

def elements_twice_min(arr):
    """
    Return all elements of array arr that are greater than or equal to 2 times
    the minimum element of arr.

    Parameters
    ----------
    arr: NumPy array (n, m)

    Returns
    -------
    NumPy Array, a vector of size between: 0 and (n * m) - 1
    """
    return arr[arr >= 2*np.amin(arr)]

print(elements_twice_min([[3,7,8],[3,4,5]]))
