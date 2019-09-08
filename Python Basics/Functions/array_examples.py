def count_pair_sums(arr, number=0):
    '''
    Given an array, find the count of how many pairs of numbers in the array sum
    to the input number

    Parameters
    ----------
    arr: {list} list of integers (positive and negative)
    number: number to see if pairs sum to (default 0)

    Returns
    -------
    {int} the number of pairs found that sum to given number
    '''
    count = 0 #number of pairs found
    length = len(arr)-1 #iterator for inner loop
    i = 0       #outer loop iterator
    j = 1       # inner loop iterator

    while i < length:
        print('i=',i)
        while j <= length:
            print('j=',j)
            if (arr[i] + arr[j]) == number:
                count+=1
            j+=1
        i+=1
        j = i+1

    return count
