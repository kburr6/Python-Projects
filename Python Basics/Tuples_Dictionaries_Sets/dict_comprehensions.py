words = input('Enter words to transform to uppercase here. Separate them with spaces:')
s = words.split(' ')
result = {value: value.upper() for idx, value in enumerate(s) }
print(result)
