#words = input('Enter words to transform to lowercase here. Separate them with spaces:')
words = 'RADIO aStRoNoMy Pulsar'
result = tuple((w,w.lower()) for w in words.split())
print(result)
