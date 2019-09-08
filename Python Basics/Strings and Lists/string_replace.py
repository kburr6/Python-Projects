# Example input() statement
s = input('Please enter a string: ')

if s.endswith('!!!'):
    s = s[:-3] + '.'

# Print the string when you are done
print(s)
