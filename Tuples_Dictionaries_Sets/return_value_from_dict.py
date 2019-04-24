# Example input() statement
state = input('Please enter a State: ')

my_dct = {'Texas': 'Austin', 'Indiana': 'Indianapolis', 'Illinois': 'Chicago', 'New York': 'Albany'}

print(my_dct.get(state, 'Capital not found!'))
