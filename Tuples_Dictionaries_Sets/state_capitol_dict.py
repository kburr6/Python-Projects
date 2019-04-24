state_dictionary = {'Colorado': 'Denver', 'Alaska': 'Juneau',
                    'California': 'Sacramento', 'Georgia': 'Atlanta',
                    'Kansas': 'Topeka', 'Nebraska': 'Lincoln',
                    'Oregon': 'Salem', 'Texas': 'Austin', 'New York': 'Albany'}
state = input('Enter a state name: ')
state = state.title()

if state in state_dictionary:
    print(state_dictionary[state])
else:
    print('Capital unknown')
