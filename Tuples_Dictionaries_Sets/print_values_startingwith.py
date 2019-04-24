my_dct = {'Texas': 'Austin', 'Indiana': 'Indianapolis', 'Illinois': 'Chicago',
          'New York': 'Albany', 'Iowa': 'Des Moines',
          'California': 'Sacramento', 'Utah': 'Salt Lake City',
          'Ohio': 'Columbus'}
for value in my_dct.values():
    if value.lower().startswith('s'):
        print(value)
