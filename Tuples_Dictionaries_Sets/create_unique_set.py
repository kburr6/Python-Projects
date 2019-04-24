words = input('Enter words separated by commas: ')

word_list = words.split(', ')

unique_word_list = []

for idx in range(len(word_list)):
    if word_list[idx] not in unique_word_list:
        unique_word_list.append(word_list[idx])

#unique_word_list = str(unique_word_list)
#unique_word_list = unique_word_list.strip('[')
#unique_word_list = unique_word_list.strip(']')
#unique_word_list = unique_word_list.rstrip("'")
print(", ".join(unique_word_list))
