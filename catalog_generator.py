# this script takes the various .txt of abstracts and their respective theories specified in file_key.txt and
# produces one big catalog of papers, assigning each an ID. Outputs it to 'catalog.txt'#


cat_files = open('file_key.txt', 'r').read().split('\n')  # opens files, transforms into strings, splits new lines
output = open('catalog.txt', 'w')

article_number = 1  # numerical ID of abstracts

for fl in cat_files:  # loop in stringed filekey
    file_theory = fl.split('\t')[0]  # theory (e.g. "computational")
    file_name = fl.split('\t')[1]  # file name (e.g. "distributed cognition, quotes.txt")
    file_lines = open(file_name, 'r').read().split('\n')  # a list of every line in the file.
    article_dictionary = {}  # creates dictionary for each article. Rest of the loop fills that dictionary with the
    # data for each article.

    for line in file_lines:  # loop in list of every line of every file
        if not line:  # checks for empty strings as document end marker
            if 'AB' not in article_dictionary:  # check if there's an abstract key in the dictionary. some files
                # don't and bug the program.
                article_dictionary['AB'] = ''
            output.writelines(
                [str(article_number), '\t', file_theory, '\t', article_dictionary['PY'], '\t', article_dictionary['AU'],
                 '\t', article_dictionary['TI'], '\t', article_dictionary['SO'], '\t', article_dictionary['AB'],
                 '\n'])  # outputs tab-separated file with article identifier, field, year, author, title,
            # journal and abstracts with new line
            article_dictionary = {'AU': '0', 'TI': '0', 'AB': '0', 'SO': '0',
                                  'PY': '0'}  # wipes the identifiers to prevent key errors
            article_number = article_number + 1  # counter for IDs
            continue  # continues for next loop

        item_content = line[3:]  # line after code or empty space. author names, titles, etc.

        if line[0] != ' ':  # if it is not a continuation of a previous item type
            item_code = line[
                        0:2]  # then set the item type to the first 2 letters of the string. this stays over empty
            # lines and solves the secondary author problems.

        if item_code == 'AU' and line[0] == ' ':  # checks for secondary authors
            article_dictionary['AU'] = article_dictionary['AU'] + '; ' + item_content  # adds secondary authors

        elif item_code == 'AU' and line[0] != ' ':  # checks for primary author and appends author
            article_dictionary['AU'] = item_content  # adds primary author to dictionary

        elif item_code == 'TI' and line[0] == ' ':  # checks for title continuations if it's more than one line
            article_dictionary['TI'] = article_dictionary['TI'] + ' ' + item_content  # adds it to first part of title

        elif item_code == 'TI' and line[0] != ' ':  # checks for first part of title
            article_dictionary['TI'] = item_content  # adds title to dictionary

        elif item_code == 'SO' and line[0] == ' ':  # checks for continuations of journal names
            article_dictionary['SO'] = article_dictionary['SO'] + ' ' + item_content

        elif item_code == 'SO' and line[0] != ' ':  # checks for firt parts of journal names
            article_dictionary['SO'] = item_content  # adds journal to dictionary

        elif item_code == 'AB' and line[0] == ' ':
            article_dictionary['AB'] = article_dictionary['AB'] + ' ' + item_content

        elif item_code == 'AB' and line[0] != ' ':  # checks for abstract
            article_dictionary['AB'] = item_content  # adds abstract to AB key in dictionary

        elif item_code == 'PY':  # checks for publication year
            article_dictionary['PY'] = item_content  # adds year to dictionary
