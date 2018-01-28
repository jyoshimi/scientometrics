# script takes a record from WoS that includes reference information and extracts for each paper the author, title,
# journal, and publication year, along with a list of the cited references.

import re  # use regex to remove punctuation


output = open('catalog.txt', 'w')

article_number = 1  # numerical ID of articles

file_lines = open("citationRecord.txt", 'r').read().split('\n')  # a list of every line in the file.
article_dictionary = {'CR': ''}  # creates dictionary for each article. Rest of the loop fills that dictionary with the
# data for each article.

for line in file_lines:  # loop in list of every line of every file

    if not line:  # checks for empty strings at the end of each article
        article_dictionary['CR'] = article_dictionary['CR'][:-3]  # remove the ' / ' at the end

        output.writelines(str(article_number) + '\t' + article_dictionary['AU'] + '\t' + article_dictionary['TI'] + '\t'
                          + article_dictionary['SO'] + '\t' + article_dictionary['PY'] + '\t' + article_dictionary['CR']
                          + '\n')
        # outputs tab-separated file with article number, author, title, journal and PY, with CITATIONS at\
        # the end.
        article_dictionary = {'AU': '0', 'TI': '0', 'SO': '0', 'PY': '0', 'CR': ''}  # wipes identifiers
        article_number = article_number + 1  # plus one for article counter
        continue  # continues for next loop

    item_content = line[3:]  # content of the line starts after 3 spaces, be it code or empty space when continuation

    if line[0] != ' ':  # empty at first space means it's a continuation of the previous item
        item_code = line[0:2]  # then set the item type to the first 2 letters of the string e.g. "AU". this is not
        # wiped out when line is continued.

    if item_code == 'AU' and line[0] == ' ':  # if filling authors and this is a continuation of a first author
        article_dictionary['AU'] = article_dictionary['AU'] + '; ' + item_content  # adds secondary authors e.g.
        # husserl; heidegger.

    elif item_code == 'AU' and line[0] != ' ':  # if filling author and it's not a continuation
        article_dictionary['AU'] = item_content  # adds primary author

    elif item_code == 'TI' and line[0] == ' ':  # checks for title continuations if it's more than one line
        article_dictionary['TI'] = article_dictionary['TI'] + ' ' + item_content  # adds it to first part of title

    elif item_code == 'TI' and line[0] != ' ':  # checks for first part of title
        article_dictionary['TI'] = item_content  # adds title to dictionary

    elif item_code == 'SO' and line[0] == ' ':  # checks for continuations of journal names
        article_dictionary['SO'] = article_dictionary['SO'] + ' ' + item_content

    elif item_code == 'SO' and line[0] != ' ':  # checks for first parts of journal names
        article_dictionary['SO'] = item_content  # adds journal to dictionary

    elif item_code == 'PY':  # checks for publication year. year is never continued from previous line.
        article_dictionary['PY'] = item_content  # adds year to dictionary

    elif item_code == 'CR':  # checks if it's citation record

        record_items = item_content.split(', ')

        # clean author name#
        record_items[0] = re.sub(r'[^\w\s]', '', record_items[0])  # removes punctuation and symbols from AUTHOR
        split_name = record_items[0].split(' ')  # split it by spaces. goal is to leave everything as LAST NAME INITIAL
        record_items[0] = split_name[0]  # add last name
        if len(split_name) > 1:  # if there's any more information
            record_items[0] += ' ' + split_name[1][0]  # first character of first name
        record_items[0] = record_items[0].lower()  # lower case names for better comparison

        # add items to a dictionary of the citation #

        if len(record_items) >= 3:  # if the record is complete ASSUMING: if it doesn't have year/journal,
            # it wont have DOI
            these_citations = {'AU': record_items[0], 'PY': record_items[1], 'JO': record_items[2], 'DOI': ''}
            # this line of the citations
            for item in record_items:  # look for DOI in a very inelegant way.
                if item[0:3] == 'DOI':
                    split_item = item.split(' ')
                    # split it on the spaces, just because sometimes there are two "DOI" words.
                    for sub_item in split_item:
                        if sub_item != 'DOI':
                            these_citations['DOI'] = sub_item

        elif len(record_items) == 2:  # TODO: test. only missing year??? is there any with year but not journal?
            these_citations = these_citations = {'AU': record_items[0], 'PY': '', 'JO': record_items[1], 'DOI': ''}

        elif len(record_items) == 1:  # TODO: assuming it'll only have the author name. WRONG FOR SOME. LOOK BY HAND. NOT MANY.
            these_citations = these_citations = {'AU': record_items[0], 'PY': '', 'JO': '', 'DOI': ''}

        # transform dictionary of the citation to a string and add it to the article dictionary #

        for thing in these_citations.values(): # construct string from this point: loop through the dictionary of the citation
            if thing == '':  # break when you find a
                continue
            article_dictionary['CR'] += thing
            article_dictionary['CR'] += '; '  # each info of citation is separated by comma

        article_dictionary['CR'] = article_dictionary['CR'][:-2]  # remove the final ;
        article_dictionary['CR'] += ' / '  # each citation is separated by semicolon
