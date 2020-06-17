SETTING UP A PROJECT

After cloning this directory we suggest you set up a project using RSTudio > New Project > Existing Director

DIRECTORY CONTENTS

Edge lists
	complete_edge_list.csv		the main data
	small_edge_list.csv			only authors cited more than 5 times
								also removes authors who cited those people
	tiny_edge					very small list for testing

matrix, colnames, etc. are intermediate files.

GETTING THE DATA

For information how the code is downloaded and cleaned up, see the code comments in citation_matrix_WOS.r.  Also the discussion of getting the data from web of science.

ANALZYING THE DATA

Main analysis script is network_R.R.  Uses igraph for community detection and tidygraph to plot.

Efforts in python, ipynb, and gephi also included, but not used

GETTING DATA FROM WEB OF SCIENCE

http://apps.webofknowledge.com/

Basic Search: 
	Topic: "Phenomenology" 
	Timespan: 1970-2017
	More settings: only use core collection
	Then press "search"

Filter:  (Select -> Refine)

	Categories: Philosophy
	Document Type: Article (Can later try: Book Review, Proceedings Paper, Editorial Material; Book chapter might not have citations)

Summary: You searched for: TOPIC: (phenomenology) 
	Refined by:
	WEB OF SCIENCE CATEGORIES: ( PHILOSOPHY ) AND DOCUMENT TYPES: ( ARTICLE ) 
	Timespan: 1970-2017. 
	Indexes: SCI-EXPANDED, SSCI, A&HCI, CPCI-S, CPCI-SSH, BKCI-S, BKCI-SSH, ESCI.

Save:

	Export 	> Other file formats
		500 at a time
		Full record and Cited References
		Format:plain text

NOTES ON THE WOS SEARCH

Raw output is over 40K.  So we filtered by philosophy, since "phenomenology" is used in other fields in other senses.  Also filtred by document type article, to filter out reviews, editorials, book chapters in books.  These other sources are not as well indexed.

Articles in all languages were searched, even though the actual search phrase was "Phenomenology" (case insensitive) in the Topics field.

Topic is based on 1) The Title of the article, review,  etc. 2) The Abstract 3) The Keywords and Keywords Plus fields.  For indexing reasons all entries in WOS have at least the abstract in english.  See

	https://clarivate.libguides.com/woscc/searchtips 

	Search for "English" in this web page:
	https://blog.scholasticahq.com/post/how-to-get-journal-indexed-web-of-science-core-collection/

HOW NAME CHANGE DOCUMENT WAS PRODUCED

all.names <- c(colnames(citing.matrix.raw), citing.matrix.raw$first.author) %>% 
  unique() %>% 
  enframe() %>% 
  mutate(short.name = shorten.name(value)) %>% 
  rename(old.name = value) %>% 
  select(-name) %>% 
  arrange(old.name) %>% 
  filter(str_detect(old.name, "[[:digit:]]", negate = TRUE))
write_csv(all.names, "current_names.csv")