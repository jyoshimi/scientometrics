GET DATA FROM WEB OF SCIENCE


	http://apps.webofknowledge.com/WOS_GeneralSearch_input.do?product=WOS&search_mode=GeneralSearch&SID=6Ab49AoDqVSMbAU6NEu&preferencesSaved=

	Search: 
		Query: Phenomenology [Topic]
		Timespan: 1970-2017
		More settings: only use core collection

	Filter:  	(Select -> Refine)

		Categories: Philosophy
		Document Type: Article (Can later try: Book Review, Proceedings Paper, Editorial Material; Book chapter might not have citations)

	Summary: You searched for: TOPIC: (phenomenology) Refined by: WEB OF SCIENCE CATEGORIES: ( PHILOSOPHY ) AND DOCUMENT TYPES: ( ARTICLE ) Timespan: 1970-2017. Indexes: SCI-EXPANDED, SSCI, A&HCI, CPCI-S, CPCI-SSH, BKCI-S, BKCI-SSH, ESCI.

	Save:

		Save to other file formats
			500 at a time
			Full record and Cited References
			Format:plain text

RUN R SCRIPT

	Creates the citation matrix (co-occurrence matrix)

	Does some cleanup (see code comments).  May do more of this in python.

MAYBE PYTHON

GEPHI
	- Use the regular open command to get the csv
	- Graph type directed
	---
	- Turn off edges
	- Set Nodes > size > ranking > in-degree
	- Turn on labels, and set size thing to "node size"
	-- 
	- Layout
		- I like open ord for clustering
		- Do label adjust to get text readable.
	---
	- Find communities
		- Statistics > run  modularity
		- nodes > partition > modules


	---- order by centrality ---





