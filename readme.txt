GETTING THE DATA

See citation_matrix_WOS.r.  Has lots of comments.

Plans to do further cleanup in here. Pablo reccomends R for data science.
	https://r4ds.had.co.nz/

THE DATA

Edge lists
	complete_edge_list.csv		the main data
	small_edge_list.csv			only authors cited more than 5 times
								also removes authors who cited those people
	tiny_edge					very small list for testing

matrix, colnames, etc. are intermediate files.


ANALZYING THE DATA

Main analysis script is network_R.R.  Uses igraph for community detection and tidygraph to plot.

Efforts in python, ipynb, and gephi also included, but not used