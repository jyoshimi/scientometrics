SETTING UP A PROJECT

After cloning this directory we suggest you set up a project using RSTudio > New Project > Existing Director

R FILES

main.R entry point for running the main analysis scripts

functions.R contains the main network analysis functions

snippets.R contains snippets used to create specific one-off graphs or tables or analyses used in the paper

citation_matrix.R is used to parse and analyze the raw citations and produce an edge list.  No need to run this unless creating new data or using new name changes.

DOWNLOADING AND USING THE NAME CHANGE DOCUMENT

Save the name changes tab as data/processed/name_changes.csv.  

The document is used by citation_matrix.R






