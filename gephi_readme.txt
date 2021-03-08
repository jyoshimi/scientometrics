 MAIN GRAPH 

- File > Open...
	- Open the .graphml file produced by the R analysis file. 
	- Use defaults to import.


- Edit labels (Not needed initially)
	- Data Laboratory Tab
	- Edit the display_label field to your liking. 
		- Don't use too many labels. They get unruly.
	- Copy display_label to label since the preview tab uses it
		- "Copy data to other column" > display_label > Select "label"

- Overview tab

	- Unselect Show Edges
	
	- Appearance > Nodes > Size > Unique > 5

	- Use display label as label (alternative to "Edit labels" for initial tuning)
		- Click attributes button at bottom-right of main graph window
		- Select display.label and unselect all other boxes

	- Appearance > Nodes > Color > Partition > "Community"
		- Adjust palette to your liking 
		- Palette > Generate... > Uncheck limit colors 
		
	- Layout  
		Choice 1: Layout > Force Atlas 2
		Choice 2: Layout > OpenOrd. Use defaults. Current graph uses seed of 4.

	- Label display
		- Click Show Node Labels button at bottom
		- Appearance > Label Size > Ranking > Degree or Strength 
			- Min Size 2 and a max size of 5, or larter
			- Can choose scaling in "spline".  Pablo likes logarithmic
			- (Note the size in overview gets much smaller in preview)
		- (Optional) Appearance > Label color > Partition > Community.
		- Layout > Label Adjust to get the labels not to overlap

- Preview tab
	- (For labels to show up here, "Edit labels" step above required. A useful alternative to export with no labels and then to edit the png directly. Svg too large to deal with.)
	- Nodes: can set border width to 0 and opacity > 70
	- Nodes > border width: 0
	- Nodes > opacity: 70
	- Edges > thickness: 0.1.
	- Edges > color: source
	- Edges > opacity: 10
	- Export > select png 
		- Options button to determine resolution via size of image


GROUP-BASED NETWORK

- Install plugins. Both must be downloaded first.
	Circular layout
		-  https://gephi.org/plugins/#/plugin/circularlayout 
	Groups by partition
		https://gephi.org/plugins/#/plugin/group-partition-gephi-plugin

- Stuff in main graph
	- First in the large network, copy community name to label
	- Be sure all communities you want are colored how you want them. Only communities with colors are grouped and sent over.

- Tools > Generate groups by partition > Create new workspace

- Get rid of self loops using the filter in overview, then create a new network

- Data Laboratory Tab
	- Manully update labels
	- Remove unwanted nodes
	- Copy edge weights to edge label

- Overview Tab
	- Appearance > Nodes > Size > Ranking > Size. Min 50 / Max 100
	- Appearance > Nodes > Text > Size > Unique > .5
	- Appearance > Edges > Text > Ranking > Weight. Min 5 / Max 10
	- Layout > circular layout
		- Fixed diameter: checked
		- Diamter: 1000

- Preview tab 
	- Node labels > Show labels: checked
	- Node labels > Font size: 60, Bold
	- Node labels > Proportional size: unchecked
	- Edges > thickness: 0.1
	- Edges > Rescale weight: checked
	- Edges > Min rescaled weight: 10
	- Edges > Max rescaled weight: 500
	- Edges > Edge color: Source
	- Edges > Opacity: 40
	- Edge Labels > Show labels: checked
	- Edge Labels > Font size: 36

- Save as SVG and fine tune in illustrator

TO MAKE A SPARSER NETWORK

- Go back to Overview. We'll do some filtering to produce a sparser network. 
- Go to Filter > Topology > In-degree Range and drag the filter to the "Query" part of the window. Define the minimum in-degree treshold. A good value is 2.
- Add a second filter, this time for edges. Filter > Attributes > Intra Edges > community, and drag it to the "subfilter" part of the queries window. This (i think???) keeps only between community edges.
- Create a new workspace starting from this filtering. This button is right next to the "reset" button in the "filters" part of the window. This will open a new workspace that has only the filtered nodes and edges. To reset the palette to the new reduced number of communities, you can repeat steps 4, 9, 10 and 11.
- Save this new graph. I find that this sparser network looks better with straight edges, so you could use the "Default Straight" preset.
