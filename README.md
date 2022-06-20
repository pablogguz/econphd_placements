# Placement outcomes for PhD graduates in Economics

I construct a novel dataset with individual-level data on PhD placement outcomes over the period 2012-2022 for a total of 24 economics departments
worldwide. Specifically, data is extracted directly from the relevant departmental websites using a mixture of both code and no-code web scraping tools. 

Placements are then sorted into 8 different categories using a text classification algorithm, namely tenure-track (academic) positions, 
post-docs, central banks, international organizations, government, think-tanks, private sector, and other (academic) positions.

This GitHub repository hosts the codes for replicating the analysis. A one-click code for running the full sequence of scripts can be found in the dofile `_master.do`.

Notes:
* As the way in which placements are reported is heterogeneous across departments, the text classification algorithm is also department-specific. For instance, 
some departments distinctively tag their tenure-track placements with the relevant job title (i.e., *Assistant Professor*), whereas in other cases tenure-track placements must be identified based on the abscence of tags that identify non-tenure-track academic placements (e.g., post-docs).
* Chicago and Northwestern already report their placements following a similar classification to ours. In these two cases, placements are directly sorted into our categories by hand without using the text classification algorithm.


