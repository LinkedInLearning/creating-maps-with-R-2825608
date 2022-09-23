# Creating Maps with R
This is the repository for the LinkedIn Learning course Creating Maps with R. The full course is available from [LinkedIn Learning][lil-course-url].

![Creating Maps with R][lil-thumbnail-url] 

If you need to learn more about creating maps with R, this beginner-friendly course introduces an end-to-end mapping workflow and shows you how to import your data directly from Excel to create both static and interactive maps. Instructor Charlie Joey Hadley explains mapping fundamentals, like geo markers, scatter plots, hexbin maps, cartograms, and more. Charlie walks you through processing GIS data from Excel and working with GIS data formats such as raster, vector, sf, and sp. She demonstrates how to create, label, and transform static maps with ggplot2, then dives into building interactive, mobile-ready maps using Leaflet, an HTML widget package for creating interactive maps with R. Plus, Charlie covers base maps and tiles, projections, the Coordinate Reference System (CRS), and more.

## Instructions
This repository has branches for each of the videos in the course. You can use the branch pop up menu in github to switch to a specific branch and take a look at the course at that stage, or you can add `/tree/BRANCH_NAME` to the URL to go to the branch you want to access.

## Branches
The branches are structured to correspond to the videos in the course. The naming convention is `CHAPTER#_MOVIE#`. As an example, the branch named `02_03` corresponds to the second chapter and the third video in that chapter. 
Some branches will have a beginning and an end state. These are marked with the letters `b` for "beginning" and `e` for "end". The `b` branch contains the code as it is at the beginning of the movie. The `e` branch contains the code as it is at the end of the movie. The `main` branch holds the final state of the code when in the course.

When switching from one exercise files branch to the next after making changes to the files, you may get a message like this:

    error: Your local changes to the following files would be overwritten by checkout:        [files]
    Please commit your changes or stash them before you switch branches.
    Aborting

To resolve this issue:
	
    Add changes to git using this command: git add .
	Commit changes using this command: git commit -m "some message"


### Instructor

Charlie Joey Hadley 
                            
Data Visualization Specialist

                            

Check out my other courses on [LinkedIn Learning](https://www.linkedin.com/learning/instructors/charlie-joey-hadley).

[lil-course-url]: https://www.linkedin.com/learning/creating-maps-with-r
[lil-thumbnail-url]: https://cdn.lynda.com/course/2825608/2825608-1663712344751-16x9.jpg
