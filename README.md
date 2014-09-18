Schelling's Segregation Model
=====================
Thomas Schelling's Segregation Model shows that very small preferences for one's neighbors to be of a certain type (e.g., race) can lead to widespread, or even total, segregation. To demonstrate, Schelling placed pennies and nickels on a grid and moved them around if they were "unsatisfied" by their neighbors.

I've simulated his model here, where the Schelling function takes:

* The total population of up to three "types" of people (red, green, and blue)
* Each type's preferred ratio of neighbors of their same type to a total number of neighbors

If an individual's preferred ratio is not met nor exceeded, s/he is given another random housing location. This continues until everyone is satisfied.


Schelling's original 1971 paper can be found here: http://www.stat.berkeley.edu/~aldous/157/Papers/Schelling_Seg_Models.pdf