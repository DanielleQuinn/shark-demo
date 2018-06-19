###########################################
# John Muir Elementary School Shark Study #
###########################################

# How many sharks did we catch?
nrow(data)

# How many different kind of sharks did we catch?
n_distinct(data$species)

# How many of each of our study species did we catch?
table(studydata$species)

# Let's put our data on a graph!
bargraph_sharks()

# We are interested in learning about where each species lives
# Let's see where each of our shark species can be found

# Start by drawing the ocean
draw_ocean()

# Add the ocean floor so we can see different depths of water
add_floor()

# Now add the sharks!
add_sharks()

# What do we know about migration?
# Let's see if each species migrates into deeper or shallower water during different seasons
# One species at a time
by_season("Greenland Shark")
by_season("Spinytail Skate")
by_season("Deepwater Catshark")
by_season("Black Dogfish")


