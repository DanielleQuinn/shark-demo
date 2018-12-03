# How many sharks did we catch?
nrow(data)

# How many of each of our study species did we catch?
count_sharks()

# Let's put our data on a graph!
bargraph_sharks()

# We are interested in learning about where each species lives
# Let's see where each of our shark species can be found

# Start by drawing the ocean
draw_ocean()

# Now add the sharks!
add_sharks()

# What have we learned about each species?
# What fishing rules could we make to protect each species?

# What do we know about migration?
# Let's see if each species migrates into deeper or shallower water during different seasons
# One species at a time
by_season("Greenland Shark")
by_season("Spinytail Skate")
by_season("Deep-Water Catshark")
by_season("Black Dogfish")

# What have we learned about each species?
# What fishing rules could we make to protect each species?
