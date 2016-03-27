# Aim: create a schematic diagram of how the PCT can be used

library(DiagrammeR)


n = create_nodes(nodes = 1:4,
                 label = c("Explore current cycling levels",
                           "Visualise future scenarios",
                           "Identify priority areas",
                           "Communicate evidence base")
                )

e = create_edges(from = c(1, 2, 3),
                 to = c(2, 3, 4))

g = create_graph(n, e)

g = add_node(g, label = "Area level", from = c(1, 2), to = c(3, 4))
g = add_node(g, label = "Desire line level", from = c(1, 2), to = c(3, 4))
g = add_node(g, label = "Route level", from = c(1, 2), to = c(3, 4))
g = add_node(g, label = "Route network level", from = c(1, 2), to = c(3, 4))

# g = add_edge(g, 5, 6)
# g = add_edge(g, 6, 7)
# g = add_edge(g, 7, 8)
render_graph(g)