library(DiagrammeR)

n = c("Data collection and analysis", "Modelling", "Implementation", "Monitoring and evaluation", "Public engagement")
n = create_nodes(1:5, label = n)
e = create_edges(from = c(1:4, 4), to = c(2:4, 1, 2))
g = create_graph(nodes_df = n, edges_df = e)
png(filename = "figures/transport-planning-stages-5.png", width = 600, height = 500)
render_graph(g)
dev.off()
e = create_edges(from = c(1:4, 4, rep(5, 3)), to = c(2:4, 1, 2, 2:4))
g = create_graph(nodes_df = n, edges_df = e)
png(filename = "figures/transport-planning-stages.png", width = 600, height = 500)
pdf(file = "figures/transport-planning-stages.pdf")
render_graph(g)
dev.off()

# Aim: create a schematic diagram of how the PCT can be used



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
render_graph(g)
# g = add_edge(g, 5, 6)
# g = add_edge(g, 6, 7)
# g = add_edge(g, 7, 8)

# New diagram focussing on inputs and outputs
nlabs = c(
  "Social data",
  "Environmental data",
  "Zone level",
  "Origin-destination level",
  "Scenario parameters",
  "Regional data",
  "Data processing scripts",
  "Scenario results",
  "Route-allocated flows",
  "Route network",
  "User interface",
  "Visualisation",
  "Area level output",
  "Desire line level",
  "Route network level"
)
n = create_nodes( nodes = 1:length(nlabs),
  color = c(
    "black",
    "black",
    "black",
    "black",
    "black",
    "black",
    "red",
    "black",
    "black",
    "black",
    "red",
    "red",
    "blue",
    "blue",
    "blue"
  ),
  label = nlabs
  )
e = create_edges(c(1, 2, 3, 4, 5, 6, 7, 7, 7 , 8 , 9 , 10, 11, 12, 12, 12),
                 c(3, 4, 6, 6, 6, 7, 8, 9, 10, 11, 11, 11, 12, 13, 14, 15),
                color = "black")
g = create_graph(n, e)
# g = add_edge(g, 9, 10)
render_graph(g)
