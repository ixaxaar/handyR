
require(Rgraphviz)
require(gRbase)
require(gRim)

# Undirected graphs
undirected.graph = ug(~a:b:e + a:c:e + b:d:e + c:d:e + c:g + d:f)
plot(undirected.graph)

# Subset of a graph
subset.graph = subGraph(c("a", "b", "e"), undirected.graph)

# Directed graphs
directed.graph = dag(~a + b:a + c:a:b + d:c:e + e:a)
plot(directed.graph)

# Moralize a DAG
plot(moralize(directed.graph))

# Parents and children
parents("c", directed.graph)
children("c", directed.graph)

# Is the graph complete?
is.complete(undirected.graph, set=c("a", "b"))

# Ancestral graph of a node
ag = ancestralGraph(c("d"), directed.graph)
plot(ag)


# For A->B->C
# Conditional probablity tables
arr.a = parray("A", levels=2, values=c(.01,.99)))
arr.ba = parray(c("B","A"), levels=c(2,2), values=c(.95,.05, .001,.999))
arr.cb = parray(c("C","B"), levels=c(2,2), values=c(.80,.20, .010,.990))

# Joint distributions
arr.a.ba = tableMult(arr.a, arr.ba)
arr.a.ba.cb = tableMult(arr.a.ba, arr.cb)

# Marginal distributions
mar.ab = tableMargin(arr.a.ba.cb, margin=c('A','B'))
mar.b = tableMargin(mar.ab, margin=c('B'))

# Conditional probability tables using gRain
require(gRain)
yn = c("yes","no")
a    = cptable(~asia, values=c(1,99),levels=yn)
t.a  = cptable(~tub+asia, values=c(5,95,1,99),levels=yn)
s    = cptable(~smoke, values=c(5,5), levels=yn)
l.s  = cptable(~lung+smoke, values=c(1,9,1,99), levels=yn)
b.s  = cptable(~bronc+smoke, values=c(6,4,3,7), levels=yn)
e.lt = cptable(~either+lung+tub,values=c(1,0,1,0,1,0,0,1),levels=yn)
x.e  = cptable(~xray+either, values=c(98,2,5,95), levels=yn)
d.be = cptable(~dysp+bronc+either, values=c(9,1,7,3,8,2,1,9), levels=yn)

# Compile conditional prob table
plist = compileCPT(list(a, t.a, s, l.s, b.s, e.lt, x.e, d.be))
bnet = grain(plist)

plist
par(mfrow=c(2,1))
plot(bnet)
plot(triangulate(moralize(bnet$dag)))

# Querying and updating beliefs
querygrain(bnet, nodes=c('lung','tub','bronc'))
bnet.f <- setFinding(bnet, nodes=c('asia','dysp'), state=c('yes','yes'))
pFinding(bnet.f)
querygrain(bnet.f, nodes=c('lung','tub','bronc'))






