OntTools
~~~
getLCA
~
if (root == null) {
    throw new JenaException("The given OntModel has a language profile that does not define a generic root class (such as owl:Thing)");
}
~~~
getLCA
~
if (root == null) {
    throw new JenaException("The given OntModel has a language profile that does not define a generic root class (such as owl:Thing)");
}
~~~
findShortestPath
~~~
namedHierarchyRoots
~~~
lca
~
// already visited
return clsSet;
~
// compute the LCA of the sub-tree
DisjointSet v = lca(child, uCls, vCls, index);
~
// union the two disjoint sets together
clsSet.union(v);
~
// propagate the distinguished member
clsSet.find().setAncestor(clsSet);
~
checkSolution(uCls, vCls, index);
~
if (cls.equals(vCls)) {
    checkSolution(vCls, uCls, index);
}
~
// log.debug( "Entering lca(), cls = " + cls );
DisjointSet clsSet = index.getSet(cls);
~
if (clsSet.isBlack()) {
    // already visited
    return clsSet;
}
~
// not visited yet
clsSet.setAncestor(clsSet);
~
// for each child of cls
for (Iterator<OntClass> i = cls.listSubClasses(true); i.hasNext(); ) {
    OntClass child = i.next();
    if (child.equals(cls) || child.equals(cls.getProfile().NOTHING())) {
        // we ignore the reflexive case and bottom
        continue;
    }
    // compute the LCA of the sub-tree
    DisjointSet v = lca(child, uCls, vCls, index);
    // union the two disjoint sets together
    clsSet.union(v);
    // propagate the distinguished member
    clsSet.find().setAncestor(clsSet);
}
~
// this node is done
clsSet.setBlack();
~
// are we inspecting one of the elements we're interested in?
if (cls.equals(uCls)) {
    checkSolution(uCls, vCls, index);
} else if (cls.equals(vCls)) {
    checkSolution(vCls, uCls, index);
}
~
return clsSet;
~~~
checkSolution
~
vSet.setUsed();
~
// log.debug( "Found LCA: u = " + uCls + ", v = " + vCls  );
OntClass lca = (OntClass) vSet.find().getAncestor().getNode();
~
// log.debug( "Found LCA: lca = " + lca );
index.setLCA(uCls, vCls, lca);
~~~
partitionByNamed
