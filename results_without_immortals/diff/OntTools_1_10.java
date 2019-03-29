OntTools
~~~
getLCA
~
if (root == null) {
    throw new JenaException("The given OntModel has a language profile that does not define a generic root class (such as owl:Thing)");
}
~
root = root.inModel(m);
~
return getLCA(m, root.as(OntClass.class), u, v);
~~~
getLCA
~
if (root == null) {
    throw new JenaException("The given OntModel has a language profile that does not define a generic root class (such as owl:Thing)");
}
~
root = root.inModel(m);
~
return getLCA(m, root.as(OntClass.class), u, v);
~~~
findShortestPath
~~~
namedHierarchyRoots
~~~
lca
~~~
checkSolution
~~~
partitionByNamed
