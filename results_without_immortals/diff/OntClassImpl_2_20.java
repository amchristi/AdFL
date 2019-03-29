OntClassImpl
~~~
setSuperClass
~~~
addSuperClass
~~~
getSuperClass
~~~
listSuperClasses
~~~
listSuperClasses
~~~
hasSuperClass
~~~
hasSuperClass
~~~
hasSuperClass
~~~
removeSuperClass
~~~
setSubClass
~
// first we have to remove all of the inverse sub-class links
checkProfile(getProfile().SUB_CLASS_OF(), "SUB_CLASS_OF");
~
cls.as(OntClass.class).addSuperClass(this);
~~~
addSubClass
~~~
getSubClass
~~~
listSubClasses
~~~
listSubClasses
~~~
hasSubClass
~~~
hasSubClass
~~~
hasSubClass
~~~
removeSubClass
~~~
setEquivalentClass
~~~
addEquivalentClass
~~~
getEquivalentClass
~~~
listEquivalentClasses
~~~
hasEquivalentClass
~~~
removeEquivalentClass
~~~
setDisjointWith
~~~
addDisjointWith
~~~
getDisjointWith
~~~
listDisjointWith
~~~
isDisjointWith
~~~
removeDisjointWith
~~~
listDeclaredProperties
~~~
listDeclaredProperties
~~~
hasDeclaredProperty
~~~
listInstances
~~~
listInstances
~~~
createIndividual
~~~
createIndividual
~~~
dropIndividual
~~~
isHierarchyRoot
~~~
asEnumeratedClass
~~~
asUnionClass
~~~
asIntersectionClass
~~~
asComplementClass
~~~
asRestriction
~~~
isEnumeratedClass
~
checkProfile(getProfile().ONE_OF(), "ONE_OF");
~
return hasProperty(getProfile().ONE_OF());
~~~
isUnionClass
~
checkProfile(getProfile().UNION_OF(), "UNION_OF");
~
return hasProperty(getProfile().UNION_OF());
~~~
isIntersectionClass
~
checkProfile(getProfile().INTERSECTION_OF(), "INTERSECTION_OF");
~
return hasProperty(getProfile().INTERSECTION_OF());
~~~
isComplementClass
~
checkProfile(getProfile().COMPLEMENT_OF(), "COMPLEMENT_OF");
~
return hasProperty(getProfile().COMPLEMENT_OF());
~~~
isRestriction
~
checkProfile(getProfile().RESTRICTION(), "RESTRICTION");
~
return hasProperty(getProfile().ON_PROPERTY()) || hasProperty(RDF.type, getProfile().RESTRICTION());
~~~
convertToEnumeratedClass
~
setPropertyValue(getProfile().ONE_OF(), "ONE_OF", individuals);
~~~
convertToIntersectionClass
~
setPropertyValue(getProfile().INTERSECTION_OF(), "INTERSECTION_OF", classes);
~
return as(IntersectionClass.class);
~~~
convertToUnionClass
~
setPropertyValue(getProfile().UNION_OF(), "UNION_OF", classes);
~~~
convertToComplementClass
~
setPropertyValue(getProfile().COMPLEMENT_OF(), "COMPLEMENT_OF", cls);
~~~
convertToRestriction
~
if (!hasRDFType(getProfile().RESTRICTION(), "RESTRICTION", false)) {
    setRDFType(getProfile().RESTRICTION());
}
~
setPropertyValue(getProfile().ON_PROPERTY(), "ON_PROPERTY", prop);
~~~
hasSuperClassDirect
~~~
testDomain
~
// not a generic domain
isGlobal = false;
~
if (domain.equals(this)) {
    // super-classes), then we've detected the direct property case
    seenDirect = true;
} else if (!canProveSuperClass(domain)) {
    // there is a class in the domain of p that is not a super-class of this class
    return false;
}
~
// there are some well-known values we ignore
if (!(domain.equals(getProfile().THING()) || domain.equals(RDFS.Resource))) {
    // not a generic domain
    isGlobal = false;
    if (domain.equals(this)) {
        // super-classes), then we've detected the direct property case
        seenDirect = true;
    } else if (!canProveSuperClass(domain)) {
        // there is a class in the domain of p that is not a super-class of this class
        return false;
    }
}
~~~
listAllProperties
~
// check reasoner capabilities - major performance improvement for inf models
if (mOnt.getReasoner() != null) {
    Model caps = mOnt.getReasoner().getReasonerCapabilities();
    if (caps.contains(null, ReasonerVocabulary.supportsP, OWL.ObjectProperty)) {
        // all owl:ObjectProperty, owl:DatatypeProperty, etc, are rdf:Property resources
        return pi;
    }
}
~
// otherwise, we manually check the other property types
if (prof.OBJECT_PROPERTY() != null) {
    pi = pi.andThen(mOnt.listStatements(null, RDF.type, prof.OBJECT_PROPERTY()));
}
~
if (prof.DATATYPE_PROPERTY() != null) {
    pi = pi.andThen(mOnt.listStatements(null, RDF.type, prof.DATATYPE_PROPERTY()));
}
~
if (prof.FUNCTIONAL_PROPERTY() != null) {
    pi = pi.andThen(mOnt.listStatements(null, RDF.type, prof.FUNCTIONAL_PROPERTY()));
}
~
if (prof.INVERSE_FUNCTIONAL_PROPERTY() != null) {
    pi = pi.andThen(mOnt.listStatements(null, RDF.type, prof.INVERSE_FUNCTIONAL_PROPERTY()));
}
~
if (prof.SYMMETRIC_PROPERTY() != null) {
    pi = pi.andThen(mOnt.listStatements(null, RDF.type, prof.SYMMETRIC_PROPERTY()));
}
~
if (prof.TRANSITIVE_PROPERTY() != null) {
    pi = pi.andThen(mOnt.listStatements(null, RDF.type, prof.TRANSITIVE_PROPERTY()));
}
~
if (prof.ANNOTATION_PROPERTY() != null) {
    pi = pi.andThen(mOnt.listStatements(null, RDF.type, prof.ANNOTATION_PROPERTY()));
}
~~~
canProveSuperClass
