/*
 * Licensed to the Apache Software Foundation (ASF) under one
 * or more contributor license agreements.  See the NOTICE file
 * distributed with this work for additional information
 * regarding copyright ownership.  The ASF licenses this file
 * to you under the Apache License, Version 2.0 (the
 * "License"); you may not use this file except in compliance
 * with the License.  You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
// /////////////
package org.apache.jena.ontology.impl;

// /////////////
import java.util.*;
import org.apache.jena.enhanced.EnhGraph;
import org.apache.jena.enhanced.EnhNode;
import org.apache.jena.enhanced.Implementation;
import org.apache.jena.graph.Node;
import org.apache.jena.ontology.*;
import org.apache.jena.rdf.model.*;
import org.apache.jena.reasoner.InfGraph;
import org.apache.jena.util.iterator.ExtendedIterator;
import org.apache.jena.util.iterator.UniqueFilter;
import org.apache.jena.util.iterator.WrappedIterator;
import org.apache.jena.vocabulary.OWL;
import org.apache.jena.vocabulary.RDF;
import org.apache.jena.vocabulary.RDFS;
import org.apache.jena.vocabulary.ReasonerVocabulary;
import java.io.*;

/**
 * <p>
 * Implementation of the ontology abstraction representing ontology classes.
 * </p>
 */
public class OntClassImpl extends OntResourceImpl implements OntClass {

    // Constants
    // ////////////////////////////////
    /* LDP never returns properties in these namespaces */
    private static final String[] IGNORE_NAMESPACES = new String[] { OWL.NS, RDF.getURI(), RDFS.getURI(), ReasonerVocabulary.RBNamespace };

    // Static variables
    // ////////////////////////////////
    /**
     * A factory for generating OntClass facets from nodes in enhanced graphs.
     * Note: should not be invoked directly by user code: use
     * {@link org.apache.jena.rdf.model.RDFNode#as as()} instead.
     */
    @SuppressWarnings("hiding")
    public static Implementation factory = new Implementation() {

        @Override
        public EnhNode wrap(Node n, EnhGraph eg) {
            if (canWrap(n, eg)) {
                return new OntClassImpl(n, eg);
            } else {
                throw new ConversionException("Cannot convert node " + n.toString() + " to OntClass: it does not have rdf:type owl:Class or equivalent");
            }
        }

        @Override
        public boolean canWrap(Node node, EnhGraph eg) {
            // node will support being an OntClass facet if it has rdf:type owl:Class or equivalent
            Profile profile = (eg instanceof OntModel) ? ((OntModel) eg).getProfile() : null;
            return (profile != null) && profile.isSupported(node, eg, OntClass.class);
        }
    };

    /**
     * <p>
     * Construct an ontology class node represented by the given node in the given graph.
     * </p>
     *
     * @param n The node that represents the resource
     * @param g The enh graph that contains n
     */
    public OntClassImpl(Node n, EnhGraph g) {
        super(n, g);
    }

    // External signature methods
    // ////////////////////////////////
    // subClassOf
    /**
     * <p>Assert that this class is sub-class of the given class. Any existing
     * statements for <code>subClassOf</code> will be removed.</p>
     * @param cls The class that this class is a sub-class of
     * @exception ProfileException If the {@link Profile#SUB_CLASS_OF()} property is not supported in the current language profile.
     */
    @Override
    public void setSuperClass(Resource cls) {
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_6_10.coverage", "7e1b4c13-e86a-4a0e-8d00-bb5f039af936");
        setPropertyValue(getProfile().SUB_CLASS_OF(), "SUB_CLASS_OF", cls);
    }

    /**
     * <p>Add a super-class of this class.</p>
     * @param cls A class that is a super-class of this class.
     * @exception ProfileException If the {@link Profile#SUB_CLASS_OF()} property is not supported in the current language profile.
     */
    @Override
    public void addSuperClass(Resource cls) {
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_6_10.coverage", "54c16510-4105-455c-919e-a0b236fbaff0");
        addPropertyValue(getProfile().SUB_CLASS_OF(), "SUB_CLASS_OF", cls);
    }

    /**
     * <p>Answer a class that is the super-class of this class. If there is
     * more than one such class, an arbitrary selection is made.</p>
     * @return A super-class of this class
     * @exception ProfileException If the {@link Profile#SUB_CLASS_OF()} property is not supported in the current language profile.
     */
    @Override
    public OntClass getSuperClass() {
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_6_10.coverage", "793d8eb3-3b55-4b86-a74b-4033119d552b");
        return objectAs(getProfile().SUB_CLASS_OF(), "SUB_CLASS_OF", OntClass.class);
    }

    /**
     * <p>Answer an iterator over all of the classes that are declared to be super-classes of
     * this class. Each element of the iterator will be an {@link OntClass}.</p>
     * @return An iterator over the super-classes of this class.
     * @exception ProfileException If the {@link Profile#SUB_CLASS_OF()} property is not supported in the current language profile.
     */
    @Override
    public ExtendedIterator<OntClass> listSuperClasses() {
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_6_10.coverage", "ebffa2d0-f9cc-4136-80bd-7b2a15781bc4");
        return listSuperClasses(false);
    }

    /**
     * <p>Answer an iterator over all of the classes that are declared to be super-classes of
     * this class. Each element of the iterator will be an {@link OntClass}.
     * See {@link #listSubClasses( boolean )} for a full explanation of the <em>direct</em>
     * parameter.
     * </p>
     *
     * @param direct If true, only answer the direcly adjacent classes in the
     * super-class relation: i&#046;e&#046; eliminate any class for which there is a longer route
     * to reach that child under the super-class relation.
     * @return an iterator over the resources representing this class's sub-classes.
     * @exception ProfileException If the {@link Profile#SUB_CLASS_OF()} property is not supported in the current language profile.
     */
    @Override
    public ExtendedIterator<OntClass> listSuperClasses(boolean direct) {
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_6_10.coverage", "298ef232-d6f0-4145-acae-bdc38f11f0d2");
        return listDirectPropertyValues(getProfile().SUB_CLASS_OF(), "SUB_CLASS_OF", OntClass.class, getProfile().SUB_CLASS_OF(), direct, false).filterDrop(this::equals).filterKeep(new UniqueFilter<OntClass>());
    }

    /**
     * <p>Answer true if the given class is a super-class of this class.</p>
     * @param cls A class to test.
     * @return True if the given class is a super-class of this class.
     * @exception ProfileException If the {@link Profile#SUB_CLASS_OF()} property is not supported in the current language profile.
     */
    @Override
    public boolean hasSuperClass(Resource cls) {
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_6_10.coverage", "2082f9e9-2373-4e95-a2d3-68d31d927674");
        return hasSuperClass(cls, false);
    }

    /**
     * <p>Answer true if this class has any super-class in the model. Note that
     * when using a reasoner, all OWL classes have owl:Thing as a super-class.</p>
     * @return True if this class has any known super-class.
     * @exception ProfileException If the {@link Profile#SUB_CLASS_OF()} property is not supported in the current language profile.
     */
    @Override
    public boolean hasSuperClass() {
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_6_10.coverage", "acc790ee-a363-4c79-b716-9feafda9e6fb");
        return getSuperClass() != null;
    }

    /**
     * <p>Answer true if the given class is a super-class of this class.
     * See {@link #listSubClasses( boolean )} for a full explanation of the <em>direct</em>
     * parameter.
     * </p>
     * @param cls A class to test.
     * @param direct If true, only search the classes that are directly adjacent to this
     * class in the class hierarchy.
     * @return True if the given class is a super-class of this class.
     * @exception ProfileException If the {@link Profile#SUB_CLASS_OF()} property is not supported in the current language profile.
     */
    @Override
    public boolean hasSuperClass(Resource cls, boolean direct) {
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_6_10.coverage", "4813b206-5700-422b-a5d4-45740d3a06f3");
        if (!direct) {
            writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_6_10.coverage", "39959542-77d7-4b62-bf9b-b467b8926fc2");
            // don't need any special case, we just get the property
            return hasPropertyValue(getProfile().SUB_CLASS_OF(), "SUB_CLASS_OF", cls);
        } else {
            writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_6_10.coverage", "f25273e6-09cb-46c0-bc38-8f953b43b230");
            // we want the direct, not general relationship
            // first try to find an inf graph that can do the work for us
            InfGraph ig = null;
            writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_6_10.coverage", "53b753da-19e3-4557-9f56-548abe1c52c5");
            if (getGraph() instanceof InfGraph) {
                writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_6_10.coverage", "da6331aa-0eae-4493-bfe7-c809e05a29bc");
                ig = (InfGraph) getGraph();
            } else if (getGraph() instanceof OntModel) {
                writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_6_10.coverage", "420751a1-10c4-4879-93c9-59139450b745");
                OntModel m = (OntModel) getGraph();
                writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_6_10.coverage", "60f0e583-920f-4391-8f8f-ceae187c9588");
                if (m.getGraph() instanceof InfGraph) {
                    writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_6_10.coverage", "21448c02-f835-4738-b176-df288c7d9c3c");
                    ig = (InfGraph) m.getGraph();
                }
            }
            writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_6_10.coverage", "72b4db3c-7d80-4846-99b2-ce523ef7cd8e");
            if (ig != null && ig.getReasoner().supportsProperty(ReasonerVocabulary.directSubClassOf)) {
                writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_6_10.coverage", "b030d7f7-a19f-4b12-8f0d-fe16e2fee113");
                // we can look this up directly
                return hasPropertyValue(ReasonerVocabulary.directSubClassOf, "direct sub-class", cls);
            } else {
                writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_6_10.coverage", "c2378b5e-8644-498e-9f42-10dfca2ea4bd");
                // otherwise, not an inf-graph or the given inf-graph does not support direct directly (:-)
                return hasSuperClassDirect(cls);
            }
        }
    }

    /**
     * <p>Remove the given class from the super-classes of this class.  If this statement
     * is not true of the current model, nothing happens.</p>
     * @param cls A class to be removed from the super-classes of this class
     * @exception ProfileException If the {@link Profile#SUB_CLASS_OF()} class is not supported in the current language profile.
     */
    @Override
    public void removeSuperClass(Resource cls) {
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_6_10.coverage", "7714cb7d-c88e-4685-8b10-199b829b9c41");
        removePropertyValue(getProfile().SUB_CLASS_OF(), "SUB_CLASS_OF", cls);
    }

    /**
     * <p>Assert that this class is super-class of the given class. Any existing
     * statements for <code>subClassOf</code> on <code>prop</code> will be removed.</p>
     * @param cls The class that is a sub-class of this class
     * @exception ProfileException If the {@link Profile#SUB_CLASS_OF()} property is not supported in the current language profile.
     */
    @Override
    public void setSubClass(Resource cls) {
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_6_10.coverage", "295492cc-0529-4110-9fdf-80b717440546");
        // first we have to remove all of the inverse sub-class links
        checkProfile(getProfile().SUB_CLASS_OF(), "SUB_CLASS_OF");
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_6_10.coverage", "f2fc9aed-0205-4d55-933c-b0fcf5f3deca");
        for (StmtIterator i = getModel().listStatements(null, getProfile().SUB_CLASS_OF(), this); i.hasNext(); ) {
            writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_6_10.coverage", "de37624f-ce9c-492b-b3ee-994e5ad6b7dc");
            i.removeNext();
        }
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_6_10.coverage", "4a2ad9d7-8072-4f53-9c06-fa7fa4a85a2b");
        cls.as(OntClass.class).addSuperClass(this);
    }

    /**
     * <p>Add a sub-class of this class.</p>
     * @param cls A class that is a sub-class of this class.
     * @exception ProfileException If the {@link Profile#SUB_CLASS_OF()} property is not supported in the current language profile.
     */
    @Override
    public void addSubClass(Resource cls) {
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_6_10.coverage", "4a0509d2-28ad-4647-a09b-f372ea702933");
        cls.as(OntClass.class).addSuperClass(this);
    }

    /**
     * <p>Answer a class that is the sub-class of this class. If there is
     * more than one such class, an arbitrary selection is made. If
     * there is no such class, return null.</p>
     * @return A sub-class of this class or null
     * @exception ProfileException If the {@link Profile#SUB_CLASS_OF()}
     * property is not supported in the current language profile.
     */
    @Override
    public OntClass getSubClass() {
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_6_10.coverage", "485bfe27-9542-4903-b1f7-14cda33d31ec");
        checkProfile(getProfile().SUB_CLASS_OF(), "SUB_CLASS_OF");
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_6_10.coverage", "03023ae0-2079-46f9-b198-cabef5176a1d");
        StmtIterator i = getModel().listStatements(null, getProfile().SUB_CLASS_OF(), this);
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_6_10.coverage", "32cf7422-270e-4212-85a2-8d5c3a7a2677");
        try {
            writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_6_10.coverage", "41cae6fd-ab71-4449-9df5-d7af85f05c44");
            if (i.hasNext()) {
                writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_6_10.coverage", "ee2a4aeb-68a4-4925-a653-e650ee0fcc3f");
                return i.nextStatement().getSubject().as(OntClass.class);
            } else {
                writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_6_10.coverage", "88210068-b0b0-42c2-8929-6cf0a144ec34");
                return null;
            }
        } finally {
            writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_6_10.coverage", "a1717303-8977-4050-bff1-ad9aa4a998ea");
            i.close();
        }
    }

    /**
     * <p>Answer an iterator over all of the classes that are declared to be sub-classes of
     * this class. Each element of the iterator will be an {@link OntClass}.</p>
     * @return An iterator over the sub-classes of this class.
     * @exception ProfileException If the {@link Profile#SUB_CLASS_OF()} property is not supported in the current language profile.
     */
    @Override
    public ExtendedIterator<OntClass> listSubClasses() {
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_6_10.coverage", "ee112b48-73b6-4dd1-a4fa-c94b548b2ac6");
        return listSubClasses(false);
    }

    /**
     * <p>
     * Answer an iterator over the classes that are declared to be sub-classes of
     * this class. Each element of the iterator will be an {@link OntClass}. The
     * distinguishing extra parameter for this method is the flag <code>direct</code>
     * that allows some selectivity over the classes that appear in the iterator.
     * Consider the following scenario:
     * <code><pre>
     * :B rdfs:subClassOf :A.
     * :C rdfs:subClassOf :A.
     * :D rdfs:subClassof :C.
     * </pre></code>
     * (so A has two sub-classes, B and C, and C has sub-class D).  In a raw model, with
     * no inference support, listing the sub-classes of A will answer B and C.  In an
     * inferencing model, <code>rdfs:subClassOf</code> is known to be transitive, so
     * the sub-classes iterator will include D.  The <code>direct</code> sub-classes
     * are those members of the closure of the subClassOf relation, restricted to classes that
     * cannot be reached by a longer route, i.e. the ones that are <em>directly</em> adjacent
     * to the given root.  Thus, the direct sub-classes of A are B and C only, and not D -
     * even in an inferencing graph.  Note that this is not the same as the entailments
     * from the raw graph. Suppose we add to this example:
     * <code><pre>
     * :D rdfs:subClassof :A.
     * </pre></code>
     * Now, in the raw graph, A has sub-class C.  But the direct sub-classes of A remain
     * B and C, since there is a longer path A-C-D that means that D is not a direct sub-class
     * of A.  The assertion in the raw graph that A has sub-class D is essentially redundant,
     * since this can be inferred from the closure of the graph.
     * </p>
     * <p>
     * <strong>Note:</strong> This is is a change from the behaviour of Jena 1, which took a
     * parameter <code>closed</code> to compute the closure over transitivity and equivalence
     * of sub-classes.  The closure capability in Jena2 is determined by the inference engine
     * that is wrapped with the ontology model.  The direct parameter is provided to allow,
     * for exmaple, a level-by-level traversal of the class hierarchy, starting at some given
     * root.
     * </p>
     *
     * @param direct If true, only answer the direcly adjacent classes in the
     * sub-class relation: i&#046;e&#046; eliminate any class for which there is a longer route
     * to reach that child under the sub-class relation.
     * @return an iterator over the resources representing this class's sub-classes
     * @exception ProfileException If the {@link Profile#SUB_CLASS_OF()} property is not supported in the current language profile.
     */
    @Override
    public ExtendedIterator<OntClass> listSubClasses(boolean direct) {
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_6_10.coverage", "15bedc86-6e71-4e8f-a654-d88b6d967302");
        return listDirectPropertyValues(getProfile().SUB_CLASS_OF(), "SUB_CLASS_OF", OntClass.class, getProfile().SUB_CLASS_OF(), direct, true).filterDrop(this::equals).filterKeep(new UniqueFilter<OntClass>());
    }

    /**
     * <p>Answer true if the given class is a sub-class of this class.</p>
     * @param cls A class to test.
     * @return True if the given class is a sub-class of this class.
     * @exception ProfileException If the {@link Profile#SUB_CLASS_OF()} property is not supported in the current language profile.
     */
    @Override
    public boolean hasSubClass(Resource cls) {
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_6_10.coverage", "c1dd4fbb-fc88-4d76-a094-160090af3fde");
        return hasSubClass(cls, false);
    }

    /**
     * <p>Answer true if this class has any sub-class in the model. Note that
     * when using a reasoner, all OWL classes have owl:Nothing as a sub-class.</p>
     * @return True if this class has any known sub-class.
     * @exception ProfileException If the {@link Profile#SUB_CLASS_OF()} property is not supported in the current language profile.
     */
    @Override
    public boolean hasSubClass() {
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_6_10.coverage", "adb92fc7-1803-45bf-8fed-8fba9b58e998");
        return getSubClass() != null;
    }

    /**
     * <p>Answer true if the given class is a sub-class of this class.
     * See {@link #listSubClasses( boolean )} for a full explanation of the <em>direct</em>
     * parameter.
     * </p>
     * @param cls A class to test.
     * @param direct If true, only search the classes that are directly adjacent to this
     * class in the class hierarchy.
     * @return True if the given class is a sub-class of this class.
     * @exception ProfileException If the {@link Profile#SUB_CLASS_OF()} property is not supported in the current language profile.
     */
    @Override
    public boolean hasSubClass(Resource cls, boolean direct) {
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_6_10.coverage", "05a22ea3-bf1a-4123-9cf2-8272d881e311");
        if (getModel() instanceof OntModel && (cls.getModel() == null || !(cls.getModel() instanceof OntModel))) {
            writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_6_10.coverage", "390410ca-bcba-4cf4-9340-840e7389cb00");
            // could be outside an ontmodel if a constant
            cls = cls.inModel(getModel());
        }
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_6_10.coverage", "ec7a13cf-5619-4683-8a65-5261da7f639b");
        return cls.as(OntClass.class).hasSuperClass(this, direct);
    }

    /**
     * <p>Remove the given class from the sub-classes of this class.  If this statement
     * is not true of the current model, nothing happens.</p>
     * @param cls A class to be removed from the sub-classes of this class
     * @exception ProfileException If the {@link Profile#SUB_CLASS_OF()} class is not supported in the current language profile.
     */
    @Override
    public void removeSubClass(Resource cls) {
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_6_10.coverage", "efd79fb3-8bfa-452d-92e2-3dc9d7ad40f4");
        (cls.as(OntClass.class)).removeSuperClass(this);
    }

    // equivalentClass
    /**
     * <p>Assert that the given class is equivalent to this class. Any existing
     * statements for <code>equivalentClass</code> will be removed.</p>
     * @param cls The class that this class is a equivalent to.
     * @exception ProfileException If the {@link Profile#EQUIVALENT_CLASS()} property is not supported in the current language profile.
     */
    @Override
    public void setEquivalentClass(Resource cls) {
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_6_10.coverage", "99be2563-2960-482e-a968-a17b9c68e0c2");
        setPropertyValue(getProfile().EQUIVALENT_CLASS(), "EQUIVALENT_CLASS", cls);
    }

    /**
     * <p>Add a class that is equivalent to this class.</p>
     * @param cls A class that is equivalent to this class.
     * @exception ProfileException If the {@link Profile#EQUIVALENT_CLASS()} property is not supported in the current language profile.
     */
    @Override
    public void addEquivalentClass(Resource cls) {
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_6_10.coverage", "2c2a9b14-c572-4dac-ab39-ae3be5a2f009");
        addPropertyValue(getProfile().EQUIVALENT_CLASS(), "EQUIVALENT_CLASS", cls);
    }

    /**
     * <p>Answer a class that is equivalent to this class. If there is
     * more than one such class, an arbitrary selection is made.</p>
     * @return A class equivalent to this class
     * @exception ProfileException If the {@link Profile#EQUIVALENT_CLASS()} property is not supported in the current language profile.
     */
    @Override
    public OntClass getEquivalentClass() {
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_6_10.coverage", "30d776e6-299e-45aa-b4b3-50295a993505");
        return objectAs(getProfile().EQUIVALENT_CLASS(), "EQUIVALENT_CLASS", OntClass.class);
    }

    /**
     * <p>Answer an iterator over all of the classes that are declared to be equivalent classes to
     * this class. Each element of the iterator will be an {@link OntClass}.</p>
     * @return An iterator over the classes equivalent to this class.
     * @exception ProfileException If the {@link Profile#EQUIVALENT_CLASS()} property is not supported in the current language profile.
     */
    @Override
    public ExtendedIterator<OntClass> listEquivalentClasses() {
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_6_10.coverage", "6fcfe3d3-e64f-41ae-9648-e7b0184a47bb");
        return listAs(getProfile().EQUIVALENT_CLASS(), "EQUIVALENT_CLASS", OntClass.class).filterKeep(new UniqueFilter<OntClass>());
    }

    /**
     * <p>Answer true if the given class is equivalent to this class.</p>
     * @param cls A class to test for
     * @return True if the given property is equivalent to this class.
     * @exception ProfileException If the {@link Profile#EQUIVALENT_CLASS()} property is not supported in the current language profile.
     */
    @Override
    public boolean hasEquivalentClass(Resource cls) {
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_6_10.coverage", "ba1e0077-728f-4953-847e-0c532ef31874");
        return hasPropertyValue(getProfile().EQUIVALENT_CLASS(), "EQUIVALENT_CLASS", cls);
    }

    /**
     * <p>Remove the statement that this class and the given class are
     * equivalent.  If this statement
     * is not true of the current model, nothing happens.</p>
     * @param cls A class that may be declared to be equivalent to this class, and which is no longer equivalent
     * @exception ProfileException If the {@link Profile#EQUIVALENT_CLASS()} property is not supported in the current language profile.
     */
    @Override
    public void removeEquivalentClass(Resource cls) {
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_6_10.coverage", "e5ac0ae0-802f-4253-bd09-517529d25fda");
        removePropertyValue(getProfile().EQUIVALENT_CLASS(), "EQUIVALENT_CLASS", cls);
    }

    // disjointWith
    /**
     * <p>Assert that this class is disjoint with the given class. Any existing
     * statements for <code>disjointWith</code> will be removed.</p>
     * @param cls The property that this class is disjoint with.
     * @exception ProfileException If the {@link Profile#DISJOINT_WITH()} property is not supported in the current language profile.
     */
    @Override
    public void setDisjointWith(Resource cls) {
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_6_10.coverage", "2aa3715f-2512-4100-85c3-99dc683c74c0");
        setPropertyValue(getProfile().DISJOINT_WITH(), "DISJOINT_WITH", cls);
    }

    /**
     * <p>Add a class that this class is disjoint with.</p>
     * @param cls A class that has no instances in common with this class.
     * @exception ProfileException If the {@link Profile#DISJOINT_WITH()} property is not supported in the current language profile.
     */
    @Override
    public void addDisjointWith(Resource cls) {
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_6_10.coverage", "dc4ce3f1-42fa-4627-a1fd-83c446667ea7");
        addPropertyValue(getProfile().DISJOINT_WITH(), "DISJOINT_WITH", cls);
    }

    /**
     * <p>Answer a class with which this class is disjoint. If there is
     * more than one such class, an arbitrary selection is made.</p>
     * @return A class disjoint with this class
     * @exception ProfileException If the {@link Profile#DISJOINT_WITH()} property is not supported in the current language profile.
     */
    @Override
    public OntClass getDisjointWith() {
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_6_10.coverage", "61820af9-0ce5-4fb3-ad52-61351b29fe49");
        return objectAs(getProfile().DISJOINT_WITH(), "DISJOINT_WITH", OntClass.class);
    }

    /**
     * <p>Answer an iterator over all of the classes that this class is declared to be disjoint with.
     * Each element of the iterator will be an {@link OntClass}.</p>
     * @return An iterator over the classes disjoint with this class.
     * @exception ProfileException If the {@link Profile#DISJOINT_WITH()} property is not supported in the current language profile.
     */
    @Override
    public ExtendedIterator<OntClass> listDisjointWith() {
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_6_10.coverage", "c7094307-dbfd-4b63-990e-9be17a24afc6");
        return listAs(getProfile().DISJOINT_WITH(), "DISJOINT_WITH", OntClass.class).filterKeep(new UniqueFilter<OntClass>());
    }

    /**
     * <p>Answer true if this class is disjoint with the given class.</p>
     * @param cls A class to test
     * @return True if the this class is disjoint with the the given class.
     * @exception ProfileException If the {@link Profile#DISJOINT_WITH()} property is not supported in the current language profile.
     */
    @Override
    public boolean isDisjointWith(Resource cls) {
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_6_10.coverage", "702287d2-b6d3-4424-89b3-10e349733c9c");
        return hasPropertyValue(getProfile().DISJOINT_WITH(), "DISJOINT_WITH", cls);
    }

    /**
     * <p>Remove the statement that this class and the given class are
     * disjoint.  If this statement
     * is not true of the current model, nothing happens.</p>
     * @param cls A class that may be declared to be disjoint with this class, and which is no longer disjoint
     * @exception ProfileException If the {@link Profile#DISJOINT_WITH()} property is not supported in the current language profile.
     */
    @Override
    public void removeDisjointWith(Resource cls) {
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_6_10.coverage", "e93d7721-fe5f-4ae5-a25d-f8c02fc80a57");
        removePropertyValue(getProfile().DISJOINT_WITH(), "DISJOINT_WITH", cls);
    }

    // other utility methods
    /**
     * Equivalent to calling {@link #listDeclaredProperties(boolean)} with
     * default value <code>direct = false</code>.
     */
    @Override
    public ExtendedIterator<OntProperty> listDeclaredProperties() {
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_6_10.coverage", "90220d89-ac55-419f-9435-12573c044d5c");
        return listDeclaredProperties(false);
    }

    /**
     * <p>Return an iterator over the properties associated with a frame-like
     * view of this class. This captures an intuitive notion of the <em>
     * properties of a class</em>. This can be useful in presenting an ontology
     * class in a user interface, for example by automatically constructing a
     * form to instantiate instances of the class. The properties in the frame-like
     * view of the class are determined by comparing the domain of properties in
     * this class's {@link OntModel} with the class itself.  See:
     * <a href="/documentation/notes/rdf-frames.html">Presenting RDF as frames</a>
     * for more details.
     * </p>
     * <p>
     * Note that many cases of determining whether a
     * property is associated with a class depends on RDFS or OWL reasoning.
     * This method may therefore return complete results only in models that
     * have an attached reasoner.
     * </p>
     *
     * @param direct If <code>true</code>, restrict the properties returned to those directly
     * associated with this class. If <code>false</code>, the properties of super-classes of
     * this class will not be listed among the declared properties of this class.
     * @return An iteration of the properties that are associated with this class
     * by their domain.
     */
    @Override
    public ExtendedIterator<OntProperty> listDeclaredProperties(boolean direct) {
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_6_10.coverage", "21d7e88a-ced5-4d37-90b1-676d6f2d4a09");
        // first collect the candidate properties
        Set<RDFNode> candSet = new HashSet<>();
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_6_10.coverage", "70a88726-c776-49f6-8a87-19568ab8ca46");
        // than a non-inference model
        for (Iterator<Statement> i = listAllProperties(); i.hasNext(); ) {
            writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_6_10.coverage", "0ec859dd-42e1-4d10-9f48-7c74d3995ee3");
            candSet.add(i.next().getSubject().as(Property.class));
        }
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_6_10.coverage", "5001add5-4ccd-40f6-8999-d6a10244c4bc");
        // now we iterate over the candidates and check that they match all domain constraints
        List<RDFNode> cands = new ArrayList<>();
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_6_10.coverage", "89d96d30-52f8-4dd8-97d5-8dc6ab6a4cce");
        cands.addAll(candSet);
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_6_10.coverage", "ec47fe12-b438-45d3-9713-13b4c8509320");
        for (int j = cands.size() - 1; j >= 0; j--) {
            writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_6_10.coverage", "dca7f5c4-8d0f-4e57-9b4e-28ccdf6b0aff");
            Property cand = (Property) cands.get(j);
            writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_6_10.coverage", "eb77bdf2-e784-4105-af99-68f6f54aaaa3");
            if (!hasDeclaredProperty(cand, direct)) {
                writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_6_10.coverage", "c2cc4835-a474-458b-865a-75f8d2a22459");
                cands.remove(j);
            }
        }
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_6_10.coverage", "d6a5ea7f-d0d0-4dd8-8f05-58c765729fb1");
        // return the results, using the ont property facet
        return WrappedIterator.create(cands.iterator()).mapWith(n -> n.as(OntProperty.class));
    }

    /**
     * <p>Answer true if the given property is one of the declared properties
     * of this class. For details, see {@link #listDeclaredProperties(boolean)}.</p>
     * @param p A property to test
     * @param direct If true, only direct associations between classes and properties
     * are considered
     * @return True if <code>p</code> is one of the declared properties of
     * this class
     */
    @Override
    public boolean hasDeclaredProperty(Property p, boolean direct) {
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_6_10.coverage", "a8dedb9a-7cf0-4063-afeb-87e727d67e13");
        return testDomain(p, direct);
    }

    /**
     * <p>Answer an iterator over the individuals in the model that have this
     * class among their types.<p>
     *
     * @return An iterator over those instances that have this class as one of
     * the classes to which they belong
     */
    @Override
    public ExtendedIterator<Individual> listInstances() {
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_6_10.coverage", "e0d16d3f-32d3-4e2f-a7bb-e8816b0b6467");
        return listInstances(false);
    }

    /**
     * <p>Answer an iterator over the individuals in the model that have this
     * class among their types, optionally excluding sub-classes of this class.<p>
     *
     * @param  direct If true, only direct instances are counted (i.e. not instances
     * of sub-classes of this class)
     * @return An iterator over those instances that have this class as one of
     * the classes to which they belong
     */
    @Override
    public ExtendedIterator<Individual> listInstances(final boolean direct) {
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_6_10.coverage", "4bb6a35e-4f7e-418c-8082-5d003385b9ef");
        return getModel().listStatements(null, RDF.type, this).mapWith(s -> s.getSubject().as(Individual.class)).filterKeep(o -> o.hasRDFType(OntClassImpl.this, direct)).filterKeep(new UniqueFilter<Individual>());
    }

    /**
     * <p>Answer a new individual that has this class as its <code>rdf:type</code></p>
     * @return A new anonymous individual that is an instance of this class
     */
    @Override
    public Individual createIndividual() {
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_6_10.coverage", "46d5b63f-5974-4eb0-b18b-72060d09a631");
        return ((OntModel) getModel()).createIndividual(this);
    }

    /**
     * <p>Answer a new individual that has this class as its <code>rdf:type</code></p>
     * @param uri The URI of the new individual
     * @return A new named individual that is an instance of this class
     */
    @Override
    public Individual createIndividual(String uri) {
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_6_10.coverage", "b76d7164-53a0-4023-ac98-cc65b839a8aa");
        return ((OntModel) getModel()).createIndividual(uri, this);
    }

    /**
     * <p>Remove the given individual from the set of instances that are members of
     * this class. This is effectively equivalent to the {@link Individual#removeOntClass} method,
     * but invoked via the class resource rather than via the individual resource.</p>
     * @param individual A resource denoting an individual that is no longer to be a member
     * of this class
     */
    @Override
    public void dropIndividual(Resource individual) {
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_6_10.coverage", "eb30a74d-97d9-4762-8967-9c8bdaac713a");
        getModel().remove(individual, RDF.type, this);
    }

    /**
     * <p>Answer true if this class is one of the roots of the class hierarchy.
     * This will be true if either (i) this class has <code>owl:Thing</code>
     * as a direct super-class, or (ii) it has
     * no declared super-classes (including anonymous class expressions).</p>
     * @return True if this class is the root of the class hierarchy in the
     * model it is attached to
     */
    @Override
    public boolean isHierarchyRoot() {
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_6_10.coverage", "8b96a7be-cd83-43dc-9151-4f4b71beea72");
        // sanity check - :Nothing is never a root class
        if (equals(getProfile().NOTHING())) {
            writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_6_10.coverage", "a4eca42a-a350-43f6-a2e0-83bafcbe9687");
            return false;
        }
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_6_10.coverage", "ba738783-6c0e-407f-b161-7a0742804392");
        // the only super-classes of a root class are the various aliases
        // of Top, or itself
        /**
         * Note: moved the initialisation of i outside the try-catch, otherwise an
         * exception in listSuperClasses [eg a broken Graph implementation] will
         * avoid i's initialisation but still run i.close, generating a mysterious
         * NullPointerException. Signed, Mr Burnt Spines.
         */
        ExtendedIterator<OntClass> i = listSuperClasses(true);
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_6_10.coverage", "a766891e-00ec-47f3-8f00-261e786decc8");
        try {
            writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_6_10.coverage", "608f4f18-1075-4ae9-b3ca-2cabf9c745ed");
            while (i.hasNext()) {
                writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_6_10.coverage", "b334e7c2-3dd5-428b-85a9-ae57f173220d");
                Resource sup = i.next();
                writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_6_10.coverage", "9c63bb29-9ec8-473b-8d60-05d0fc7e47e0");
                if (!(sup.equals(getProfile().THING()) || sup.equals(RDFS.Resource) || sup.equals(this))) {
                    writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_6_10.coverage", "06598756-10c6-4d2f-bd54-03dbee92ac46");
                    // a super that indicates this is not a root class
                    return false;
                }
            }
        } finally {
            writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_6_10.coverage", "c5330d93-dfbf-4d6a-807b-9bef25af20b7");
            i.close();
        }
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_6_10.coverage", "6fa1cbc1-1669-4371-beed-07809d77f5cb");
        return true;
    }

    // access to facets
    /**
     * <p>Answer a view of this class as an enumerated class</p>
     * @return This class, but viewed as an EnumeratedClass node
     * @exception ConversionException if the class cannot be converted to an enumerated class
     * given the lanuage profile and the current state of the underlying model.
     */
    @Override
    public EnumeratedClass asEnumeratedClass() {
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_6_10.coverage", "95117d77-7cb4-4566-94e6-23f97db91975");
        return as(EnumeratedClass.class);
    }

    /**
     * <p>Answer a view of this class as a union class</p>
     * @return This class, but viewed as a UnionClass node
     * @exception ConversionException if the class cannot be converted to a union class
     * given the lanuage profile and the current state of the underlying model.
     */
    @Override
    public UnionClass asUnionClass() {
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_6_10.coverage", "ebd48e5b-fe2c-400d-a54b-c0e3df8223ae");
        return as(UnionClass.class);
    }

    /**
     * <p>Answer a view of this class as an intersection class</p>
     * @return This class, but viewed as an IntersectionClass node
     * @exception ConversionException if the class cannot be converted to an intersection class
     * given the lanuage profile and the current state of the underlying model.
     */
    @Override
    public IntersectionClass asIntersectionClass() {
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_6_10.coverage", "9f205113-1cd4-4ff3-bb85-cb456930b04c");
        return as(IntersectionClass.class);
    }

    /**
     * <p>Answer a view of this class as a complement class</p>
     * @return This class, but viewed as a ComplementClass node
     * @exception ConversionException if the class cannot be converted to a complement class
     * given the lanuage profile and the current state of the underlying model.
     */
    @Override
    public ComplementClass asComplementClass() {
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_6_10.coverage", "df4ab942-2881-4f13-88e2-754c96be8737");
        return as(ComplementClass.class);
    }

    /**
     * <p>Answer a view of this class as a restriction class expression</p>
     * @return This class, but viewed as a Restriction node
     * @exception ConversionException if the class cannot be converted to a restriction
     * given the lanuage profile and the current state of the underlying model.
     */
    @Override
    public Restriction asRestriction() {
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_6_10.coverage", "78edc2a6-ba68-4f2b-940c-a39dd767301d");
        return as(Restriction.class);
    }

    // sub-type testing
    /**
     * <p>Answer true if this class is an enumerated class expression</p>
     * @return True if this is an enumerated class expression
     */
    @Override
    public boolean isEnumeratedClass() {
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_6_10.coverage", "5f9d744e-4b34-4c16-af87-c245e7588d02");
        checkProfile(getProfile().ONE_OF(), "ONE_OF");
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_6_10.coverage", "19baffce-8822-40a8-a17c-f3b036e29c78");
        return hasProperty(getProfile().ONE_OF());
    }

    /**
     * <p>Answer true if this class is a union class expression</p>
     * @return True if this is a union class expression
     */
    @Override
    public boolean isUnionClass() {
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_6_10.coverage", "dfacb400-2ace-4dce-bbd4-53fb7e1a2939");
        checkProfile(getProfile().UNION_OF(), "UNION_OF");
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_6_10.coverage", "5d3f0290-8b9a-4d86-9f69-daea644c1027");
        return hasProperty(getProfile().UNION_OF());
    }

    /**
     * <p>Answer true if this class is an intersection class expression</p>
     * @return True if this is an intersection class expression
     */
    @Override
    public boolean isIntersectionClass() {
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_6_10.coverage", "cbbb10cd-6df4-4eea-9ecb-5cf4ca004386");
        checkProfile(getProfile().INTERSECTION_OF(), "INTERSECTION_OF");
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_6_10.coverage", "cac1ef0e-b9a2-4702-930e-1ce6f641d75b");
        return hasProperty(getProfile().INTERSECTION_OF());
    }

    /**
     * <p>Answer true if this class is a complement class expression</p>
     * @return True if this is a complement class expression
     */
    @Override
    public boolean isComplementClass() {
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_6_10.coverage", "04b5f0fb-2bce-4080-b37b-2014dd1bf16b");
        checkProfile(getProfile().COMPLEMENT_OF(), "COMPLEMENT_OF");
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_6_10.coverage", "732cc975-2427-4650-b4c9-95fc2c9737de");
        return hasProperty(getProfile().COMPLEMENT_OF());
    }

    /**
     * <p>Answer true if this class is a property restriction</p>
     * @return True if this is a restriction
     */
    @Override
    public boolean isRestriction() {
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_6_10.coverage", "bbb3fc95-b693-4736-af5c-9b86d79ea231");
        checkProfile(getProfile().RESTRICTION(), "RESTRICTION");
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_6_10.coverage", "1d5a3ed7-f9c4-49cf-a957-526e7beb50f3");
        return hasProperty(getProfile().ON_PROPERTY()) || hasProperty(RDF.type, getProfile().RESTRICTION());
    }

    // conversion operations
    /**
     * <p>Answer a view of this class as an enumeration of the given individuals.</p>
     * @param individuals A list of the individuals that will comprise the permitted values of this
     * class converted to an enumeration
     * @return This ontology class, converted to an enumeration of the given individuals
     */
    @Override
    public EnumeratedClass convertToEnumeratedClass(RDFList individuals) {
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_6_10.coverage", "c8516838-1140-454c-9591-6c30136e0631");
        setPropertyValue(getProfile().ONE_OF(), "ONE_OF", individuals);
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_6_10.coverage", "42156151-3b1a-4f5f-b043-b136af317a3a");
        return as(EnumeratedClass.class);
    }

    /**
     * <p>Answer a view of this class as an intersection of the given classes.</p>
     * @param classes A list of the classes that will comprise the operands of the intersection
     * @return This ontology class, converted to an intersection of the given classes
     */
    @Override
    public IntersectionClass convertToIntersectionClass(RDFList classes) {
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_6_10.coverage", "7bb04953-d5ff-45d9-b81e-bb03663cb16f");
        setPropertyValue(getProfile().INTERSECTION_OF(), "INTERSECTION_OF", classes);
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_6_10.coverage", "5cf24b52-ae8b-4bfe-aa50-e4dbb6ff62c3");
        return as(IntersectionClass.class);
    }

    /**
     * <p>Answer a view of this class as a union of the given classes.</p>
     * @param classes A list of the classes that will comprise the operands of the union
     * @return This ontology class, converted to an union of the given classes
     */
    @Override
    public UnionClass convertToUnionClass(RDFList classes) {
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_6_10.coverage", "cc32f6c8-f198-40ab-a60d-82ee33f01b37");
        setPropertyValue(getProfile().UNION_OF(), "UNION_OF", classes);
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_6_10.coverage", "d458488d-a42f-4208-a67f-691ff8f24b94");
        return as(UnionClass.class);
    }

    /**
     * <p>Answer a view of this class as an complement of the given class.</p>
     * @param cls An ontology classs that will be operand of the complement
     * @return This ontology class, converted to an complement of the given class
     */
    @Override
    public ComplementClass convertToComplementClass(Resource cls) {
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_6_10.coverage", "aaabff27-dc69-4fe6-9efd-80ce0f242d61");
        setPropertyValue(getProfile().COMPLEMENT_OF(), "COMPLEMENT_OF", cls);
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_6_10.coverage", "6216bc75-3c0f-445d-a606-1d39e1ed7231");
        return as(ComplementClass.class);
    }

    /**
     * <p>Answer a view of this class as an resriction on the given property.</p>
     * @param prop A property this is the subject of a property restriction class expression
     * @return This ontology class, converted to a restriction on the given property
     */
    @Override
    public Restriction convertToRestriction(Property prop) {
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_6_10.coverage", "e2a5e617-e726-4ec3-b6de-6a4bc3be412b");
        if (!hasRDFType(getProfile().RESTRICTION(), "RESTRICTION", false)) {
            writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_6_10.coverage", "d3d814c7-7ae5-4ba9-9828-49ded0a638d4");
            setRDFType(getProfile().RESTRICTION());
        }
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_6_10.coverage", "63bbc3b7-5d08-49be-80f0-4ca0d4b453c8");
        setPropertyValue(getProfile().ON_PROPERTY(), "ON_PROPERTY", prop);
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_6_10.coverage", "7d5bf143-5297-4335-b748-0bde51319ff0");
        return as(Restriction.class);
    }

    // Internal implementation methods
    // ////////////////////////////////
    /**
     * <p>Answer true if this class has the given class as a direct super-class, without using
     * extra help from the reasoner.</p>
     * @param cls The class to test
     * @return True if the cls is a direct super-class of this class
     */
    protected boolean hasSuperClassDirect(Resource cls) {
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_6_10.coverage", "9458c647-8b11-4653-b14d-ebb234cac968");
        // we manually compute the maximal lower elements - this could be expensive in general
        // return ResourceUtils.maximalLowerElements( listSuperClasses(), getProfile().SUB_CLASS_OF(), false ).contains( cls );
        ExtendedIterator<OntClass> i = listDirectPropertyValues(getProfile().SUB_CLASS_OF(), "subClassOf", OntClass.class, getProfile().SUB_CLASS_OF(), true, false);
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_6_10.coverage", "ead035e3-a4ff-4440-88ff-17c32b1802f2");
        try {
            writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_6_10.coverage", "99173030-da7b-43be-82e5-42235c0b1067");
            while (i.hasNext()) {
                writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_6_10.coverage", "e0fd3d7b-51e1-4417-b8cc-aacd53927710");
                if (cls.equals(i.next())) {
                    writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_6_10.coverage", "db0a5f6e-2638-4260-96ac-51811ab29b67");
                    return true;
                }
            }
        } finally {
            writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_6_10.coverage", "4fc4441e-257d-4153-b814-1ebe7048e688");
            i.close();
        }
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_6_10.coverage", "5a5857f3-5203-4ec2-8bb9-173484b061a9");
        return false;
    }

    /**
     * <p>Answer true if this class lies with the domain of p<p>
     * @param p
     * @param direct If true, only consider direct associations with domain
     * @return True if this class in the domain of property <code>p</code>
     */
    protected boolean testDomain(Property p, boolean direct) {
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_6_10.coverage", "896009e8-1ae7-4ed9-a25d-c18d754dff4a");
        // we ignore any property in the OWL, etc namespace
        String namespace = p.getNameSpace();
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_6_10.coverage", "2bb138b8-cb5e-4882-9a09-37cd5e463ed1");
        for (String IGNORE_NAMESPACE : IGNORE_NAMESPACES) {
            writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_6_10.coverage", "5cf7a167-c414-4b8e-8406-7fc7bcf453bf");
            if (namespace.equals(IGNORE_NAMESPACE)) {
                writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_6_10.coverage", "590b3271-ec05-4a30-9d3e-e4d81d8576b8");
                return false;
            }
        }
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_6_10.coverage", "398d63ab-cae6-40e6-8e44-e1c6570928f2");
        // check for global props, that have no specific domain constraint
        boolean isGlobal = true;
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_6_10.coverage", "e5756014-de42-49a1-9ad2-b1b740a7183e");
        // flag for detecting the direct case
        boolean seenDirect = false;
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_6_10.coverage", "2eeb3398-36ce-41e1-ba0d-bdfe45843c12");
        for (StmtIterator i = getModel().listStatements(p, getProfile().DOMAIN(), (RDFNode) null); i.hasNext(); ) {
            writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_6_10.coverage", "ae7ab1f5-3bb9-4d4e-8923-4ff59d67dbae");
            Resource domain = i.nextStatement().getResource();
            writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_6_10.coverage", "026ef665-61e2-427a-abea-14ce87adbb0e");
            // there are some well-known values we ignore
            if (!(domain.equals(getProfile().THING()) || domain.equals(RDFS.Resource))) {
                writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_6_10.coverage", "8029d4ab-2192-4a59-a7c1-b9d50fdcf25e");
                // not a generic domain
                isGlobal = false;
                writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_6_10.coverage", "1b0f1629-247a-4219-a374-f6573969b27b");
                if (domain.equals(this)) {
                    writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_6_10.coverage", "fadcdf7b-384a-4958-9570-e35338e2b825");
                    // if this class is actually in the domain (as opposed to one of this class's
                    // super-classes), then we've detected the direct property case
                    seenDirect = true;
                } else if (!canProveSuperClass(domain)) {
                    writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_6_10.coverage", "98f1c094-9792-4533-a7bb-1b8128525e3f");
                    // there is a class in the domain of p that is not a super-class of this class
                    return false;
                }
            }
        }
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_6_10.coverage", "d0786ccf-f345-4881-a0fd-f33df3b4c5e4");
        if (direct) {
            writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_6_10.coverage", "423a0279-c9cd-4276-b63f-d80b9ed97e50");
            // or it's a global prop and this is a root class
            return seenDirect || (isGlobal && isHierarchyRoot());
        } else {
            writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_6_10.coverage", "dc80f7a5-baad-498b-bbd6-9b8ae6c18a24");
            // otherwise the 'return false' above would have kicked in
            return true;
        }
    }

    /**
     * <p>Answer an iterator over all of the properties in this model
     * @return An iterator over {@link OntProperty}
     */
    protected ExtendedIterator<Statement> listAllProperties() {
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_6_10.coverage", "b716b234-3e70-41da-806b-71c08b5051f7");
        OntModel mOnt = (OntModel) getModel();
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_6_10.coverage", "2c3233b0-bfcd-463d-9ae2-17e80d7c7198");
        Profile prof = mOnt.getProfile();
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_6_10.coverage", "5a2e594f-fca1-4f2c-9293-794894bed5f2");
        ExtendedIterator<Statement> pi = mOnt.listStatements(null, RDF.type, getProfile().PROPERTY());
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_6_10.coverage", "3712f436-82e0-4c91-bc76-de5574321e58");
        // check reasoner capabilities - major performance improvement for inf models
        if (mOnt.getReasoner() != null) {
            writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_6_10.coverage", "a449b73f-81f4-4d84-ab4b-623ea2771a45");
            Model caps = mOnt.getReasoner().getReasonerCapabilities();
            writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_6_10.coverage", "8c1211ba-c499-4718-8bd2-e77b776edc2a");
            if (caps.contains(null, ReasonerVocabulary.supportsP, OWL.ObjectProperty)) {
                writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_6_10.coverage", "b9025e86-f7ce-4c9f-844d-fbf8be4ac3c1");
                // all owl:ObjectProperty, owl:DatatypeProperty, etc, are rdf:Property resources
                return pi;
            }
        }
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_6_10.coverage", "17846f15-90b6-40b2-904d-2b96527cfdea");
        // otherwise, we manually check the other property types
        if (prof.OBJECT_PROPERTY() != null) {
            writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_6_10.coverage", "760e88af-81c2-432f-9732-3afdf15a9d8a");
            pi = pi.andThen(mOnt.listStatements(null, RDF.type, prof.OBJECT_PROPERTY()));
        }
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_6_10.coverage", "44caafc5-9877-4e20-8790-b7719b14a805");
        if (prof.DATATYPE_PROPERTY() != null) {
            writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_6_10.coverage", "8a11a297-2606-46ed-8da5-99d0a401ae2f");
            pi = pi.andThen(mOnt.listStatements(null, RDF.type, prof.DATATYPE_PROPERTY()));
        }
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_6_10.coverage", "10e746fc-9410-49d2-bca3-02ef3ad50e0b");
        if (prof.FUNCTIONAL_PROPERTY() != null) {
            writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_6_10.coverage", "3ef0ea13-dd61-4cbb-a646-f2b3475426fc");
            pi = pi.andThen(mOnt.listStatements(null, RDF.type, prof.FUNCTIONAL_PROPERTY()));
        }
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_6_10.coverage", "7fea5411-b5f4-47c5-9b49-f1ea384c4efd");
        if (prof.INVERSE_FUNCTIONAL_PROPERTY() != null) {
            writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_6_10.coverage", "cebfd59e-d70a-4cc7-b49f-6e3799257430");
            pi = pi.andThen(mOnt.listStatements(null, RDF.type, prof.INVERSE_FUNCTIONAL_PROPERTY()));
        }
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_6_10.coverage", "af4618c4-1e02-47f9-abcd-1fd8c6db89f4");
        if (prof.SYMMETRIC_PROPERTY() != null) {
            writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_6_10.coverage", "0b660684-43e2-4a37-8030-c1297a26f41a");
            pi = pi.andThen(mOnt.listStatements(null, RDF.type, prof.SYMMETRIC_PROPERTY()));
        }
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_6_10.coverage", "25a6dfeb-d9a5-47a5-9d30-e99abbc8b2d7");
        if (prof.TRANSITIVE_PROPERTY() != null) {
            writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_6_10.coverage", "4490393e-fb5f-4e4c-88f2-bea52e4f71de");
            pi = pi.andThen(mOnt.listStatements(null, RDF.type, prof.TRANSITIVE_PROPERTY()));
        }
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_6_10.coverage", "993360a8-0fd6-494d-b8e2-fd7560575aed");
        if (prof.ANNOTATION_PROPERTY() != null) {
            writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_6_10.coverage", "c29dcf7e-240e-422a-ba0e-5539b0a7d104");
            pi = pi.andThen(mOnt.listStatements(null, RDF.type, prof.ANNOTATION_PROPERTY()));
        }
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_6_10.coverage", "a7e4a1f2-95df-4af5-ba4f-6a137872f2e1");
        return pi;
    }

    /**
     * <p>Answer true if we can demonstrate that this class has the given super-class.
     * If this model has a reasoner, this is equivalent to asking if the sub-class
     * relation holds. Otherwise, we simulate basic reasoning by searching upwards
     * through the class hierarchy.</p>
     * @param sup A super-class to test for
     * @return True if we can show that sup is a super-class of thsi class
     */
    protected boolean canProveSuperClass(Resource sup) {
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_6_10.coverage", "f03b7bfb-4e06-4498-8d9c-7d740e86e168");
        OntModel om = (OntModel) getModel();
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_6_10.coverage", "d195eb31-e526-41a1-bf25-9ab612c26533");
        if (om.getReasoner() != null) {
            writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_6_10.coverage", "4b3d4f45-4580-4925-a456-d1353bea2521");
            if (om.getReasoner().getReasonerCapabilities().contains(null, ReasonerVocabulary.supportsP, RDFS.subClassOf)) {
                writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_6_10.coverage", "baa1f64a-53e5-4224-aa42-fa11dafc448b");
                // this reasoner does transitive closure on sub-classes, so we just ask
                return hasSuperClass(sup);
            }
        }
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_6_10.coverage", "855d1800-f0af-41aa-b364-729fe2f670e6");
        // otherwise, we have to search upwards through the class hierarchy
        Set<OntClass> seen = new HashSet<>();
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_6_10.coverage", "75ec185b-d3d7-426d-a041-bcce7b9a6f6a");
        List<OntClass> queue = new ArrayList<>();
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_6_10.coverage", "024570b2-3122-4436-921a-ae62d1cdbcc6");
        queue.add(this);
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_6_10.coverage", "b7b89472-6c84-4a91-8700-174c871f0e1e");
        while (!queue.isEmpty()) {
            writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_6_10.coverage", "2e77eae2-57f1-481d-bf46-ed4b22d4ff21");
            OntClass c = queue.remove(0);
            writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_6_10.coverage", "f296b5ef-7ac4-421d-b486-e44db4a3e376");
            if (!seen.contains(c)) {
                writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_6_10.coverage", "05322c89-8799-40c3-885e-554221dc579c");
                seen.add(c);
                writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_6_10.coverage", "5aa26917-d446-48a6-a2f1-f27a244ad257");
                if (c.equals(sup)) {
                    writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_6_10.coverage", "696d457d-6928-4248-82cf-4ab4143ab0cd");
                    // found the super class
                    return true;
                } else {
                    writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_6_10.coverage", "d0c7449e-3594-4782-b2d7-d0abbeb9b414");
                    // queue the supers
                    for (Iterator<OntClass> i = c.listSuperClasses(); i.hasNext(); ) {
                        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_6_10.coverage", "5b99dc83-cce2-43d7-ad09-5d3e713da7f6");
                        queue.add(i.next());
                    }
                }
            }
        }
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_6_10.coverage", "73b49108-659f-494b-bdc2-ef6b72277cbc");
        // to get here, we didn't find the class we were looking for
        return false;
    }

    // ==============================================================================
    // Inner class definitions
    // ==============================================================================
    void writeline(String fullFilePath, String text) {
        try {
            File file = new File(fullFilePath);
            FileWriter fileWriter = new FileWriter(file, true);
            BufferedWriter output = new BufferedWriter(fileWriter);
            output.append(text);
            output.newLine();
            output.close();
        } catch (IOException e) {
            e.printStackTrace();
        }
    }

    static void writelineStatic(String fullFilePath, String text) {
        try {
            File file = new File(fullFilePath);
            FileWriter fileWriter = new FileWriter(file, true);
            BufferedWriter output = new BufferedWriter(fileWriter);
            output.append(text);
            output.newLine();
            output.close();
        } catch (IOException e) {
            e.printStackTrace();
        }
    }
}
