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
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_1_10.coverage", "aa25b41e-f733-4115-a472-f044e10b7001");
        setPropertyValue(getProfile().SUB_CLASS_OF(), "SUB_CLASS_OF", cls);
    }

    /**
     * <p>Add a super-class of this class.</p>
     * @param cls A class that is a super-class of this class.
     * @exception ProfileException If the {@link Profile#SUB_CLASS_OF()} property is not supported in the current language profile.
     */
    @Override
    public void addSuperClass(Resource cls) {
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_1_10.coverage", "faedb9b6-1fea-4d10-b50b-6712b0309d39");
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
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_1_10.coverage", "f84c2553-b4dc-4790-a64e-a1086d51044e");
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
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_1_10.coverage", "64ea5b6b-6177-406f-941c-fba3e659f406");
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
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_1_10.coverage", "3cb56103-63d3-401a-a45b-6f9e6f993922");
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
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_1_10.coverage", "7ae2e38b-726d-498d-9a42-f0d9628bfa59");
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
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_1_10.coverage", "ffb38717-06a6-4d1b-9e2f-3db82e6367d9");
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
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_1_10.coverage", "8a22bf4d-45f6-404e-95d2-6cfa3b9081f5");
        if (!direct) {
            writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_1_10.coverage", "b870bfb9-3fa1-4819-a5b9-064f7f8d32df");
            // don't need any special case, we just get the property
            return hasPropertyValue(getProfile().SUB_CLASS_OF(), "SUB_CLASS_OF", cls);
        } else {
            writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_1_10.coverage", "2af6f9be-113e-4da0-92de-69067d4ef414");
            // we want the direct, not general relationship
            // first try to find an inf graph that can do the work for us
            InfGraph ig = null;
            writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_1_10.coverage", "d1260530-3a13-48f7-98a2-0c54bb997343");
            if (getGraph() instanceof InfGraph) {
                writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_1_10.coverage", "bb8b77c5-2e99-4ee9-ab30-e67fb36e6ca7");
                ig = (InfGraph) getGraph();
            } else if (getGraph() instanceof OntModel) {
                writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_1_10.coverage", "5beadbca-c5d5-4444-b600-dff05e44cf80");
                OntModel m = (OntModel) getGraph();
                writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_1_10.coverage", "b6eb47e2-d70b-4dd5-88d0-41c0b8f8495a");
                if (m.getGraph() instanceof InfGraph) {
                    writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_1_10.coverage", "cdc6fc12-2563-417c-97c9-59cd7bd58514");
                    ig = (InfGraph) m.getGraph();
                }
            }
            writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_1_10.coverage", "2ff1a5e4-6231-4c4f-b011-9108f18a8f5c");
            if (ig != null && ig.getReasoner().supportsProperty(ReasonerVocabulary.directSubClassOf)) {
                writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_1_10.coverage", "4888ef42-383e-46c4-a8ba-3931bbcee18d");
                // we can look this up directly
                return hasPropertyValue(ReasonerVocabulary.directSubClassOf, "direct sub-class", cls);
            } else {
                writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_1_10.coverage", "a86768c3-c030-48b4-9a85-49fc1473f96a");
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
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_1_10.coverage", "3820c99a-6a63-4d3a-a569-321558176a95");
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
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_1_10.coverage", "b8e3350f-d8de-410c-9f06-55e23d8d44d7");
        // first we have to remove all of the inverse sub-class links
        checkProfile(getProfile().SUB_CLASS_OF(), "SUB_CLASS_OF");
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_1_10.coverage", "e89b1eed-2290-45b3-b212-85ef095106bc");
        for (StmtIterator i = getModel().listStatements(null, getProfile().SUB_CLASS_OF(), this); i.hasNext(); ) {
            writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_1_10.coverage", "06bd0605-0394-46e4-a9bc-4131eea5ec33");
            i.removeNext();
        }
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_1_10.coverage", "ba9c55d4-7952-41f0-a676-41cff9cad7e3");
        cls.as(OntClass.class).addSuperClass(this);
    }

    /**
     * <p>Add a sub-class of this class.</p>
     * @param cls A class that is a sub-class of this class.
     * @exception ProfileException If the {@link Profile#SUB_CLASS_OF()} property is not supported in the current language profile.
     */
    @Override
    public void addSubClass(Resource cls) {
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_1_10.coverage", "391460c9-99bc-4335-9573-dbb9e7552fde");
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
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_1_10.coverage", "ad4253f8-1156-42e7-a3c0-8a1d14cd3b79");
        checkProfile(getProfile().SUB_CLASS_OF(), "SUB_CLASS_OF");
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_1_10.coverage", "62f8054d-5201-487a-8863-5389f608dba1");
        StmtIterator i = getModel().listStatements(null, getProfile().SUB_CLASS_OF(), this);
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_1_10.coverage", "346b5e6f-9f0f-4443-b6c3-3693f6cd2e5f");
        try {
            writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_1_10.coverage", "4b6a5425-ff55-4c05-802f-f522554ad603");
            if (i.hasNext()) {
                writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_1_10.coverage", "92973f6b-08ad-41e3-8c75-68a6582dc8ce");
                return i.nextStatement().getSubject().as(OntClass.class);
            } else {
                writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_1_10.coverage", "90746f5c-5e6f-4c3a-bbf7-a0ca20964163");
                return null;
            }
        } finally {
            writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_1_10.coverage", "d70237bb-1061-4dd7-a943-c6c836939ceb");
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
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_1_10.coverage", "b0da869a-9b72-4834-b2eb-ccabd5317927");
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
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_1_10.coverage", "be73f4aa-4fc5-4fe2-8542-2cf704a27838");
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
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_1_10.coverage", "344ae620-7e1d-4e62-9cea-4b252729918e");
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
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_1_10.coverage", "68e4c794-b901-4e15-b5f2-b718d32051f0");
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
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_1_10.coverage", "9e68ef46-4bca-4ef2-9f63-d743d3248173");
        if (getModel() instanceof OntModel && (cls.getModel() == null || !(cls.getModel() instanceof OntModel))) {
            writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_1_10.coverage", "e013b954-dee3-49c2-91d5-696c79fa8669");
            // could be outside an ontmodel if a constant
            cls = cls.inModel(getModel());
        }
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_1_10.coverage", "e8f0d4d4-dd41-4312-83cf-736ecc090f6e");
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
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_1_10.coverage", "966b841a-637e-4b54-9a7c-a7a97220e2a6");
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
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_1_10.coverage", "f2d0bccd-bf8f-4e7d-b06d-2df121b2627a");
        setPropertyValue(getProfile().EQUIVALENT_CLASS(), "EQUIVALENT_CLASS", cls);
    }

    /**
     * <p>Add a class that is equivalent to this class.</p>
     * @param cls A class that is equivalent to this class.
     * @exception ProfileException If the {@link Profile#EQUIVALENT_CLASS()} property is not supported in the current language profile.
     */
    @Override
    public void addEquivalentClass(Resource cls) {
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_1_10.coverage", "f9758184-6a30-4537-89bd-bfd5ecea225f");
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
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_1_10.coverage", "2c88546e-dd4c-4b45-a922-056f2c2d7cde");
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
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_1_10.coverage", "035bc6c0-72bf-4971-b27e-515b9c4dbf71");
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
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_1_10.coverage", "c530d434-5b9f-439f-b1c0-eafbfb0126d2");
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
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_1_10.coverage", "351996e1-ea75-440a-b4d1-b72b18727533");
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
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_1_10.coverage", "54a46281-1ed9-4db1-b04b-ae7370fa421b");
        setPropertyValue(getProfile().DISJOINT_WITH(), "DISJOINT_WITH", cls);
    }

    /**
     * <p>Add a class that this class is disjoint with.</p>
     * @param cls A class that has no instances in common with this class.
     * @exception ProfileException If the {@link Profile#DISJOINT_WITH()} property is not supported in the current language profile.
     */
    @Override
    public void addDisjointWith(Resource cls) {
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_1_10.coverage", "ce4ca3f1-4eb2-424c-93d9-a9d2ba2fc85f");
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
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_1_10.coverage", "68d25f49-b417-4aa1-a9a3-1922a4a22a9a");
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
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_1_10.coverage", "6f707501-fb1f-4783-9a9e-227e40be8bfa");
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
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_1_10.coverage", "af756dd0-d29d-474b-bf5c-73c225baf05a");
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
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_1_10.coverage", "4350ee73-1a34-42aa-8259-4fa0e23c5eb1");
        removePropertyValue(getProfile().DISJOINT_WITH(), "DISJOINT_WITH", cls);
    }

    // other utility methods
    /**
     * Equivalent to calling {@link #listDeclaredProperties(boolean)} with
     * default value <code>direct = false</code>.
     */
    @Override
    public ExtendedIterator<OntProperty> listDeclaredProperties() {
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_1_10.coverage", "ada49197-3005-4f58-a5a9-e616065fe55c");
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
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_1_10.coverage", "aa4a8c76-f5d9-4ce3-9e69-52108c407cf0");
        // first collect the candidate properties
        Set<RDFNode> candSet = new HashSet<>();
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_1_10.coverage", "4efa8fa2-85b9-4805-8b44-485da9d99a97");
        // than a non-inference model
        for (Iterator<Statement> i = listAllProperties(); i.hasNext(); ) {
            writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_1_10.coverage", "972d3850-513b-4a86-a571-dece78c0d865");
            candSet.add(i.next().getSubject().as(Property.class));
        }
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_1_10.coverage", "85d015f0-1ee9-4057-a082-52d335f91e31");
        // now we iterate over the candidates and check that they match all domain constraints
        List<RDFNode> cands = new ArrayList<>();
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_1_10.coverage", "c5aaa968-9483-4679-b48d-71c62eb7350e");
        cands.addAll(candSet);
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_1_10.coverage", "0ab97d32-5d22-43f9-a0af-e13a6e638081");
        for (int j = cands.size() - 1; j >= 0; j--) {
            writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_1_10.coverage", "ecfe7186-9cf4-4da0-98db-6031beb1fd67");
            Property cand = (Property) cands.get(j);
            writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_1_10.coverage", "a5101fb5-3f2f-4dca-8776-2fe40098dc9a");
            if (!hasDeclaredProperty(cand, direct)) {
                writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_1_10.coverage", "651f7206-e6a5-4059-90cb-616e58d5e0af");
                cands.remove(j);
            }
        }
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_1_10.coverage", "09e799be-3e5e-4e2c-bfbc-504e47f4f999");
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
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_1_10.coverage", "314204d5-5e33-470e-bd34-9c91e665ac80");
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
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_1_10.coverage", "c4a0208a-c511-4d0a-9560-727cdb974183");
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
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_1_10.coverage", "2f88391d-29d5-4c5c-a8b6-650664f902d3");
        return getModel().listStatements(null, RDF.type, this).mapWith(s -> s.getSubject().as(Individual.class)).filterKeep(o -> o.hasRDFType(OntClassImpl.this, direct)).filterKeep(new UniqueFilter<Individual>());
    }

    /**
     * <p>Answer a new individual that has this class as its <code>rdf:type</code></p>
     * @return A new anonymous individual that is an instance of this class
     */
    @Override
    public Individual createIndividual() {
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_1_10.coverage", "8876ee43-7a22-43f6-a43a-3cfc804b3d67");
        return ((OntModel) getModel()).createIndividual(this);
    }

    /**
     * <p>Answer a new individual that has this class as its <code>rdf:type</code></p>
     * @param uri The URI of the new individual
     * @return A new named individual that is an instance of this class
     */
    @Override
    public Individual createIndividual(String uri) {
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_1_10.coverage", "c2ba4de2-40a4-42f0-93ff-2f9aefc4f886");
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
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_1_10.coverage", "8ecad7f9-4737-4224-b436-e524f770664d");
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
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_1_10.coverage", "8010e44d-f7f4-433f-a392-e1b01cd5803a");
        // sanity check - :Nothing is never a root class
        if (equals(getProfile().NOTHING())) {
            writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_1_10.coverage", "f9059a10-c298-4e92-ae6b-ae2219c4e414");
            return false;
        }
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_1_10.coverage", "28297624-101b-4535-97f2-7f143ac03447");
        // the only super-classes of a root class are the various aliases
        // of Top, or itself
        /**
         * Note: moved the initialisation of i outside the try-catch, otherwise an
         * exception in listSuperClasses [eg a broken Graph implementation] will
         * avoid i's initialisation but still run i.close, generating a mysterious
         * NullPointerException. Signed, Mr Burnt Spines.
         */
        ExtendedIterator<OntClass> i = listSuperClasses(true);
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_1_10.coverage", "de37d264-2ed9-4f0a-a84c-a0bb2d9efa05");
        try {
            writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_1_10.coverage", "82d85a20-a5d0-47fa-80e7-748cd3522204");
            while (i.hasNext()) {
                writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_1_10.coverage", "7acf23d4-83ed-4f46-bff6-dc397084dc30");
                Resource sup = i.next();
                writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_1_10.coverage", "9dac681d-6156-403a-a36e-39d2d762e2ae");
                if (!(sup.equals(getProfile().THING()) || sup.equals(RDFS.Resource) || sup.equals(this))) {
                    writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_1_10.coverage", "cbdcb435-a58a-41b0-8dc1-1962f941b125");
                    // a super that indicates this is not a root class
                    return false;
                }
            }
        } finally {
            writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_1_10.coverage", "58d6577c-29c0-40a2-be2e-9f02e4326dff");
            i.close();
        }
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_1_10.coverage", "ab5c74aa-4852-4569-878d-18aa5330966e");
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
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_1_10.coverage", "4057ad1d-75a9-475c-80c3-a40fe0981e81");
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
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_1_10.coverage", "dc5067fc-01e2-4562-9a94-bfbfd5f08be6");
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
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_1_10.coverage", "c1610f90-80d9-48a1-9c69-00667f8c1492");
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
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_1_10.coverage", "8cd1afa7-b0c2-457c-9bff-92e145123698");
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
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_1_10.coverage", "ac86c890-4ccb-4c58-a404-f8300e1ce492");
        return as(Restriction.class);
    }

    // sub-type testing
    /**
     * <p>Answer true if this class is an enumerated class expression</p>
     * @return True if this is an enumerated class expression
     */
    @Override
    public boolean isEnumeratedClass() {
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_1_10.coverage", "a74e9889-2f50-42ea-ba9c-418056722a6b");
        checkProfile(getProfile().ONE_OF(), "ONE_OF");
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_1_10.coverage", "12887015-fcd9-4f52-91f3-e4957d43ab47");
        return hasProperty(getProfile().ONE_OF());
    }

    /**
     * <p>Answer true if this class is a union class expression</p>
     * @return True if this is a union class expression
     */
    @Override
    public boolean isUnionClass() {
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_1_10.coverage", "e0525c6a-2c10-494d-af17-3f1922394797");
        checkProfile(getProfile().UNION_OF(), "UNION_OF");
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_1_10.coverage", "6e251412-25fe-414b-845c-0121fc53607a");
        return hasProperty(getProfile().UNION_OF());
    }

    /**
     * <p>Answer true if this class is an intersection class expression</p>
     * @return True if this is an intersection class expression
     */
    @Override
    public boolean isIntersectionClass() {
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_1_10.coverage", "36c29fad-0fde-4f7f-971c-e1e199c3eaf7");
        checkProfile(getProfile().INTERSECTION_OF(), "INTERSECTION_OF");
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_1_10.coverage", "17c66a3d-7d3e-46fd-9990-587573a62d42");
        return hasProperty(getProfile().INTERSECTION_OF());
    }

    /**
     * <p>Answer true if this class is a complement class expression</p>
     * @return True if this is a complement class expression
     */
    @Override
    public boolean isComplementClass() {
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_1_10.coverage", "147b3cc5-e5c4-44bf-9c78-43a644806713");
        checkProfile(getProfile().COMPLEMENT_OF(), "COMPLEMENT_OF");
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_1_10.coverage", "f74f3808-a702-4f61-bbcd-18951b16b953");
        return hasProperty(getProfile().COMPLEMENT_OF());
    }

    /**
     * <p>Answer true if this class is a property restriction</p>
     * @return True if this is a restriction
     */
    @Override
    public boolean isRestriction() {
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_1_10.coverage", "9d2e50db-2b3d-4352-aafc-053fa8bbfa43");
        checkProfile(getProfile().RESTRICTION(), "RESTRICTION");
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_1_10.coverage", "f802999a-26f5-4ecd-b1be-739b6b3a4890");
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
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_1_10.coverage", "7ea9ef1d-79f5-45ee-ac55-d0e24bc305ca");
        setPropertyValue(getProfile().ONE_OF(), "ONE_OF", individuals);
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_1_10.coverage", "ab1a81d3-28a3-4e06-bc70-32d679b8472e");
        return as(EnumeratedClass.class);
    }

    /**
     * <p>Answer a view of this class as an intersection of the given classes.</p>
     * @param classes A list of the classes that will comprise the operands of the intersection
     * @return This ontology class, converted to an intersection of the given classes
     */
    @Override
    public IntersectionClass convertToIntersectionClass(RDFList classes) {
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_1_10.coverage", "82a06c3d-a8fe-48c8-ad0b-ac5a61624159");
        setPropertyValue(getProfile().INTERSECTION_OF(), "INTERSECTION_OF", classes);
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_1_10.coverage", "af74b625-1e54-43fc-adbc-996dd1c164dc");
        return as(IntersectionClass.class);
    }

    /**
     * <p>Answer a view of this class as a union of the given classes.</p>
     * @param classes A list of the classes that will comprise the operands of the union
     * @return This ontology class, converted to an union of the given classes
     */
    @Override
    public UnionClass convertToUnionClass(RDFList classes) {
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_1_10.coverage", "e7bfab54-a5e6-4625-859a-e1bf8cf46eef");
        setPropertyValue(getProfile().UNION_OF(), "UNION_OF", classes);
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_1_10.coverage", "4e1c89c2-5082-43ed-8b5f-5149b5b929d4");
        return as(UnionClass.class);
    }

    /**
     * <p>Answer a view of this class as an complement of the given class.</p>
     * @param cls An ontology classs that will be operand of the complement
     * @return This ontology class, converted to an complement of the given class
     */
    @Override
    public ComplementClass convertToComplementClass(Resource cls) {
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_1_10.coverage", "c9ed8e2d-a826-4bd7-9e66-f24711fa294a");
        setPropertyValue(getProfile().COMPLEMENT_OF(), "COMPLEMENT_OF", cls);
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_1_10.coverage", "87b376e3-20ed-4229-97d4-10e4de8d7bca");
        return as(ComplementClass.class);
    }

    /**
     * <p>Answer a view of this class as an resriction on the given property.</p>
     * @param prop A property this is the subject of a property restriction class expression
     * @return This ontology class, converted to a restriction on the given property
     */
    @Override
    public Restriction convertToRestriction(Property prop) {
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_1_10.coverage", "cc925605-7f95-49c3-a81c-0d43e915287f");
        if (!hasRDFType(getProfile().RESTRICTION(), "RESTRICTION", false)) {
            writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_1_10.coverage", "df2687c8-614c-441f-9c21-c41e6c5f4495");
            setRDFType(getProfile().RESTRICTION());
        }
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_1_10.coverage", "de71e130-d853-4e3b-957e-680debc0cc42");
        setPropertyValue(getProfile().ON_PROPERTY(), "ON_PROPERTY", prop);
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_1_10.coverage", "b474da88-0db9-4362-b1aa-d1586f464569");
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
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_1_10.coverage", "ce05994e-2d62-4639-8b73-95d286d3261e");
        // we manually compute the maximal lower elements - this could be expensive in general
        // return ResourceUtils.maximalLowerElements( listSuperClasses(), getProfile().SUB_CLASS_OF(), false ).contains( cls );
        ExtendedIterator<OntClass> i = listDirectPropertyValues(getProfile().SUB_CLASS_OF(), "subClassOf", OntClass.class, getProfile().SUB_CLASS_OF(), true, false);
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_1_10.coverage", "3a29bd9a-aebf-48b8-b8c9-f9efb1497222");
        try {
            writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_1_10.coverage", "d4f28732-6b7d-4573-81cd-98ef082a0a90");
            while (i.hasNext()) {
                writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_1_10.coverage", "1e231120-2820-406a-824d-aca1500e79ab");
                if (cls.equals(i.next())) {
                    writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_1_10.coverage", "35eb0706-5734-41a9-bf67-04739964391c");
                    return true;
                }
            }
        } finally {
            writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_1_10.coverage", "2bebaad2-2015-4d34-990a-f37a95230ffc");
            i.close();
        }
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_1_10.coverage", "bb40025d-a630-4f78-8b77-c00b53594ec7");
        return false;
    }

    /**
     * <p>Answer true if this class lies with the domain of p<p>
     * @param p
     * @param direct If true, only consider direct associations with domain
     * @return True if this class in the domain of property <code>p</code>
     */
    protected boolean testDomain(Property p, boolean direct) {
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_1_10.coverage", "f6abee2a-a020-4173-95a0-a5b8d6e7b93e");
        // we ignore any property in the OWL, etc namespace
        String namespace = p.getNameSpace();
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_1_10.coverage", "d85fe106-b081-44e0-bace-944ed3e9bbe5");
        for (String IGNORE_NAMESPACE : IGNORE_NAMESPACES) {
            writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_1_10.coverage", "80635967-5f9f-4536-b6b7-69f8b590f7a2");
            if (namespace.equals(IGNORE_NAMESPACE)) {
                writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_1_10.coverage", "56597438-4089-4b05-a6d2-2785e52fa7ac");
                return false;
            }
        }
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_1_10.coverage", "ad799cd2-5cf7-4850-8ba6-9ddcc417190a");
        // check for global props, that have no specific domain constraint
        boolean isGlobal = true;
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_1_10.coverage", "3bd37764-342a-4450-a0db-2e9f790b6216");
        // flag for detecting the direct case
        boolean seenDirect = false;
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_1_10.coverage", "5cc3eade-d27e-430f-bc61-c1bef659a435");
        for (StmtIterator i = getModel().listStatements(p, getProfile().DOMAIN(), (RDFNode) null); i.hasNext(); ) {
            writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_1_10.coverage", "bf5cb4e4-f743-499f-a9f1-79d3983509b1");
            Resource domain = i.nextStatement().getResource();
            writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_1_10.coverage", "154523e7-48c2-4ce4-8232-354c2f84cf14");
            // there are some well-known values we ignore
            if (!(domain.equals(getProfile().THING()) || domain.equals(RDFS.Resource))) {
                writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_1_10.coverage", "4f660e14-ae62-4447-bd62-fbcf685a3bc4");
                // not a generic domain
                isGlobal = false;
                writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_1_10.coverage", "564e4d41-36b9-4028-ae8d-5c8197dfbadb");
                if (domain.equals(this)) {
                    writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_1_10.coverage", "18740f22-df20-47d5-8447-216bdc60d8e1");
                    // if this class is actually in the domain (as opposed to one of this class's
                    // super-classes), then we've detected the direct property case
                    seenDirect = true;
                } else if (!canProveSuperClass(domain)) {
                    writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_1_10.coverage", "bfd0a387-c323-43e8-aa9e-88e2bc62ef0f");
                    // there is a class in the domain of p that is not a super-class of this class
                    return false;
                }
            }
        }
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_1_10.coverage", "aacfb382-80aa-4fd8-bda8-db4f839c45ec");
        if (direct) {
            writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_1_10.coverage", "a629485b-5a1f-45a0-b9b7-2af031ea4f09");
            // or it's a global prop and this is a root class
            return seenDirect || (isGlobal && isHierarchyRoot());
        } else {
            writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_1_10.coverage", "b47fd984-f42e-4f19-9e25-a7ba82168378");
            // otherwise the 'return false' above would have kicked in
            return true;
        }
    }

    /**
     * <p>Answer an iterator over all of the properties in this model
     * @return An iterator over {@link OntProperty}
     */
    protected ExtendedIterator<Statement> listAllProperties() {
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_1_10.coverage", "5c09dcb8-75bc-4b2c-a6be-8c6fe6758efc");
        OntModel mOnt = (OntModel) getModel();
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_1_10.coverage", "8bc26fb4-53bd-4ba5-854a-ae1c01992318");
        Profile prof = mOnt.getProfile();
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_1_10.coverage", "8af08edb-f752-4ae1-874e-b48d6e187c6a");
        ExtendedIterator<Statement> pi = mOnt.listStatements(null, RDF.type, getProfile().PROPERTY());
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_1_10.coverage", "d7e6a787-bede-4c6f-8cd2-43d5a339a1a7");
        // check reasoner capabilities - major performance improvement for inf models
        if (mOnt.getReasoner() != null) {
            writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_1_10.coverage", "ea6cf679-6994-4683-bf0e-9dcf0a2a53b4");
            Model caps = mOnt.getReasoner().getReasonerCapabilities();
            writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_1_10.coverage", "0472e24c-bc7f-46aa-9e02-5d1676e7bf92");
            if (caps.contains(null, ReasonerVocabulary.supportsP, OWL.ObjectProperty)) {
                writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_1_10.coverage", "1213e89e-c40a-496c-be22-954212563f8e");
                // all owl:ObjectProperty, owl:DatatypeProperty, etc, are rdf:Property resources
                return pi;
            }
        }
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_1_10.coverage", "13e0965a-2cc5-4670-b7e5-8e5d7baecd3f");
        // otherwise, we manually check the other property types
        if (prof.OBJECT_PROPERTY() != null) {
            writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_1_10.coverage", "2f4c0215-de99-448c-bebd-e7f507dfcf9e");
            pi = pi.andThen(mOnt.listStatements(null, RDF.type, prof.OBJECT_PROPERTY()));
        }
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_1_10.coverage", "04430c53-f903-4f58-a2cb-31e1257b5315");
        if (prof.DATATYPE_PROPERTY() != null) {
            writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_1_10.coverage", "ae9ac74c-31d8-4e42-a29f-7ccf0b81b53c");
            pi = pi.andThen(mOnt.listStatements(null, RDF.type, prof.DATATYPE_PROPERTY()));
        }
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_1_10.coverage", "a9ddda22-be20-4afc-adcc-acc7e018f30f");
        if (prof.FUNCTIONAL_PROPERTY() != null) {
            writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_1_10.coverage", "9690f537-ed24-4a87-9471-05d4af91a706");
            pi = pi.andThen(mOnt.listStatements(null, RDF.type, prof.FUNCTIONAL_PROPERTY()));
        }
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_1_10.coverage", "7584be7d-efca-4426-b854-47d2afee4673");
        if (prof.INVERSE_FUNCTIONAL_PROPERTY() != null) {
            writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_1_10.coverage", "5f962820-f5da-47a8-a167-0fb5c32e8ee3");
            pi = pi.andThen(mOnt.listStatements(null, RDF.type, prof.INVERSE_FUNCTIONAL_PROPERTY()));
        }
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_1_10.coverage", "b32ff0a0-9ea4-4e2a-89a3-1c4dd95aa99a");
        if (prof.SYMMETRIC_PROPERTY() != null) {
            writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_1_10.coverage", "a7653c0f-3d42-4fe0-bb25-9833215006e1");
            pi = pi.andThen(mOnt.listStatements(null, RDF.type, prof.SYMMETRIC_PROPERTY()));
        }
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_1_10.coverage", "875994a4-1e0d-44ec-aef8-51196b3fe056");
        if (prof.TRANSITIVE_PROPERTY() != null) {
            writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_1_10.coverage", "a530f603-728a-41fe-b35e-02a1efbb0088");
            pi = pi.andThen(mOnt.listStatements(null, RDF.type, prof.TRANSITIVE_PROPERTY()));
        }
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_1_10.coverage", "b1ee10ce-174b-4a8b-8cfa-d74b3328eecc");
        if (prof.ANNOTATION_PROPERTY() != null) {
            writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_1_10.coverage", "bc8588f7-3cb3-4913-98e2-275cade364c0");
            pi = pi.andThen(mOnt.listStatements(null, RDF.type, prof.ANNOTATION_PROPERTY()));
        }
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_1_10.coverage", "275a4f8a-5eb2-4286-86ef-b938745f50aa");
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
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_1_10.coverage", "e62d0282-6dab-424e-b5c7-a6617eb48dd3");
        OntModel om = (OntModel) getModel();
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_1_10.coverage", "78af1457-433f-4819-a3cb-8e6c475536fe");
        if (om.getReasoner() != null) {
            writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_1_10.coverage", "5aeadab4-4c60-4f6e-b129-9a00bf30b385");
            if (om.getReasoner().getReasonerCapabilities().contains(null, ReasonerVocabulary.supportsP, RDFS.subClassOf)) {
                writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_1_10.coverage", "fb8c3120-d957-4b50-820d-127576a4813d");
                // this reasoner does transitive closure on sub-classes, so we just ask
                return hasSuperClass(sup);
            }
        }
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_1_10.coverage", "04173176-43bc-4784-bf55-26e0031012b5");
        // otherwise, we have to search upwards through the class hierarchy
        Set<OntClass> seen = new HashSet<>();
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_1_10.coverage", "9fba247c-6616-4a2c-a7e0-43d4a65f0b05");
        List<OntClass> queue = new ArrayList<>();
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_1_10.coverage", "acfcae94-5931-456e-b1c8-e96ce7d8e2f9");
        queue.add(this);
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_1_10.coverage", "92ffef1c-c28e-4703-89cc-1ac8673f26ca");
        while (!queue.isEmpty()) {
            writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_1_10.coverage", "880ec749-5d0a-45b6-98e9-cac4794ecca1");
            OntClass c = queue.remove(0);
            writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_1_10.coverage", "df10cdfe-71fc-4b32-8417-1e5f1e7c9c39");
            if (!seen.contains(c)) {
                writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_1_10.coverage", "452a8ffa-64d1-46c2-8986-27a6375d4a3c");
                seen.add(c);
                writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_1_10.coverage", "bab7eebb-69b1-4f8b-9ff1-8e8e7c2c8ee0");
                if (c.equals(sup)) {
                    writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_1_10.coverage", "a5fbd298-8eb2-4871-9b1b-f64c702728cc");
                    // found the super class
                    return true;
                } else {
                    writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_1_10.coverage", "513a426e-f44e-4568-a22a-ca7ddc736eaf");
                    // queue the supers
                    for (Iterator<OntClass> i = c.listSuperClasses(); i.hasNext(); ) {
                        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_1_10.coverage", "284a3208-1c65-4575-91a5-7c120dd1591c");
                        queue.add(i.next());
                    }
                }
            }
        }
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_1_10.coverage", "52aa846a-2eac-4d37-997b-39a7739491fe");
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
