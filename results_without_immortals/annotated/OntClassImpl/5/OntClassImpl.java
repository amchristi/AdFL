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
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_5_10.coverage", "a64add63-20f7-4966-a00f-9ab7bbf1a104");
        setPropertyValue(getProfile().SUB_CLASS_OF(), "SUB_CLASS_OF", cls);
    }

    /**
     * <p>Add a super-class of this class.</p>
     * @param cls A class that is a super-class of this class.
     * @exception ProfileException If the {@link Profile#SUB_CLASS_OF()} property is not supported in the current language profile.
     */
    @Override
    public void addSuperClass(Resource cls) {
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_5_10.coverage", "9430a41d-b07e-42e3-8a0c-810aaf865acf");
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
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_5_10.coverage", "91684905-01ee-48ff-a3c4-fd58b20cf1e6");
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
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_5_10.coverage", "cc3df908-222b-4181-8e77-1366e8c7cf43");
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
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_5_10.coverage", "eefc1cd7-0e36-4b99-9610-4316b0a5f9e2");
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
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_5_10.coverage", "7e8438eb-8e0f-4d5b-b911-0aef65446b09");
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
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_5_10.coverage", "d6dff433-3f01-416c-b49d-b4c98d582767");
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
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_5_10.coverage", "72c762c1-3321-4f70-bb7a-5094c626fc5c");
        if (!direct) {
            writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_5_10.coverage", "cafd180f-e625-4998-91ce-90e38f8483b7");
            // don't need any special case, we just get the property
            return hasPropertyValue(getProfile().SUB_CLASS_OF(), "SUB_CLASS_OF", cls);
        } else {
            writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_5_10.coverage", "c831458d-f3d5-4d06-8c4d-c22e6899a6b5");
            // we want the direct, not general relationship
            // first try to find an inf graph that can do the work for us
            InfGraph ig = null;
            writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_5_10.coverage", "129f6d5a-60dd-4e7f-bc85-43da2485c7d0");
            if (getGraph() instanceof InfGraph) {
                writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_5_10.coverage", "ef5951fa-ba8a-49fa-9a26-d7ed4e210285");
                ig = (InfGraph) getGraph();
            } else if (getGraph() instanceof OntModel) {
                writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_5_10.coverage", "31f5adf7-1234-4f3e-8475-483f2e6086b0");
                OntModel m = (OntModel) getGraph();
                writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_5_10.coverage", "99e55049-014e-4770-9159-9cd6579854ba");
                if (m.getGraph() instanceof InfGraph) {
                    writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_5_10.coverage", "f633292a-e29e-4b40-97ea-e102648f7f75");
                    ig = (InfGraph) m.getGraph();
                }
            }
            writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_5_10.coverage", "fc6e5d8e-92f3-4078-bd55-497380546c55");
            if (ig != null && ig.getReasoner().supportsProperty(ReasonerVocabulary.directSubClassOf)) {
                writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_5_10.coverage", "1c95b3c1-b6d3-4108-8fc6-bd057c27ac62");
                // we can look this up directly
                return hasPropertyValue(ReasonerVocabulary.directSubClassOf, "direct sub-class", cls);
            } else {
                writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_5_10.coverage", "eafdca75-e698-41a5-ac7b-522fcaca3d2d");
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
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_5_10.coverage", "9e7e0555-f5c9-44c5-96d2-1d7d60d0183e");
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
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_5_10.coverage", "a4269ee8-d370-49c6-b806-ab38b53c8c07");
        // first we have to remove all of the inverse sub-class links
        checkProfile(getProfile().SUB_CLASS_OF(), "SUB_CLASS_OF");
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_5_10.coverage", "f78f6baf-ed65-44c3-9e75-b05b023f0e78");
        for (StmtIterator i = getModel().listStatements(null, getProfile().SUB_CLASS_OF(), this); i.hasNext(); ) {
            writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_5_10.coverage", "6eed7a65-067d-4308-8be8-84b109fec758");
            i.removeNext();
        }
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_5_10.coverage", "c87658e5-76f3-49a6-9074-ec84ca8aad19");
        cls.as(OntClass.class).addSuperClass(this);
    }

    /**
     * <p>Add a sub-class of this class.</p>
     * @param cls A class that is a sub-class of this class.
     * @exception ProfileException If the {@link Profile#SUB_CLASS_OF()} property is not supported in the current language profile.
     */
    @Override
    public void addSubClass(Resource cls) {
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_5_10.coverage", "7fc80e20-253d-438b-88c6-f662daba0663");
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
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_5_10.coverage", "7178452f-3c3b-41b2-8bd4-5a5d1ec684e3");
        checkProfile(getProfile().SUB_CLASS_OF(), "SUB_CLASS_OF");
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_5_10.coverage", "ca46afb0-e07d-4620-8875-28af35c8105c");
        StmtIterator i = getModel().listStatements(null, getProfile().SUB_CLASS_OF(), this);
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_5_10.coverage", "ebff78dd-7133-43e0-a236-87c6e56f142f");
        try {
            writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_5_10.coverage", "ec74242e-c1d6-402e-8e73-f939d29a99ab");
            if (i.hasNext()) {
                writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_5_10.coverage", "002b2297-461b-46a6-997b-18cb954c3549");
                return i.nextStatement().getSubject().as(OntClass.class);
            } else {
                writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_5_10.coverage", "338af9a9-3c94-4fcd-be7e-431501b7b469");
                return null;
            }
        } finally {
            writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_5_10.coverage", "48b41d26-0d0e-400e-b5e0-198821e536dc");
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
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_5_10.coverage", "3f520e56-fb04-4572-8f96-98c77ad677df");
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
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_5_10.coverage", "575524e1-cdd6-42ad-9521-bf7d799930c8");
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
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_5_10.coverage", "f7e43e6f-e8fd-4629-a167-95811deeb8b8");
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
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_5_10.coverage", "51c1b14f-b79f-4ad1-b542-5b37a0023280");
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
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_5_10.coverage", "dfde62ba-87f7-4b5c-a042-2eca8b95f6e0");
        if (getModel() instanceof OntModel && (cls.getModel() == null || !(cls.getModel() instanceof OntModel))) {
            writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_5_10.coverage", "9bd5a0f5-9c09-4eb3-bf52-0159c30401d3");
            // could be outside an ontmodel if a constant
            cls = cls.inModel(getModel());
        }
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_5_10.coverage", "bda7154a-41d9-4512-a135-263a0b628821");
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
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_5_10.coverage", "66cc63cf-b83c-4941-82cf-df37c44d9e69");
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
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_5_10.coverage", "4a7175da-1e92-44e5-8c8c-d5fc985720a5");
        setPropertyValue(getProfile().EQUIVALENT_CLASS(), "EQUIVALENT_CLASS", cls);
    }

    /**
     * <p>Add a class that is equivalent to this class.</p>
     * @param cls A class that is equivalent to this class.
     * @exception ProfileException If the {@link Profile#EQUIVALENT_CLASS()} property is not supported in the current language profile.
     */
    @Override
    public void addEquivalentClass(Resource cls) {
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_5_10.coverage", "07c4a288-c4ab-449e-93f3-3fc085aeeae5");
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
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_5_10.coverage", "0517a9c6-a550-4618-a88e-45655535e24b");
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
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_5_10.coverage", "eda3b859-7ad3-40c0-b207-38d7aa1c6a7a");
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
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_5_10.coverage", "dc77a0f2-060a-481d-9970-fbdb36a45602");
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
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_5_10.coverage", "d0928fac-784e-4aac-b110-934284b3ef54");
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
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_5_10.coverage", "ba4fd9d3-01ac-4179-bbc6-7093bcef7a75");
        setPropertyValue(getProfile().DISJOINT_WITH(), "DISJOINT_WITH", cls);
    }

    /**
     * <p>Add a class that this class is disjoint with.</p>
     * @param cls A class that has no instances in common with this class.
     * @exception ProfileException If the {@link Profile#DISJOINT_WITH()} property is not supported in the current language profile.
     */
    @Override
    public void addDisjointWith(Resource cls) {
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_5_10.coverage", "5dd55540-e878-4072-992d-a65f0dad5902");
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
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_5_10.coverage", "8b4faeb5-eaab-45dc-bd7d-64a583a2db0e");
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
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_5_10.coverage", "7c4cdf73-4c56-45ae-b8ec-14f7f6096f77");
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
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_5_10.coverage", "9a8a3029-21ef-4029-9754-67a3d3c35bfa");
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
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_5_10.coverage", "cb49ea34-f1ce-45f6-8a55-200c7d4d89c6");
        removePropertyValue(getProfile().DISJOINT_WITH(), "DISJOINT_WITH", cls);
    }

    // other utility methods
    /**
     * Equivalent to calling {@link #listDeclaredProperties(boolean)} with
     * default value <code>direct = false</code>.
     */
    @Override
    public ExtendedIterator<OntProperty> listDeclaredProperties() {
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_5_10.coverage", "70153760-855b-4daf-a0fc-fe8e40c73def");
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
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_5_10.coverage", "2ac8662c-2c4b-413a-9e8f-ec6b4f3044e9");
        // first collect the candidate properties
        Set<RDFNode> candSet = new HashSet<>();
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_5_10.coverage", "e90406ce-d616-46da-9031-ebdcc5b59a00");
        // than a non-inference model
        for (Iterator<Statement> i = listAllProperties(); i.hasNext(); ) {
            writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_5_10.coverage", "24f2fb7c-8d44-418b-8bc0-dc05538cbd6d");
            candSet.add(i.next().getSubject().as(Property.class));
        }
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_5_10.coverage", "b349c24c-bf86-4f93-8182-113bb3c8d6ca");
        // now we iterate over the candidates and check that they match all domain constraints
        List<RDFNode> cands = new ArrayList<>();
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_5_10.coverage", "b413f02f-3904-4613-9585-58e9f05797ad");
        cands.addAll(candSet);
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_5_10.coverage", "556f7ef0-ee44-449c-98ec-bdb00dad2ff9");
        for (int j = cands.size() - 1; j >= 0; j--) {
            writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_5_10.coverage", "9e78da0b-4622-4cac-abf7-77389fd0c5e5");
            Property cand = (Property) cands.get(j);
            writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_5_10.coverage", "cea4bb0e-35a1-4781-821a-03fa5329159e");
            if (!hasDeclaredProperty(cand, direct)) {
                writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_5_10.coverage", "1b39e4b3-4d3a-4df2-ad20-7bfabee16523");
                cands.remove(j);
            }
        }
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_5_10.coverage", "6f8354a2-8c7a-43cf-8992-84d00b83392d");
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
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_5_10.coverage", "9a671725-dc90-4e35-9d30-822905604445");
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
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_5_10.coverage", "8b0bc620-df96-485a-8455-9a4dea6311c0");
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
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_5_10.coverage", "7eb0960d-03a6-48d5-91fe-b293721649c3");
        return getModel().listStatements(null, RDF.type, this).mapWith(s -> s.getSubject().as(Individual.class)).filterKeep(o -> o.hasRDFType(OntClassImpl.this, direct)).filterKeep(new UniqueFilter<Individual>());
    }

    /**
     * <p>Answer a new individual that has this class as its <code>rdf:type</code></p>
     * @return A new anonymous individual that is an instance of this class
     */
    @Override
    public Individual createIndividual() {
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_5_10.coverage", "05f6534c-2a81-4ff2-a1c0-ae97033d85ee");
        return ((OntModel) getModel()).createIndividual(this);
    }

    /**
     * <p>Answer a new individual that has this class as its <code>rdf:type</code></p>
     * @param uri The URI of the new individual
     * @return A new named individual that is an instance of this class
     */
    @Override
    public Individual createIndividual(String uri) {
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_5_10.coverage", "481364ee-5440-4d21-a0a8-58f5afc29ae0");
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
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_5_10.coverage", "4c493ecc-020e-464f-ac30-8d4a24f1bda1");
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
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_5_10.coverage", "c8beee97-4d6f-42d2-9b76-6353cc0b4f19");
        // sanity check - :Nothing is never a root class
        if (equals(getProfile().NOTHING())) {
            writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_5_10.coverage", "84f9b220-d602-46f6-867b-fe415ba5c8cd");
            return false;
        }
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_5_10.coverage", "2bbdce5f-b691-4902-9644-600047a54a67");
        // the only super-classes of a root class are the various aliases
        // of Top, or itself
        /**
         * Note: moved the initialisation of i outside the try-catch, otherwise an
         * exception in listSuperClasses [eg a broken Graph implementation] will
         * avoid i's initialisation but still run i.close, generating a mysterious
         * NullPointerException. Signed, Mr Burnt Spines.
         */
        ExtendedIterator<OntClass> i = listSuperClasses(true);
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_5_10.coverage", "3133c86e-cda9-4fc7-acba-bcf643c72767");
        try {
            writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_5_10.coverage", "ebe523cb-9c4a-40e1-88ec-b780f50765a2");
            while (i.hasNext()) {
                writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_5_10.coverage", "078d3fcd-22bd-4225-ae05-8143b1957ec1");
                Resource sup = i.next();
                writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_5_10.coverage", "d3f12199-3904-4fa4-819f-91afae5e9ccb");
                if (!(sup.equals(getProfile().THING()) || sup.equals(RDFS.Resource) || sup.equals(this))) {
                    writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_5_10.coverage", "b13b680c-f569-41ae-943c-82c3a89e6896");
                    // a super that indicates this is not a root class
                    return false;
                }
            }
        } finally {
            writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_5_10.coverage", "053abf2f-dfa3-4952-bb26-98c36d27eac2");
            i.close();
        }
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_5_10.coverage", "ea1ce0f3-054f-441c-b031-e736d6909114");
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
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_5_10.coverage", "dd560389-367f-4f28-8eb5-6b80bfc5511c");
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
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_5_10.coverage", "d081cf0c-ef90-4d54-892f-75eb91d2c2c6");
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
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_5_10.coverage", "757c7948-b046-46a2-8da0-4a355ec95c0d");
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
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_5_10.coverage", "33bcbf1e-dbe7-4229-b5a6-405606f08e6a");
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
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_5_10.coverage", "b93f431e-116f-40c6-ae58-c14718e87a79");
        return as(Restriction.class);
    }

    // sub-type testing
    /**
     * <p>Answer true if this class is an enumerated class expression</p>
     * @return True if this is an enumerated class expression
     */
    @Override
    public boolean isEnumeratedClass() {
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_5_10.coverage", "36b1e2cb-d8bd-4a61-b2ca-801da319319c");
        checkProfile(getProfile().ONE_OF(), "ONE_OF");
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_5_10.coverage", "ccf178a2-e1cd-43e1-a785-a34e9bbf823b");
        return hasProperty(getProfile().ONE_OF());
    }

    /**
     * <p>Answer true if this class is a union class expression</p>
     * @return True if this is a union class expression
     */
    @Override
    public boolean isUnionClass() {
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_5_10.coverage", "5d273f6c-23e2-44aa-b9d9-d5be353f781f");
        checkProfile(getProfile().UNION_OF(), "UNION_OF");
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_5_10.coverage", "245c6291-1e00-4452-b436-32165faa92e2");
        return hasProperty(getProfile().UNION_OF());
    }

    /**
     * <p>Answer true if this class is an intersection class expression</p>
     * @return True if this is an intersection class expression
     */
    @Override
    public boolean isIntersectionClass() {
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_5_10.coverage", "5803e8ee-2b9a-4513-b860-3698db70fc90");
        checkProfile(getProfile().INTERSECTION_OF(), "INTERSECTION_OF");
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_5_10.coverage", "321f2c31-6fc5-4691-89fc-33024e24bb98");
        return hasProperty(getProfile().INTERSECTION_OF());
    }

    /**
     * <p>Answer true if this class is a complement class expression</p>
     * @return True if this is a complement class expression
     */
    @Override
    public boolean isComplementClass() {
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_5_10.coverage", "4d5a13d6-c170-48b9-9ebf-22261e978c80");
        checkProfile(getProfile().COMPLEMENT_OF(), "COMPLEMENT_OF");
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_5_10.coverage", "f1665399-1721-47a3-913c-0c7d273b25d2");
        return hasProperty(getProfile().COMPLEMENT_OF());
    }

    /**
     * <p>Answer true if this class is a property restriction</p>
     * @return True if this is a restriction
     */
    @Override
    public boolean isRestriction() {
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_5_10.coverage", "4086e377-7c2a-414c-b226-9213348d6334");
        checkProfile(getProfile().RESTRICTION(), "RESTRICTION");
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_5_10.coverage", "82420a29-fd9d-4697-8025-4be029d80de0");
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
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_5_10.coverage", "34263431-7093-4b9f-9ac0-96ab6fd905c4");
        setPropertyValue(getProfile().ONE_OF(), "ONE_OF", individuals);
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_5_10.coverage", "1280bfea-bfcc-45ef-8280-bd78254bd0d6");
        return as(EnumeratedClass.class);
    }

    /**
     * <p>Answer a view of this class as an intersection of the given classes.</p>
     * @param classes A list of the classes that will comprise the operands of the intersection
     * @return This ontology class, converted to an intersection of the given classes
     */
    @Override
    public IntersectionClass convertToIntersectionClass(RDFList classes) {
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_5_10.coverage", "41c7a9af-b261-4121-be42-d1dc355436b6");
        setPropertyValue(getProfile().INTERSECTION_OF(), "INTERSECTION_OF", classes);
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_5_10.coverage", "08988457-03e5-46c1-97e0-927f33455bb9");
        return as(IntersectionClass.class);
    }

    /**
     * <p>Answer a view of this class as a union of the given classes.</p>
     * @param classes A list of the classes that will comprise the operands of the union
     * @return This ontology class, converted to an union of the given classes
     */
    @Override
    public UnionClass convertToUnionClass(RDFList classes) {
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_5_10.coverage", "50802b05-dd74-4709-a30c-c79480f39a5a");
        setPropertyValue(getProfile().UNION_OF(), "UNION_OF", classes);
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_5_10.coverage", "ac19ce20-6797-41d4-b513-4807bdbad3b3");
        return as(UnionClass.class);
    }

    /**
     * <p>Answer a view of this class as an complement of the given class.</p>
     * @param cls An ontology classs that will be operand of the complement
     * @return This ontology class, converted to an complement of the given class
     */
    @Override
    public ComplementClass convertToComplementClass(Resource cls) {
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_5_10.coverage", "b85fb928-3f6a-409d-aa3b-ed727670c954");
        setPropertyValue(getProfile().COMPLEMENT_OF(), "COMPLEMENT_OF", cls);
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_5_10.coverage", "66098ae8-5221-4cd9-b0a0-fafceeca4edb");
        return as(ComplementClass.class);
    }

    /**
     * <p>Answer a view of this class as an resriction on the given property.</p>
     * @param prop A property this is the subject of a property restriction class expression
     * @return This ontology class, converted to a restriction on the given property
     */
    @Override
    public Restriction convertToRestriction(Property prop) {
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_5_10.coverage", "b9ced682-b141-4f3a-a5e6-a388fc07da34");
        if (!hasRDFType(getProfile().RESTRICTION(), "RESTRICTION", false)) {
            writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_5_10.coverage", "b81a5a0d-cce7-4979-a969-04c6daa025a0");
            setRDFType(getProfile().RESTRICTION());
        }
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_5_10.coverage", "60d192ad-5621-4693-88eb-317a8b1b1bd8");
        setPropertyValue(getProfile().ON_PROPERTY(), "ON_PROPERTY", prop);
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_5_10.coverage", "cc457ec8-c011-487f-84d5-ca59346c9867");
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
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_5_10.coverage", "0edf26eb-5b8d-4bf1-aa90-d26588061086");
        // we manually compute the maximal lower elements - this could be expensive in general
        // return ResourceUtils.maximalLowerElements( listSuperClasses(), getProfile().SUB_CLASS_OF(), false ).contains( cls );
        ExtendedIterator<OntClass> i = listDirectPropertyValues(getProfile().SUB_CLASS_OF(), "subClassOf", OntClass.class, getProfile().SUB_CLASS_OF(), true, false);
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_5_10.coverage", "c2b51035-bd81-4b14-8da1-62f179488599");
        try {
            writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_5_10.coverage", "30aebd33-beac-4511-a81f-dda4c5820706");
            while (i.hasNext()) {
                writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_5_10.coverage", "a5570f41-f453-4d3b-97bf-38905b0fea98");
                if (cls.equals(i.next())) {
                    writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_5_10.coverage", "ab073d75-df9f-4e57-ae10-2f6ac0291371");
                    return true;
                }
            }
        } finally {
            writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_5_10.coverage", "7dab24ed-74fa-4ecd-9efe-d13bb93badbe");
            i.close();
        }
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_5_10.coverage", "ebe325dc-fc10-40a9-9cff-32379db9069f");
        return false;
    }

    /**
     * <p>Answer true if this class lies with the domain of p<p>
     * @param p
     * @param direct If true, only consider direct associations with domain
     * @return True if this class in the domain of property <code>p</code>
     */
    protected boolean testDomain(Property p, boolean direct) {
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_5_10.coverage", "6a2618b6-bb99-48a7-bbe3-711260ee9a77");
        // we ignore any property in the OWL, etc namespace
        String namespace = p.getNameSpace();
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_5_10.coverage", "9276207f-8c61-45bf-bdd1-659673a639b5");
        for (String IGNORE_NAMESPACE : IGNORE_NAMESPACES) {
            writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_5_10.coverage", "c44e2261-7f7d-497a-ad07-5376650fdd4e");
            if (namespace.equals(IGNORE_NAMESPACE)) {
                writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_5_10.coverage", "726fd98b-8049-4b21-8aa1-30c48b529526");
                return false;
            }
        }
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_5_10.coverage", "00f8326f-f2a6-4edf-b98f-1f0f3582da50");
        // check for global props, that have no specific domain constraint
        boolean isGlobal = true;
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_5_10.coverage", "2a537222-893c-4658-943c-d54475e4115a");
        // flag for detecting the direct case
        boolean seenDirect = false;
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_5_10.coverage", "bb8e1e89-d38a-43b9-a399-a9e1626b775e");
        for (StmtIterator i = getModel().listStatements(p, getProfile().DOMAIN(), (RDFNode) null); i.hasNext(); ) {
            writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_5_10.coverage", "e24bfea1-1cb2-453c-88e6-8aa6df2e1946");
            Resource domain = i.nextStatement().getResource();
            writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_5_10.coverage", "7e4997ba-9451-4146-8eda-f297fde09ed0");
            // there are some well-known values we ignore
            if (!(domain.equals(getProfile().THING()) || domain.equals(RDFS.Resource))) {
                writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_5_10.coverage", "041c9f64-971f-4b1f-942c-10754578d5f9");
                // not a generic domain
                isGlobal = false;
                writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_5_10.coverage", "babeebe2-b60c-4d03-91ab-349960f1c6f8");
                if (domain.equals(this)) {
                    writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_5_10.coverage", "d71789ec-c29e-4c9f-96f1-b1fcf300277e");
                    // if this class is actually in the domain (as opposed to one of this class's
                    // super-classes), then we've detected the direct property case
                    seenDirect = true;
                } else if (!canProveSuperClass(domain)) {
                    writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_5_10.coverage", "c35d23b6-03e1-423c-8fde-6ed2468671d5");
                    // there is a class in the domain of p that is not a super-class of this class
                    return false;
                }
            }
        }
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_5_10.coverage", "308b54e8-88b9-4992-bdd9-34fe180b7e23");
        if (direct) {
            writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_5_10.coverage", "ee7edfa6-037e-4130-bfa8-ae824405c36d");
            // or it's a global prop and this is a root class
            return seenDirect || (isGlobal && isHierarchyRoot());
        } else {
            writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_5_10.coverage", "fa8c6663-8d15-48c4-82c3-c404e62cd1a1");
            // otherwise the 'return false' above would have kicked in
            return true;
        }
    }

    /**
     * <p>Answer an iterator over all of the properties in this model
     * @return An iterator over {@link OntProperty}
     */
    protected ExtendedIterator<Statement> listAllProperties() {
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_5_10.coverage", "cd012b56-914b-4151-90c5-11b7a2875cf6");
        OntModel mOnt = (OntModel) getModel();
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_5_10.coverage", "a1c3c9a5-1bad-47db-ae6d-b6ecda9d3260");
        Profile prof = mOnt.getProfile();
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_5_10.coverage", "2046c06c-0de2-496d-9811-d6218cbf9522");
        ExtendedIterator<Statement> pi = mOnt.listStatements(null, RDF.type, getProfile().PROPERTY());
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_5_10.coverage", "d33baece-95d3-4165-beae-46da8ec7a94f");
        // check reasoner capabilities - major performance improvement for inf models
        if (mOnt.getReasoner() != null) {
            writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_5_10.coverage", "9a03c19d-2da8-4832-a896-c0859a123c63");
            Model caps = mOnt.getReasoner().getReasonerCapabilities();
            writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_5_10.coverage", "089ab415-cdd6-4415-80d2-6c6fe0d7aa0f");
            if (caps.contains(null, ReasonerVocabulary.supportsP, OWL.ObjectProperty)) {
                writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_5_10.coverage", "b3519dec-7842-4984-a6be-8394f0af9f55");
                // all owl:ObjectProperty, owl:DatatypeProperty, etc, are rdf:Property resources
                return pi;
            }
        }
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_5_10.coverage", "0ecd2f41-db4f-4866-ae03-b73c3f93c7d1");
        // otherwise, we manually check the other property types
        if (prof.OBJECT_PROPERTY() != null) {
            writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_5_10.coverage", "811eb5a4-67ed-458e-8138-49dda6606862");
            pi = pi.andThen(mOnt.listStatements(null, RDF.type, prof.OBJECT_PROPERTY()));
        }
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_5_10.coverage", "2536690a-886f-48b8-9dea-d2aa497fb6ea");
        if (prof.DATATYPE_PROPERTY() != null) {
            writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_5_10.coverage", "77590bcc-1643-45ce-ab0f-d65449cd02c4");
            pi = pi.andThen(mOnt.listStatements(null, RDF.type, prof.DATATYPE_PROPERTY()));
        }
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_5_10.coverage", "a75e91a6-f0f5-4e7e-b6d4-e4e1d893534d");
        if (prof.FUNCTIONAL_PROPERTY() != null) {
            writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_5_10.coverage", "5d5ff80c-cc02-409b-980e-bc20809f031f");
            pi = pi.andThen(mOnt.listStatements(null, RDF.type, prof.FUNCTIONAL_PROPERTY()));
        }
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_5_10.coverage", "e00ff7de-73b6-400b-9bbd-2dad3cceaf17");
        if (prof.INVERSE_FUNCTIONAL_PROPERTY() != null) {
            writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_5_10.coverage", "172fca8a-df71-4b1a-9df0-e833d27b2c82");
            pi = pi.andThen(mOnt.listStatements(null, RDF.type, prof.INVERSE_FUNCTIONAL_PROPERTY()));
        }
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_5_10.coverage", "d7cd372b-f111-4234-af3e-f73a0df61de4");
        if (prof.SYMMETRIC_PROPERTY() != null) {
            writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_5_10.coverage", "33668a6b-6057-4400-9e5b-b75fe84ee00b");
            pi = pi.andThen(mOnt.listStatements(null, RDF.type, prof.SYMMETRIC_PROPERTY()));
        }
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_5_10.coverage", "96b93f06-5c8b-4c15-905b-ece814c04a3f");
        if (prof.TRANSITIVE_PROPERTY() != null) {
            writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_5_10.coverage", "13cca2f6-6827-4a30-80bf-a5668348d4ba");
            pi = pi.andThen(mOnt.listStatements(null, RDF.type, prof.TRANSITIVE_PROPERTY()));
        }
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_5_10.coverage", "a91899a2-4563-4fe8-a401-0d9d8b415bea");
        if (prof.ANNOTATION_PROPERTY() != null) {
            writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_5_10.coverage", "06db041e-d844-4bcf-9aa7-a443b83bd10e");
            pi = pi.andThen(mOnt.listStatements(null, RDF.type, prof.ANNOTATION_PROPERTY()));
        }
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_5_10.coverage", "8caddc65-16f3-4abb-b627-dea1b0041ca0");
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
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_5_10.coverage", "1c9373f0-a27c-4217-bfb8-7c323f595677");
        OntModel om = (OntModel) getModel();
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_5_10.coverage", "51a407cc-73fb-4a68-8086-17ba0be7d00c");
        if (om.getReasoner() != null) {
            writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_5_10.coverage", "495a1c0f-1691-4313-a473-8e110803606b");
            if (om.getReasoner().getReasonerCapabilities().contains(null, ReasonerVocabulary.supportsP, RDFS.subClassOf)) {
                writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_5_10.coverage", "adff3190-3bb0-4288-985d-1f755230e062");
                // this reasoner does transitive closure on sub-classes, so we just ask
                return hasSuperClass(sup);
            }
        }
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_5_10.coverage", "7e23996f-b88c-4b8d-8f26-0f8d4bda3db3");
        // otherwise, we have to search upwards through the class hierarchy
        Set<OntClass> seen = new HashSet<>();
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_5_10.coverage", "366ab845-3bb0-469a-a74b-7908301a3603");
        List<OntClass> queue = new ArrayList<>();
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_5_10.coverage", "5c6ecffc-9377-4cd3-827a-4f08fdc9fee0");
        queue.add(this);
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_5_10.coverage", "dbd2ff28-ec3a-46eb-b77b-4490edf0e456");
        while (!queue.isEmpty()) {
            writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_5_10.coverage", "70e0bdfb-70a5-42df-8df6-923e0abae66d");
            OntClass c = queue.remove(0);
            writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_5_10.coverage", "a8401004-02c8-4e80-bed6-099dae4ecc5a");
            if (!seen.contains(c)) {
                writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_5_10.coverage", "80b205f8-d40d-484a-9c49-aeb0d2a0bd29");
                seen.add(c);
                writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_5_10.coverage", "498812c8-51f4-4984-961c-8d573083fa77");
                if (c.equals(sup)) {
                    writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_5_10.coverage", "14d2351a-b49e-412e-bdcf-29916ea26ac2");
                    // found the super class
                    return true;
                } else {
                    writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_5_10.coverage", "b8b7eb0d-95b7-40ab-9955-801636956860");
                    // queue the supers
                    for (Iterator<OntClass> i = c.listSuperClasses(); i.hasNext(); ) {
                        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_5_10.coverage", "5a0861f3-8a1b-4f02-9d38-73ecb4184900");
                        queue.add(i.next());
                    }
                }
            }
        }
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_5_10.coverage", "6ae1e6aa-dc94-457a-894b-2ab1c05f3e13");
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
