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
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_8_10.coverage", "d26599c4-c9f7-4a61-8d4c-3cfbe966199d");
        setPropertyValue(getProfile().SUB_CLASS_OF(), "SUB_CLASS_OF", cls);
    }

    /**
     * <p>Add a super-class of this class.</p>
     * @param cls A class that is a super-class of this class.
     * @exception ProfileException If the {@link Profile#SUB_CLASS_OF()} property is not supported in the current language profile.
     */
    @Override
    public void addSuperClass(Resource cls) {
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_8_10.coverage", "ff3cb779-f3f4-4e02-b539-805fc76aba66");
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
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_8_10.coverage", "ad52e769-0432-417e-8bdb-d55220d7627c");
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
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_8_10.coverage", "4086e6fb-f213-45c9-84d6-06a1cfcb899e");
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
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_8_10.coverage", "51fd62d9-3ebf-4869-9588-e7bcddd39d8d");
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
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_8_10.coverage", "cbaeedf1-9e76-4416-b2cb-61f767bba535");
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
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_8_10.coverage", "32c5cc18-5394-470a-9927-aba0885b590b");
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
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_8_10.coverage", "94b37937-4d9f-4e5c-ad00-ea88b91a0086");
        if (!direct) {
            writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_8_10.coverage", "a29d39b4-d982-436e-b51d-0d9a97c3c205");
            // don't need any special case, we just get the property
            return hasPropertyValue(getProfile().SUB_CLASS_OF(), "SUB_CLASS_OF", cls);
        } else {
            writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_8_10.coverage", "7775fd2d-fcfd-4be4-9cc0-48870778f072");
            // we want the direct, not general relationship
            // first try to find an inf graph that can do the work for us
            InfGraph ig = null;
            writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_8_10.coverage", "55200866-7135-421b-9fe2-0a87554988f2");
            if (getGraph() instanceof InfGraph) {
                writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_8_10.coverage", "1fec1f54-20a7-4a08-9a89-7aa271281efd");
                ig = (InfGraph) getGraph();
            } else if (getGraph() instanceof OntModel) {
                writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_8_10.coverage", "1e090b4f-5f1f-46ed-9e1c-4e8000c9eed1");
                OntModel m = (OntModel) getGraph();
                writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_8_10.coverage", "ae582f24-52bc-49e6-be28-49e66d83d378");
                if (m.getGraph() instanceof InfGraph) {
                    writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_8_10.coverage", "02a2e7f3-27bc-43ad-8be9-a8fb703c3a37");
                    ig = (InfGraph) m.getGraph();
                }
            }
            writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_8_10.coverage", "ac825afc-a2f5-4df7-b712-e0cc7c2e25cc");
            if (ig != null && ig.getReasoner().supportsProperty(ReasonerVocabulary.directSubClassOf)) {
                writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_8_10.coverage", "09738daf-73ce-4aba-a167-c54581ee077f");
                // we can look this up directly
                return hasPropertyValue(ReasonerVocabulary.directSubClassOf, "direct sub-class", cls);
            } else {
                writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_8_10.coverage", "e3c7a6ee-008a-4413-bfbb-891ca66dd8ad");
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
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_8_10.coverage", "b1f8d952-4e5b-4654-b217-58e42c7b1b54");
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
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_8_10.coverage", "8960c301-c13d-4014-85da-ca885c7dca6c");
        // first we have to remove all of the inverse sub-class links
        checkProfile(getProfile().SUB_CLASS_OF(), "SUB_CLASS_OF");
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_8_10.coverage", "b2ebd47c-889e-46db-a18c-0275cc5e249d");
        for (StmtIterator i = getModel().listStatements(null, getProfile().SUB_CLASS_OF(), this); i.hasNext(); ) {
            writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_8_10.coverage", "195d0da0-0b39-4001-8943-186a274dfe36");
            i.removeNext();
        }
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_8_10.coverage", "cecaaceb-fbf6-4d2d-830c-d2c33ef9d5c0");
        cls.as(OntClass.class).addSuperClass(this);
    }

    /**
     * <p>Add a sub-class of this class.</p>
     * @param cls A class that is a sub-class of this class.
     * @exception ProfileException If the {@link Profile#SUB_CLASS_OF()} property is not supported in the current language profile.
     */
    @Override
    public void addSubClass(Resource cls) {
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_8_10.coverage", "37c76a01-de5c-4a0a-90f2-00a607b679c5");
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
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_8_10.coverage", "dc007245-2edf-485b-bd60-e218b868feba");
        checkProfile(getProfile().SUB_CLASS_OF(), "SUB_CLASS_OF");
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_8_10.coverage", "dd8673d8-e965-4a84-a60d-393711f8f18d");
        StmtIterator i = getModel().listStatements(null, getProfile().SUB_CLASS_OF(), this);
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_8_10.coverage", "628dfc11-853e-42df-b01c-b7f3f4b81e33");
        try {
            writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_8_10.coverage", "91ec6941-3e5a-4435-90c3-a0b148a6c6c1");
            if (i.hasNext()) {
                writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_8_10.coverage", "694efee7-ffb3-4fb4-af44-8c61c07f7b86");
                return i.nextStatement().getSubject().as(OntClass.class);
            } else {
                writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_8_10.coverage", "5e78a4f2-3f11-48f8-a415-45e397918cbe");
                return null;
            }
        } finally {
            writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_8_10.coverage", "c1a828f6-13ef-4c94-9d5a-aef2d7a6a649");
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
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_8_10.coverage", "2a90f918-022a-40b6-9275-e508e28b1636");
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
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_8_10.coverage", "35580f1e-e439-4a33-ac5e-d254d6a4c1ce");
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
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_8_10.coverage", "2a9834a6-2c02-481a-86f6-2c44c14c239e");
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
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_8_10.coverage", "d22eb02d-0109-4496-9696-e1cb784d57aa");
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
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_8_10.coverage", "27cbdaca-a99b-4495-8b9f-6ec9a2b4f62d");
        if (getModel() instanceof OntModel && (cls.getModel() == null || !(cls.getModel() instanceof OntModel))) {
            writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_8_10.coverage", "edba4b06-2c47-410c-8b86-5cb79723a1c0");
            // could be outside an ontmodel if a constant
            cls = cls.inModel(getModel());
        }
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_8_10.coverage", "3ae3cbcd-d835-490b-9bdd-99fbc479a0db");
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
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_8_10.coverage", "46beab36-69b5-469f-a6a4-727f339f1b60");
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
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_8_10.coverage", "3a080340-f19b-41a4-8446-85e093aacdca");
        setPropertyValue(getProfile().EQUIVALENT_CLASS(), "EQUIVALENT_CLASS", cls);
    }

    /**
     * <p>Add a class that is equivalent to this class.</p>
     * @param cls A class that is equivalent to this class.
     * @exception ProfileException If the {@link Profile#EQUIVALENT_CLASS()} property is not supported in the current language profile.
     */
    @Override
    public void addEquivalentClass(Resource cls) {
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_8_10.coverage", "bb35c9c6-e0f4-47fb-a881-5f9342b0532b");
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
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_8_10.coverage", "c85a49c6-e357-4cc7-91cf-8c2ae672e388");
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
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_8_10.coverage", "ef43b55a-b1e0-4565-bf4f-128a053ce779");
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
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_8_10.coverage", "fe270a7d-fe4b-40f0-aea6-e459a59d0ce9");
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
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_8_10.coverage", "836cac6c-a4d1-4864-9c5b-fa47c3297905");
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
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_8_10.coverage", "14156f3f-66b1-4954-905f-e071c1002e4e");
        setPropertyValue(getProfile().DISJOINT_WITH(), "DISJOINT_WITH", cls);
    }

    /**
     * <p>Add a class that this class is disjoint with.</p>
     * @param cls A class that has no instances in common with this class.
     * @exception ProfileException If the {@link Profile#DISJOINT_WITH()} property is not supported in the current language profile.
     */
    @Override
    public void addDisjointWith(Resource cls) {
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_8_10.coverage", "9dceefdb-e3c7-4ff3-b170-b722bfb304d1");
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
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_8_10.coverage", "9de5e252-f1b5-482b-82d4-f59690da5b0b");
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
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_8_10.coverage", "77aedd22-e6ea-4672-b0cb-6863e963333f");
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
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_8_10.coverage", "9f0179ad-58c4-4e70-aa38-6abcf877d6c8");
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
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_8_10.coverage", "4bd99a1a-5aef-418f-8d43-4b3e51ccca66");
        removePropertyValue(getProfile().DISJOINT_WITH(), "DISJOINT_WITH", cls);
    }

    // other utility methods
    /**
     * Equivalent to calling {@link #listDeclaredProperties(boolean)} with
     * default value <code>direct = false</code>.
     */
    @Override
    public ExtendedIterator<OntProperty> listDeclaredProperties() {
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_8_10.coverage", "44c9f7e1-2f4b-4b07-a171-ac6bc953d28c");
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
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_8_10.coverage", "ae3d2306-fffe-4efc-9def-9bf696628aff");
        // first collect the candidate properties
        Set<RDFNode> candSet = new HashSet<>();
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_8_10.coverage", "4eb60626-3cf1-4b82-836f-2c58c7b73f14");
        // than a non-inference model
        for (Iterator<Statement> i = listAllProperties(); i.hasNext(); ) {
            writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_8_10.coverage", "4fa8f63e-2dbd-4426-bc20-28a63bd25414");
            candSet.add(i.next().getSubject().as(Property.class));
        }
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_8_10.coverage", "7dcba223-bf53-4974-b27c-cf561b3db502");
        // now we iterate over the candidates and check that they match all domain constraints
        List<RDFNode> cands = new ArrayList<>();
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_8_10.coverage", "91724528-a8e6-4282-9221-6e4eecf9b790");
        cands.addAll(candSet);
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_8_10.coverage", "26054596-8b26-4713-9cc0-de5454802dd1");
        for (int j = cands.size() - 1; j >= 0; j--) {
            writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_8_10.coverage", "6ae3ec64-579c-4630-92cd-76a7996d7da5");
            Property cand = (Property) cands.get(j);
            writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_8_10.coverage", "868fb295-2cb5-4182-a132-d913728dd674");
            if (!hasDeclaredProperty(cand, direct)) {
                writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_8_10.coverage", "cd5da222-070e-4d29-a586-bd54921a02bd");
                cands.remove(j);
            }
        }
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_8_10.coverage", "05bebf85-ce73-4165-b3c1-109179b5bc3e");
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
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_8_10.coverage", "aaa98634-ffa5-4743-9adc-eef6873ff4fa");
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
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_8_10.coverage", "91263700-73a0-4a1e-996b-b098883bf805");
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
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_8_10.coverage", "b299593a-b65d-4d7b-85a0-ab8285a30fed");
        return getModel().listStatements(null, RDF.type, this).mapWith(s -> s.getSubject().as(Individual.class)).filterKeep(o -> o.hasRDFType(OntClassImpl.this, direct)).filterKeep(new UniqueFilter<Individual>());
    }

    /**
     * <p>Answer a new individual that has this class as its <code>rdf:type</code></p>
     * @return A new anonymous individual that is an instance of this class
     */
    @Override
    public Individual createIndividual() {
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_8_10.coverage", "835f7191-eded-4a00-b07e-615f62e5397a");
        return ((OntModel) getModel()).createIndividual(this);
    }

    /**
     * <p>Answer a new individual that has this class as its <code>rdf:type</code></p>
     * @param uri The URI of the new individual
     * @return A new named individual that is an instance of this class
     */
    @Override
    public Individual createIndividual(String uri) {
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_8_10.coverage", "2e9deb25-0e77-4b61-af05-26d5250949e4");
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
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_8_10.coverage", "4735acf3-fd5a-43ea-947f-fc95e74a9d2e");
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
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_8_10.coverage", "e1cd39d5-c028-48ae-8e74-8eccf5fb7aca");
        // sanity check - :Nothing is never a root class
        if (equals(getProfile().NOTHING())) {
            writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_8_10.coverage", "ffce1322-4df5-4fd9-bc33-9db71e940658");
            return false;
        }
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_8_10.coverage", "b42bf446-1449-431c-99f3-2a6ced7fe4cd");
        // the only super-classes of a root class are the various aliases
        // of Top, or itself
        /**
         * Note: moved the initialisation of i outside the try-catch, otherwise an
         * exception in listSuperClasses [eg a broken Graph implementation] will
         * avoid i's initialisation but still run i.close, generating a mysterious
         * NullPointerException. Signed, Mr Burnt Spines.
         */
        ExtendedIterator<OntClass> i = listSuperClasses(true);
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_8_10.coverage", "7da5f8f2-9167-411f-849e-4d0b14ec95c4");
        try {
            writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_8_10.coverage", "84e68b93-daff-43d8-ab1e-352621260417");
            while (i.hasNext()) {
                writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_8_10.coverage", "b89f1be0-6ebe-4cc7-a004-280efb9c567d");
                Resource sup = i.next();
                writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_8_10.coverage", "4f97e0cf-97c7-4e7c-93b9-80e78b0c913e");
                if (!(sup.equals(getProfile().THING()) || sup.equals(RDFS.Resource) || sup.equals(this))) {
                    writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_8_10.coverage", "00683485-9512-465b-8be4-ba1b93acfe07");
                    // a super that indicates this is not a root class
                    return false;
                }
            }
        } finally {
            writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_8_10.coverage", "e46c738e-6802-4997-bc9a-323437b292f0");
            i.close();
        }
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_8_10.coverage", "cc810935-50e2-4295-b7f0-a3e8b9a3faff");
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
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_8_10.coverage", "c8043644-90cf-4820-8f56-4689c40c7ff8");
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
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_8_10.coverage", "ae195e76-e029-4364-85f2-9912fda0e3f1");
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
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_8_10.coverage", "276bfe9b-090a-413e-b087-cca7d55f862c");
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
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_8_10.coverage", "43970737-7435-4fd7-a7f5-3fa7631db519");
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
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_8_10.coverage", "b7fc278a-982d-4598-b225-1884d6c799c1");
        return as(Restriction.class);
    }

    // sub-type testing
    /**
     * <p>Answer true if this class is an enumerated class expression</p>
     * @return True if this is an enumerated class expression
     */
    @Override
    public boolean isEnumeratedClass() {
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_8_10.coverage", "255a2fff-aab3-40fc-9f0d-0c513ab81a26");
        checkProfile(getProfile().ONE_OF(), "ONE_OF");
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_8_10.coverage", "1bdc6ebc-967d-45f3-8b0b-9848691e3f63");
        return hasProperty(getProfile().ONE_OF());
    }

    /**
     * <p>Answer true if this class is a union class expression</p>
     * @return True if this is a union class expression
     */
    @Override
    public boolean isUnionClass() {
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_8_10.coverage", "4a1d96f8-677a-4bd9-8aaf-3f70812fb7e8");
        checkProfile(getProfile().UNION_OF(), "UNION_OF");
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_8_10.coverage", "ac93a92f-c9f6-42b2-bcb9-ea179036d1e4");
        return hasProperty(getProfile().UNION_OF());
    }

    /**
     * <p>Answer true if this class is an intersection class expression</p>
     * @return True if this is an intersection class expression
     */
    @Override
    public boolean isIntersectionClass() {
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_8_10.coverage", "c2b9d1cc-55e4-48c1-b301-dd1c61d9389e");
        checkProfile(getProfile().INTERSECTION_OF(), "INTERSECTION_OF");
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_8_10.coverage", "246991ce-640f-4aa7-a625-635beb1d14b6");
        return hasProperty(getProfile().INTERSECTION_OF());
    }

    /**
     * <p>Answer true if this class is a complement class expression</p>
     * @return True if this is a complement class expression
     */
    @Override
    public boolean isComplementClass() {
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_8_10.coverage", "abf923b6-e424-480a-bec2-fada8b54a702");
        checkProfile(getProfile().COMPLEMENT_OF(), "COMPLEMENT_OF");
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_8_10.coverage", "84543fdc-55d6-4956-ad3e-d46e58f7fc2a");
        return hasProperty(getProfile().COMPLEMENT_OF());
    }

    /**
     * <p>Answer true if this class is a property restriction</p>
     * @return True if this is a restriction
     */
    @Override
    public boolean isRestriction() {
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_8_10.coverage", "919b8b0e-a61b-4acc-bbcf-083829a9c2d0");
        checkProfile(getProfile().RESTRICTION(), "RESTRICTION");
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_8_10.coverage", "e25fbfcb-88a1-4965-9262-c4f33941fb47");
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
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_8_10.coverage", "7427eb88-c710-4631-b34c-972a4b2f23c1");
        setPropertyValue(getProfile().ONE_OF(), "ONE_OF", individuals);
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_8_10.coverage", "fb899f16-0539-46b1-ab5f-5c8b6945f25b");
        return as(EnumeratedClass.class);
    }

    /**
     * <p>Answer a view of this class as an intersection of the given classes.</p>
     * @param classes A list of the classes that will comprise the operands of the intersection
     * @return This ontology class, converted to an intersection of the given classes
     */
    @Override
    public IntersectionClass convertToIntersectionClass(RDFList classes) {
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_8_10.coverage", "cebeabe6-4bf0-4ad9-a5b4-823fcd006d7b");
        setPropertyValue(getProfile().INTERSECTION_OF(), "INTERSECTION_OF", classes);
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_8_10.coverage", "5e0bee2a-3b0e-4f2f-8d49-c2098b01e0df");
        return as(IntersectionClass.class);
    }

    /**
     * <p>Answer a view of this class as a union of the given classes.</p>
     * @param classes A list of the classes that will comprise the operands of the union
     * @return This ontology class, converted to an union of the given classes
     */
    @Override
    public UnionClass convertToUnionClass(RDFList classes) {
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_8_10.coverage", "68adf27c-294a-409d-ba95-fd32ef7fd2f9");
        setPropertyValue(getProfile().UNION_OF(), "UNION_OF", classes);
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_8_10.coverage", "f4305013-9653-4592-9bc6-9383448ab9fe");
        return as(UnionClass.class);
    }

    /**
     * <p>Answer a view of this class as an complement of the given class.</p>
     * @param cls An ontology classs that will be operand of the complement
     * @return This ontology class, converted to an complement of the given class
     */
    @Override
    public ComplementClass convertToComplementClass(Resource cls) {
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_8_10.coverage", "0409643a-d58d-4871-8882-33349c65b606");
        setPropertyValue(getProfile().COMPLEMENT_OF(), "COMPLEMENT_OF", cls);
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_8_10.coverage", "fe1e6826-6f2a-4f6e-9ba8-0afc17308c1e");
        return as(ComplementClass.class);
    }

    /**
     * <p>Answer a view of this class as an resriction on the given property.</p>
     * @param prop A property this is the subject of a property restriction class expression
     * @return This ontology class, converted to a restriction on the given property
     */
    @Override
    public Restriction convertToRestriction(Property prop) {
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_8_10.coverage", "a48297cf-ebe4-4f96-bb74-e7c7c6a14634");
        if (!hasRDFType(getProfile().RESTRICTION(), "RESTRICTION", false)) {
            writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_8_10.coverage", "e9458608-cbce-4e63-80d9-b4ea9bfbb75a");
            setRDFType(getProfile().RESTRICTION());
        }
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_8_10.coverage", "31018250-e2eb-450b-9b6e-aaf60e18e44f");
        setPropertyValue(getProfile().ON_PROPERTY(), "ON_PROPERTY", prop);
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_8_10.coverage", "4559ee98-023f-4063-ac11-362b7d656b69");
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
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_8_10.coverage", "9ef7bdfc-7f59-4122-958a-a9a3008c4ca2");
        // we manually compute the maximal lower elements - this could be expensive in general
        // return ResourceUtils.maximalLowerElements( listSuperClasses(), getProfile().SUB_CLASS_OF(), false ).contains( cls );
        ExtendedIterator<OntClass> i = listDirectPropertyValues(getProfile().SUB_CLASS_OF(), "subClassOf", OntClass.class, getProfile().SUB_CLASS_OF(), true, false);
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_8_10.coverage", "09de2286-64ee-4c1d-8187-3f0e22e94d80");
        try {
            writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_8_10.coverage", "a35d82e3-16a7-40d2-9ab6-e152e5685696");
            while (i.hasNext()) {
                writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_8_10.coverage", "b58101b2-dbcd-44f6-89e6-01573d88af46");
                if (cls.equals(i.next())) {
                    writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_8_10.coverage", "18454220-90db-4af6-8109-2d1cbfeb216f");
                    return true;
                }
            }
        } finally {
            writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_8_10.coverage", "c5143dc2-2540-4388-97c6-73f1e6801c96");
            i.close();
        }
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_8_10.coverage", "53919f2a-3a44-411a-85d1-101e1684a498");
        return false;
    }

    /**
     * <p>Answer true if this class lies with the domain of p<p>
     * @param p
     * @param direct If true, only consider direct associations with domain
     * @return True if this class in the domain of property <code>p</code>
     */
    protected boolean testDomain(Property p, boolean direct) {
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_8_10.coverage", "97a4ff83-7cc4-4203-adff-fe37a9a000bf");
        // we ignore any property in the OWL, etc namespace
        String namespace = p.getNameSpace();
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_8_10.coverage", "fd97aed1-16c2-4739-80ad-a34d6ed78346");
        for (String IGNORE_NAMESPACE : IGNORE_NAMESPACES) {
            writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_8_10.coverage", "ad518fe4-fa2b-459c-89c2-1826bf7095f7");
            if (namespace.equals(IGNORE_NAMESPACE)) {
                writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_8_10.coverage", "facd5e41-74d6-426b-98be-4e2de33d30a8");
                return false;
            }
        }
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_8_10.coverage", "5f0c4a6f-5372-416f-987e-9a41b67addb7");
        // check for global props, that have no specific domain constraint
        boolean isGlobal = true;
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_8_10.coverage", "a2194b5e-4f0f-4076-b210-c6047391f1b5");
        // flag for detecting the direct case
        boolean seenDirect = false;
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_8_10.coverage", "9fb56049-cbe1-406a-9ae1-00b89331d6f3");
        for (StmtIterator i = getModel().listStatements(p, getProfile().DOMAIN(), (RDFNode) null); i.hasNext(); ) {
            writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_8_10.coverage", "ed28fde9-4cd2-40ca-a904-0dbdc3d57ef4");
            Resource domain = i.nextStatement().getResource();
            writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_8_10.coverage", "366275fa-15f7-4a91-839b-4d79e5e55893");
            // there are some well-known values we ignore
            if (!(domain.equals(getProfile().THING()) || domain.equals(RDFS.Resource))) {
                writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_8_10.coverage", "8876c2a8-d3e9-49c1-9e25-57a08a65d51b");
                // not a generic domain
                isGlobal = false;
                writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_8_10.coverage", "1e204b5f-6019-4496-b31e-773d4f5227c8");
                if (domain.equals(this)) {
                    writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_8_10.coverage", "b251344c-2ea3-4afe-ac99-a47d016692a7");
                    // if this class is actually in the domain (as opposed to one of this class's
                    // super-classes), then we've detected the direct property case
                    seenDirect = true;
                } else if (!canProveSuperClass(domain)) {
                    writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_8_10.coverage", "74ef20ef-2788-4d5a-bab9-9f4166d80bfe");
                    // there is a class in the domain of p that is not a super-class of this class
                    return false;
                }
            }
        }
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_8_10.coverage", "ae8f9cf2-dba8-4a88-98a8-b3dadceb754f");
        if (direct) {
            writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_8_10.coverage", "789467a5-5468-4642-9c17-cb8f00a04638");
            // or it's a global prop and this is a root class
            return seenDirect || (isGlobal && isHierarchyRoot());
        } else {
            writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_8_10.coverage", "fbbe5de8-5d19-40a2-8cfd-9703853f63a0");
            // otherwise the 'return false' above would have kicked in
            return true;
        }
    }

    /**
     * <p>Answer an iterator over all of the properties in this model
     * @return An iterator over {@link OntProperty}
     */
    protected ExtendedIterator<Statement> listAllProperties() {
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_8_10.coverage", "0ea08d3b-9cff-48a8-bf9b-da63c4cec94f");
        OntModel mOnt = (OntModel) getModel();
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_8_10.coverage", "eef14381-e16b-46f7-9e51-75bf9942923c");
        Profile prof = mOnt.getProfile();
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_8_10.coverage", "f7e87c7a-75b2-48f9-8053-a5fb1a630ed5");
        ExtendedIterator<Statement> pi = mOnt.listStatements(null, RDF.type, getProfile().PROPERTY());
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_8_10.coverage", "40529f6e-acd5-48bf-bb94-f9a27ea90738");
        // check reasoner capabilities - major performance improvement for inf models
        if (mOnt.getReasoner() != null) {
            writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_8_10.coverage", "9817f40e-c7cc-4160-a231-1a74ad19b8ab");
            Model caps = mOnt.getReasoner().getReasonerCapabilities();
            writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_8_10.coverage", "3bf5a996-77b0-4b35-9821-436f1dd5baca");
            if (caps.contains(null, ReasonerVocabulary.supportsP, OWL.ObjectProperty)) {
                writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_8_10.coverage", "6b978b5e-7cb5-48f9-942c-acdf7e27a63f");
                // all owl:ObjectProperty, owl:DatatypeProperty, etc, are rdf:Property resources
                return pi;
            }
        }
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_8_10.coverage", "41bb6a9f-9ac0-4d1b-bfb2-7755ded52b8e");
        // otherwise, we manually check the other property types
        if (prof.OBJECT_PROPERTY() != null) {
            writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_8_10.coverage", "d0a16d1c-3232-447f-b2e1-36ed515349f7");
            pi = pi.andThen(mOnt.listStatements(null, RDF.type, prof.OBJECT_PROPERTY()));
        }
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_8_10.coverage", "0f91ad30-fb8f-413e-9c6b-0da016c3beac");
        if (prof.DATATYPE_PROPERTY() != null) {
            writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_8_10.coverage", "8f48241c-fb26-4826-a61d-1d50feed2397");
            pi = pi.andThen(mOnt.listStatements(null, RDF.type, prof.DATATYPE_PROPERTY()));
        }
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_8_10.coverage", "f9e2f594-8221-4e9b-96b0-d84e69f9d9a1");
        if (prof.FUNCTIONAL_PROPERTY() != null) {
            writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_8_10.coverage", "1f99d58c-d0a7-4625-be81-15742e1ffb87");
            pi = pi.andThen(mOnt.listStatements(null, RDF.type, prof.FUNCTIONAL_PROPERTY()));
        }
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_8_10.coverage", "d60a8bf4-53a8-44e8-a72e-eb653bef12df");
        if (prof.INVERSE_FUNCTIONAL_PROPERTY() != null) {
            writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_8_10.coverage", "cced5be2-8d3d-46fb-87e5-40fb174d8a23");
            pi = pi.andThen(mOnt.listStatements(null, RDF.type, prof.INVERSE_FUNCTIONAL_PROPERTY()));
        }
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_8_10.coverage", "19310413-70c5-4658-8321-39d8f5ccca36");
        if (prof.SYMMETRIC_PROPERTY() != null) {
            writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_8_10.coverage", "8e99ae29-84d8-4da6-9441-0be2618b44c2");
            pi = pi.andThen(mOnt.listStatements(null, RDF.type, prof.SYMMETRIC_PROPERTY()));
        }
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_8_10.coverage", "f6071d3c-c70b-4df5-b8bf-c22fb9d92032");
        if (prof.TRANSITIVE_PROPERTY() != null) {
            writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_8_10.coverage", "9d8f5bf1-82f7-4201-9836-b9d18f1728fa");
            pi = pi.andThen(mOnt.listStatements(null, RDF.type, prof.TRANSITIVE_PROPERTY()));
        }
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_8_10.coverage", "dc018e5e-f816-4b81-a55a-60c9a82af78b");
        if (prof.ANNOTATION_PROPERTY() != null) {
            writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_8_10.coverage", "20523d85-b643-4ad0-b692-631ce80aa695");
            pi = pi.andThen(mOnt.listStatements(null, RDF.type, prof.ANNOTATION_PROPERTY()));
        }
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_8_10.coverage", "66004faa-e00b-42a8-b647-3caccd5e1ef8");
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
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_8_10.coverage", "f8da7c9a-2784-4b91-8e66-99da3bd1cc54");
        OntModel om = (OntModel) getModel();
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_8_10.coverage", "92eadec2-e50b-44a2-9053-65226bbdb423");
        if (om.getReasoner() != null) {
            writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_8_10.coverage", "3198ab49-0fea-4371-bd1a-79deebaf931f");
            if (om.getReasoner().getReasonerCapabilities().contains(null, ReasonerVocabulary.supportsP, RDFS.subClassOf)) {
                writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_8_10.coverage", "c3c7c043-7975-44f4-919f-a58213e39f80");
                // this reasoner does transitive closure on sub-classes, so we just ask
                return hasSuperClass(sup);
            }
        }
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_8_10.coverage", "7dd02fdb-c4b5-4811-92fb-bfc52aa2efaf");
        // otherwise, we have to search upwards through the class hierarchy
        Set<OntClass> seen = new HashSet<>();
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_8_10.coverage", "5bb8bf2d-0913-4329-b0a6-605ff371dc18");
        List<OntClass> queue = new ArrayList<>();
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_8_10.coverage", "296bfd7b-f0f2-41c6-b600-865f5b8e1c1e");
        queue.add(this);
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_8_10.coverage", "a07e44c1-d0f2-48b4-959a-c37c8a2ef83b");
        while (!queue.isEmpty()) {
            writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_8_10.coverage", "35a13fa6-73b7-43f8-9a7b-717604fca708");
            OntClass c = queue.remove(0);
            writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_8_10.coverage", "797daa79-19f7-498b-bcb6-96e6907c4015");
            if (!seen.contains(c)) {
                writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_8_10.coverage", "0c259323-3419-45f4-a1ac-b71eedea8dcc");
                seen.add(c);
                writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_8_10.coverage", "e5b0292f-835f-4eac-b12f-77d06c71653c");
                if (c.equals(sup)) {
                    writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_8_10.coverage", "fc284278-b37a-4a87-b0b3-acbebd4a576c");
                    // found the super class
                    return true;
                } else {
                    writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_8_10.coverage", "7f5b34ca-6d5b-469a-85c4-914a717b2b77");
                    // queue the supers
                    for (Iterator<OntClass> i = c.listSuperClasses(); i.hasNext(); ) {
                        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_8_10.coverage", "d02995fe-10bd-4aee-8fbd-cce1e45dc595");
                        queue.add(i.next());
                    }
                }
            }
        }
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_8_10.coverage", "d6d0a33c-dd0e-40cf-b396-5c586db5dcda");
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
