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
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_10_10.coverage", "8c7a03fa-f025-4979-8e8b-85b9c233ba53");
        setPropertyValue(getProfile().SUB_CLASS_OF(), "SUB_CLASS_OF", cls);
    }

    /**
     * <p>Add a super-class of this class.</p>
     * @param cls A class that is a super-class of this class.
     * @exception ProfileException If the {@link Profile#SUB_CLASS_OF()} property is not supported in the current language profile.
     */
    @Override
    public void addSuperClass(Resource cls) {
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_10_10.coverage", "f1e92aaf-c807-44dd-ba2c-8015d0c80fa0");
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
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_10_10.coverage", "939209ba-8ed8-49e5-89e7-ae60346a7baa");
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
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_10_10.coverage", "bac67e00-7ec4-454f-8373-9a46b54d21a0");
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
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_10_10.coverage", "9aee463c-e188-47f2-8928-ad7d490066f0");
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
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_10_10.coverage", "edaff4f1-cfcc-49dc-b6f3-76150d708290");
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
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_10_10.coverage", "177a2d85-ed99-4893-85ed-59b96816d57c");
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
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_10_10.coverage", "04e71549-bc01-48f4-bf9f-866ee5cd1eb9");
        if (!direct) {
            writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_10_10.coverage", "ef8fdcca-d8df-4226-9318-9d3687fdea38");
            // don't need any special case, we just get the property
            return hasPropertyValue(getProfile().SUB_CLASS_OF(), "SUB_CLASS_OF", cls);
        } else {
            writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_10_10.coverage", "c8e91ea1-e30b-406f-bbb4-9a30b418a3e2");
            // we want the direct, not general relationship
            // first try to find an inf graph that can do the work for us
            InfGraph ig = null;
            writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_10_10.coverage", "a115084b-0fd8-45bb-b0fd-8783b64c0dce");
            if (getGraph() instanceof InfGraph) {
                writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_10_10.coverage", "29f85a1e-15bf-406a-aada-f939b4b2e522");
                ig = (InfGraph) getGraph();
            } else if (getGraph() instanceof OntModel) {
                writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_10_10.coverage", "00d5b9b2-5a54-48ae-ac19-f2db77f54caa");
                OntModel m = (OntModel) getGraph();
                writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_10_10.coverage", "228a07c5-18ee-4d6f-8887-a35af3f9c54b");
                if (m.getGraph() instanceof InfGraph) {
                    writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_10_10.coverage", "16d421bf-3c18-40ac-8b27-3416ce2d4f3a");
                    ig = (InfGraph) m.getGraph();
                }
            }
            writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_10_10.coverage", "2f2258dd-8624-4a8f-b55f-03b50475fb1b");
            if (ig != null && ig.getReasoner().supportsProperty(ReasonerVocabulary.directSubClassOf)) {
                writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_10_10.coverage", "91acc7ba-bc0e-4262-8d3e-e475042b270c");
                // we can look this up directly
                return hasPropertyValue(ReasonerVocabulary.directSubClassOf, "direct sub-class", cls);
            } else {
                writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_10_10.coverage", "05bb2c46-3626-46ab-96a1-197ac12c6770");
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
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_10_10.coverage", "1348a356-97fa-4afe-912f-521d973a0326");
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
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_10_10.coverage", "b931c891-7ee9-4575-8aba-f4299493862c");
        // first we have to remove all of the inverse sub-class links
        checkProfile(getProfile().SUB_CLASS_OF(), "SUB_CLASS_OF");
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_10_10.coverage", "ad04f713-659e-4cf6-858d-40e754d33c80");
        for (StmtIterator i = getModel().listStatements(null, getProfile().SUB_CLASS_OF(), this); i.hasNext(); ) {
            writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_10_10.coverage", "efec7406-599a-483e-a43f-3c7b2d62d3fb");
            i.removeNext();
        }
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_10_10.coverage", "3fe910d5-7627-4930-b3fb-a2d75abc24bf");
        cls.as(OntClass.class).addSuperClass(this);
    }

    /**
     * <p>Add a sub-class of this class.</p>
     * @param cls A class that is a sub-class of this class.
     * @exception ProfileException If the {@link Profile#SUB_CLASS_OF()} property is not supported in the current language profile.
     */
    @Override
    public void addSubClass(Resource cls) {
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_10_10.coverage", "9da5dd02-5ab1-4a70-9c3d-c90a3e63db9d");
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
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_10_10.coverage", "7e225aa9-60c5-4a88-a10a-b9b2d4aaec29");
        checkProfile(getProfile().SUB_CLASS_OF(), "SUB_CLASS_OF");
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_10_10.coverage", "26bb18c1-ff4a-47a4-a785-95d49cf8998c");
        StmtIterator i = getModel().listStatements(null, getProfile().SUB_CLASS_OF(), this);
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_10_10.coverage", "704671a5-3dd6-4f4b-8501-2345cb509cae");
        try {
            writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_10_10.coverage", "42ea45cb-c4c9-45cf-af47-77b8664b91cd");
            if (i.hasNext()) {
                writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_10_10.coverage", "195438e1-79f8-4f20-a583-4d288843a2f1");
                return i.nextStatement().getSubject().as(OntClass.class);
            } else {
                writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_10_10.coverage", "825136f0-a0f6-4f2f-8216-e2627622ac44");
                return null;
            }
        } finally {
            writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_10_10.coverage", "cb5b73af-cc1d-45dc-a503-52de5342199f");
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
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_10_10.coverage", "d2aeca47-9648-4fcf-95df-683aba0aa1ca");
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
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_10_10.coverage", "8f18e12c-3339-48bc-8bbd-8e3fdfad0126");
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
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_10_10.coverage", "184e26b5-ac15-40d8-80a1-114da98471be");
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
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_10_10.coverage", "1831753d-40ad-4811-9bf2-9355bb389ad5");
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
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_10_10.coverage", "480dbc52-c27f-4366-b581-925707da85db");
        if (getModel() instanceof OntModel && (cls.getModel() == null || !(cls.getModel() instanceof OntModel))) {
            writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_10_10.coverage", "02cf7528-d928-4069-bb5c-40e5c567f12d");
            // could be outside an ontmodel if a constant
            cls = cls.inModel(getModel());
        }
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_10_10.coverage", "d41c7a39-630f-4f5f-9743-4a7fcbab8aa3");
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
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_10_10.coverage", "9db1e63b-53f1-481f-a552-64e03da909ba");
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
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_10_10.coverage", "9b808c6e-6496-4a57-a89e-88157e2db594");
        setPropertyValue(getProfile().EQUIVALENT_CLASS(), "EQUIVALENT_CLASS", cls);
    }

    /**
     * <p>Add a class that is equivalent to this class.</p>
     * @param cls A class that is equivalent to this class.
     * @exception ProfileException If the {@link Profile#EQUIVALENT_CLASS()} property is not supported in the current language profile.
     */
    @Override
    public void addEquivalentClass(Resource cls) {
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_10_10.coverage", "c9e2711c-e945-4764-82ab-e8789b2a2656");
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
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_10_10.coverage", "7f77fd18-6ec9-4bea-9735-45f9c03df4f2");
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
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_10_10.coverage", "d7f7aee4-6ecb-47f2-9935-f27b7f9f5fc7");
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
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_10_10.coverage", "b899fde8-957b-45c1-8b3d-258df0927b42");
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
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_10_10.coverage", "9d6638ff-40c0-43e9-88d7-ed998ec0187c");
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
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_10_10.coverage", "aa369ae8-2074-4c73-bfb0-9b18c1bc05a8");
        setPropertyValue(getProfile().DISJOINT_WITH(), "DISJOINT_WITH", cls);
    }

    /**
     * <p>Add a class that this class is disjoint with.</p>
     * @param cls A class that has no instances in common with this class.
     * @exception ProfileException If the {@link Profile#DISJOINT_WITH()} property is not supported in the current language profile.
     */
    @Override
    public void addDisjointWith(Resource cls) {
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_10_10.coverage", "609a8214-f8ec-4eb4-8695-6773b39b1c6d");
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
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_10_10.coverage", "0b990063-83d9-436e-89f0-53e6f93bbce4");
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
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_10_10.coverage", "317b2f16-9f94-4e6d-bf77-aa8c2f2e73d4");
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
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_10_10.coverage", "e6e51bb7-0b8e-4616-83b9-dded7e9b4cb3");
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
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_10_10.coverage", "de00490a-c9b2-4ba4-83bc-9a8b9cd4e80c");
        removePropertyValue(getProfile().DISJOINT_WITH(), "DISJOINT_WITH", cls);
    }

    // other utility methods
    /**
     * Equivalent to calling {@link #listDeclaredProperties(boolean)} with
     * default value <code>direct = false</code>.
     */
    @Override
    public ExtendedIterator<OntProperty> listDeclaredProperties() {
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_10_10.coverage", "56ff8a6d-7c43-4180-be72-42c75fea9151");
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
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_10_10.coverage", "5c4b8ae1-7589-45da-9061-485362e617e7");
        // first collect the candidate properties
        Set<RDFNode> candSet = new HashSet<>();
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_10_10.coverage", "89033ae7-59f4-44c0-b5c2-130ca79ba29d");
        // than a non-inference model
        for (Iterator<Statement> i = listAllProperties(); i.hasNext(); ) {
            writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_10_10.coverage", "95bd0f84-8230-49d2-896a-2282bdf0a649");
            candSet.add(i.next().getSubject().as(Property.class));
        }
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_10_10.coverage", "4523bb13-7f93-4c95-a289-247e3c8bb679");
        // now we iterate over the candidates and check that they match all domain constraints
        List<RDFNode> cands = new ArrayList<>();
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_10_10.coverage", "d2d316b7-0448-46df-94be-66ad95e260ed");
        cands.addAll(candSet);
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_10_10.coverage", "4c248c2f-9384-473e-8569-236240d0cbb2");
        for (int j = cands.size() - 1; j >= 0; j--) {
            writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_10_10.coverage", "c6e95696-312b-478d-b244-59005d7c13a7");
            Property cand = (Property) cands.get(j);
            writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_10_10.coverage", "a39a441a-a7c9-44eb-8b72-6840a1cd8574");
            if (!hasDeclaredProperty(cand, direct)) {
                writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_10_10.coverage", "923a8e65-d4c8-4afb-9b76-07961d201669");
                cands.remove(j);
            }
        }
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_10_10.coverage", "2197461d-fb54-4be0-806f-b867dc85c874");
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
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_10_10.coverage", "9c47a59f-8973-4caf-800d-060a13dedba2");
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
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_10_10.coverage", "8701205b-36e5-48ab-abdf-a0495dea47e8");
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
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_10_10.coverage", "fba1296c-51bc-45f5-9a5f-34fcc3faab34");
        return getModel().listStatements(null, RDF.type, this).mapWith(s -> s.getSubject().as(Individual.class)).filterKeep(o -> o.hasRDFType(OntClassImpl.this, direct)).filterKeep(new UniqueFilter<Individual>());
    }

    /**
     * <p>Answer a new individual that has this class as its <code>rdf:type</code></p>
     * @return A new anonymous individual that is an instance of this class
     */
    @Override
    public Individual createIndividual() {
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_10_10.coverage", "1b518998-f47d-4bf5-8497-f20e9e60fc5f");
        return ((OntModel) getModel()).createIndividual(this);
    }

    /**
     * <p>Answer a new individual that has this class as its <code>rdf:type</code></p>
     * @param uri The URI of the new individual
     * @return A new named individual that is an instance of this class
     */
    @Override
    public Individual createIndividual(String uri) {
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_10_10.coverage", "bffcbc25-49d1-406e-b507-3869bbcff619");
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
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_10_10.coverage", "7c1cc825-2064-46db-b636-2ef644edfe2b");
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
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_10_10.coverage", "b6f27df8-b29e-4c56-849e-071e8fd0022a");
        // sanity check - :Nothing is never a root class
        if (equals(getProfile().NOTHING())) {
            writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_10_10.coverage", "66b73ba8-f201-4247-b261-2fc43a4d9700");
            return false;
        }
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_10_10.coverage", "f166f3c3-c0c0-4d67-aaa1-a25070bd32d3");
        // the only super-classes of a root class are the various aliases
        // of Top, or itself
        /**
         * Note: moved the initialisation of i outside the try-catch, otherwise an
         * exception in listSuperClasses [eg a broken Graph implementation] will
         * avoid i's initialisation but still run i.close, generating a mysterious
         * NullPointerException. Signed, Mr Burnt Spines.
         */
        ExtendedIterator<OntClass> i = listSuperClasses(true);
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_10_10.coverage", "6f6dc2e7-3385-4b8c-a5b2-29ac0ec73e42");
        try {
            writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_10_10.coverage", "1b155167-757a-46be-b844-b92d53aff285");
            while (i.hasNext()) {
                writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_10_10.coverage", "70f53bed-dee2-4eb8-9a03-0e949bd74a78");
                Resource sup = i.next();
                writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_10_10.coverage", "57f0a879-2f0f-455c-a672-c5996c00813d");
                if (!(sup.equals(getProfile().THING()) || sup.equals(RDFS.Resource) || sup.equals(this))) {
                    writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_10_10.coverage", "28926f4d-7b78-469a-8835-23d35421e29e");
                    // a super that indicates this is not a root class
                    return false;
                }
            }
        } finally {
            writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_10_10.coverage", "992a6f7f-b9fa-4946-9bd7-32fadde7082b");
            i.close();
        }
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_10_10.coverage", "1c9c5aaa-7f5a-4600-9313-5063974b89fd");
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
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_10_10.coverage", "7eefa6a2-7868-4d31-b626-de15cfdee98e");
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
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_10_10.coverage", "37b945ed-e05b-4665-be06-8a0f8421608b");
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
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_10_10.coverage", "7e5866e1-dd3d-4e2f-b250-ab9f7976ea9b");
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
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_10_10.coverage", "92d71320-8804-49ae-9144-3a14465449a1");
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
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_10_10.coverage", "a65beca4-4605-478d-aa8f-cf78a931e2e1");
        return as(Restriction.class);
    }

    // sub-type testing
    /**
     * <p>Answer true if this class is an enumerated class expression</p>
     * @return True if this is an enumerated class expression
     */
    @Override
    public boolean isEnumeratedClass() {
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_10_10.coverage", "3b1f799e-46ac-4756-893a-a34d5f71c5fe");
        checkProfile(getProfile().ONE_OF(), "ONE_OF");
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_10_10.coverage", "77331d38-0f5d-4ba2-b0e1-a3d9f91cac0b");
        return hasProperty(getProfile().ONE_OF());
    }

    /**
     * <p>Answer true if this class is a union class expression</p>
     * @return True if this is a union class expression
     */
    @Override
    public boolean isUnionClass() {
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_10_10.coverage", "230aba24-f966-4ba3-ae00-294147813afd");
        checkProfile(getProfile().UNION_OF(), "UNION_OF");
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_10_10.coverage", "2ae1d32b-fe1e-48b9-b390-065bb3749a60");
        return hasProperty(getProfile().UNION_OF());
    }

    /**
     * <p>Answer true if this class is an intersection class expression</p>
     * @return True if this is an intersection class expression
     */
    @Override
    public boolean isIntersectionClass() {
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_10_10.coverage", "7c10da3f-6f67-4320-a0b2-f41bc2e3a38d");
        checkProfile(getProfile().INTERSECTION_OF(), "INTERSECTION_OF");
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_10_10.coverage", "f466e2df-c9a2-4629-9e23-fa6540c86386");
        return hasProperty(getProfile().INTERSECTION_OF());
    }

    /**
     * <p>Answer true if this class is a complement class expression</p>
     * @return True if this is a complement class expression
     */
    @Override
    public boolean isComplementClass() {
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_10_10.coverage", "9a3b3bc0-c85a-4255-92a1-22b373ae4215");
        checkProfile(getProfile().COMPLEMENT_OF(), "COMPLEMENT_OF");
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_10_10.coverage", "2e5046e5-5abc-4239-899d-6bf3a8dcdcbb");
        return hasProperty(getProfile().COMPLEMENT_OF());
    }

    /**
     * <p>Answer true if this class is a property restriction</p>
     * @return True if this is a restriction
     */
    @Override
    public boolean isRestriction() {
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_10_10.coverage", "24e8eb65-c47d-44dd-b838-6782dd5077ef");
        checkProfile(getProfile().RESTRICTION(), "RESTRICTION");
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_10_10.coverage", "364e4c03-7639-4a29-b6cf-0fcb79d0b172");
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
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_10_10.coverage", "fec8957c-2075-4bd4-8950-424a0a93a7e0");
        setPropertyValue(getProfile().ONE_OF(), "ONE_OF", individuals);
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_10_10.coverage", "4c3314bb-45c8-457d-84ea-05d424ac987c");
        return as(EnumeratedClass.class);
    }

    /**
     * <p>Answer a view of this class as an intersection of the given classes.</p>
     * @param classes A list of the classes that will comprise the operands of the intersection
     * @return This ontology class, converted to an intersection of the given classes
     */
    @Override
    public IntersectionClass convertToIntersectionClass(RDFList classes) {
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_10_10.coverage", "b89f4c9a-2a0e-4e7b-b492-667a32016376");
        setPropertyValue(getProfile().INTERSECTION_OF(), "INTERSECTION_OF", classes);
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_10_10.coverage", "5e4c3eb4-08f4-452f-bb5a-6172f3c51c77");
        return as(IntersectionClass.class);
    }

    /**
     * <p>Answer a view of this class as a union of the given classes.</p>
     * @param classes A list of the classes that will comprise the operands of the union
     * @return This ontology class, converted to an union of the given classes
     */
    @Override
    public UnionClass convertToUnionClass(RDFList classes) {
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_10_10.coverage", "b706bc1d-20d0-42c9-b47b-b051ed347316");
        setPropertyValue(getProfile().UNION_OF(), "UNION_OF", classes);
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_10_10.coverage", "353e4acd-046b-4ce7-97e9-8c43103a9a3e");
        return as(UnionClass.class);
    }

    /**
     * <p>Answer a view of this class as an complement of the given class.</p>
     * @param cls An ontology classs that will be operand of the complement
     * @return This ontology class, converted to an complement of the given class
     */
    @Override
    public ComplementClass convertToComplementClass(Resource cls) {
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_10_10.coverage", "230f5d7a-c4e8-40b8-bb28-942e9790414c");
        setPropertyValue(getProfile().COMPLEMENT_OF(), "COMPLEMENT_OF", cls);
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_10_10.coverage", "f11b7194-6868-4916-bf8d-64a0447b139c");
        return as(ComplementClass.class);
    }

    /**
     * <p>Answer a view of this class as an resriction on the given property.</p>
     * @param prop A property this is the subject of a property restriction class expression
     * @return This ontology class, converted to a restriction on the given property
     */
    @Override
    public Restriction convertToRestriction(Property prop) {
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_10_10.coverage", "e728f758-c05f-492d-afae-4cb6f419ec73");
        if (!hasRDFType(getProfile().RESTRICTION(), "RESTRICTION", false)) {
            writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_10_10.coverage", "7e952022-3c1b-4d29-a62c-44c25ba5ed7c");
            setRDFType(getProfile().RESTRICTION());
        }
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_10_10.coverage", "3432dce2-fc84-4156-800d-caf8e9796384");
        setPropertyValue(getProfile().ON_PROPERTY(), "ON_PROPERTY", prop);
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_10_10.coverage", "9011e7a8-ea66-4bb8-8d17-3f3f7dad4324");
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
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_10_10.coverage", "0348be92-818b-4b0c-9344-d7b706ac5505");
        // we manually compute the maximal lower elements - this could be expensive in general
        // return ResourceUtils.maximalLowerElements( listSuperClasses(), getProfile().SUB_CLASS_OF(), false ).contains( cls );
        ExtendedIterator<OntClass> i = listDirectPropertyValues(getProfile().SUB_CLASS_OF(), "subClassOf", OntClass.class, getProfile().SUB_CLASS_OF(), true, false);
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_10_10.coverage", "47cd28db-5e7c-4dcc-b1ee-949b114893cc");
        try {
            writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_10_10.coverage", "17c6a483-d449-4d69-90ab-a5894ec50bec");
            while (i.hasNext()) {
                writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_10_10.coverage", "765e88cd-a8a0-4a89-adbb-7e17c34ef95c");
                if (cls.equals(i.next())) {
                    writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_10_10.coverage", "72a875c7-db50-43cf-9fbc-b868faff3a9a");
                    return true;
                }
            }
        } finally {
            writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_10_10.coverage", "c659987a-4b85-4e8e-8adc-51ffd52fb316");
            i.close();
        }
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_10_10.coverage", "03eb7cd3-d0f8-4cd2-892f-b8fb8ffa9cd4");
        return false;
    }

    /**
     * <p>Answer true if this class lies with the domain of p<p>
     * @param p
     * @param direct If true, only consider direct associations with domain
     * @return True if this class in the domain of property <code>p</code>
     */
    protected boolean testDomain(Property p, boolean direct) {
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_10_10.coverage", "77ba03e1-76ea-4a31-8a32-28f7b3a5b6dd");
        // we ignore any property in the OWL, etc namespace
        String namespace = p.getNameSpace();
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_10_10.coverage", "69dbb309-5dfa-47e7-a958-83e5122ab840");
        for (String IGNORE_NAMESPACE : IGNORE_NAMESPACES) {
            writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_10_10.coverage", "d1213855-a0d0-46f1-a77d-122617c14d63");
            if (namespace.equals(IGNORE_NAMESPACE)) {
                writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_10_10.coverage", "38d97b62-82af-4695-be9a-edf257d6784c");
                return false;
            }
        }
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_10_10.coverage", "860befd8-a887-4aaa-9e74-bac6227ff363");
        // check for global props, that have no specific domain constraint
        boolean isGlobal = true;
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_10_10.coverage", "aa286360-0867-44d1-a115-3b97b5ab1708");
        // flag for detecting the direct case
        boolean seenDirect = false;
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_10_10.coverage", "a357dee0-c396-4aab-be2b-eb3bab3fc561");
        for (StmtIterator i = getModel().listStatements(p, getProfile().DOMAIN(), (RDFNode) null); i.hasNext(); ) {
            writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_10_10.coverage", "84bd9d65-3a75-4bc7-bd9e-eaa1bd4358ca");
            Resource domain = i.nextStatement().getResource();
            writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_10_10.coverage", "8f68b13d-2c84-4645-bd9b-6aa5d176f169");
            // there are some well-known values we ignore
            if (!(domain.equals(getProfile().THING()) || domain.equals(RDFS.Resource))) {
                writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_10_10.coverage", "2f5d65f0-27ef-4ab6-8dff-ec05e6b58b30");
                // not a generic domain
                isGlobal = false;
                writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_10_10.coverage", "f91326af-f51a-4760-bfaa-de680f73b722");
                if (domain.equals(this)) {
                    writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_10_10.coverage", "55356b35-554f-4025-8c5f-637bf19e8cf3");
                    // if this class is actually in the domain (as opposed to one of this class's
                    // super-classes), then we've detected the direct property case
                    seenDirect = true;
                } else if (!canProveSuperClass(domain)) {
                    writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_10_10.coverage", "70e892b4-a586-4c2d-9d34-059c6283443f");
                    // there is a class in the domain of p that is not a super-class of this class
                    return false;
                }
            }
        }
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_10_10.coverage", "c3cfbc51-6d66-46cd-990b-05f4da5f6873");
        if (direct) {
            writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_10_10.coverage", "f33bdb2b-540f-44d2-8958-cbf19e6ed450");
            // or it's a global prop and this is a root class
            return seenDirect || (isGlobal && isHierarchyRoot());
        } else {
            writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_10_10.coverage", "7ca4b256-5c41-48de-b582-30790bc20760");
            // otherwise the 'return false' above would have kicked in
            return true;
        }
    }

    /**
     * <p>Answer an iterator over all of the properties in this model
     * @return An iterator over {@link OntProperty}
     */
    protected ExtendedIterator<Statement> listAllProperties() {
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_10_10.coverage", "5b8bf593-383b-4287-b7b9-78eeed655539");
        OntModel mOnt = (OntModel) getModel();
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_10_10.coverage", "e9f7c9f0-919f-46a7-a8e3-010e82986520");
        Profile prof = mOnt.getProfile();
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_10_10.coverage", "4edd6461-fc74-45de-a9f6-63f3442d9dfb");
        ExtendedIterator<Statement> pi = mOnt.listStatements(null, RDF.type, getProfile().PROPERTY());
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_10_10.coverage", "58214977-aad9-4453-ad6f-5cb68386da24");
        // check reasoner capabilities - major performance improvement for inf models
        if (mOnt.getReasoner() != null) {
            writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_10_10.coverage", "323eaddd-ccf6-4a5e-8ea1-e52e490edfff");
            Model caps = mOnt.getReasoner().getReasonerCapabilities();
            writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_10_10.coverage", "e2faba94-cbfd-4195-ab9f-89f70e6aa588");
            if (caps.contains(null, ReasonerVocabulary.supportsP, OWL.ObjectProperty)) {
                writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_10_10.coverage", "b45645cf-c427-4571-ab84-cb517a81243b");
                // all owl:ObjectProperty, owl:DatatypeProperty, etc, are rdf:Property resources
                return pi;
            }
        }
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_10_10.coverage", "47e07dad-6e34-48df-836a-d4ffe2b384d4");
        // otherwise, we manually check the other property types
        if (prof.OBJECT_PROPERTY() != null) {
            writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_10_10.coverage", "bcbd0bff-352f-4fa3-8b02-63a0721cfc4c");
            pi = pi.andThen(mOnt.listStatements(null, RDF.type, prof.OBJECT_PROPERTY()));
        }
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_10_10.coverage", "84caaf71-d65e-4f73-9a49-33f965a1291e");
        if (prof.DATATYPE_PROPERTY() != null) {
            writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_10_10.coverage", "3e16364a-ee63-4b76-90a3-1722d214da86");
            pi = pi.andThen(mOnt.listStatements(null, RDF.type, prof.DATATYPE_PROPERTY()));
        }
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_10_10.coverage", "4e6c4697-860e-491f-bfe7-1cc636bd1b4f");
        if (prof.FUNCTIONAL_PROPERTY() != null) {
            writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_10_10.coverage", "30590e71-85da-49c2-9f7d-b77bf4a71214");
            pi = pi.andThen(mOnt.listStatements(null, RDF.type, prof.FUNCTIONAL_PROPERTY()));
        }
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_10_10.coverage", "400f176d-c3d1-4570-9c38-a38b845e35ac");
        if (prof.INVERSE_FUNCTIONAL_PROPERTY() != null) {
            writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_10_10.coverage", "ec5cb159-1732-46b9-8f8e-b4c2efc4d956");
            pi = pi.andThen(mOnt.listStatements(null, RDF.type, prof.INVERSE_FUNCTIONAL_PROPERTY()));
        }
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_10_10.coverage", "42214406-4c52-4f72-ac9c-8ac8faef2dcf");
        if (prof.SYMMETRIC_PROPERTY() != null) {
            writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_10_10.coverage", "20d814b1-9242-4a6a-bc0c-a9359c2ee1cc");
            pi = pi.andThen(mOnt.listStatements(null, RDF.type, prof.SYMMETRIC_PROPERTY()));
        }
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_10_10.coverage", "35e7dc3f-d2e1-41dc-9378-027be8904586");
        if (prof.TRANSITIVE_PROPERTY() != null) {
            writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_10_10.coverage", "6a3d4de3-21ae-498f-9171-50a7c38631f3");
            pi = pi.andThen(mOnt.listStatements(null, RDF.type, prof.TRANSITIVE_PROPERTY()));
        }
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_10_10.coverage", "5c83d9cc-2cb0-49f7-ba7d-a88e0a77b91e");
        if (prof.ANNOTATION_PROPERTY() != null) {
            writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_10_10.coverage", "3acf8d34-2780-486f-af5d-2d8aeeb8ea2c");
            pi = pi.andThen(mOnt.listStatements(null, RDF.type, prof.ANNOTATION_PROPERTY()));
        }
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_10_10.coverage", "7a38f069-3c8e-4e18-8e80-3f1914970d46");
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
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_10_10.coverage", "54cf5614-03eb-4535-b9f6-cd18ee8004c0");
        OntModel om = (OntModel) getModel();
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_10_10.coverage", "a4528490-219d-4edf-8ae5-410a8a7e4c7c");
        if (om.getReasoner() != null) {
            writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_10_10.coverage", "fddd500f-66e4-40f4-bc6f-09438183e54d");
            if (om.getReasoner().getReasonerCapabilities().contains(null, ReasonerVocabulary.supportsP, RDFS.subClassOf)) {
                writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_10_10.coverage", "6d88173c-70a5-46b9-b516-109c1eee0bf0");
                // this reasoner does transitive closure on sub-classes, so we just ask
                return hasSuperClass(sup);
            }
        }
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_10_10.coverage", "96b00692-0def-45fb-8060-613ca9a40425");
        // otherwise, we have to search upwards through the class hierarchy
        Set<OntClass> seen = new HashSet<>();
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_10_10.coverage", "b6fd1831-137d-4f89-8c9c-4dff1e92a2d7");
        List<OntClass> queue = new ArrayList<>();
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_10_10.coverage", "93cf2b76-c124-4562-9478-4ff8cdca0275");
        queue.add(this);
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_10_10.coverage", "0a48c415-2a29-48c7-b232-1ab51d599789");
        while (!queue.isEmpty()) {
            writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_10_10.coverage", "aba0d06a-e99f-4a8c-bd2f-9d9b0f224b5a");
            OntClass c = queue.remove(0);
            writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_10_10.coverage", "ffe0a359-ac3a-4c88-8105-0b0f08943224");
            if (!seen.contains(c)) {
                writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_10_10.coverage", "9b54e059-d17e-48c2-8468-2e57efa0039a");
                seen.add(c);
                writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_10_10.coverage", "c5f81e6a-e585-465e-a859-1fc1979b5716");
                if (c.equals(sup)) {
                    writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_10_10.coverage", "d3bcdd26-87fd-4fbc-829c-911317312126");
                    // found the super class
                    return true;
                } else {
                    writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_10_10.coverage", "e38692d1-52c6-4d4c-a380-659447f1e11c");
                    // queue the supers
                    for (Iterator<OntClass> i = c.listSuperClasses(); i.hasNext(); ) {
                        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_10_10.coverage", "eced163f-0881-4e56-96bf-f5785cb7d738");
                        queue.add(i.next());
                    }
                }
            }
        }
        writeline("/home/ubuntu/results/coverage/OntClassImpl/OntClassImpl_10_10.coverage", "a6a2d177-b275-4a83-9837-fc4844f7f0fe");
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
