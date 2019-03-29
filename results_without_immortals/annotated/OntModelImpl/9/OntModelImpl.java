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
import java.io.InputStream;
import java.io.OutputStream;
import java.io.Reader;
import java.io.Writer;
import java.util.*;
import java.util.function.Predicate;
import org.apache.jena.enhanced.BuiltinPersonalities;
import org.apache.jena.enhanced.EnhNode;
import org.apache.jena.graph.Graph;
import org.apache.jena.graph.Node;
import org.apache.jena.graph.NodeFactory;
import org.apache.jena.graph.Triple;
import org.apache.jena.graph.compose.MultiUnion;
import org.apache.jena.ontology.*;
import org.apache.jena.rdf.listeners.StatementListener;
import org.apache.jena.rdf.model.*;
import org.apache.jena.rdf.model.impl.IteratorFactory;
import org.apache.jena.rdf.model.impl.ModelCom;
import org.apache.jena.reasoner.Derivation;
import org.apache.jena.reasoner.InfGraph;
import org.apache.jena.reasoner.Reasoner;
import org.apache.jena.reasoner.ValidityReport;
import org.apache.jena.shared.ConfigException;
import org.apache.jena.util.iterator.*;
import org.apache.jena.vocabulary.RDF;
import org.apache.jena.vocabulary.RDFS;
import org.apache.jena.vocabulary.ReasonerVocabulary;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import java.io.*;

/**
 * <p>
 * Implementation of a model that can process general ontologies in OWL
 * and similar languages.
 * </p>
 */
public class OntModelImpl extends ModelCom implements OntModel {

    // Constants
    // ////////////////////////////////
    /**
     * This variable is how the OntModel knows how to construct
     * a syntax checker. This part of the design may change.
     */
    public static String owlSyntaxCheckerClassName = "org.apache.jena.ontology.tidy.JenaChecker";

    // Static variables
    // ////////////////////////////////
    private static Logger s_log = LoggerFactory.getLogger(OntModelImpl.class);

    private static Class<?> owlSyntaxCheckerClass;

    // Instance variables
    // ////////////////////////////////
    /**
     * The model specification this model is using to define its structure
     */
    protected OntModelSpec m_spec;

    /**
     * List of URI strings of documents that have been imported into this one
     */
    protected Set<String> m_imported = new HashSet<String>();

    /**
     * Mode switch for strict checking mode
     */
    protected boolean m_strictMode = true;

    /**
     * The union graph that contains the imports closure - there is always one of these, which may also be _the_ graph for the model
     */
    protected MultiUnion m_union = new MultiUnion();

    /**
     * The listener that detects dynamically added or removed imports statements
     */
    protected ImportsListener m_importsListener = null;

    /**
     * Cached deductions model
     */
    private Model m_deductionsModel = null;

    /**
     * <p>
     * Construct a new ontology model, using the given model as a base.  The document manager
     * given in the specification object
     * will be used to build the imports closure of the model if its policy permits.
     * </p>
     *
     * @param model The base model that may contain existing statements for the ontology.
     * if it is null, a fresh model is created as the base.
     * @param spec A specification object that allows us to specify parameters and structure for the
     * ontology model to be constructed.
     */
    public OntModelImpl(OntModelSpec spec, Model model) {
        this(spec, makeBaseModel(spec, model), true);
    }

    /**
     * Construct a new ontology model from the given specification. The base model is
     * produced using the baseModelMaker.
     */
    public OntModelImpl(OntModelSpec spec) {
        this(spec, spec.createBaseModel(), true);
    }

    /**
     * @param spec the specification for the OntModel
     * @param model the base model [must be non-null]
     * @param withImports If true, we load the imports as sub-models
     */
    private OntModelImpl(OntModelSpec spec, Model model, boolean withImports) {
        // we haven't built the full graph yet, so we pass a vestigial form up to the super constructor
        super(generateGraph(spec, model.getGraph()), BuiltinPersonalities.model);
        m_spec = spec;
        // extract the union graph from whatever generateGraph() created
        m_union = (getGraph() instanceof MultiUnion) ? ((MultiUnion) getGraph()) : (MultiUnion) ((InfGraph) getGraph()).getRawGraph();
        if (withImports) {
            loadImports();
        }
        // set the default prefixes
        if (spec != null && spec.getKnownPrefixes() != null) {
            try {
                // Protect in case the graph is read-only.
                // Prefixes are hints
                String[][] p = spec.getKnownPrefixes();
                for (String[] pair : p) {
                    setNsPrefix(pair[0], pair[1]);
                }
            } catch (Exception ex) {
            }
        }
        // force the inference engine, if we have one, to see the new graph data
        rebind();
    }

    // External signature methods
    // ////////////////////////////////
    /**
     * <p>
     * Answer a reference to the document manager that this model is using to manage
     * ontology &lt;-&gt; mappings, and to load the imports closure. <strong>Note</strong>
     * the default ontology model {@linkplain OntModelSpec specifications} each have
     * a contained default document manager. Changing the document managers specified by
     * these default specification may (in fact, probably will)
     * affect other models built with the same specification
     * policy. This may or may not be as desired by the programmer!
     * </p>
     * @return A reference to this model's document manager, obtained from the specification object
     */
    @Override
    public OntDocumentManager getDocumentManager() {
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "907ac379-6ca1-4198-b8da-d221e48dc431");
        return m_spec.getDocumentManager();
    }

    /**
     * <p>
     * Answer an iterator that ranges over the ontology resources in this model, i&#046;e&#046;
     * the resources with <code>rdf:type Ontology</code> or equivalent. These resources
     * typically contain metadata about the ontology document that contains them.
     * </p>
     * <p>
     * Specifically, the resources in this iterator will those whose type corresponds
     * to the value given in the ontology vocabulary associated with this model, see
     * {@link Profile#ONTOLOGY}.
     * </p>
     * <p>
     * <strong>Note:</strong> the number of nodes returned by this iterator will vary according to
     * the completeness of the deductive extension of the underlying graph.  See class
     * overview for more details.
     * </p>
     *
     * @return An iterator over ontology resources.
     */
    @Override
    public ExtendedIterator<Ontology> listOntologies() {
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "a09e40f4-e146-4fa1-8385-904c34b217da");
        checkProfileEntry(getProfile().ONTOLOGY(), "ONTOLOGY");
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "cb6cb112-68ee-49bf-8056-aec47c4be01c");
        return findByTypeAs(getProfile().ONTOLOGY(), Ontology.class).filterKeep(new UniqueFilter<Ontology>());
    }

    /**
     * <p>
     * Answer an iterator that ranges over the property resources in this model, i&#046;e&#046;
     * the resources with <code>rdf:type Property</code> or equivalent.  An <code>OntProperty</code>
     * is equivalent to an <code>rdfs:Property</code> in a normal RDF graph; this type is
     * provided as a common super-type for the more specific {@link ObjectProperty} and
     * {@link DatatypeProperty} property types.
     * </p>
     * <p><strong>Note</strong> This method searches for nodes in the underlying model whose
     * <code>rdf:type</code> is <code>rdf:Property</code>. This type is <em>entailed</em> by
     * specific property sub-types, such as <code>owl:ObjectProperty</code>. An important
     * consequence of this is that in <em>models without an attached reasoner</em> (e.g. in the
     * <code>OWL_MEM</code> {@link OntModelSpec}), the entailed type will not be present
     * and this method will omit such properties from the returned iterator. <br />
     * <strong>Solution</strong> There are two
     * ways to address to this issue: either use a reasoning engine to ensure that type entailments
     * are taking place correctly, or call {@link #listAllOntProperties()}. Note
     * that <code>listAllOntProperties</code> is potentially less efficient than this method.</p>
     * <p>
     * The resources returned by this iterator will those whose type corresponds
     * to the value given in the ontology vocabulary associated with this model.
     * </p>
     *
     * @return An iterator over property resources.
     */
    @Override
    public ExtendedIterator<OntProperty> listOntProperties() {
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "d48ab70c-ef15-434e-bf7e-8d9505bc77fe");
        ExtendedIterator<OntProperty> i = findByTypeAs(RDF.Property, OntProperty.class).filterKeep(new UniqueFilter<OntProperty>());
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "4edf7155-8d4a-4fb0-a3a4-10fca2f6690a");
        // if we are in OWL_FULL, the properties should also include the annotation properties
        if (getReasoner() != null && getProfile().equals(ProfileRegistry.getInstance().getProfile(ProfileRegistry.OWL_LANG))) {
            writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "d7c8a1fe-6f5a-419b-803a-d7f4a9c311ee");
            // we are using a reasoner, and in OWL Full
            // so add the annotation properties too
            i = i.andThen(listAnnotationProperties());
        }
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "17cb90ab-9e36-46c2-ae47-84ee8e534489");
        return i;
    }

    /**
     * <p>Answer an iterator over all of the ontology properties in this model, including
     * object properties, datatype properties, annotation properties, etc. This method
     * takes a different approach to calculating the set of property resources to return,
     * and is robust against the absence of a reasoner attached to the model (see note
     * in {@link #listOntProperties()} for explanation). However, the calculation used by
     * this method is potentially less efficient than the alternative <code>listOntProperties()</code>.
     * Users whose models have an attached reasoner are recommended to use
     * {@link #listOntProperties()}.</p>
     * @return An iterator over all available properties in a model, irrespective of
     * whether a reasoner is available to perform <code>rdf:type</code> entailments.
     * Each property will appear exactly once in the iterator.
     */
    @Override
    public ExtendedIterator<OntProperty> listAllOntProperties() {
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "4792ac58-635b-4b74-b567-7aa77c457b14");
        ExtendedIterator<OntProperty> i = findByTypeAs(RDF.Property, OntProperty.class).andThen(listObjectProperties()).andThen(listDatatypeProperties()).andThen(listAnnotationProperties()).andThen(listFunctionalProperties()).andThen(listTransitiveProperties()).andThen(listSymmetricProperties());
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "d6140202-df8d-4996-8837-42d828f679b0");
        // we must filter for uniqueness
        return i.filterKeep(new UniqueFilter<OntProperty>());
    }

    /**
     * <p>
     * Answer an iterator that ranges over the object property resources in this model, i&#046;e&#046;
     * the resources with <code>rdf:type ObjectProperty</code> or equivalent.  An object
     * property is a property that is defined in the ontology language semantics as a
     * one whose range comprises individuals (rather than datatyped literals).
     * </p>
     * <p>
     * Specifically, the resources in this iterator will those whose type corresponds
     * to the value given in the ontology vocabulary associated with this model: see
     * {@link Profile#OBJECT_PROPERTY}.
     * </p>
     * <p>
     * <strong>Note:</strong> the number of nodes returned by this iterator will vary according to
     * the completeness of the deductive extension of the underlying graph.  See class
     * overview for more details.
     * </p>
     *
     * @return An iterator over object property resources.
     */
    @Override
    public ExtendedIterator<ObjectProperty> listObjectProperties() {
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "aeef556d-25e5-4e67-83cf-426edbd18164");
        checkProfileEntry(getProfile().OBJECT_PROPERTY(), "OBJECT_PROPERTY");
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "129eddd9-31f8-4c3f-a1e7-ab38c31f5bd2");
        return findByTypeAs(getProfile().OBJECT_PROPERTY(), ObjectProperty.class).filterKeep(new UniqueFilter<ObjectProperty>());
    }

    /**
     * <p>
     * Answer an iterator that ranges over the datatype property resources in this model, i&#046;e&#046;
     * the resources with <code>rdf:type DatatypeProperty</code> or equivalent.  An datatype
     * property is a property that is defined in the ontology language semantics as a
     * one whose range comprises datatyped literals (rather than individuals).
     * </p>
     * <p>
     * Specifically, the resources in this iterator will those whose type corresponds
     * to the value given in the ontology vocabulary associated with this model: see
     * {@link Profile#DATATYPE_PROPERTY}.
     * </p>
     * <p>
     * <strong>Note:</strong> the number of nodes returned by this iterator will vary according to
     * the completeness of the deductive extension of the underlying graph.  See class
     * overview for more details.
     * </p>
     *
     * @return An iterator over datatype property resources.
     */
    @Override
    public ExtendedIterator<DatatypeProperty> listDatatypeProperties() {
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "d9b8e73e-6883-47fb-aa47-7775c1d5ff0f");
        checkProfileEntry(getProfile().DATATYPE_PROPERTY(), "DATATYPE_PROPERTY");
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "2393e71c-7728-4b62-874d-1fb7bde8525c");
        return findByTypeAs(getProfile().DATATYPE_PROPERTY(), DatatypeProperty.class).filterKeep(new UniqueFilter<DatatypeProperty>());
    }

    /**
     * <p>
     * Answer an iterator that ranges over the functional property resources in this model, i&#046;e&#046;
     * the resources with <code>rdf:type FunctionalProperty</code> or equivalent.  A functional
     * property is a property that is defined in the ontology language semantics as having
     * a unique domain element for each instance of the relationship.
     * </p>
     * <p>
     * Specifically, the resources in this iterator will those whose type corresponds
     * to the value given in the ontology vocabulary associated with this model: see
     * {@link Profile#FUNCTIONAL_PROPERTY}.
     * </p>
     *
     * @return An iterator over functional property resources.
     */
    @Override
    public ExtendedIterator<FunctionalProperty> listFunctionalProperties() {
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "7624d34f-be70-4bc9-97c3-205ce240d2c3");
        checkProfileEntry(getProfile().FUNCTIONAL_PROPERTY(), "FUNCTIONAL_PROPERTY");
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "ceaf2124-b59d-48b1-8aa6-53bc3d1d3c1e");
        return findByTypeAs(getProfile().FUNCTIONAL_PROPERTY(), FunctionalProperty.class).filterKeep(new UniqueFilter<FunctionalProperty>());
    }

    /**
     * <p>
     * Answer an iterator that ranges over the transitive property resources in this model, i&#046;e&#046;
     * the resources with <code>rdf:type TransitiveProperty</code> or equivalent.
     * </p>
     * <p>
     * Specifically, the resources in this iterator will those whose type corresponds
     * to the value given in the ontology vocabulary associated with this model: see
     * {@link Profile#TRANSITIVE_PROPERTY}.
     * </p>
     *
     * @return An iterator over transitive property resources.
     */
    @Override
    public ExtendedIterator<TransitiveProperty> listTransitiveProperties() {
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "cd6b0d9d-ac48-464b-8794-cd0a53e57d53");
        checkProfileEntry(getProfile().TRANSITIVE_PROPERTY(), "TRANSITIVE_PROPERTY");
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "cb9c67a0-2461-4ba6-8def-416fde2a0395");
        return findByTypeAs(getProfile().TRANSITIVE_PROPERTY(), TransitiveProperty.class).filterKeep(new UniqueFilter<TransitiveProperty>());
    }

    /**
     * <p>
     * Answer an iterator that ranges over the symmetric property resources in this model, i&#046;e&#046;
     * the resources with <code>rdf:type SymmetricProperty</code> or equivalent.
     * </p>
     * <p>
     * Specifically, the resources in this iterator will those whose type corresponds
     * to the value given in the ontology vocabulary associated with this model: see
     * {@link Profile#SYMMETRIC_PROPERTY}.
     * </p>
     *
     * @return An iterator over symmetric property resources.
     */
    @Override
    public ExtendedIterator<SymmetricProperty> listSymmetricProperties() {
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "9986eed6-3ed1-496f-aaf8-f7d366e1a865");
        checkProfileEntry(getProfile().SYMMETRIC_PROPERTY(), "SYMMETRIC_PROPERTY");
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "ec006d2f-d111-49f2-b6d2-4a71499ce7b6");
        return findByTypeAs(getProfile().SYMMETRIC_PROPERTY(), SymmetricProperty.class).filterKeep(new UniqueFilter<SymmetricProperty>());
    }

    /**
     * <p>
     * Answer an iterator that ranges over the inverse functional property resources in this model, i&#046;e&#046;
     * the resources with <code>rdf:type InverseFunctionalProperty</code> or equivalent.
     * </p>
     * <p>
     * Specifically, the resources in this iterator will those whose type corresponds
     * to the value given in the ontology vocabulary associated with this model: see
     * {@link Profile#INVERSE_FUNCTIONAL_PROPERTY}.
     * </p>
     *
     * @return An iterator over inverse functional property resources.
     */
    @Override
    public ExtendedIterator<InverseFunctionalProperty> listInverseFunctionalProperties() {
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "a2468f3a-aa99-4a42-af20-0a94cdac7b23");
        checkProfileEntry(getProfile().INVERSE_FUNCTIONAL_PROPERTY(), "INVERSE_FUNCTIONAL_PROPERTY");
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "057315df-51d5-4931-a87d-bdd6fe4a91c7");
        return findByTypeAs(getProfile().INVERSE_FUNCTIONAL_PROPERTY(), InverseFunctionalProperty.class).filterKeep(new UniqueFilter<InverseFunctionalProperty>());
    }

    /**
     * <p>
     * Answer an iterator over the individuals in this model. Where possible, an individual
     * is defined as an instance of the <em>top</em> class in an ontology, i.e. <code>owl:Thing</code>
     * or <code>daml:Thing</code>. However, since this test relies on the presence of an inference
     * capability, and is not defined in cases where there is no <em>top</em> class (such as RDFS),
     * a secondary heuristic is used when needed: an individual is an instance of a class defined
     * in the ontology (i.e. it is a resource with an <code>rdf:type</code>, where the
     * <code>rdf:type</code> of that resource is a class or restriction in the ontology.
     * </p>
     *
     * @return An iterator over Individuals.
     */
    @Override
    public ExtendedIterator<Individual> listIndividuals() {
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "5052c177-3d27-46a2-b94f-cf37fc2048ac");
        // since the reasoner implements some OWL full functionality for RDF compatibility, we
        // have to decide which strategy to use for identifying individuals depending on whether
        // or not a powerful reasoner (i.e. owl:Thing/daml:Thing aware) is being used with this model
        boolean supportsIndAsThing = false;
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "76944046-a52d-48ce-b2b3-e3f877796cb4");
        if (getGraph() instanceof InfGraph) {
            writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "b4650dd3-f17d-4cce-9e0c-8e83c268eb42");
            supportsIndAsThing = ((InfGraph) getGraph()).getReasoner().getReasonerCapabilities().contains(null, ReasonerVocabulary.supportsP, ReasonerVocabulary.individualAsThingP);
        }
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "32a7f862-e536-419c-bb29-212771d35bb0");
        if (!supportsIndAsThing || (getProfile().THING() == null) || getProfile().CLASS().equals(RDFS.Class)) {
            writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "cbb3c022-084f-473b-86ea-24d2a07ebb05");
            // no inference, or we are in RDFS land, so we pick things that have rdf:type whose rdf:type is Class
            // it's tricky to make this efficient and cover all possible cases. I've changed the code to
            // make use of the isIndividual() test on OntResource, at the expense of some redundant queries
            // to the model, which could become expensive in the case of a DB model - ijd Apr-23-09
            Set<Individual> results = new HashSet<Individual>();
            writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "dea22d97-7c21-4d58-86b2-baf6942f7c0d");
            for (Iterator<Statement> i = listStatements(null, RDF.type, (RDFNode) null); i.hasNext(); ) {
                writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "eaa808bd-85e4-40ff-b764-4aa678e8a516");
                OntResource r = i.next().getSubject().as(OntResource.class);
                writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "b1120c31-1f97-467c-b710-9661dca4097c");
                if (r.isIndividual()) {
                    writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "1ae257e8-615e-437a-b38d-eec198fe1f4c");
                    results.add(r.as(Individual.class));
                }
            }
            writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "1cdbda20-c02b-47d4-8fbe-766aaee494a7");
            return WrappedIterator.create(results.iterator());
        } else {
            writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "e1335ffb-c8fd-4d11-877b-f266d06c7e8d");
            // we have inference, so we pick the nodes that are of type Thing
            return findByTypeAs(getProfile().THING(), Individual.class).filterKeep(new UniqueFilter<Individual>());
        }
    }

    /**
     * <p>
     * Answer an iterator that ranges over the resources in this model that are
     * instances of the given class.
     * </p>
     *
     * @return An iterator over individual resources whose <code>rdf:type</code>
     * is <code>cls</code>.
     */
    @Override
    public ExtendedIterator<Individual> listIndividuals(Resource cls) {
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "c1092fc5-6365-407a-83f1-f1a5ff5366ea");
        return findByTypeAs(cls, Individual.class).filterKeep(new UniqueFilter<Individual>());
    }

    /**
     * <p>
     * Answer an iterator that ranges over all of the various forms of class description resource
     * in this model.  Class descriptions include {@link #listEnumeratedClasses enumerated}
     * classes, {@link #listUnionClasses union} classes, {@link #listComplementClasses complement}
     * classes, {@link #listIntersectionClasses intersection} classes, {@link #listClasses named}
     * classes and {@link #listRestrictions property restrictions}.
     * </p>
     * <p>
     * <strong>Note:</strong> the number of nodes returned by this iterator will vary according to
     * the completeness of the deductive extension of the underlying graph.  See class
     * overview for more details.
     * </p>
     *
     * @return An iterator over class description resources.
     */
    @Override
    public ExtendedIterator<OntClass> listClasses() {
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "00440677-868a-405d-a10e-2e6e37a5893d");
        return findByTypeAs(getProfile().getClassDescriptionTypes(), OntClass.class).filterKeep(new UniqueFilter<OntClass>());
    }

    /**
     * <p>Answer an iterator over the classes in this ontology model that represent
     * the uppermost nodes of the class hierarchy.  Depending on the underlying
     * reasoner configuration, if any, these will be calculated as the classes
     * that have Top (i.e. <code>owl:Thing</code> or <code>daml:Thing</code>)
     * as a direct super-class, or the classes which have no declared super-class.</p>
     * @return An iterator of the root classes in the local class hierarchy
     */
    @Override
    public ExtendedIterator<OntClass> listHierarchyRootClasses() {
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "6724448e-bef7-4df5-81ad-324ece6b5da1");
        // look for the shortcut of using direct subClass on :Thing
        if (getReasoner() != null) {
            writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "adb9d464-2689-4bc7-bcda-ac3106b55069");
            Model conf = getReasoner().getReasonerCapabilities();
            writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "c4f57f88-7d6b-429a-bf39-25f3f393fc8c");
            if (conf != null && conf.contains(null, ReasonerVocabulary.supportsP, ReasonerVocabulary.directSubClassOf) && getProfile().THING() != null) {
                writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "da9144e7-8839-4b29-9a82-d6f59634aee1");
                // we have have both direct sub-class of and a :Thing class to test against
                return listStatements(null, ReasonerVocabulary.directSubClassOf, getProfile().THING()).mapWith(s -> s.getSubject().as(OntClass.class));
            }
        }
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "ce5ab623-4cd5-406c-8921-a873c1e1e7c3");
        // no easy shortcut, so we use brute force
        return listClasses().filterDrop(OntResource::isOntLanguageTerm).filterKeep(OntClass::isHierarchyRoot);
    }

    /**
     * <p>
     * Answer an iterator that ranges over the enumerated class class-descriptions
     * in this model, i&#046;e&#046; the class resources specified to have a property
     * <code>oneOf</code> (or equivalent) and a list of values.
     * </p>
     * <p>
     * <strong>Note:</strong> the number of nodes returned by this iterator will vary according to
     * the completeness of the deductive extension of the underlying graph.  See class
     * overview for more details.
     * </p>
     *
     * @return An iterator over enumerated class resources.
     * @see Profile#ONE_OF
     */
    @Override
    public ExtendedIterator<EnumeratedClass> listEnumeratedClasses() {
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "17059b0d-1716-41c9-9e7e-eb95857ac0ba");
        checkProfileEntry(getProfile().ONE_OF(), "ONE_OF");
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "ae13a12e-2b6f-4664-a29d-ee885ede8487");
        return findByDefiningPropertyAs(getProfile().ONE_OF(), EnumeratedClass.class).filterKeep(new UniqueFilter<EnumeratedClass>());
    }

    /**
     * <p>
     * Answer an iterator that ranges over the union class-descriptions
     * in this model, i&#046;e&#046; the class resources specified to have a property
     * <code>unionOf</code> (or equivalent) and a list of values.
     * </p>
     * <p>
     * <strong>Note:</strong> the number of nodes returned by this iterator will vary according to
     * the completeness of the deductive extension of the underlying graph.  See class
     * overview for more details.
     * </p>
     *
     * @return An iterator over union class resources.
     * @see Profile#UNION_OF
     */
    @Override
    public ExtendedIterator<UnionClass> listUnionClasses() {
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "e34845ec-756b-4300-88f0-f784704d5526");
        checkProfileEntry(getProfile().UNION_OF(), "UNION_OF");
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "d6dcb095-9b02-4761-b287-f5dbb038fe14");
        return findByDefiningPropertyAs(getProfile().UNION_OF(), UnionClass.class).filterKeep(new UniqueFilter<UnionClass>());
    }

    /**
     * <p>
     * Answer an iterator that ranges over the complement class-descriptions
     * in this model, i&#046;e&#046; the class resources specified to have a property
     * <code>complementOf</code> (or equivalent) and a list of values.
     * </p>
     * <p>
     * <strong>Note:</strong> the number of nodes returned by this iterator will vary according to
     * the completeness of the deductive extension of the underlying graph.  See class
     * overview for more details.
     * </p>
     *
     * @return An iterator over complement class resources.
     * @see Profile#COMPLEMENT_OF
     */
    @Override
    public ExtendedIterator<ComplementClass> listComplementClasses() {
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "cb943a31-742a-4d9e-b5e1-485e4e2474cc");
        checkProfileEntry(getProfile().COMPLEMENT_OF(), "COMPLEMENT_OF");
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "4e07a4a8-2589-41b9-bc55-5d012667fdde");
        return findByDefiningPropertyAs(getProfile().COMPLEMENT_OF(), ComplementClass.class).filterKeep(new UniqueFilter<ComplementClass>());
    }

    /**
     * <p>
     * Answer an iterator that ranges over the intersection class-descriptions
     * in this model, i&#046;e&#046; the class resources specified to have a property
     * <code>intersectionOf</code> (or equivalent) and a list of values.
     * </p>
     * <p>
     * <strong>Note:</strong> the number of nodes returned by this iterator will vary according to
     * the completeness of the deductive extension of the underlying graph.  See class
     * overview for more details.
     * </p>
     *
     * @return An iterator over complement class resources.
     * @see Profile#INTERSECTION_OF
     */
    @Override
    public ExtendedIterator<IntersectionClass> listIntersectionClasses() {
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "02f0f511-2095-4c93-ab1f-3a918ed02805");
        checkProfileEntry(getProfile().INTERSECTION_OF(), "INTERSECTION_OF");
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "795e9abf-b107-4b3b-9015-0a62e2411efa");
        return findByDefiningPropertyAs(getProfile().INTERSECTION_OF(), IntersectionClass.class).filterKeep(new UniqueFilter<IntersectionClass>());
    }

    /**
     * <p>
     * Answer an iterator that ranges over the named class-descriptions
     * in this model, i&#046;e&#046; resources with <code>rdf:type
     * Class</code> (or equivalent) and a node URI.
     * </p>
     * <p>
     * <strong>Note:</strong> the number of nodes returned by this iterator will vary according to
     * the completeness of the deductive extension of the underlying graph.  See class
     * overview for more details.
     * </p>
     *
     * @return An iterator over named class resources.
     */
    @Override
    public ExtendedIterator<OntClass> listNamedClasses() {
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "472ebd5f-6532-40f3-bd4d-250396c349ce");
        return listClasses().filterDrop(OntClass::isAnon);
    }

    /**
     * <p>
     * Answer an iterator that ranges over the property restriction class-descriptions
     * in this model, i&#046;e&#046; resources with <code>rdf:type
     * Restriction</code> (or equivalent).
     * </p>
     * <p>
     * <strong>Note:</strong> the number of nodes returned by this iterator will vary according to
     * the completeness of the deductive extension of the underlying graph.  See class
     * overview for more details.
     * </p>
     *
     * @return An iterator over restriction class resources.
     * @see Profile#RESTRICTION
     */
    @Override
    public ExtendedIterator<Restriction> listRestrictions() {
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "2e2391c5-7de6-48bf-9c10-a700bc7db6c2");
        checkProfileEntry(getProfile().RESTRICTION(), "RESTRICTION");
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "3fe789dc-2e4d-4666-979e-871938519748");
        return findByTypeAs(getProfile().RESTRICTION(), Restriction.class).filterKeep(new UniqueFilter<Restriction>());
    }

    /**
     * <p>
     * Answer an iterator that ranges over the nodes that denote pair-wise disjointness between
     * sets of classes.
     * </p>
     * <p>
     * <strong>Note:</strong> the number of nodes returned by this iterator will vary according to
     * the completeness of the deductive extension of the underlying graph.  See class
     * overview for more details.
     * </p>
     *
     * @return An iterator over AllDifferent nodes.
     */
    @Override
    public ExtendedIterator<AllDifferent> listAllDifferent() {
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "60784743-7243-4316-b6c0-fd9e39a11ba0");
        checkProfileEntry(getProfile().ALL_DIFFERENT(), "ALL_DIFFERENT");
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "f70f0fb0-5fe8-41a8-8f26-27a51b0162aa");
        return findByTypeAs(getProfile().ALL_DIFFERENT(), AllDifferent.class).filterKeep(new UniqueFilter<AllDifferent>());
    }

    /**
     * <p>Answer an iterator over the DataRange objects in this ontology, if there
     * are any.</p>
     * @return An iterator, whose values are {@link DataRange} objects.
     */
    @Override
    public ExtendedIterator<DataRange> listDataRanges() {
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "054a17a1-d1cf-4aa0-ac99-963a7f085021");
        checkProfileEntry(getProfile().DATARANGE(), "DATARANGE");
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "e7bb80ec-5ea8-4974-8166-a2c13d484108");
        return findByTypeAs(getProfile().DATARANGE(), DataRange.class).filterKeep(new UniqueFilter<DataRange>());
    }

    /**
     * <p>
     * Answer an iterator that ranges over the properties in this model that are declared
     * to be annotation properties. Not all supported languages define annotation properties
     * (the category of annotation properties is chiefly an OWL innovation).
     * </p>
     * <p>
     * <strong>Note:</strong> the number of nodes returned by this iterator will vary according to
     * the completeness of the deductive extension of the underlying graph.  See class
     * overview for more details.
     * </p>
     *
     * @return An iterator over annotation properties.
     * @see Profile#getAnnotationProperties()
     */
    @Override
    public ExtendedIterator<AnnotationProperty> listAnnotationProperties() {
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "8e982c36-8de1-4a7e-ad60-6f7d587ae6f7");
        checkProfileEntry(getProfile().ANNOTATION_PROPERTY(), "ANNOTATION_PROPERTY");
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "17c8dc9c-ff2f-45e9-8cb4-b0c4645099a8");
        Resource r = getProfile().ANNOTATION_PROPERTY();
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "c7656e67-454a-4027-840a-a48ba52ac73c");
        if (r == null) {
            writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "c0e31f93-76d2-42a0-b9b9-2e9ecd984342");
            return new NullIterator<AnnotationProperty>();
        } else {
            writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "140e6162-ce68-49c4-8992-14ac99f57e91");
            return findByType(r).mapWith(p -> getNodeAs(p.getSubject(), AnnotationProperty.class)).filterKeep(new UniqueFilter<AnnotationProperty>());
        }
    }

    /**
     * <p>
     * Answer a resource that represents an ontology description node in this model. If a resource
     * with the given uri exists in the model, and can be viewed as an Ontology, return the
     * Ontology facet, otherwise return null.
     * </p>
     *
     * @param uri The uri for the ontology node. Conventionally, this corresponds to the base URI
     * of the document itself.
     * @return An Ontology resource or null.
     */
    @Override
    public Ontology getOntology(String uri) {
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "7de4c61c-90dd-49b8-84af-1a0d48e4231a");
        return (Ontology) findByURIAs(uri, Ontology.class);
    }

    /**
     * <p>
     * Answer a resource that represents an Individual node in this model. If a resource
     * with the given uri exists in the model, and can be viewed as an Individual, return the
     * Individual facet, otherwise return null.
     * </p>
     *
     * @param uri The URI for the requried individual
     * @return An Individual resource or null.
     */
    @Override
    public Individual getIndividual(String uri) {
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "d7766fb7-a290-4550-87be-dbbe5d766b37");
        return (Individual) findByURIAs(uri, Individual.class);
    }

    /**
     * <p>
     * Answer a resource representing an generic property in this model. If a property
     * with the given uri exists in the model, return the
     * OntProperty facet, otherwise return null.
     * </p>
     *
     * @param uri The uri for the property.
     * @return An OntProperty resource or null.
     */
    @Override
    public OntProperty getOntProperty(String uri) {
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "fe7a46ef-d6a6-4b93-b657-7601f735f5b9");
        return (OntProperty) findByURIAs(uri, OntProperty.class);
    }

    /**
     * <p>
     * Answer a resource representing an object property in this model. If a resource
     * with the given uri exists in the model, and can be viewed as an ObjectProperty, return the
     * ObjectProperty facet, otherwise return null.
     * </p>
     *
     * @param uri The uri for the object property. May not be null.
     * @return An ObjectProperty resource or null.
     */
    @Override
    public ObjectProperty getObjectProperty(String uri) {
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "ac33f287-94e5-4415-a0b9-fb236e823e17");
        return (ObjectProperty) findByURIAs(uri, ObjectProperty.class);
    }

    /**
     * <p>Answer a resource representing a transitive property. If a resource
     * with the given uri exists in the model, and can be viewed as a TransitiveProperty, return the
     * TransitiveProperty facet, otherwise return null. </p>
     * @param uri The uri for the property. May not be null.
     * @return A TransitiveProperty resource or null
     */
    @Override
    public TransitiveProperty getTransitiveProperty(String uri) {
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "302fe97d-d07c-4479-a608-2867e0d76cb3");
        return (TransitiveProperty) findByURIAs(uri, TransitiveProperty.class);
    }

    /**
     * <p>Answer a resource representing a symmetric property. If a resource
     * with the given uri exists in the model, and can be viewed as a SymmetricProperty, return the
     * SymmetricProperty facet, otherwise return null. </p>
     * @param uri The uri for the property. May not be null.
     * @return A SymmetricProperty resource or null
     */
    @Override
    public SymmetricProperty getSymmetricProperty(String uri) {
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "148fb309-227e-4464-8bef-d9bc49b81ad0");
        return (SymmetricProperty) findByURIAs(uri, SymmetricProperty.class);
    }

    /**
     * <p>Answer a resource representing an inverse functional property. If a resource
     * with the given uri exists in the model, and can be viewed as a InverseFunctionalProperty, return the
     * InverseFunctionalProperty facet, otherwise return null. </p>
     * @param uri The uri for the property. May not be null.
     * @return An InverseFunctionalProperty resource or null
     */
    @Override
    public InverseFunctionalProperty getInverseFunctionalProperty(String uri) {
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "c5b89606-0822-4aea-a9f4-fb3cde2c2e3e");
        return (InverseFunctionalProperty) findByURIAs(uri, InverseFunctionalProperty.class);
    }

    /**
     * <p>
     * Answer a resource that represents datatype property in this model. . If a resource
     * with the given uri exists in the model, and can be viewed as a DatatypeProperty, return the
     * DatatypeProperty facet, otherwise return null.
     * </p>
     *
     * @param uri The uri for the datatype property. May not be null.
     * @return A DatatypeProperty resource or null
     */
    @Override
    public DatatypeProperty getDatatypeProperty(String uri) {
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "0c31396d-7ad6-4bf1-9f4f-adee6b7fa272");
        return (DatatypeProperty) findByURIAs(uri, DatatypeProperty.class);
    }

    /**
     * <p>
     * Answer a resource that represents an annotation property in this model. If a resource
     * with the given uri exists in the model, and can be viewed as an AnnotationProperty, return the
     * AnnotationProperty facet, otherwise return null.
     * </p>
     *
     * @param uri The uri for the annotation property. May not be null.
     * @return An AnnotationProperty resource or null
     */
    @Override
    public AnnotationProperty getAnnotationProperty(String uri) {
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "bc7a4c36-808c-4158-9d61-11ec67dcd9d4");
        return (AnnotationProperty) findByURIAs(uri, AnnotationProperty.class);
    }

    /**
     * <p>
     * Answer a resource that represents a class description node in this model. If a resource
     * with the given uri exists in the model, and can be viewed as an OntClass, return the
     * OntClass facet, otherwise return null.
     * </p>
     *
     * @param uri The uri for the class node, or null for an anonymous class.
     * @return An OntClass resource or null.
     */
    @Override
    public OntClass getOntClass(String uri) {
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "cd56c55f-87a2-4b3e-b82d-fcfb0b929744");
        OntClass c = (OntClass) findByURIAs(uri, OntClass.class);
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "3f68936b-9ba9-4924-b09a-1a89a7e6040d");
        // special case for nothing and thing
        if (c == null) {
            writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "70eab97a-c862-4913-b7dc-9632f621ce5b");
            Resource thing = getProfile().THING();
            writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "8a887ed7-3b95-4270-9395-223038812084");
            if (thing != null && thing.getURI().equals(uri)) {
                writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "f65aa377-8899-43d1-ad8e-f7958bd9260c");
                c = thing.inModel(this).as(OntClass.class);
            }
            writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "165de273-fc76-4ad4-9bdc-124d767e2b86");
            Resource nothing = getProfile().NOTHING();
            writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "5853b536-2606-47b3-8ef9-8ad93b5d4b5f");
            if (nothing != null && nothing.getURI().equals(uri)) {
                writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "39fbb5e9-1a04-4150-a304-d9f39b926b1d");
                c = nothing.inModel(this).as(OntClass.class);
            }
        }
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "4349b6f2-c383-4fca-986f-83336530494f");
        return c;
    }

    /**
     * <p>Answer a resource representing the class that is the complement of another class. If a resource
     * with the given uri exists in the model, and can be viewed as a ComplementClass, return the
     * ComplementClass facet, otherwise return null. </p>
     * @param uri The URI of the new complement class.
     * @return A complement class or null
     */
    @Override
    public ComplementClass getComplementClass(String uri) {
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "befa3c67-ea1d-420e-94c5-0bfd99ede25e");
        return (ComplementClass) findByURIAs(uri, ComplementClass.class);
    }

    /**
     * <p>Answer a resource representing the class that is the enumeration of a list of individuals. If a resource
     * with the given uri exists in the model, and can be viewed as an EnumeratedClass, return the
     * EnumeratedClass facet, otherwise return null. </p>
     * @param uri The URI of the new enumeration class.
     * @return An enumeration class or null
     */
    @Override
    public EnumeratedClass getEnumeratedClass(String uri) {
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "d9d7ec33-9461-4fb3-945b-d2037f5b3285");
        return (EnumeratedClass) findByURIAs(uri, EnumeratedClass.class);
    }

    /**
     * <p>Answer a resource representing the class that is the union of a list of class desctiptions. If a resource
     * with the given uri exists in the model, and can be viewed as a UnionClass, return the
     * UnionClass facet, otherwise return null. </p>
     * @param uri The URI of the new union class.
     * @return A union class description or null
     */
    @Override
    public UnionClass getUnionClass(String uri) {
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "47273d50-7330-446e-bb58-ee47a52aeee5");
        return (UnionClass) findByURIAs(uri, UnionClass.class);
    }

    /**
     * <p>Answer a resource representing the class that is the intersection of a list of class descriptions. If a resource
     * with the given uri exists in the model, and can be viewed as a IntersectionClass, return the
     * IntersectionClass facet, otherwise return null. </p>
     * @param uri The URI of the new intersection class.
     * @return An intersection class description or null
     */
    @Override
    public IntersectionClass getIntersectionClass(String uri) {
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "3c0d6437-4f8b-495a-9f92-bd719831d387");
        return (IntersectionClass) findByURIAs(uri, IntersectionClass.class);
    }

    /**
     * <p>
     * Answer a resource that represents a property restriction in this model. If a resource
     * with the given uri exists in the model, and can be viewed as a Restriction, return the
     * Restriction facet, otherwise return null.
     * </p>
     *
     * @param uri The uri for the restriction node.
     * @return A Restriction resource or null
     */
    @Override
    public Restriction getRestriction(String uri) {
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "0cb22334-c7f5-4c31-b34f-b756d3d92225");
        return (Restriction) findByURIAs(uri, Restriction.class);
    }

    /**
     * <p>Answer a class description defined as the class of those individuals that have the given
     * resource as the value of the given property. If a resource
     * with the given uri exists in the model, and can be viewed as a HasValueRestriction, return the
     * HasValueRestriction facet, otherwise return null. </p>
     *
     * @param uri The URI for the restriction
     * @return A resource representing a has-value restriction or null
     */
    @Override
    public HasValueRestriction getHasValueRestriction(String uri) {
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "88740830-e784-41b7-ac7c-d335ba72d8f2");
        return (HasValueRestriction) findByURIAs(uri, HasValueRestriction.class);
    }

    /**
     * <p>Answer a class description defined as the class of those individuals that have at least
     * one property with a value belonging to the given class. If a resource
     * with the given uri exists in the model, and can be viewed as a SomeValuesFromRestriction, return the
     * SomeValuesFromRestriction facet, otherwise return null. </p>
     *
     * @param uri The URI for the restriction
     * @return A resource representing a some-values-from restriction, or null
     */
    @Override
    public SomeValuesFromRestriction getSomeValuesFromRestriction(String uri) {
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "79880abd-f4b6-4b99-8688-690c57e67aff");
        return (SomeValuesFromRestriction) findByURIAs(uri, SomeValuesFromRestriction.class);
    }

    /**
     * <p>Answer a class description defined as the class of those individuals for which all values
     * of the given property belong to the given class. If a resource
     * with the given uri exists in the model, and can be viewed as an AllValuesFromResriction, return the
     * AllValuesFromRestriction facet, otherwise return null. </p>
     *
     * @param uri The URI for the restriction
     * @return A resource representing an all-values-from restriction or null
     */
    @Override
    public AllValuesFromRestriction getAllValuesFromRestriction(String uri) {
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "07ae13f0-f8b2-491f-9b82-eea31f982b4b");
        return (AllValuesFromRestriction) findByURIAs(uri, AllValuesFromRestriction.class);
    }

    /**
     * <p>Answer a class description defined as the class of those individuals that have exactly
     * the given number of values for the given property. If a resource
     * with the given uri exists in the model, and can be viewed as a CardinalityRestriction, return the
     * CardinalityRestriction facet, otherwise return null. </p>
     *
     * @param uri The URI for the restriction
     * @return A resource representing a has-value restriction, or null
     */
    @Override
    public CardinalityRestriction getCardinalityRestriction(String uri) {
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "b0d51f95-22e3-43b1-a829-1abfb93af59c");
        return (CardinalityRestriction) findByURIAs(uri, CardinalityRestriction.class);
    }

    /**
     * <p>Answer a class description defined as the class of those individuals that have at least
     * the given number of values for the given property. If a resource
     * with the given uri exists in the model, and can be viewed as a MinCardinalityRestriction, return the
     * MinCardinalityRestriction facet, otherwise return null. </p>
     *
     * @param uri The URI for the restriction
     * @return A resource representing a min-cardinality restriction, or null
     */
    @Override
    public MinCardinalityRestriction getMinCardinalityRestriction(String uri) {
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "7f2a8394-d1ea-449e-b479-f1cfa10bf770");
        return (MinCardinalityRestriction) findByURIAs(uri, MinCardinalityRestriction.class);
    }

    /**
     * <p>Answer a class description defined as the class of those individuals that have at most
     * the given number of values for the given property. If a resource
     * with the given uri exists in the model, and can be viewed as a MaxCardinalityRestriction, return the
     * MaxCardinalityRestriction facet, otherwise return null.</p>
     *
     * @param uri The URI for the restriction
     * @return A resource representing a mas-cardinality restriction, or null
     */
    @Override
    public MaxCardinalityRestriction getMaxCardinalityRestriction(String uri) {
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "99dc5a67-f13c-4ab3-8dbf-2ffe76db448b");
        return (MaxCardinalityRestriction) findByURIAs(uri, MaxCardinalityRestriction.class);
    }

    /**
     * <p>Answer a class description defined as the class of those individuals that have a property
     * p, all values of which are members of a given class. Typically used with a cardinality constraint.
     * If a resource
     * with the given uri exists in the model, and can be viewed as a QualifiedRestriction, return the
     * QualifiedRestriction facet, otherwise return null.</p>
     *
     * @param uri The URI for the restriction
     * @return A resource representing a qualified restriction, or null
     */
    @Override
    public QualifiedRestriction getQualifiedRestriction(String uri) {
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "9195b1ca-cabb-482c-81de-583a6eed8430");
        return (QualifiedRestriction) findByURIAs(uri, QualifiedRestriction.class);
    }

    /**
     * <p>Answer a class description defined as the class of those individuals that have a property
     * p, with cardinality N, all values of which are members of a given class.
     * If a resource
     * with the given uri exists in the model, and can be viewed as a CardinalityQRestriction, return the
     * CardinalityQRestriction facet, otherwise return null.</p>
     *
     * @param uri The URI for the restriction
     * @return A resource representing a qualified cardinality restriction, or null
     */
    @Override
    public CardinalityQRestriction getCardinalityQRestriction(String uri) {
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "e355c0b0-8b62-4c6f-9fee-fbf7991ff7c1");
        return (CardinalityQRestriction) findByURIAs(uri, CardinalityQRestriction.class);
    }

    /**
     * <p>Answer a class description defined as the class of those individuals that have a property
     * p, with min cardinality N, all values of which are members of a given class.
     * If a resource
     * with the given uri exists in the model, and can be viewed as a MinCardinalityQRestriction, return the
     * MinCardinalityQRestriction facet, otherwise return null.</p>
     *
     * @param uri The URI for the restriction
     * @return A resource representing a qualified min cardinality restriction, or null
     */
    @Override
    public MinCardinalityQRestriction getMinCardinalityQRestriction(String uri) {
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "00914343-8c9d-42d4-ae7f-6c8a29cec20c");
        return (MinCardinalityQRestriction) findByURIAs(uri, MinCardinalityQRestriction.class);
    }

    /**
     * <p>Answer a class description defined as the class of those individuals that have a property
     * p, with max cardinality N, all values of which are members of a given class.
     * If a resource
     * with the given uri exists in the model, and can be viewed as a MaxCardinalityQRestriction, return the
     * MaxCardinalityQRestriction facet, otherwise return null.</p>
     *
     * @param uri The URI for the restriction
     * @return A resource representing a qualified max cardinality restriction, or null
     */
    @Override
    public MaxCardinalityQRestriction getMaxCardinalityQRestriction(String uri) {
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "470417ee-252e-4db4-9ddd-d85c73cbf89f");
        return (MaxCardinalityQRestriction) findByURIAs(uri, MaxCardinalityQRestriction.class);
    }

    /**
     * <p>
     * Answer a resource that represents an ontology description node in this model. If a resource
     * with the given uri exists in the model, it will be re-used.  If not, a new one is created in
     * the updateable sub-graph of the ontology model.
     * </p>
     *
     * @param uri The uri for the ontology node. Conventionally, this corresponds to the base URI
     * of the document itself.
     * @return An Ontology resource.
     */
    @Override
    public Ontology createOntology(String uri) {
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "4310e353-4dbe-4717-9cb5-9577e0ff2127");
        checkProfileEntry(getProfile().ONTOLOGY(), "ONTOLOGY");
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "8bfcbb07-3411-4cd9-be63-2237d4acc45c");
        return createOntResource(Ontology.class, getProfile().ONTOLOGY(), uri);
    }

    /**
     * <p>
     * Answer a resource that represents an Indvidual node in this model. A new anonymous resource
     * will be created in the updateable sub-graph of the ontology model.
     * </p>
     *
     * @param cls Resource representing the ontology class to which the individual belongs
     * @return A new anoymous Individual of the given class.
     */
    @Override
    public Individual createIndividual(Resource cls) {
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "ab1569a4-559a-48f0-884b-35ca1e92c5a5");
        return createOntResource(Individual.class, cls, null);
    }

    /**
     * <p>
     * Answer a resource that represents an Individual node in this model. If a resource
     * with the given uri exists in the model, it will be re-used.  If not, a new one is created in
     * the updateable sub-graph of the ontology model.
     * </p>
     *
     * @param cls Resource representing the ontology class to which the individual belongs
     * @param uri The uri for the individual, or null for an anonymous individual.
     * @return An Individual resource.
     */
    @Override
    public Individual createIndividual(String uri, Resource cls) {
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "a00a9089-b749-47b0-a803-43e3550c9954");
        return createOntResource(Individual.class, cls, uri);
    }

    /**
     * <p>
     * Answer a resource representing an generic property in this model.  Effectively
     * this method is an alias for {@link #createProperty( String )}, except that
     * the return type is {@link OntProperty}, which allow more convenient access to
     * a property's position in the property hierarchy, domain, range, etc.
     * </p>
     *
     * @param uri The uri for the property. May not be null.
     * @return An OntProperty resource.
     */
    @Override
    public OntProperty createOntProperty(String uri) {
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "38a07252-776d-4710-b371-46f4ed38a242");
        Property p = createProperty(uri);
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "95a3f29a-b74f-47ac-b207-745a001a0ead");
        p.addProperty(RDF.type, getProfile().PROPERTY());
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "553a0682-9802-4927-a921-af8f9f7b4626");
        return p.as(OntProperty.class);
    }

    /**
     * <p>
     * Answer a resource representing an object property in this model,
     * and that is not a functional property.
     * </p>
     *
     * @param uri The uri for the object property. May not be null.
     * @return An ObjectProperty resource.
     * @see #createObjectProperty( String, boolean )
     */
    @Override
    public ObjectProperty createObjectProperty(String uri) {
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "61a20122-224b-4b5f-91c8-b334a1d14042");
        return createObjectProperty(uri, false);
    }

    /**
     * <p>
     * Answer a resource that represents an object property in this model.  An object property
     * is defined to have a range of individuals, rather than datatypes.
     * If a resource
     * with the given uri exists in the model, it will be re-used.  If not, a new one is created in
     * the updateable sub-graph of the ontology model.
     * </p>
     *
     * @param uri The uri for the object property. May not be null.
     * @param functional If true, the resource will also be typed as a {@link FunctionalProperty},
     * that is, a property that has a unique range value for any given domain value.
     * @return An ObjectProperty resource, optionally also functional.
     */
    @Override
    public ObjectProperty createObjectProperty(String uri, boolean functional) {
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "0ce20c51-ab7f-4d73-a8cd-d4e5b3223123");
        checkProfileEntry(getProfile().OBJECT_PROPERTY(), "OBJECT_PROPERTY");
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "66650c29-9bd7-481f-b324-274d898f622a");
        ObjectProperty p = createOntResource(ObjectProperty.class, getProfile().OBJECT_PROPERTY(), uri);
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "7897a3c7-523b-4bbb-b3c9-030dfba5d99e");
        if (functional) {
            writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "6f4a9ba0-63f0-412c-8ce8-5d833db2ff9b");
            checkProfileEntry(getProfile().FUNCTIONAL_PROPERTY(), "FUNCTIONAL_PROPERTY");
            writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "f16c2775-c75e-4e42-bc22-7d7301978cae");
            p.addProperty(RDF.type, getProfile().FUNCTIONAL_PROPERTY());
        }
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "68fb5a1a-0219-4776-b759-0fe2155036b7");
        return p;
    }

    /**
     * <p>Answer a resource representing a transitive property</p>
     * @param uri The uri for the property. May not be null.
     * @return An TransitiveProperty resource
     * @see #createTransitiveProperty( String, boolean )
     */
    @Override
    public TransitiveProperty createTransitiveProperty(String uri) {
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "4f9bab7d-c816-48e3-ba52-8c702d20289f");
        return createTransitiveProperty(uri, false);
    }

    /**
     * <p>Answer a resource representing a transitive property, which is optionally
     * also functional. <strong>Note:</strong> although it is permitted in OWL full
     * to have functional transitive properties, it makes the language undecideable.
     * Functional transitive properties are not permitted in OWL Lite or OWL DL.</p>
     * @param uri The uri for the property. May not be null.
     * @param functional If true, the property is also functional
     * @return An TransitiveProperty resource, optionally also functional.
     */
    @Override
    public TransitiveProperty createTransitiveProperty(String uri, boolean functional) {
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "6897918d-591b-4685-94e9-0194ad445302");
        checkProfileEntry(getProfile().TRANSITIVE_PROPERTY(), "TRANSITIVE_PROPERTY");
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "ef0e3117-91d2-44f4-8357-ae7ad0e3e9d4");
        TransitiveProperty p = createOntResource(TransitiveProperty.class, getProfile().TRANSITIVE_PROPERTY(), uri);
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "3f3bdd99-b103-4d48-9203-e1ce60c72414");
        if (functional) {
            writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "dc7e633c-317d-46a9-9433-f19a4b5ed7f3");
            checkProfileEntry(getProfile().FUNCTIONAL_PROPERTY(), "FUNCTIONAL_PROPERTY");
            writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "50f996e1-42d7-4a41-8b8c-42e594e36567");
            p.addProperty(RDF.type, getProfile().FUNCTIONAL_PROPERTY());
        }
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "68715a06-b650-486b-9e93-bb1df383ec80");
        return p;
    }

    /**
     * <p>Answer a resource representing a symmetric property</p>
     * @param uri The uri for the property. May not be null.
     * @return An SymmetricProperty resource
     * @see #createSymmetricProperty( String, boolean )
     */
    @Override
    public SymmetricProperty createSymmetricProperty(String uri) {
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "eb170d98-e2a3-4d9d-b634-2b9ff638429c");
        return createSymmetricProperty(uri, false);
    }

    /**
     * <p>Answer a resource representing a symmetric property, which is optionally
     * also functional.</p>
     * @param uri The uri for the property. May not be null.
     * @param functional If true, the property is also functional
     * @return An SymmetricProperty resource, optionally also functional.
     */
    @Override
    public SymmetricProperty createSymmetricProperty(String uri, boolean functional) {
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "e953dc84-9776-43bd-a8be-7ecede4274a3");
        checkProfileEntry(getProfile().SYMMETRIC_PROPERTY(), "SYMMETRIC_PROPERTY");
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "d12f62f2-63e3-442f-bba4-b600f2733dd3");
        SymmetricProperty p = createOntResource(SymmetricProperty.class, getProfile().SYMMETRIC_PROPERTY(), uri);
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "a92f356a-4e2a-4242-9f2a-d85ea886849c");
        if (functional) {
            writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "0f7a7afc-19ff-4744-8939-9f2c93a255d5");
            checkProfileEntry(getProfile().FUNCTIONAL_PROPERTY(), "FUNCTIONAL_PROPERTY");
            writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "4d9aa361-5e96-4e95-a18c-3c4465c64554");
            p.addProperty(RDF.type, getProfile().FUNCTIONAL_PROPERTY());
        }
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "0529c328-34ce-42fa-a698-29327d1f69ba");
        return p;
    }

    /**
     * <p>Answer a resource representing an inverse functional property</p>
     * @param uri The uri for the property. May not be null.
     * @return An InverseFunctionalProperty resource
     * @see #createInverseFunctionalProperty( String, boolean )
     */
    @Override
    public InverseFunctionalProperty createInverseFunctionalProperty(String uri) {
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "14d7d86f-9502-49d1-8a0b-dca68b5ad227");
        return createInverseFunctionalProperty(uri, false);
    }

    /**
     * <p>Answer a resource representing an inverse functional property, which is optionally
     * also functional.</p>
     * @param uri The uri for the property. May not be null.
     * @param functional If true, the property is also functional
     * @return An InverseFunctionalProperty resource, optionally also functional.
     */
    @Override
    public InverseFunctionalProperty createInverseFunctionalProperty(String uri, boolean functional) {
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "f75d6853-3f57-4d2b-91d8-9195aa5e72d8");
        checkProfileEntry(getProfile().INVERSE_FUNCTIONAL_PROPERTY(), "INVERSE_FUNCTIONAL_PROPERTY");
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "367d0ba2-f61e-447c-93c5-c01e66b50d08");
        InverseFunctionalProperty p = createOntResource(InverseFunctionalProperty.class, getProfile().INVERSE_FUNCTIONAL_PROPERTY(), uri);
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "0bd559fe-9d8a-4564-b45b-21d4fd3c979b");
        if (functional) {
            writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "64a42390-1269-4c63-a582-a5919bac48e7");
            checkProfileEntry(getProfile().FUNCTIONAL_PROPERTY(), "FUNCTIONAL_PROPERTY");
            writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "80113ce1-56c6-424c-822b-6d6fd80097e8");
            p.addProperty(RDF.type, getProfile().FUNCTIONAL_PROPERTY());
        }
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "3868ed87-32dc-424d-84dc-8b2a7124e802");
        return p;
    }

    /**
     * <p>
     * Answer a resource that represents datatype property in this model, and that is
     * not a functional property.
     * </p>
     *
     * @param uri The uri for the datatype property. May not be null.
     * @return A DatatypeProperty resource.
     * @see #createDatatypeProperty( String, boolean )
     */
    @Override
    public DatatypeProperty createDatatypeProperty(String uri) {
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "eb4bf024-2f1a-45ca-94c7-ef2a1b5ff4d6");
        return createDatatypeProperty(uri, false);
    }

    /**
     * <p>
     * Answer a resource that represents datatype property in this model. A datatype property
     * is defined to have a range that is a concrete datatype, rather than an individual.
     * If a resource
     * with the given uri exists in the model, it will be re-used.  If not, a new one is created in
     * the updateable sub-graph of the ontology model.
     * </p>
     *
     * @param uri The uri for the datatype property. May not be null.
     * @param functional If true, the resource will also be typed as a {@link FunctionalProperty},
     * that is, a property that has a unique range value for any given domain value.
     * @return A DatatypeProperty resource.
     */
    @Override
    public DatatypeProperty createDatatypeProperty(String uri, boolean functional) {
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "e520dcb3-8c4a-4254-9692-68219f17e402");
        checkProfileEntry(getProfile().DATATYPE_PROPERTY(), "DATATYPE_PROPERTY");
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "74c686c0-ab4a-422c-9829-0520b5c97322");
        DatatypeProperty p = createOntResource(DatatypeProperty.class, getProfile().DATATYPE_PROPERTY(), uri);
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "f195e07f-0b30-45ec-9d61-d19bfb1d70df");
        if (functional) {
            writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "1766872c-6de3-4d5b-82e3-22185468e673");
            checkProfileEntry(getProfile().FUNCTIONAL_PROPERTY(), "FUNCTIONAL_PROPERTY");
            writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "892e4833-8cd0-4f59-b0e4-ea5f3b6eac76");
            p.addProperty(RDF.type, getProfile().FUNCTIONAL_PROPERTY());
        }
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "34c5bf9b-6879-43d6-b20c-02dff11bdb5c");
        return p;
    }

    /**
     * <p>
     * Answer a resource that represents an annotation property in this model. If a resource
     * with the given uri exists in the model, it will be re-used.  If not, a new one is created in
     * the updateable sub-graph of the ontology model.
     * </p>
     *
     * @param uri The uri for the annotation property.
     * @return An AnnotationProperty resource.
     */
    @Override
    public AnnotationProperty createAnnotationProperty(String uri) {
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "9f6dcda0-2eb9-4a37-bf47-41ed2f04ee60");
        checkProfileEntry(getProfile().ANNOTATION_PROPERTY(), "ANNOTATION_PROPERTY");
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "917b6250-a826-4d08-9bc6-860d8c4c2091");
        return createOntResource(AnnotationProperty.class, getProfile().ANNOTATION_PROPERTY(), uri);
    }

    /**
     * <p>
     * Answer a resource that represents an anonymous class description in this model. A new
     * anonymous resource of <code>rdf:type C</code>, where C is the class type from the
     * language profile.
     * </p>
     *
     * @return An anonymous Class resource.
     */
    @Override
    public OntClass createClass() {
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "dfbd026b-7e8e-464c-bd1a-e8650e7d5035");
        checkProfileEntry(getProfile().CLASS(), "CLASS");
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "af702418-f176-464c-abd4-d3129dc63ec1");
        return createOntResource(OntClass.class, getProfile().CLASS(), null);
    }

    /**
     * <p>
     * Answer a resource that represents a class description node in this model. If a resource
     * with the given uri exists in the model, it will be re-used.  If not, a new one is created in
     * the updateable sub-graph of the ontology model.
     * </p>
     *
     * @param uri The uri for the class node, or null for an anonymous class.
     * @return A Class resource.
     */
    @Override
    public OntClass createClass(String uri) {
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "508b648a-9e14-46a3-8595-2fe7ebe95d97");
        checkProfileEntry(getProfile().CLASS(), "CLASS");
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "b4884c1d-7f33-450d-b334-ab9ac4d2fd66");
        return createOntResource(OntClass.class, getProfile().CLASS(), uri);
    }

    /**
     * <p>Answer a resource representing the class that is the complement of the given argument class</p>
     * @param uri The URI of the new complement class, or null for an anonymous class description.
     * @param cls Resource denoting the class that the new class is a complement of
     * @return A complement class
     */
    @Override
    public ComplementClass createComplementClass(String uri, Resource cls) {
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "0f5ab7fb-7f19-4069-8264-8a0a8d47d7ee");
        checkProfileEntry(getProfile().CLASS(), "CLASS");
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "2149053e-9404-4eb2-9df2-d3febbd51813");
        OntClass c = createOntResource(OntClass.class, getProfile().CLASS(), uri);
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "0fb5cde8-89d1-49f0-b638-e79d2a0e473f");
        checkProfileEntry(getProfile().COMPLEMENT_OF(), "COMPLEMENT_OF");
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "602bbf32-f139-43c4-9e00-68541ed04ae5");
        // if the class that this class is a complement of is not specified, use owl:nothing or daml:nothing
        c.addProperty(getProfile().COMPLEMENT_OF(), (cls == null) ? getProfile().NOTHING() : cls);
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "4a9a0390-a11a-4624-b03e-0eb2f32b89e9");
        return c.as(ComplementClass.class);
    }

    /**
     * <p>Answer a resource representing the class that is the enumeration of the given list of individuals</p>
     * @param uri The URI of the new enumeration class, or null for an anonymous class description.
     * @param members An optional list of resources denoting the individuals in the enumeration
     * @return An enumeration class
     */
    @Override
    public EnumeratedClass createEnumeratedClass(String uri, RDFList members) {
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "2bce0663-0ccc-468a-b28d-7593d2c3e48f");
        checkProfileEntry(getProfile().CLASS(), "CLASS");
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "9f1d10e6-aaee-41cc-8925-e52b681a96ad");
        OntClass c = createOntResource(OntClass.class, getProfile().CLASS(), uri);
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "83390be7-64f0-4348-9fbb-f5ff407e7720");
        checkProfileEntry(getProfile().ONE_OF(), "ONE_OF");
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "6a5b3248-c9a2-4f23-a7db-bbfd453a39b4");
        c.addProperty(getProfile().ONE_OF(), (members == null) ? createList() : members);
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "2a12ac31-809b-4552-80c4-a9d4eaffcc15");
        return c.as(EnumeratedClass.class);
    }

    /**
     * <p>Answer a resource representing the class that is the union of the given list of class desctiptions</p>
     * @param uri The URI of the new union class, or null for an anonymous class description.
     * @param members A list of resources denoting the classes that comprise the union
     * @return A union class description
     */
    @Override
    public UnionClass createUnionClass(String uri, RDFList members) {
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "51f16f93-54b7-446d-b564-e226c12dee85");
        checkProfileEntry(getProfile().CLASS(), "CLASS");
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "2b896b40-eb85-4c31-9235-d2ff933b5e3a");
        OntClass c = createOntResource(OntClass.class, getProfile().CLASS(), uri);
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "43579a59-297b-4ecf-8ffb-92fdc576243d");
        checkProfileEntry(getProfile().UNION_OF(), "UNION_OF");
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "4f93729b-1b7f-47c9-ba7a-cac799c2e711");
        c.addProperty(getProfile().UNION_OF(), (members == null) ? createList() : members);
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "5ec54941-6cbc-4192-8693-3af47436201d");
        return c.as(UnionClass.class);
    }

    /**
     * <p>Answer a resource representing the class that is the intersection of the given list of class descriptions.</p>
     * @param uri The URI of the new intersection class, or null for an anonymous class description.
     * @param members A list of resources denoting the classes that comprise the intersection
     * @return An intersection class description
     */
    @Override
    public IntersectionClass createIntersectionClass(String uri, RDFList members) {
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "aeec7a2d-5d6c-4fd3-9976-c92c143c3911");
        checkProfileEntry(getProfile().CLASS(), "CLASS");
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "cdc7478b-6922-4aa3-be25-2a11ba1c3400");
        OntClass c = createOntResource(OntClass.class, getProfile().CLASS(), uri);
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "c078b6f3-b30b-49e7-8129-93ef1a8d438e");
        checkProfileEntry(getProfile().INTERSECTION_OF(), "INTERSECTION_OF");
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "1a8d9cd0-a806-40f6-a317-3507a9c9e1d4");
        c.addProperty(getProfile().INTERSECTION_OF(), (members == null) ? createList() : members);
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "d8322162-d465-4188-9821-200b4cb1bf90");
        return c.as(IntersectionClass.class);
    }

    /**
     * <p>
     * Answer a resource that represents an anonymous property restriction in this model. A new
     * anonymous resource of <code>rdf:type R</code>, where R is the restriction type from the
     * language profile.
     * </p>
     *
     * @param p The property that is restricted by this restriction
     * @return An anonymous Restriction resource.
     */
    @Override
    public Restriction createRestriction(Property p) {
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "c50e9e79-dcd5-4c76-99a8-af0511c79703");
        checkProfileEntry(getProfile().RESTRICTION(), "RESTRICTION");
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "f53f5618-b639-4a19-ab1e-2fe82798a65d");
        Restriction r = createOntResource(Restriction.class, getProfile().RESTRICTION(), null);
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "0d35f8b2-e255-4c2c-bbf5-ec0c56cab5b1");
        if (p != null) {
            writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "2a46faaa-c626-464e-ba7d-3e3d45c8a3ce");
            r.setOnProperty(p);
        }
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "90cfc198-892d-468a-bcdc-c5743582f10b");
        return r;
    }

    /**
     * <p>
     * Answer a resource that represents a property restriction in this model. If a resource
     * with the given uri exists in the model, it will be re-used.  If not, a new one is created in
     * the updateable sub-graph of the ontology model.
     * </p>
     *
     * @param uri The uri for the restriction node, or null for an anonymous restriction.
     * @param p The property that is restricted by this restriction
     * @return A Restriction resource.
     */
    @Override
    public Restriction createRestriction(String uri, Property p) {
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "39cb315a-2986-4520-b5a8-cdcc6705423d");
        checkProfileEntry(getProfile().RESTRICTION(), "RESTRICTION");
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "38ad499c-9988-4ccd-9ef8-a0605d272cb4");
        Restriction r = createOntResource(Restriction.class, getProfile().RESTRICTION(), uri);
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "e63a1bea-7bea-4528-982c-57e22b1d8007");
        if (p != null) {
            writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "2d79f5b5-ba6f-4d95-b889-9ead2a1ff422");
            r.setOnProperty(p);
        }
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "a8713dce-d681-4c29-be30-bee093cd3542");
        return r;
    }

    /**
     * <p>Answer a class description defined as the class of those individuals that have the given
     * resource as the value of the given property</p>
     *
     * @param uri The optional URI for the restriction, or null for an anonymous restriction (which
     * should be the normal case)
     * @param prop The property the restriction applies to
     * @param value The value of the property, as a resource or RDF literal
     * @return A new resource representing a has-value restriction
     */
    @Override
    public HasValueRestriction createHasValueRestriction(String uri, Property prop, RDFNode value) {
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "4b86f2f5-9aae-4e09-9701-9337d30dd042");
        checkProfileEntry(getProfile().RESTRICTION(), "RESTRICTION");
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "9fd440ab-3b46-45c4-ace7-ebb6b7c9099c");
        Restriction r = createOntResource(Restriction.class, getProfile().RESTRICTION(), uri);
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "e77390c5-3fa1-4e03-89dc-c8f164e1c61a");
        if (prop == null || value == null) {
            writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "10dadf30-ecfc-4d37-a647-ad6437b69258");
            throw new IllegalArgumentException("Cannot create hasValueRestriction with a null property or value");
        }
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "678a2d00-1c8a-40c4-b251-9196c01bc49a");
        checkProfileEntry(getProfile().HAS_VALUE(), "HAS_VALUE");
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "20881047-b23b-4238-b876-f61e8b5dceec");
        r.addProperty(getProfile().ON_PROPERTY(), prop);
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "324ec845-46f0-4815-9a66-0509f281130c");
        r.addProperty(getProfile().HAS_VALUE(), value);
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "04c494af-16d0-444e-9fc2-f8531a764035");
        return r.as(HasValueRestriction.class);
    }

    /**
     * <p>Answer a class description defined as the class of those individuals that have at least
     * one property with a value belonging to the given class</p>
     *
     * @param uri The optional URI for the restriction, or null for an anonymous restriction (which
     * should be the normal case)
     * @param prop The property the restriction applies to
     * @param cls The class to which at least one value of the property belongs
     * @return A new resource representing a some-values-from restriction
     */
    @Override
    public SomeValuesFromRestriction createSomeValuesFromRestriction(String uri, Property prop, Resource cls) {
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "cc00efed-55b4-496c-b7d8-3b1d3775cc11");
        checkProfileEntry(getProfile().RESTRICTION(), "RESTRICTION");
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "7cc55ae5-e20c-4bab-9c92-90c7433d345c");
        Restriction r = createOntResource(Restriction.class, getProfile().RESTRICTION(), uri);
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "a01e91d2-b081-43d4-908d-265fb1a0d543");
        if (prop == null || cls == null) {
            writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "0be35985-7701-459e-9b5c-fd8af283659b");
            throw new IllegalArgumentException("Cannot create someValuesFromRestriction with a null property or class");
        }
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "852e6b1a-0b44-4ded-997b-980bb2fa2f64");
        checkProfileEntry(getProfile().SOME_VALUES_FROM(), "SOME_VALUES_FROM");
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "2d14f5ee-3c79-4afe-a8c4-2c30a4b81460");
        r.addProperty(getProfile().ON_PROPERTY(), prop);
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "63dfced8-6d68-4930-b0e2-67b2b79c6d52");
        r.addProperty(getProfile().SOME_VALUES_FROM(), cls);
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "7fc4892f-8aba-4711-9474-f30c7c3c8db3");
        return r.as(SomeValuesFromRestriction.class);
    }

    /**
     * <p>Answer a class description defined as the class of those individuals for which all values
     * of the given property belong to the given class</p>
     *
     * @param uri The optional URI for the restriction, or null for an anonymous restriction (which
     * should be the normal case)
     * @param prop The property the restriction applies to
     * @param cls The class to which any value of the property belongs
     * @return A new resource representing an all-values-from restriction
     */
    @Override
    public AllValuesFromRestriction createAllValuesFromRestriction(String uri, Property prop, Resource cls) {
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "eb3a1372-666d-4110-b2ba-c97473ca4939");
        checkProfileEntry(getProfile().RESTRICTION(), "RESTRICTION");
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "b896d496-1dd7-47c0-a3a4-47080bdd316e");
        Restriction r = createOntResource(Restriction.class, getProfile().RESTRICTION(), uri);
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "888dbc7c-7f33-4bef-8201-8e54cd0acf79");
        if (prop == null || cls == null) {
            writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "ba767b5b-4cdb-4e78-bc12-3df00f510057");
            throw new IllegalArgumentException("Cannot create allValuesFromRestriction with a null property or class");
        }
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "6821aa63-0243-4cc4-9c77-bc3435a7f431");
        checkProfileEntry(getProfile().ALL_VALUES_FROM(), "ALL_VALUES_FROM");
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "954b336d-5eb6-45ff-93f0-77c608f29132");
        r.addProperty(getProfile().ON_PROPERTY(), prop);
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "3bb99874-b228-4736-9e77-212e188be537");
        r.addProperty(getProfile().ALL_VALUES_FROM(), cls);
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "6505ee64-db6e-4c93-b646-e448dbaed220");
        return r.as(AllValuesFromRestriction.class);
    }

    /**
     * <p>Answer a class description defined as the class of those individuals that have exactly
     * the given number of values for the given property.</p>
     *
     * @param uri The optional URI for the restriction, or null for an anonymous restriction (which
     * should be the normal case)
     * @param prop The property the restriction applies to
     * @param cardinality The exact cardinality of the property
     * @return A new resource representing a has-value restriction
     */
    @Override
    public CardinalityRestriction createCardinalityRestriction(String uri, Property prop, int cardinality) {
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "ff7693f0-812c-4a92-9c7f-9c191f33cb20");
        checkProfileEntry(getProfile().RESTRICTION(), "RESTRICTION");
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "5df2249b-156b-4026-ac9a-d6bfcc45b7a3");
        Restriction r = createOntResource(Restriction.class, getProfile().RESTRICTION(), uri);
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "135fe245-cfac-4b2a-8dc3-e931d9fc5eeb");
        if (prop == null) {
            writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "a2508822-4ee6-427c-9045-051487141944");
            throw new IllegalArgumentException("Cannot create cardinalityRestriction with a null property");
        }
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "5ac7db85-ffbb-457b-8614-89794581a7b9");
        checkProfileEntry(getProfile().CARDINALITY(), "CARDINALITY");
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "c81abd4d-f7e4-4338-8399-70499b8e875e");
        r.addProperty(getProfile().ON_PROPERTY(), prop);
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "b4b2dfa0-8a10-471a-b44f-77232f9fad6f");
        r.addProperty(getProfile().CARDINALITY(), createTypedLiteral(cardinality));
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "714ba85c-7b97-4a0b-b713-2e0bccff84c6");
        return r.as(CardinalityRestriction.class);
    }

    /**
     * <p>Answer a class description defined as the class of those individuals that have at least
     * the given number of values for the given property.</p>
     *
     * @param uri The optional URI for the restriction, or null for an anonymous restriction (which
     * should be the normal case)
     * @param prop The property the restriction applies to
     * @param cardinality The minimum cardinality of the property
     * @return A new resource representing a min-cardinality restriction
     */
    @Override
    public MinCardinalityRestriction createMinCardinalityRestriction(String uri, Property prop, int cardinality) {
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "256ed26d-f01e-4dd2-a08a-c7756b4b5c91");
        checkProfileEntry(getProfile().RESTRICTION(), "RESTRICTION");
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "54e1ecd9-814c-4a15-93f4-435f0aa1537d");
        Restriction r = createOntResource(Restriction.class, getProfile().RESTRICTION(), uri);
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "f3c62ed5-174c-4c08-888a-8aefd3be1d99");
        if (prop == null) {
            writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "d14151eb-f843-44e0-b4d5-bb1bb6ba072f");
            throw new IllegalArgumentException("Cannot create minCardinalityRestriction with a null property");
        }
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "8dc4724d-8610-4c6e-9407-84894cac89e2");
        checkProfileEntry(getProfile().MIN_CARDINALITY(), "MIN_CARDINALITY");
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "bfe8452a-e6e2-418e-97c5-b332ad1cd35c");
        r.addProperty(getProfile().ON_PROPERTY(), prop);
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "34e3f4d5-ae96-4fb2-94e0-ee6b590d9e6a");
        r.addProperty(getProfile().MIN_CARDINALITY(), createTypedLiteral(cardinality));
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "629195ce-b229-4d11-8122-cdbd85fb4f04");
        return r.as(MinCardinalityRestriction.class);
    }

    /**
     * <p>Answer a class description defined as the class of those individuals that have at most
     * the given number of values for the given property.</p>
     *
     * @param uri The optional URI for the restriction, or null for an anonymous restriction (which
     * should be the normal case)
     * @param prop The property the restriction applies to
     * @param cardinality The maximum cardinality of the property
     * @return A new resource representing a mas-cardinality restriction
     */
    @Override
    public MaxCardinalityRestriction createMaxCardinalityRestriction(String uri, Property prop, int cardinality) {
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "62ea548b-d01a-42d6-9a5b-4c068926595b");
        checkProfileEntry(getProfile().RESTRICTION(), "RESTRICTION");
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "67f30daf-a3ce-4b3a-bbc1-1fdc84b15610");
        Restriction r = createOntResource(Restriction.class, getProfile().RESTRICTION(), uri);
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "7a9c607b-b980-4122-94e6-60e39a95df74");
        if (prop == null) {
            writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "d4e46023-ead4-463d-9f6b-4fdd991de85f");
            throw new IllegalArgumentException("Cannot create maxCardinalityRestriction with a null property");
        }
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "6e257814-f3df-43c6-ae33-70edda2b4542");
        checkProfileEntry(getProfile().MAX_CARDINALITY(), "MAX_CARDINALITY");
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "38a14262-2323-49a1-a7da-de289e25164a");
        r.addProperty(getProfile().ON_PROPERTY(), prop);
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "fbbab477-39e1-4b58-8004-30a0362718f2");
        r.addProperty(getProfile().MAX_CARDINALITY(), createTypedLiteral(cardinality));
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "32e957a3-44b9-4e91-8611-e6f9238c8c80");
        return r.as(MaxCardinalityRestriction.class);
    }

    /**
     * <p>Answer a class description defined as the class of those individuals that have at most
     * the given number of values for the given property, all values of which belong to the given
     * class.</p>
     *
     * @param uri The optional URI for the restriction, or null for an anonymous restriction (which
     * should be the normal case)
     * @param prop The property the restriction applies to
     * @param cardinality The maximum cardinality of the property
     * @param cls The class to which all values of the restricted property should belong
     * @return A new resource representing a mas-cardinality restriction
     */
    @Override
    public MaxCardinalityQRestriction createMaxCardinalityQRestriction(String uri, Property prop, int cardinality, OntClass cls) {
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "456375a6-8e55-4f00-8f41-a0be04565e3f");
        checkProfileEntry(getProfile().RESTRICTION(), "RESTRICTION");
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "e714c9c1-9eea-4e95-8157-e8f9f51caa72");
        checkProfileEntry(getProfile().ON_PROPERTY(), "ON_PROPERTY");
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "f5edf74e-f34d-4861-8266-9f26e0547c00");
        checkProfileEntry(getProfile().MAX_CARDINALITY_Q(), "MAX_CARDINALITY_Q");
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "7e44d636-27fb-4904-973e-76a4f602b3fc");
        checkProfileEntry(getProfile().HAS_CLASS_Q(), "HAS_CLASS_Q");
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "39ca1e30-f879-448b-a61a-c141cc48224f");
        if (prop == null) {
            writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "8800e505-9c70-46c1-8934-6b2a6c7bf29b");
            throw new IllegalArgumentException("Cannot create MaxCardinalityQRestriction with a null property");
        }
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "3172e680-a96d-4116-bf23-09782f1a17a1");
        if (cls == null) {
            writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "201782a5-135c-459b-b7d4-8229470694b9");
            throw new IllegalArgumentException("Cannot create MaxCardinalityQRestriction with a null class");
        }
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "5b76b7a7-3b57-4071-887f-4f4fa374dcaa");
        Restriction r = createOntResource(Restriction.class, getProfile().RESTRICTION(), uri);
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "105161a2-a959-45f4-b9b1-bf26c6bf467d");
        r.addProperty(getProfile().ON_PROPERTY(), prop);
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "a6ce367f-6e37-4b02-bf0b-2413a4fbc0a6");
        r.addProperty(getProfile().MAX_CARDINALITY_Q(), createTypedLiteral(cardinality));
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "7e6d7111-a53a-4a44-bc03-d672e9cd2cf4");
        r.addProperty(getProfile().HAS_CLASS_Q(), cls);
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "292ec11d-eee7-4d9e-90b0-7711b1d4514b");
        return r.as(MaxCardinalityQRestriction.class);
    }

    /**
     * <p>Answer a class description defined as the class of those individuals that have at least
     * the given number of values for the given property, all values of which belong to the given
     * class.</p>
     *
     * @param uri The optional URI for the restriction, or null for an anonymous restriction (which
     * should be the normal case)
     * @param prop The property the restriction applies to
     * @param cardinality The minimun cardinality of the property
     * @param cls The class to which all values of the restricted property should belong
     * @return A new resource representing a mas-cardinality restriction
     */
    @Override
    public MinCardinalityQRestriction createMinCardinalityQRestriction(String uri, Property prop, int cardinality, OntClass cls) {
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "c4e64389-bafc-4b3b-8682-a187afab4070");
        checkProfileEntry(getProfile().RESTRICTION(), "RESTRICTION");
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "dd7cc8fc-4f0d-49fe-bff1-49eed4602d87");
        checkProfileEntry(getProfile().ON_PROPERTY(), "ON_PROPERTY");
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "8113e1aa-d6b2-4e57-af84-37c66efb604a");
        checkProfileEntry(getProfile().MIN_CARDINALITY_Q(), "MIN_CARDINALITY_Q");
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "ec6f11ac-94ea-4f07-beb9-c8fac4b584a2");
        checkProfileEntry(getProfile().HAS_CLASS_Q(), "HAS_CLASS_Q");
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "062d00f7-43ec-4619-bd6f-3c8afaa8765e");
        if (prop == null) {
            writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "56d517e8-252d-4b9c-8547-9eaca73de4f1");
            throw new IllegalArgumentException("Cannot create MinCardinalityQRestriction with a null property");
        }
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "baa686e2-b60d-4685-a3e8-eefc5707a27a");
        if (cls == null) {
            writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "b8b4acba-fafb-4fd1-a0ba-b824ca21f234");
            throw new IllegalArgumentException("Cannot create MinCardinalityQRestriction with a null class");
        }
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "2da67658-a8ca-45b1-bdba-4a7efd6fefa4");
        Restriction r = createOntResource(Restriction.class, getProfile().RESTRICTION(), uri);
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "d282ab00-6533-4d69-9769-e88b6094d7fe");
        r.addProperty(getProfile().ON_PROPERTY(), prop);
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "faedbac6-6f75-431a-9e39-396b0cad714f");
        r.addProperty(getProfile().MIN_CARDINALITY_Q(), createTypedLiteral(cardinality));
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "9d4936b9-23b2-4485-add5-c4b3d42b0f68");
        r.addProperty(getProfile().HAS_CLASS_Q(), cls);
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "5f89d5a6-70e5-4891-a108-35f8a1b8117e");
        return r.as(MinCardinalityQRestriction.class);
    }

    /**
     * <p>Answer a class description defined as the class of those individuals that have exactly
     * the given number of values for the given property, all values of which belong to the given
     * class.</p>
     *
     * @param uri The optional URI for the restriction, or null for an anonymous restriction (which
     * should be the normal case)
     * @param prop The property the restriction applies to
     * @param cardinality The cardinality of the property
     * @param cls The class to which all values of the restricted property should belong
     * @return A new resource representing a mas-cardinality restriction
     */
    @Override
    public CardinalityQRestriction createCardinalityQRestriction(String uri, Property prop, int cardinality, OntClass cls) {
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "51824210-25c1-4755-b0a0-59f72c2a3c62");
        checkProfileEntry(getProfile().RESTRICTION(), "RESTRICTION");
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "03e86121-0736-4930-8c0b-b621fc0afa52");
        checkProfileEntry(getProfile().ON_PROPERTY(), "ON_PROPERTY");
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "7a89579d-d0b9-42fb-97e2-e2bd66bb3bb3");
        checkProfileEntry(getProfile().CARDINALITY_Q(), "CARDINALITY_Q");
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "4a106315-9b21-487f-a3a4-0f8904eb39cb");
        checkProfileEntry(getProfile().HAS_CLASS_Q(), "HAS_CLASS_Q");
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "e3f97f92-8485-4694-8ca4-503a856fbb9b");
        if (prop == null) {
            writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "5033eec4-aeca-4273-95c0-1f55e8ecafd1");
            throw new IllegalArgumentException("Cannot create CardinalityQRestriction with a null property");
        }
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "91bc4ed2-5660-45c9-85d5-cfb5a74fc7c1");
        if (cls == null) {
            writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "efe33b9d-15dd-4972-b7c2-487ae29c1a09");
            throw new IllegalArgumentException("Cannot create CardinalityQRestriction with a null class");
        }
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "89f26e44-f811-4449-8258-727f7ce6781e");
        Restriction r = createOntResource(Restriction.class, getProfile().RESTRICTION(), uri);
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "ec478f4f-1fb3-41dd-b57d-a7fd7e8041ab");
        r.addProperty(getProfile().ON_PROPERTY(), prop);
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "4fa0e457-5bb9-4746-a7fc-9ac4c3d7b27e");
        r.addProperty(getProfile().CARDINALITY_Q(), createTypedLiteral(cardinality));
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "17f08715-57f4-403c-b6ba-f48c584fea1c");
        r.addProperty(getProfile().HAS_CLASS_Q(), cls);
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "4f546f71-4264-4feb-81f1-11a123c6d4c3");
        return r.as(CardinalityQRestriction.class);
    }

    /**
     * <p>Answer a data range defined as the given set of concrete data values.  DataRange resources
     * are necessarily bNodes.</p>
     *
     * @param literals An iterator over a set of literals that will be the members of the data range,
     * or null to define an empty data range
     * @return A new data range containing the given literals as permissible values
     */
    @Override
    public DataRange createDataRange(RDFList literals) {
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "44b1c889-d9ad-44fe-a8df-93efe3126b64");
        checkProfileEntry(getProfile().DATARANGE(), "DATARANGE");
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "0540a5d5-04a8-4c9b-a8ab-16dbc0120ab7");
        DataRange d = createOntResource(DataRange.class, getProfile().DATARANGE(), null);
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "750716c3-fbc4-46d1-99d3-8a42fde857e5");
        checkProfileEntry(getProfile().ONE_OF(), "ONE_OF");
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "060d3aef-2442-439f-9624-40cfa674b172");
        d.addProperty(getProfile().ONE_OF(), (literals == null) ? createList() : literals);
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "e32dacc3-446b-432e-b629-71572053fa62");
        return d;
    }

    /**
     * <p>
     * Answer a new, anonymous node representing the fact that a given set of classes are all
     * pair-wise distinct.  <code>AllDifferent</code> is a feature of OWL only, and is something
     * of an anomoly in that it exists only to give a place to anchor the <code>distinctMembers</code>
     * property, which is the actual expression of the fact.
     * </p>
     *
     * @return A new AllDifferent resource
     */
    @Override
    public AllDifferent createAllDifferent() {
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "ce729405-52ae-4bf3-8e07-b2acf2b2026b");
        return createAllDifferent(null);
    }

    /**
     * <p>
     * Answer a new, anonymous node representing the fact that a given set of classes are all
     * pair-wise distinct.  <code>AllDifferent</code> is a feature of OWL only, and is something
     * of an anomoly in that it exists only to give a place to anchor the <code>distinctMembers</code>
     * property, which is the actual expression of the fact.
     * </p>
     * @param differentMembers A list of the class expressions that denote a set of mutually disjoint classes
     * @return A new AllDifferent resource
     */
    @Override
    public AllDifferent createAllDifferent(RDFList differentMembers) {
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "0e4c2b4a-9cb6-4aff-aad0-f1918042862e");
        checkProfileEntry(getProfile().ALL_DIFFERENT(), "ALL_DIFFERENT");
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "f4768d64-4a20-45c4-95ff-3a5a378fd564");
        AllDifferent ad = createOntResource(AllDifferent.class, getProfile().ALL_DIFFERENT(), null);
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "c63d240c-272f-4ce6-94cb-c5200e42386b");
        ad.setDistinctMembers((differentMembers == null) ? createList() : differentMembers);
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "aaa54a5c-befc-4e71-a8ed-5df810ae59b0");
        return ad;
    }

    /**
     * <p>
     * Answer a resource that represents a generic ontology node in this model. If a resource
     * with the given uri exists in the model, it will be re-used.  If not, a new one is created in
     * the updateable sub-graph of the ontology model.
     * </p>
     * <p>
     * This is a generic method for creating any known ontology value.  The selector that determines
     * which resource to create is the same as as the argument to the {@link RDFNode#as as()}
     * method: the Java class object of the desired abstraction.  For example, to create an
     * ontology class via this mechanism, use:
     * <code><pre>
     * OntClass c = (OntClass) myModel.createOntResource( OntClass.class, null,
     * "http://example.org/ex#Parrot" );
     * </pre></code>
     * </p>
     *
     * @param javaClass The Java class object that represents the ontology abstraction to create
     * @param rdfType Optional resource denoting the ontology class to which an individual or
     * axiom belongs, if that is the type of resource being created.
     * @param uri The uri for the ontology resource, or null for an anonymous resource.
     * @return An ontology resource, of the type specified by the <code>javaClass</code>
     */
    @Override
    public <T extends OntResource> T createOntResource(Class<T> javaClass, Resource rdfType, String uri) {
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "6fc50a08-f6c6-4ff2-98e2-10f137022261");
        return getResourceWithType(uri, rdfType).as(javaClass);
    }

    /**
     * <p>Answer a resource presenting the {@link OntResource} facet, which has the
     * given URI.</p>
     * @param uri The URI of the resource, or null for an anonymous resource (aka bNode)
     * @return An OntResource with the given URI
     */
    @Override
    public OntResource createOntResource(String uri) {
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "58c506a6-1a8d-4609-9519-5ea6a8209eca");
        return getResource(uri).as(OntResource.class);
    }

    /**
     * <p>Answer a new empty list.  This method overrides the list create method in ModelCom,
     * to allow both DAML and RDFS lists to be created.</p>
     * @return An RDF-encoded list of no elements, using the current language profile
     */
    @Override
    public RDFList createList() {
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "2d96f021-f426-4088-9f6b-ee7bf8ed9a07");
        Resource list = getResource(getProfile().NIL().getURI());
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "e326d0bf-7518-4361-9051-2d239fe2bfba");
        return list.as(RDFList.class);
    }

    /**
     * <p>
     * Answer the language profile (for example, OWL or DAML+OIL) that this model is
     * working to.
     * </p>
     *
     * @return A language profile
     */
    @Override
    public Profile getProfile() {
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "beb5f3b0-e665-43d0-a95d-35e9df7bd6f8");
        return m_spec.getProfile();
    }

    /**
     * <p>Determine which models this model imports (by looking for, for example,
     * <code>owl:imports</code> statements, and load each of those models as an
     * import. A check is made to determine if a model has already been imported,
     * if so, the import is ignored. Thus this method is safe against circular
     * sets of import statements. Note that actual implementation is delegated to
     * the associated {@link OntDocumentManager}.
     */
    @Override
    public void loadImports() {
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "05e6c564-8ade-4e39-889e-380a1ea95ad3");
        // load the imports closure, according to the policies in my document manager
        getDocumentManager().loadImports(this);
    }

    /**
     * <p>
     * Answer true if this model has had the given URI document imported into it. This is
     * important to know since an import only occurs once, and we also want to be able to
     * detect cycles of imports.
     * </p>
     *
     * @param uri An ontology URI
     * @return True if the document corresponding to the URI has been successfully loaded
     * into this model
     */
    @Override
    public boolean hasLoadedImport(String uri) {
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "54a65485-1d1f-412e-973f-02e269378d68");
        return m_imported.contains(uri);
    }

    /**
     * <p>
     * Record that this model has now imported the document with the given
     * URI, so that it will not be re-imported in the future.
     * </p>
     *
     * @param uri A document URI that has now been imported into the model.
     */
    @Override
    public void addLoadedImport(String uri) {
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "29056ef1-c7e5-45b3-bf10-6ad84ee2a3ea");
        m_imported.add(uri);
    }

    /**
     * <p>
     * Record that this model no longer imports the document with the given
     * URI.
     * </p>
     *
     * @param uri A document URI that is no longer imported into the model.
     */
    @Override
    public void removeLoadedImport(String uri) {
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "c0c72ab6-7f4a-4af9-95f6-b43a8c06f71a");
        m_imported.remove(uri);
    }

    /**
     * <p>
     * Answer a list of the imported URI's in this ontology model. Detection of <code>imports</code>
     * statments will be according to the local language profile
     * </p>
     *
     * @return The imported ontology URI's as a set. Note that since the underlying graph is
     * not ordered, the order of values in the list in successive calls to this method is
     * not guaranteed to be preserved.
     */
    @Override
    public Set<String> listImportedOntologyURIs() {
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "fa370933-f2fa-420f-bea2-fe347f75b656");
        return listImportedOntologyURIs(false);
    }

    /**
     * <p>
     * Answer a list of the imported URI's in this ontology model, and optionally in the closure
     * of this model's imports. Detection of <code>imports</code>
     * statments will be according to the local language profile.  Note that, in order to allow this
     * method to be called during the imports closure process, we <b>only query the base model</b>,
     * thus side-stepping the any attached reasoner.
     * </p>
     * @param closure If true, the set of uri's returned will include not only those directly
     * imported by this model, but those imported by the model's imports transitively.
     * @return The imported ontology URI's as a list. Note that since the underlying graph is
     * not ordered, the order of values in the list in successive calls to this method is
     * not guaranteed to be preserved.
     */
    @Override
    public Set<String> listImportedOntologyURIs(boolean closure) {
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "902c14e4-fd4d-4814-9d71-7bb25fa60cbd");
        Set<String> results = new HashSet<String>();
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "cdd0b362-252d-4993-9c81-2601e05e8e0f");
        List<Model> queue = new ArrayList<Model>();
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "67ee7edf-2aa4-4e75-b3ab-19327ef3f136");
        queue.add(getBaseModel());
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "5374ed27-b810-414f-a3df-db8208b66813");
        while (!queue.isEmpty()) {
            writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "8c03970e-dc54-4850-b48b-e9e3a55e8e5d");
            Model m = queue.remove(0);
            writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "3be0b5dc-19e4-4fd3-b63b-bb514bc41ee4");
            // list the ontology nodes
            if (getProfile().ONTOLOGY() != null && getProfile().IMPORTS() != null) {
                writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "aff33676-0cc4-4479-9e70-900219a20e3f");
                StmtIterator i = m.listStatements(null, getProfile().IMPORTS(), (RDFNode) null);
                writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "faac6073-fc46-4305-8cc6-147e0164f0b9");
                while (i.hasNext()) {
                    writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "0102e980-b18d-42e2-b0db-d4ac03a85ea6");
                    Statement s = i.nextStatement();
                    writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "fa21309d-7828-4205-a467-f28c443561d5");
                    String uri = s.getResource().getURI();
                    writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "94c5f41f-491e-41ec-ae97-874ae58ef067");
                    if (!results.contains(uri)) {
                        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "57952afc-7be9-4b6d-995f-a16cff843661");
                        // this is a new uri, so we add it
                        results.add(uri);
                        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "cb5d3ebb-87ed-4d95-9788-28aeb6abf235");
                        // and push the model on the stack if we know it
                        Model mi = getDocumentManager().getModel(uri);
                        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "c7ca7399-4f04-442a-98c9-929d7ed84cca");
                        if (closure && mi != null && !queue.contains(mi)) {
                            writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "be702f92-3f09-498f-b5c4-e104e9188831");
                            queue.add(mi);
                        }
                    }
                }
            }
        }
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "defa3855-cad3-4339-8cfb-17b594815dc1");
        return results;
    }

    /**
     * <p>
     * Answer the model maker associated with this model (used for constructing the
     * constituent models of the imports closure).
     * </p>
     *
     * @return The local graph factory
     */
    @Override
    public ModelMaker getImportModelMaker() {
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "0b8d6421-302c-4bef-aae6-206ba00f1beb");
        return m_spec.getImportModelMaker();
    }

    /**
     * @deprecated use getImportModelMaker instead.
     */
    @Override
    @Deprecated
    public ModelMaker getModelMaker() {
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "fc66c666-95b8-4bf1-87cc-afe906700be6");
        return getImportModelMaker();
    }

    /**
     * <p>Read statements into the model from the given source, and then load
     * imported ontologies (according to the document manager policy).</p>
     * @param uri URI to read from, may be mapped to a local source by the document manager
     */
    @Override
    public Model read(String uri) {
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "8edc71fb-05a1-4239-aea0-e0c340a4ce9d");
        return read(uri, null, null);
    }

    /**
     * <p>Read statements into the model from the given source, and then load
     * imported ontologies (according to the document manager policy).</p>
     * @param reader An input reader
     * @param base The base URI
     */
    @Override
    public Model read(Reader reader, String base) {
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "3a2dc2f7-1728-4cd2-ada5-8fe7758ae88a");
        super.read(reader, base);
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "386bafde-e53a-41ab-82af-5e9efd43377f");
        loadImports();
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "31558bc2-7480-44ac-8206-fbd4d14634a0");
        rebind();
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "9425d66d-f7eb-4fca-84a5-a4a8d6b7a9ea");
        return this;
    }

    /**
     * <p>Read statements into the model from the given source, and then load
     * imported ontologies (according to the document manager policy).</p>
     * @param reader An input stream
     * @param base The base URI
     */
    @Override
    public Model read(InputStream reader, String base) {
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "c77deb52-4534-4a6c-9ec3-2ca10a86be91");
        super.read(reader, base);
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "ec339535-f5c1-4cfb-bc7f-5b5f127e0ced");
        loadImports();
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "cb0ecaa1-932a-4dd9-ac98-e5d63df0ad67");
        rebind();
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "e4f73c5e-136b-4e30-98b7-5c32e2c37ecd");
        return this;
    }

    /**
     * <p>Read statements into the model from the given source, and then load
     * imported ontologies (according to the document manager policy).</p>
     * @param uri URI to read from, may be mapped to a local source by the document manager
     * @param syntax The source syntax
     * @return This model, to allow chaining calls
     */
    @Override
    public Model read(String uri, String syntax) {
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "f5c32016-707c-4955-948c-1bbe4e284eed");
        return read(uri, null, syntax);
    }

    /**
     * <p>Read statements into the model from the given source, and then load
     * imported ontologies (according to the document manager policy).</p>
     * @param uri URI to read from, may be mapped to a local source by the document manager
     * @param base The base URI for this model
     * @param syntax The source syntax
     * @return This model, to allow chaining calls
     */
    @Override
    public Model read(String uri, String base, String syntax) {
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "ab20b0d9-019c-4756-b0a9-0df4f7876860");
        // we don't want to load this document again if imported by one of the imports
        addLoadedImport(uri);
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "038f698d-2f57-4207-9a5b-8d9e3b8e4938");
        OntDocumentManager odm = getDocumentManager();
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "018bae04-de5f-4905-93bc-cf12c0d95fdc");
        String sourceURL = odm.doAltURLMapping(uri);
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "f9f04adc-e89c-46c2-9ad1-18112235cb4d");
        // invoke the read hook from the ODM
        String source = odm.getReadHook().beforeRead(this, sourceURL, odm);
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "60e1dd52-2050-4090-8dea-433406585a83");
        if (source == null) {
            writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "f4454dec-ad6a-4ffc-ab34-060a5141862f");
            s_log.warn("ReadHook returned null, so skipping assuming previous value: " + sourceURL);
            writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "346c543a-4cba-4dd1-bde7-f0a63283d413");
            source = sourceURL;
        } else {
            writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "b65a83d7-3656-4d8e-88e7-12cc552d4260");
            // now we can actually do the read, check first if we should use negotiation
            if (// require non-null base
            base == null && // and that negotiation makes sense (don't conneg to file:)
            !ignoreFileURI(source) && // and that we haven't remapped the URI
            source.equals(uri)) {
                writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "36678b0e-84cc-4776-b9ee-358341898f15");
                if (syntax == null) {
                    writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "26d2ddef-6495-4f70-b994-4e4b62df6e73");
                    readDelegate(source);
                } else {
                    writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "0accdd37-270d-47d0-b2ca-8891cfb8aac6");
                    readDelegate(source, syntax);
                }
            } else {
                writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "f521e9d3-7bc8-4304-809a-6638a6a77e8f");
                // if we were given the base, use it ... otherwise default to the base being the source
                readDelegate(source, (base == null ? uri : base), syntax);
            }
        }
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "0eb088aa-adb8-4d2d-8119-22cf69941d17");
        // the post read hook
        odm.getReadHook().afterRead(this, source, odm);
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "def85d32-d156-4732-ad6a-60a445b478ed");
        // cache this model against the public uri (if caching enabled)
        getDocumentManager().addModel(uri, this);
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "cd0930c5-cad5-4e4e-bcd0-bef7f7636ad9");
        loadImports();
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "2ddbc374-0fb4-4e6d-a5da-037a29142daa");
        rebind();
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "f03d2222-f88b-4c42-9acd-c857fc3bc572");
        return this;
    }

    /**
     * <p>Read statements into the model from the given source, and then load
     * imported ontologies (according to the document manager policy).</p>
     * @param reader An input reader
     * @param base The base URI
     * @param syntax The source syntax
     * @return This model, to allow chaining calls
     */
    @Override
    public Model read(Reader reader, String base, String syntax) {
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "7a9b234b-9af4-493b-9013-610c2799ddee");
        super.read(reader, base, syntax);
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "19301422-1ce4-4137-b1fe-edea35e8754c");
        loadImports();
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "32399f53-454e-4bba-bb8e-4227dd19d4eb");
        rebind();
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "f918deec-939a-48f4-a35e-4e364f66d6e1");
        return this;
    }

    /**
     * <p>Read statements into the model from the given source, and then load
     * imported ontologies (according to the document manager policy).</p>
     * @param reader An input stream
     * @param base The base URI
     * @param syntax The source syntax
     * @return This model, to allow chaining calls
     */
    @Override
    public Model read(InputStream reader, String base, String syntax) {
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "f8fa1c9b-cba9-4150-99bc-1f909594a0b6");
        super.read(reader, base, syntax);
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "4771652a-b8bf-4d35-abe4-8b226b9c11bb");
        loadImports();
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "fa5e12ac-8d40-4ff9-bcb1-9fe529403d0b");
        rebind();
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "784b1d01-a0ce-4a39-a5a2-6b4a11c1a071");
        return this;
    }

    /**
     * <p>
     * Answer the sub-graphs of this model. A sub-graph is defined as a graph that
     * is used to contain the triples from an imported document.
     * </p>
     *
     * @return A list of sub graphs for this ontology model
     */
    @Override
    public List<Graph> getSubGraphs() {
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "8565972e-6c90-453a-9348-2ba7c5e8d805");
        return getUnionGraph().getSubGraphs();
    }

    /**
     * <p>Answer an iterator over the ontologies that this ontology imports,
     * each of which will have been wrapped as an ontology model using the same
     * {@link OntModelSpec} as this model.  If this model has no imports,
     * the iterator will be non-null but will not have any values.</p>
     * @return An iterator, each value of which will be an <code>OntModel</code>
     * representing an imported ontology.
     * @deprecated This method has been re-named to <code>listSubModels</code>,
     * but note that to obtain the same behaviour as <code>listImportedModels</code>
     * from Jena 2.4 and earlier, callers should invoke {@link #listSubModels(boolean)}
     * with parameter <code>true</code>.
     * @see #listSubModels()
     * @see #listSubModels(boolean)
     */
    @Override
    @Deprecated
    public ExtendedIterator<OntModel> listImportedModels() {
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "b1c58917-656a-47f8-ab1f-282e761e0055");
        return listSubModels(true);
    }

    /**
     * <p>Answer an iterator over the ontology models that are sub-models of
     * this model. Sub-models are used, for example, to represent composite
     * documents such as the imports of a model. So if ontology A imports
     * ontologies B and C, each of B and C will be available as one of
     * the sub-models of the model containing A. This method replaces the
     * older {@link #listImportedModels}. Note that to fully replicate
     * the behaviour of <code>listImportedModels</code>, the
     * <code>withImports</code> flag must be set to true. Each model
     * returned by this method will have been wrapped as an ontology model using the same
     * {@link OntModelSpec} as this model.  If this model has no sub-models,
     * the returned iterator will be non-null but will not have any values.</p>
     *
     * @param withImports If true, each sub-model returned by this method
     * will also include its import models. So if model A imports D, and D
     * imports D, when called with <code>withImports</code> set to true, the
     * return value for <code>modelA.listSubModels(true)</code> will be an
     * iterator, whose only value is a model for D, and that model will contain
     * a sub-model representing the import of E. If <code>withImports</code>
     * is false, E will not be included as a sub-model of D.
     * @return An iterator, each value of which will be an <code>OntModel</code>
     * representing a sub-model of this ontology.
     */
    @Override
    public ExtendedIterator<OntModel> listSubModels(final boolean withImports) {
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "d6ad5d7d-57fc-4578-9f79-987ce59262bf");
        ExtendedIterator<Graph> i = WrappedIterator.create(getSubGraphs().iterator());
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "f068e672-400d-47f7-8f03-16862ddcd73d");
        return i.mapWith(o -> {
            Model base = ModelFactory.createModelForGraph(o);
            OntModel om = new OntModelImpl(m_spec, base, withImports);
            return om;
        });
    }

    /**
     * <p>Answer an iterator over the ontology models that are sub-models of
     * this model. Sub-models are used, for example, to represent composite
     * documents such as the imports of a model. So if ontology A imports
     * ontologies B and C, each of B and C will be available as one of
     * the sub-models of the model containing A.
     * <strong>Important note on behaviour change:</strong> please see
     * the comment on {@link #listSubModels(boolean)} for explanation
     * of the <code>withImports</code> flag. This zero-argument form
     * of <code>listSubModels</code> sets <code>withImports</code> to
     * false, so the returned models will not themselves contain imports.
     * This behaviour differs from the zero-argument method
     * {@link #listImportedModels()} in Jena 2.4 an earlier.</p>
     * @return An iterator, each value of which will be an <code>OntModel</code>
     * representing a sub-model of this ontology.
     * @see #listSubModels(boolean)
     */
    @Override
    public ExtendedIterator<OntModel> listSubModels() {
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "72e9ad3f-ba26-45b9-b6ca-e042843432af");
        return listSubModels(false);
    }

    /**
     * <p>Answer the number of sub-models of this model, not including the
     * base model.</p>
     * @return The number of sub-models, &ge; zero.
     */
    @Override
    public int countSubModels() {
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "69d3fd99-62cb-4d21-8026-5702e409fc3f");
        int count = 0;
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "47781ac6-f8b2-4fc7-bb43-afa2d7a62a51");
        for (Graph graph1 : getSubGraphs()) {
            writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "830f662e-e645-4ee5-8635-a7ddc4819b42");
            count++;
        }
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "18aba8a7-9b97-442c-b16d-ba5fe540de2f");
        return count;
    }

    /**
     * <p>Answer an <code>OntModel</code> representing the imported ontology
     * with the given URI. If an ontology with that URI has not been imported,
     * answer null.</p>
     * @param uri The URI of an ontology that may have been imported into the
     * ontology represented by this model
     * @return A model representing the imported ontology with the given URI, or
     * null.
     */
    @Override
    public OntModel getImportedModel(String uri) {
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "ea7145fd-a2d6-43f9-b318-58856d7918d4");
        if (listImportedOntologyURIs(true).contains(uri)) {
            writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "34032422-1a2a-40c3-9258-7de97204c8db");
            Model mi = getDocumentManager().getModel(uri);
            writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "0f0c188a-3f76-4981-8563-175f35453788");
            if (mi != null) {
                writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "0bf4f7e3-68d3-43ed-ae10-fa07e32bfb3b");
                if (mi instanceof OntModel) {
                    writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "60f57cc5-9f69-43a4-8866-465f032c946d");
                    // already a suitable ont model
                    return (OntModel) mi;
                } else {
                    writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "378711b7-7771-4cd9-8f98-b2fe938d6ec4");
                    // not in ont-model clothing yet, so re-wrap
                    return ModelFactory.createOntologyModel(m_spec, mi);
                }
            }
        }
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "c96a7a72-e2d6-4898-9ed1-efb03ccf8304");
        return null;
    }

    /**
     * <p>
     * Answer the base-graph of this model. The base-graph is the graph that
     * contains the triples read from the source document for this ontology.
     * </p>
     *
     * @return The base-graph for this ontology model
     */
    public Graph getBaseGraph() {
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "9846483f-ea0c-4980-814f-2ef75fc28a80");
        return getUnionGraph().getBaseGraph();
    }

    /**
     * <p>
     * Answer the base model of this model. The base model is the model wrapping
     * the graph that contains the triples read from the source document for this
     * ontology.  It is therefore the model that will be updated if statements are
     * added to a model that is built from a union of documents (via the
     * <code>imports</code> statements in the source document).
     * </p>
     *
     * @return The base model for this ontology model
     */
    @Override
    public Model getBaseModel() {
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "a65e94f0-e495-407f-a531-2aa76e6e97af");
        return ModelFactory.createModelForGraph(getBaseGraph());
    }

    /**
     * <p>
     * Add the given model as one of the sub-models of the enclosed ontology union model.
     * <strong>Note</strong> that if <code>model</code> is a composite model (i.e. an
     * {@link OntModel} or {@link InfModel}), the model and all of its submodels will
     * be added to the union of sub-models of this model. If this is <strong>not</strong> required,
     * callers should explicitly add only the base model:
     * </p>
     * <pre>
     * parent.addSubModel( child.getBaseModel() );
     * </pre>
     *
     * @param model A sub-model to add
     */
    @Override
    public void addSubModel(Model model) {
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "e5acd7f1-3c71-4831-bee5-0cad699278d1");
        addSubModel(model, true);
    }

    /**
     * <p>
     * Add the given model as one of the sub-models of the enclosed ontology union model.
     * <strong>Note</strong> that if <code>model</code> is a composite model (i.e. an
     * {@link OntModel} or {@link InfModel}), the model and all of its submodels will
     * be added to the union of sub-models of this model. If this is <strong>not</strong> required,
     * callers should explicitly add only the base model:
     * </p>
     * <pre>
     * parent.addSubModel( child.getBaseModel(), true );
     * </pre>
     *
     * @param model A sub-model to add
     * @param rebind If true, rebind any associated inferencing engine to the new data (which
     * may be an expensive operation)
     */
    @Override
    public void addSubModel(Model model, boolean rebind) {
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "9b86a291-ab0f-48cc-ac61-38e7ba2e89f9");
        getUnionGraph().addGraph(model.getGraph());
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "9aaaf697-6cb6-4b2d-830d-6a10bcd0f6d9");
        if (rebind) {
            writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "3cdadad3-5fa7-42a2-8537-08cc4cf919ee");
            rebind();
        }
    }

    /**
     * <p>
     * Remove the given model as one of the sub-models of the enclosed ontology union model.    Will
     * cause the associated infererence engine (if any) to update, so this may be
     * an expensive operation in some cases.
     * </p>
     *
     * @param model A sub-model to remove
     * @see #addSubModel( Model, boolean )
     */
    @Override
    public void removeSubModel(Model model) {
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "0d0d8735-823f-4f9f-8558-0f08dd696eaf");
        removeSubModel(model, true);
    }

    /**
     * <p>
     * Remove the given model as one of the sub-models of the enclosed ontology union model.
     * </p>
     *
     * @param model A sub-model to remove
     * @param rebind If true, rebind any associated inferencing engine to the new data (which
     * may be an expensive operation)
     */
    @Override
    public void removeSubModel(Model model, boolean rebind) {
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "150923bf-18fc-4810-96ca-a187321c9041");
        Graph subG = model.getGraph();
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "103a951f-d79d-4fd6-a46d-377219578821");
        getUnionGraph().removeGraph(subG);
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "9efee398-6d0b-4e8d-a903-c115b806e778");
        // originally
        if (subG instanceof MultiUnion) {
            writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "58242aff-c759-4cbd-b3e2-47184fc0de06");
            // we need to get the base graph when removing a ontmodel
            getUnionGraph().removeGraph(((MultiUnion) subG).getBaseGraph());
        }
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "0dc4225d-2678-49a4-b025-0ed4af23503a");
        if (rebind) {
            writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "b6346de3-de04-42e7-8b52-e99ff342d0ac");
            rebind();
        }
    }

    /**
     * <p>Answer true if the given node is a member of the base model of this ontology model.
     * This is an important distiction, because only the base model receives updates when the
     * ontology model is updated. Thus, removing properties of a resource that is not in the base
     * model will not actually side-effect the overall model.</p>
     * @param node An RDF node (Resource, Property or Literal) to test
     * @return True if the given node is from the base model
     */
    @Override
    public boolean isInBaseModel(RDFNode node) {
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "98f38ccf-fd6f-4d10-817f-88299c170633");
        Node n = node.asNode();
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "4be549c5-c1a2-4145-8be9-7d8317391843");
        Graph b = getBaseGraph();
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "d168f677-188c-4f27-bac9-c606a09b3a53");
        return b.contains(n, Node.ANY, Node.ANY) || b.contains(Node.ANY, n, Node.ANY) || b.contains(Node.ANY, Node.ANY, n);
    }

    /**
     * <p>Answer true if the given statement is defined in the base model of this ontology model.
     * This is an important distiction, because only the base model receives updates when the
     * ontology model is updated. Thus, removing a statement that is not in the base
     * model will not actually side-effect the overall model.</p>
     * @param stmt A statement to test
     * @return True if the given statement is from the base model
     */
    @Override
    public boolean isInBaseModel(Statement stmt) {
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "62ebe34a-981c-47c3-a73c-d98d6670bc2c");
        Node s = stmt.getSubject().asNode();
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "a4e14ede-a06a-4c11-905b-ee7175fbd731");
        Node p = stmt.getPredicate().asNode();
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "7399b10c-17ab-424d-9d50-ed8693676ee9");
        Node o = stmt.getObject().asNode();
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "d1f3216e-26f7-4859-8d6e-edfc9ca7e4bf");
        Graph b = getBaseGraph();
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "ae14fdec-ec1e-4495-995d-75217f94d113");
        return b.contains(s, p, o);
    }

    /**
     * <p>
     * Answer true if this model is currently in <i>strict checking mode</i>. Strict
     * mode means
     * that converting a common resource to a particular language element, such as
     * an ontology class, will be subject to some simple syntactic-level checks for
     * appropriateness.
     * </p>
     *
     * @return True if in strict checking mode
     */
    @Override
    public boolean strictMode() {
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "eabdd466-002a-438d-8130-064347056e05");
        return m_strictMode;
    }

    /**
     * <p>
     * Set the checking mode to strict or non-strict.
     * </p>
     *
     * @param strict
     * @see #strictMode()
     */
    @Override
    public void setStrictMode(boolean strict) {
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "52616cd8-7d89-4b88-8ace-532620afdb63");
        m_strictMode = strict;
    }

    /**
     * <p>Set the flag that controls whether adding or removing <i>imports</i>
     * statements into the
     * model will result in the imports closure changing dynamically.</p>
     * @param dynamic If true, adding or removing an imports statement to the
     * model will result in a change in the imports closure.  If false, changes
     * to the imports are not monitored dynamically. Default false.
     */
    @Override
    public void setDynamicImports(boolean dynamic) {
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "9e2a5090-3e64-4924-bbdc-b7f9423de0f2");
        if (dynamic) {
            writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "941ef8e8-8b65-44d3-9874-a2e12916cd34");
            if (m_importsListener == null) {
                writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "b2253087-648e-4347-945c-3cc53f782666");
                // turn on dynamic processing
                m_importsListener = new ImportsListener();
                writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "f0fd8319-1230-4e8f-a7ed-33249284b400");
                register(m_importsListener);
            }
        } else {
            writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "3f037e63-2cce-4218-b44d-51d57df61c51");
            if (m_importsListener != null) {
                writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "8147a9e6-3416-4aa4-8922-eac9bae750fd");
                // turn off dynamic processing
                unregister(m_importsListener);
                writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "161b9b41-9d02-4374-a391-20530353b13f");
                m_importsListener = null;
            }
        }
    }

    /**
     * <p>Answer true if the imports closure of the model will be dynamically
     * updated as imports statements are added and removed.</p>
     * @return True if the imports closure is updated dynamically.
     */
    @Override
    public boolean getDynamicImports() {
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "ad937bfe-0005-4a17-8ddf-ea04dab977c1");
        return m_importsListener != null;
    }

    /**
     * <p>Answer the ontology model specification that was used to construct this model</p>
     * @return An ont model spec instance.
     */
    @Override
    public OntModelSpec getSpecification() {
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "90abf7f1-1d89-47b4-a6c3-cbd9a83855b7");
        return m_spec;
    }

    // output operations - delegate to base model
    @Override
    public Model write(Writer writer) {
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "e3a2ef13-0ea0-4dbd-8651-01d032567882");
        return getBaseModel().write(writer);
    }

    @Override
    public Model write(Writer writer, String lang) {
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "f68ba3c2-383e-4f16-a1b9-88a5c10280db");
        return getBaseModel().write(writer, lang);
    }

    @Override
    public Model write(Writer writer, String lang, String base) {
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "b90af99d-2c0d-4fdb-b289-9b94d7d8f8f6");
        return getBaseModel().write(writer, lang, base);
    }

    @Override
    public Model write(OutputStream out) {
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "774e6eab-eec2-4dfd-b753-e9de2b00f5aa");
        return getBaseModel().write(out);
    }

    @Override
    public Model write(OutputStream out, String lang) {
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "fb062643-8a65-486d-844d-c66a01533b93");
        return getBaseModel().write(out, lang);
    }

    @Override
    public Model write(OutputStream out, String lang, String base) {
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "3b4af787-95c1-4d32-9016-a60d208e9757");
        return getBaseModel().write(out, lang, base);
    }

    @Override
    public Model writeAll(Writer writer, String lang, String base) {
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "e4fe3a38-4ffd-4d7e-b537-8a43b028e1e6");
        return super.write(writer, lang, base);
    }

    @Override
    public Model writeAll(OutputStream out, String lang, String base) {
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "b66f0ba6-aeba-489d-9396-c2a26822f30c");
        return super.write(out, lang, base);
    }

    @Override
    public Model writeAll(Writer writer, String lang) {
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "f8057981-2545-46db-9b0c-664b6f6a20c0");
        return super.write(writer, lang);
    }

    @Override
    public Model writeAll(OutputStream out, String lang) {
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "18b3aba1-1dc8-48f8-a6ab-f3541aa259cb");
        return super.write(out, lang);
    }

    // Implementation of inf model interface methods
    /**
     * Return the raw RDF model being processed (i.e. the argument
     * to the Reasonder.bind call that created this InfModel).
     */
    @Override
    public Model getRawModel() {
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "e352e298-7db8-42f4-b210-03bf438be964");
        return getBaseModel();
    }

    /**
     * Return the Reasoner which is being used to answer queries to this graph.
     */
    @Override
    public Reasoner getReasoner() {
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "a8ac738b-f829-4c73-866b-6c70599f2c95");
        return (getGraph() instanceof InfGraph) ? ((InfGraph) getGraph()).getReasoner() : null;
    }

    /**
     * Cause the inference model  to reconsult the underlying data to take
     * into account changes. Normally changes are made through the InfModel's add and
     * remove calls are will be handled appropriately. However, in some cases changes
     * are made "behind the InfModels's back" and this forces a full reconsult of
     * the changed data.
     */
    @Override
    public void rebind() {
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "6ac94a16-50a4-43f5-b403-5b6b6c817cf3");
        if (getGraph() instanceof InfGraph) {
            writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "d5580719-43ef-4e29-b70a-b850b874494a");
            ((InfGraph) getGraph()).rebind();
        }
    }

    /**
     * Perform any initial processing and caching. This call is optional. Most
     * engines either have negligable set up work or will perform an implicit
     * "prepare" if necessary. The call is provided for those occasions where
     * substantial preparation work is possible (e.g. running a forward chaining
     * rule system) and where an application might wish greater control over when
     * this prepration is done rather than just leaving to be done at first query time.
     */
    @Override
    public void prepare() {
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "ac3e048d-59ad-4b9e-b462-850be5e0d743");
        if (getGraph() instanceof InfGraph) {
            writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "d22ed248-c0a1-4ee8-87ef-72c9ed3c372a");
            ((InfGraph) getGraph()).prepare();
        }
    }

    /**
     * Reset any internal caches. Some systems, such as the tabled backchainer,
     * retain information after each query. A reset will wipe this information preventing
     * unbounded memory use at the expense of more expensive future queries. A reset
     * does not cause the raw data to be reconsulted and so is less expensive than a rebind.
     */
    @Override
    public void reset() {
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "86de821c-1764-4271-b17b-f9aac73be548");
        if (getGraph() instanceof InfGraph) {
            writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "d565d984-513c-4e12-932f-c1793f25080e");
            ((InfGraph) getGraph()).reset();
        }
    }

    /**
     * <p>Returns a derivations model. The rule reasoners typically create a
     * graph containing those triples added to the base graph due to rule firings.
     * In some applications it can useful to be able to access those deductions
     * directly, without seeing the raw data which triggered them. In particular,
     * this allows the forward rules to be used as if they were rewrite transformation
     * rules.</p>
     *
     * @return The derivations model, if one is defined, or else null
     */
    @Override
    public Model getDeductionsModel() {
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "19cb9f50-37a0-4fb8-8da3-0b7ae78b718c");
        if (m_deductionsModel == null) {
            writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "4e107785-1ae1-4bd6-a9b3-c4bfb63369ea");
            InfGraph infGraph = getInfGraph();
            writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "515be5ef-db4e-4376-9675-f41fae08247f");
            if (infGraph != null) {
                writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "9b677d95-e7a3-4c35-8278-543a5c94edf3");
                Graph deductionsGraph = infGraph.getDeductionsGraph();
                writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "53910577-a559-47fc-90b6-73f132cd22a6");
                if (deductionsGraph != null) {
                    writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "c1c039eb-960f-40d0-97f2-e1393a1351eb");
                    m_deductionsModel = ModelFactory.createModelForGraph(deductionsGraph);
                }
            }
        } else {
            writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "296d028a-d5bf-445d-96fe-94ece3f042af");
            // ensure that the cached model sees the updated changes from the
            // underlying reasoner graph
            getInfGraph().prepare();
        }
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "82770aa2-aef3-462f-9fa8-8d59fd1a9536");
        return m_deductionsModel;
    }

    /**
     * Test the consistency of the underlying data. This normally tests
     * the validity of the bound instance data against the bound
     * schema data.
     * @return a ValidityReport structure
     */
    @Override
    public ValidityReport validate() {
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "c6a83505-4abd-4ae1-b1bb-2b3fef604dec");
        return (getGraph() instanceof InfGraph) ? ((InfGraph) getGraph()).validate() : null;
    }

    /**
     * Find all the statements matching a pattern.
     * <p>Return an iterator over all the statements in a model
     * that match a pattern.  The statements selected are those
     * whose subject matches the <code>subject</code> argument,
     * whose predicate matches the <code>predicate</code> argument
     * and whose object matches the <code>object</code> argument.
     * If an argument is <code>null</code> it matches anything.</p>
     * <p>
     * The s/p/o terms may refer to resources which are temporarily defined in the "posit" model.
     * This allows one, for example, to query what resources are of type CE where CE is a
     * class expression rather than a named class - put CE in the posit arg.</p>
     *
     * @return an iterator over the subjects
     * @param subject   The subject sought
     * @param predicate The predicate sought
     * @param object    The value sought
     * @param posit Model containing additional assertions to be considered when matching statements
     */
    @Override
    public StmtIterator listStatements(Resource subject, Property predicate, RDFNode object, Model posit) {
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "f4d3443c-563e-4aa2-8a4e-ddfdc1de8c11");
        if (getGraph() instanceof InfGraph) {
            writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "e33560c6-7583-4d41-97ac-b8e9e3f5cc68");
            Graph gp = posit == null ? ModelFactory.createDefaultModel().getGraph() : posit.getGraph();
            writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "54b17907-7e3f-475a-b4ea-5f8eef15f2a1");
            Iterator<Triple> iter = getInfGraph().find(asNode(subject), asNode(predicate), asNode(object), gp);
            writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "79dca22b-c7db-4490-8e16-9744dd22c05c");
            return IteratorFactory.asStmtIterator(iter, this);
        } else {
            writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "74a88142-2e7c-4338-b94c-e1665effe5c2");
            return null;
        }
    }

    /**
     * Switch on/off drivation logging. If this is switched on then every time an inference
     * is a made that fact is recorded and the resulting record can be access through a later
     * getDerivation call. This may consume a lot of space!
     */
    @Override
    public void setDerivationLogging(boolean logOn) {
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "b820a014-c773-422c-982c-6d147abfe30e");
        if (getGraph() instanceof InfGraph) {
            writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "85b57e66-0e4f-4c94-a318-f129f762e997");
            ((InfGraph) getGraph()).setDerivationLogging(logOn);
        }
    }

    /**
     * Return the derivation of the given statement (which should be the result of
     * some previous list operation).
     * Not all reasoneers will support derivations.
     * @return an iterator over Derivation records or null if there is no derivation information
     * available for this triple.
     */
    @Override
    public Iterator<Derivation> getDerivation(Statement statement) {
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "ba6be6d4-215c-46bd-9c10-64248bb92743");
        return (getGraph() instanceof InfGraph) ? ((InfGraph) getGraph()).getDerivation(statement.asTriple()) : null;
    }

    // Internal implementation methods
    // ////////////////////////////////
    private static void initSyntaxCheckerClass() {
        writelineStatic("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "55a01e74-7898-48e4-8167-7a0d0115105c");
        if (owlSyntaxCheckerClass == null) {
            writelineStatic("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "9b066bdc-87f4-4818-8784-007d7e15f02c");
            try {
                writelineStatic("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "82feca76-049d-4db7-a00c-97de19c158d1");
                owlSyntaxCheckerClass = Class.forName(owlSyntaxCheckerClassName);
                writelineStatic("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "a55c33bc-b58e-4ee7-8896-1a7299451c3d");
                owlSyntaxCheckerClass.newInstance();
            } catch (Exception e) {
                writelineStatic("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "a548b0a1-999e-42a8-9b75-1aa165fc1d05");
                throw new ConfigException("owlsyntax.jar must be on the classpath.", e);
            }
        }
    }

    /**
     * <p>Helper method to the constructor, which interprets the spec and generates an appropriate
     * graph for this model</p>
     * @param spec The model spec to interpret
     * @param base The base model, or null
     */
    private static Graph generateGraph(OntModelSpec spec, Graph base) {
        writelineStatic("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "42d5b483-8bab-4a5a-a3ab-deeca4aa0992");
        // create a empty union graph
        MultiUnion u = new MultiUnion();
        writelineStatic("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "35d0c767-d1c9-493c-b213-3d050c0b24c3");
        u.addGraph(base);
        writelineStatic("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "39651dbc-43f8-46cb-9d11-978047c3a458");
        u.setBaseGraph(base);
        writelineStatic("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "ac823249-5755-4c44-a89b-a77e12a4efa4");
        Reasoner r = spec.getReasoner();
        writelineStatic("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "9f18533e-be2b-45be-bb44-c9b7a556c18c");
        // if we have a reasoner in the spec, bind to the union graph and return
        return r == null ? (Graph) u : r.bind(u);
    }

    /**
     * <p>Answer the union graph that contains the imports closure for this ontology</p>
     * @return The union graph
     */
    protected MultiUnion getUnionGraph() {
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "1ebc8391-9349-4891-b011-00220cc3bc3f");
        return m_union;
    }

    /**
     * Answer the resource with the given URI, if present, as the given facet
     */
    protected <T extends Resource> Resource findByURIAs(String uri, Class<T> asKey) {
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "8bd7053e-180d-4c8e-bf93-0e7c8715106c");
        if (uri == null) {
            writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "37d74dc7-3a20-4f36-be0f-249dbdb8a041");
            throw new IllegalArgumentException("Cannot get() ontology value with a null URI");
        }
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "484df061-ac49-4b6a-8e4d-a0246efd5650");
        Node n = NodeFactory.createURI(uri);
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "a6a29fe6-949a-40a1-9cd8-9e9c874a2e53");
        if (getGraph().contains(n, Node.ANY, Node.ANY)) {
            writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "5256f8ff-55c9-4e06-a98c-576a0995c703");
            // this resource is a subject in the graph
            try {
                writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "07565cfc-9ee2-48dd-8a9d-a2bafcb9c0e0");
                return getNodeAs(n, asKey);
            } catch (ConversionException ignore) {
            /**/
            }
        }
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "cd1adbe6-ea86-4825-8544-eb4571b11d8a");
        // not present, or cannot be as'ed to the desired facet
        return null;
    }

    /**
     * <p>
     * Answer an iterator over all of the resources that have
     * <code>rdf:type</code> type.
     * </p>
     *
     * @param type The resource that is the value of <code>rdf:type</code> we
     * want to match
     * @return An iterator over all triples <code>_x rdf:type type</code>
     */
    protected ExtendedIterator<Triple> findByType(Resource type) {
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "87ee309d-7c3b-4c7a-a76a-1f9b4728ded7");
        return getGraph().find(null, RDF.type.asNode(), type.asNode());
    }

    /**
     * <p>
     * Answer an iterator over all of the resources that have
     * <code>rdf:type type</code>, or optionally, one of the alternative types.
     * </p>
     *
     * @param type The resource that is the value of <code>rdf:type</code> we
     * want to match
     * @param alternates An iterator over alternative types to search for, or null
     * @return An iterator over all triples <code>_x rdf:type t</code> where t
     * is <code>type</code> or one of the values from <code>types</code>.
     */
    protected ExtendedIterator<Triple> findByType(Resource type, Iterator<Resource> alternates) {
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "56ff3971-e370-42a8-b037-66ca4d008b3d");
        ExtendedIterator<Triple> i = findByType(type);
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "fb4de26f-528f-4fe4-b70a-b3b3bfe38285");
        // compose onto i the find iterators for the alternate types
        if (alternates != null) {
            writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "965aaeda-f319-467c-a6a4-4f1f05e11cc2");
            while (alternates.hasNext()) {
                writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "62e40496-53b0-4473-b214-9fef022de067");
                i = i.andThen(findByType(alternates.next()));
            }
        }
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "2a19d87e-795f-4b95-a6ce-8dcd300882d1");
        return i.filterKeep(new UniqueFilter<Triple>());
    }

    /**
     * <p>
     * Answer an iterator over all of the resources that have
     * <code>rdf:type type</code>, or optionally, one of the alternative types,
     * and present the results <code>as()</code> the given class.
     * </p>
     *
     * @param type The resource that is the value of <code>rdf:type</code> we
     * want to match
     * @param types An iterator over alternative types to search for, or null
     * @param asKey The value to use to present the polymorphic results
     * @return An iterator over all triples <code>_x rdf:type type</code>
     */
    protected <T extends RDFNode> ExtendedIterator<T> findByTypeAs(Resource type, Iterator<Resource> types, Class<T> asKey) {
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "9e6fa00e-f215-4a53-9192-48975a41f4b7");
        return findByType(type, types).mapWith(p -> getNodeAs(p.getSubject(), asKey));
    }

    /**
     * <p>
     * Answer an iterator over all of the resources that has an
     * <code>rdf:type</code> from the types iterator,
     * and present the results <code>as()</code> the given class.
     * </p>
     *
     * @param types An iterator over types to search for.  An exception will
     * be raised if this iterator does not have at least one next() element.
     * @param asKey The value to use to present the polymorphic results
     * @return An iterator over all triples <code>_x rdf:type type</code>
     */
    protected <T extends RDFNode> ExtendedIterator<T> findByTypeAs(Iterator<Resource> types, Class<T> asKey) {
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "b6a641f6-dcfe-4aca-bea0-eee5fafdd4d8");
        return findByTypeAs(types.next(), types, asKey);
    }

    /**
     * <p>
     * Answer an iterator over resources with the given rdf:type; for each value
     * in the iterator, ensure that is is presented <code>as()</code> the
     * polymorphic object denoted by the given class key.
     * </p>
     *
     * @param type The rdf:type to search for
     * @param asKey The key to pass to as() on the subject nodes
     * @return An iterator over subjects with the given type, presenting as
     * the given polymorphic class.
     */
    protected <T extends RDFNode> ExtendedIterator<T> findByTypeAs(Resource type, Class<T> asKey) {
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "0afa5505-10b0-4c75-81c6-7289f3023972");
        return findByType(type).mapWith(p -> getNodeAs(p.getSubject(), asKey));
    }

    /**
     * <p>
     * Answer an iterator over nodes that have p as a subject
     * </p>
     *
     * @param p A property
     * @return ExtendedIterator over subjects of p.
     */
    protected ExtendedIterator<Triple> findByDefiningProperty(Property p) {
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "3c09e2ca-41d8-44ee-9470-f36ff7bdfed0");
        return getGraph().find(null, p.asNode(), null);
    }

    /**
     * <p>
     * Answer an iterator over nodes that have p as a subject, presented as
     * polymorphic enh resources of the given facet.
     * </p>
     *
     * @param p A property
     * @param asKey A facet type
     * @return ExtendedIterator over subjects of p, presented as the facet.
     */
    protected <T extends RDFNode> ExtendedIterator<T> findByDefiningPropertyAs(Property p, Class<T> asKey) {
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "6a07b69e-4635-41ff-b00f-ad39880e4843");
        return findByDefiningProperty(p).mapWith(x -> getNodeAs(x.getSubject(), asKey));
    }

    /**
     * <p>
     * Answer the resource with the given uri and that optionally has the given <code>rdf:type</code>,
     * creating the resource if necessary.
     * </p>
     *
     * @param uri The uri to use, or null for an anonymous resource
     * @param rdfType The resource to assert as the <code>rdf:type</code>, or null to leave untyped
     * @return A new or existing Resource
     */
    protected Resource getResourceWithType(String uri, Resource rdfType) {
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "fb6066dc-03ff-4770-bf4e-bb564eec5d03");
        Resource r = getResource(uri);
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "62c768a1-667a-4c78-a923-dbfa577d2a0f");
        if (rdfType != null) {
            writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "120d7e72-8a20-4844-bc43-7d7adc6ff8b6");
            r.addProperty(RDF.type, rdfType);
        }
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "ac9a037f-2eff-472c-97b7-2da9929825bc");
        return r;
    }

    /**
     * <p>Answer a resource presenting the {@link OntResource} facet, which has the given
     * URI. If no such resource is currently present in the model, return null.</p>
     * @param uri The URI of a resource
     * @return An OntResource with the given URI, or null
     */
    @Override
    public OntResource getOntResource(String uri) {
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "da9ceed0-3057-4509-846f-3e660475d920");
        Resource r = getResource(uri);
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "cd2fd154-8e73-46a1-8bbd-4303ebf4ec37");
        if (containsResource(r)) {
            writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "cb03d53a-37e7-461f-adcc-eb9cd0a533e6");
            return r.as(OntResource.class);
        }
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "dd9e3704-1b86-4191-bd8a-393a08fc04c5");
        return null;
    }

    /**
     * <p>Answer a resource presenting the {@link OntResource} facet, which
     * corresponds to the given resource but attached to this model.</p>
     * @param res An existing resource
     * @return An {@link OntResource} attached to this model that has the same URI
     * or anonID as the given resource
     */
    @Override
    public OntResource getOntResource(Resource res) {
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "79f69492-f920-4524-a3d0-8bc775fe7216");
        return res.inModel(this).as(OntResource.class);
    }

    /**
     * <p>Throw an OntologyException if the term is not in language profile</p>
     *
     * @param profileTerm The entry from the profile
     * @param desc A label for the profile term
     * @exception OntologyException if profileTerm is null.
     */
    protected void checkProfileEntry(Object profileTerm, String desc) {
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "8edba5d9-c2c7-4f6c-9fb3-a74fe9de01e4");
        if (profileTerm == null) {
            writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "5f83bc2f-4f02-4438-8dea-82f6eef3dad0");
            // not in the profile
            throw new ProfileException(desc, getProfile());
        }
    }

    /**
     * <p>Check that every member of the given list has the given rdf:type, and throw an exception if not.</p>
     * @param list The list to be checked
     * @param rdfType The rdf:type value to check for
     * @exception LanguageConsistencyException if any member of the list does not have <code>rdf:type <i>rdfType</i></code>
     */
    protected void checkListMembersRdfType(RDFList list, Resource rdfType) {
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "3ff46ed6-3584-4093-bd07-673c65aac18e");
        if (strictMode() && !((Boolean) list.reduce(new RdfTypeTestFn(rdfType), Boolean.TRUE)).booleanValue()) {
            writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "3e48ded9-fbdf-44a7-aa80-89d2dd74b41d");
            // not all of the members of the list are of the given type
            throw new LanguageConsistencyException("The members of the given list are expected to be of rdf:type " + rdfType.toString());
        }
    }

    /**
     * Answer the supplied model, unless it's null, in which case answer a new model
     * constructed as per spec.
     */
    private static Model makeBaseModel(OntModelSpec spec, Model model) {
        writelineStatic("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "bf71d720-71c7-42c3-b6ee-1c9148e97333");
        return model == null ? spec.createBaseModel() : model;
    }

    /**
     * <p>Answer the InfGraph that this model is wrapping, or null if this ontology
     * model is not wrapping an inf graph.</p>
     * @return The model's graph as an InfGraph, or null
     */
    private InfGraph getInfGraph() {
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "263042d7-9deb-4ded-ad3d-1c619ea39993");
        return (getGraph() instanceof InfGraph) ? ((InfGraph) getGraph()) : null;
    }

    /**
     * Test for whether we ignore <code>file:</code> URI's when testing for content
     * negotiation.
     * @param source
     * @return
     */
    protected boolean ignoreFileURI(String source) {
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "437ce5b1-e3ac-4f0f-b2f5-6a746f4aec92");
        return source.startsWith("file:");
    }

    /* delegation points to allow unit testing of read operations */
    protected Model readDelegate(String url) {
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "5e6edd68-09b1-4d80-a396-08c2f8527b50");
        return super.read(url);
    }

    protected Model readDelegate(String url, String lang) {
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "95730231-bebb-4106-9aa2-99a7cf83a4c9");
        return super.read(url, lang);
    }

    protected Model readDelegate(String url, String base, String lang) {
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_9_10.coverage", "fa462a5f-6f19-4fc6-a46a-691f448e50e9");
        return super.read(url, base, lang);
    }

    protected class NodeCanAs<T extends RDFNode> implements Predicate<Node> {

        protected Class<T> m_asKey;

        protected NodeCanAs(Class<T> asKey) {
            m_asKey = asKey;
        }

        @Override
        public boolean test(Node x) {
            try {
                getNodeAs(x, m_asKey);
            } catch (Exception ignore) {
                return false;
            }
            return true;
        }
    }

    /**
     * Predicate that accepts nodes that can be mapped to the given facet
     */
    protected class SubjectNodeCanAs<T extends RDFNode> implements Predicate<T> {

        protected Class<T> m_asKey;

        protected SubjectNodeCanAs(Class<T> asKey) {
            m_asKey = asKey;
        }

        @Override
        public boolean test(T x) {
            Node n = (x instanceof Triple) ? ((Triple) x).getSubject() : ((x instanceof EnhNode) ? ((EnhNode) x).asNode() : (Node) x);
            try {
                getNodeAs(n, m_asKey);
            } catch (Exception ignore) {
                return false;
            }
            return true;
        }
    }

    /**
     * Function to test the rdf type of a list
     */
    protected class RdfTypeTestFn implements RDFList.ReduceFn {

        protected Resource m_type;

        protected RdfTypeTestFn(Resource type) {
            m_type = type;
        }

        @Override
        public Object reduce(RDFNode node, Object accumulator) {
            Boolean acc = (Boolean) accumulator;
            if (acc.booleanValue()) {
                // true so far
                Resource r = (Resource) node;
                return new Boolean(r.hasProperty(RDF.type, m_type));
            } else {
                return acc;
            }
        }
    }

    /**
     * Listener for model changes that indicate a change in the imports to the model
     */
    protected class ImportsListener extends StatementListener {

        @Override
        public void addedStatement(Statement added) {
            if (added.getPredicate().equals(getProfile().IMPORTS())) {
                getDocumentManager().loadImport(OntModelImpl.this, added.getResource().getURI());
            }
        }

        @Override
        public void removedStatement(Statement removed) {
            if (removed.getPredicate().equals(getProfile().IMPORTS())) {
                getDocumentManager().unloadImport(OntModelImpl.this, removed.getResource().getURI());
            }
        }
    }

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
