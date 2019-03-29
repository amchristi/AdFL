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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "ffe0ba34-a81d-4b9d-b7d2-cb1379b3d7f4");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "58336163-5811-4b6c-b505-766f55248782");
        checkProfileEntry(getProfile().ONTOLOGY(), "ONTOLOGY");
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "7a777462-5ef4-4f6f-afdb-27f8f5fe11c1");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "046cfc61-1410-40f1-9928-b38f19257305");
        ExtendedIterator<OntProperty> i = findByTypeAs(RDF.Property, OntProperty.class).filterKeep(new UniqueFilter<OntProperty>());
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "b96f0f2a-f8de-4872-9bd8-731d142d9acc");
        // if we are in OWL_FULL, the properties should also include the annotation properties
        if (getReasoner() != null && getProfile().equals(ProfileRegistry.getInstance().getProfile(ProfileRegistry.OWL_LANG))) {
            writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "551c2aed-2907-419a-993b-aea14904bee4");
            // we are using a reasoner, and in OWL Full
            // so add the annotation properties too
            i = i.andThen(listAnnotationProperties());
        }
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "8e3352e1-8de4-408a-bfac-d384e0202d2f");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "90ee80f2-fa04-4201-ad96-3bd6f5dce810");
        ExtendedIterator<OntProperty> i = findByTypeAs(RDF.Property, OntProperty.class).andThen(listObjectProperties()).andThen(listDatatypeProperties()).andThen(listAnnotationProperties()).andThen(listFunctionalProperties()).andThen(listTransitiveProperties()).andThen(listSymmetricProperties());
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "363c1acb-0dc4-4fa9-bf26-55b1cfbb5a71");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "f058a3d0-4d31-414c-a4de-e2ce12ac87da");
        checkProfileEntry(getProfile().OBJECT_PROPERTY(), "OBJECT_PROPERTY");
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "d0d2e763-7ae3-48b1-ac0f-036bce9f5c9d");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "cb22fa60-0610-4187-8f9b-b8206018e16e");
        checkProfileEntry(getProfile().DATATYPE_PROPERTY(), "DATATYPE_PROPERTY");
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "c093c00e-e8ff-4582-9702-9a1464666c3d");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "8b7df5f4-8913-4078-914b-0e5dc52d0fd9");
        checkProfileEntry(getProfile().FUNCTIONAL_PROPERTY(), "FUNCTIONAL_PROPERTY");
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "e4452afb-a4a5-478b-9d56-d9e809fc6791");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "f9830f0c-f2a0-4e22-82fe-aea97950d83e");
        checkProfileEntry(getProfile().TRANSITIVE_PROPERTY(), "TRANSITIVE_PROPERTY");
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "7c0bd081-1850-48b7-a8c4-7cab6e09e0b7");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "bb156340-31d1-4003-9184-7f00cd50ec88");
        checkProfileEntry(getProfile().SYMMETRIC_PROPERTY(), "SYMMETRIC_PROPERTY");
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "77d1f956-0e50-4f52-85b9-aa2fb4002157");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "0a4d4bb4-097b-4d16-acd2-5f5482bed556");
        checkProfileEntry(getProfile().INVERSE_FUNCTIONAL_PROPERTY(), "INVERSE_FUNCTIONAL_PROPERTY");
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "d6d39a0b-efb8-48ca-8a6e-688df6701265");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "c838c9d1-2c81-4ac0-ac02-cc97a6bde7fa");
        // since the reasoner implements some OWL full functionality for RDF compatibility, we
        // have to decide which strategy to use for identifying individuals depending on whether
        // or not a powerful reasoner (i.e. owl:Thing/daml:Thing aware) is being used with this model
        boolean supportsIndAsThing = false;
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "a98d7863-cb9d-4849-a0e5-98280ff1be64");
        if (getGraph() instanceof InfGraph) {
            writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "9a603a08-b88e-4e33-ab71-880e861edd8e");
            supportsIndAsThing = ((InfGraph) getGraph()).getReasoner().getReasonerCapabilities().contains(null, ReasonerVocabulary.supportsP, ReasonerVocabulary.individualAsThingP);
        }
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "89195f0d-7ff1-47fc-a716-41131e4546d2");
        if (!supportsIndAsThing || (getProfile().THING() == null) || getProfile().CLASS().equals(RDFS.Class)) {
            writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "a1a56fb6-fa99-4e96-af04-24335a1e664d");
            // no inference, or we are in RDFS land, so we pick things that have rdf:type whose rdf:type is Class
            // it's tricky to make this efficient and cover all possible cases. I've changed the code to
            // make use of the isIndividual() test on OntResource, at the expense of some redundant queries
            // to the model, which could become expensive in the case of a DB model - ijd Apr-23-09
            Set<Individual> results = new HashSet<Individual>();
            writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "71eacf17-f96f-4ecc-b198-e3a4f3b4a0fb");
            for (Iterator<Statement> i = listStatements(null, RDF.type, (RDFNode) null); i.hasNext(); ) {
                writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "340cbf9b-5296-475a-bde8-5eb90e336a39");
                OntResource r = i.next().getSubject().as(OntResource.class);
                writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "fb36d381-a65f-446a-90ff-23e898684397");
                if (r.isIndividual()) {
                    writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "203a66d6-aaa3-49ce-9a9b-f684c8f319a4");
                    results.add(r.as(Individual.class));
                }
            }
            writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "ea491609-3ee2-4734-9fdd-c84d516a7610");
            return WrappedIterator.create(results.iterator());
        } else {
            writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "e1d194e4-2188-4dc9-bafc-cb5ba9599615");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "6ccbce60-8ec0-45d3-8a9a-5dc0f17071a5");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "676aef26-4751-48ad-a523-371c05b26de4");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "ce48da21-165e-4d1f-81c5-895016bf823b");
        // look for the shortcut of using direct subClass on :Thing
        if (getReasoner() != null) {
            writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "26d13844-6481-4fe8-bd17-d6fd983fe655");
            Model conf = getReasoner().getReasonerCapabilities();
            writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "41e1851e-4005-4f03-8d5a-d8a191c8db1f");
            if (conf != null && conf.contains(null, ReasonerVocabulary.supportsP, ReasonerVocabulary.directSubClassOf) && getProfile().THING() != null) {
                writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "ec4726db-4b74-401f-81de-d2f7f4e34dd8");
                // we have have both direct sub-class of and a :Thing class to test against
                return listStatements(null, ReasonerVocabulary.directSubClassOf, getProfile().THING()).mapWith(s -> s.getSubject().as(OntClass.class));
            }
        }
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "3e9a4605-e30f-48c3-805b-7de69796cc99");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "df573d1f-bc1b-4b27-b0db-ad79264f5ca5");
        checkProfileEntry(getProfile().ONE_OF(), "ONE_OF");
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "8e2b7896-f3e9-43f3-a091-1f104a66fbc7");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "3333e469-9ac6-49a4-9f19-b5214161a7b7");
        checkProfileEntry(getProfile().UNION_OF(), "UNION_OF");
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "ee7b0bda-9d4e-46db-aa7c-e0c8d07b03c9");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "8be5aae9-5f4d-4b08-84ec-9b41a93f8e7f");
        checkProfileEntry(getProfile().COMPLEMENT_OF(), "COMPLEMENT_OF");
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "438cd324-7644-491c-a213-9bb28a84e210");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "bfabac4d-4f65-48fb-84c3-a877a0d20745");
        checkProfileEntry(getProfile().INTERSECTION_OF(), "INTERSECTION_OF");
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "d1ef0390-6dc9-4513-bf03-025d3f9ad5f9");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "f86551aa-4751-4b2d-9779-d160df13b293");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "eb19503b-821b-4371-b962-24fe6f132c19");
        checkProfileEntry(getProfile().RESTRICTION(), "RESTRICTION");
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "163faf3f-67eb-4c0b-a780-5b31dec3c62c");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "71954719-15ad-411c-b817-3d70dd5860fa");
        checkProfileEntry(getProfile().ALL_DIFFERENT(), "ALL_DIFFERENT");
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "e63f626c-7b81-4314-8550-408e2cd71798");
        return findByTypeAs(getProfile().ALL_DIFFERENT(), AllDifferent.class).filterKeep(new UniqueFilter<AllDifferent>());
    }

    /**
     * <p>Answer an iterator over the DataRange objects in this ontology, if there
     * are any.</p>
     * @return An iterator, whose values are {@link DataRange} objects.
     */
    @Override
    public ExtendedIterator<DataRange> listDataRanges() {
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "59471ba1-a8a9-4a9b-a7fc-1450bd056a18");
        checkProfileEntry(getProfile().DATARANGE(), "DATARANGE");
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "dc6d25a7-bf4b-466d-a857-c8e547999a14");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "2d96b853-1576-4978-88ce-146eeb82e705");
        checkProfileEntry(getProfile().ANNOTATION_PROPERTY(), "ANNOTATION_PROPERTY");
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "3052dba0-b0f6-4fa9-8a39-fccc78ad2484");
        Resource r = getProfile().ANNOTATION_PROPERTY();
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "3ffcd240-aca1-4a1e-af74-7ca7ed3a5ea6");
        if (r == null) {
            writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "9e356210-5ba2-4c7d-99ae-ba9271064dd8");
            return new NullIterator<AnnotationProperty>();
        } else {
            writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "27c9bfb4-f230-4f6c-bb36-127b02081526");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "bf30d8df-6d05-4c05-9955-3f3afeed4b29");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "305bc653-3e44-459d-b869-09a8c3b0b8de");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "f38cb71a-7235-4f6d-89a4-4441508a81c3");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "f1116812-c335-49be-97de-b3683f8ea26f");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "210d1020-6c77-4e27-a191-4400f60e8648");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "23596cfc-81d2-47ec-b7c5-39c0384bdc13");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "f5adc6d8-8c6a-4ed1-a3d6-dc02721844cb");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "93e6420d-a425-4d9e-9eea-945430596be7");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "c3394190-871e-41ea-b69b-262c00dfebe5");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "ec4a41a1-b451-4010-baa3-1991881af3dd");
        OntClass c = (OntClass) findByURIAs(uri, OntClass.class);
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "c926b530-50bc-4a5e-96be-a43081d141fd");
        // special case for nothing and thing
        if (c == null) {
            writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "5f5244e4-3ffb-4c4c-b670-a84a70fcf8cf");
            Resource thing = getProfile().THING();
            writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "0109c729-7ddd-4960-b06d-f8ce64040b0f");
            if (thing != null && thing.getURI().equals(uri)) {
                writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "cbfdf599-86d2-4969-b7f9-fc8519017881");
                c = thing.inModel(this).as(OntClass.class);
            }
            writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "fc0b0ba6-6b68-4fd5-a57f-03515a0c59ed");
            Resource nothing = getProfile().NOTHING();
            writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "bf437cc0-1f36-4bdc-8724-585b44527593");
            if (nothing != null && nothing.getURI().equals(uri)) {
                writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "9f55cd7f-d4c2-4584-839c-5ef535c262dc");
                c = nothing.inModel(this).as(OntClass.class);
            }
        }
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "6bd53986-30e2-49f6-b368-6b58f7bcec6f");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "53fc7ee7-a939-4583-97b4-9676bd34f745");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "5e2b75c9-a67c-41c7-ac8e-c03c33b38329");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "0ff9fae6-af5a-480e-a499-bed19115ff7a");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "f0710423-60da-45d7-872e-ae7d4613f1c4");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "9ade0b77-99d0-4aea-a9bb-e331c365eec5");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "965ddb1f-6c42-4401-af83-3153c3a7cf7f");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "638c9d83-6702-44d5-ba56-4bc38bf5f32e");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "7149314d-a75c-476b-b6b9-402a34a19310");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "be38eda9-fb9c-4faf-8f9a-16543b1ac513");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "1decc858-710c-4cad-ae3f-dd9826ddd34a");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "1ff77b01-159e-4f94-9878-d00cc22f9aaa");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "3cd65c43-0419-4618-9d8d-24d8b1a2337b");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "08a12ae2-6830-4cff-b560-d59b742bf337");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "87375df5-c833-4f37-ade8-012974a0b96b");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "52f92f29-ab76-499f-98fd-96d87bfd055d");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "26e7f586-7204-431f-a57c-8a80e1554701");
        checkProfileEntry(getProfile().ONTOLOGY(), "ONTOLOGY");
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "6e0c7485-0bf6-495b-b920-abdc481e8f65");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "3f1ad8ac-97d4-439e-bf77-29b1b51b1478");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "5420a886-97c7-40d5-bdeb-4b3ac0c987f9");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "82bc8b48-8651-4952-8f94-d5afffd1d3d6");
        Property p = createProperty(uri);
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "ea623775-fd9d-4f85-93f3-4f31e26be4c5");
        p.addProperty(RDF.type, getProfile().PROPERTY());
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "0434fa2d-88ff-4cbd-aa1b-a54e69bc3eb0");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "20e570f5-ff48-49a1-8ca4-164763f08c57");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "f320c382-7d48-4331-9c00-497a5fd3fba3");
        checkProfileEntry(getProfile().OBJECT_PROPERTY(), "OBJECT_PROPERTY");
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "63420dc3-3f3c-48dd-a5f4-bfed7a09f742");
        ObjectProperty p = createOntResource(ObjectProperty.class, getProfile().OBJECT_PROPERTY(), uri);
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "0bd29208-cedc-4ca4-9e72-f851a20615c7");
        if (functional) {
            writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "9b81ef6b-12d6-4c69-989f-5acdf214f550");
            checkProfileEntry(getProfile().FUNCTIONAL_PROPERTY(), "FUNCTIONAL_PROPERTY");
            writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "b83baa6f-8b42-4d5d-a459-56b94e0c60ad");
            p.addProperty(RDF.type, getProfile().FUNCTIONAL_PROPERTY());
        }
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "0a60b51d-9dca-4126-8316-0b0213669f21");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "643b82fb-56a1-4103-9898-5b89cde0311f");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "4606c30c-2d2f-4f12-a43a-f4c323fa0386");
        checkProfileEntry(getProfile().TRANSITIVE_PROPERTY(), "TRANSITIVE_PROPERTY");
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "25ad68bc-c496-4b85-b545-514c3433c9c2");
        TransitiveProperty p = createOntResource(TransitiveProperty.class, getProfile().TRANSITIVE_PROPERTY(), uri);
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "39ac3a7e-107e-41e0-a603-a5d48b4e454a");
        if (functional) {
            writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "c7736ebf-c8d3-4417-afbf-9ec80048e19f");
            checkProfileEntry(getProfile().FUNCTIONAL_PROPERTY(), "FUNCTIONAL_PROPERTY");
            writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "39ee8d30-e888-493a-8076-2c0fec115cfc");
            p.addProperty(RDF.type, getProfile().FUNCTIONAL_PROPERTY());
        }
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "c60c9c29-cf39-466a-8a48-167ed87958da");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "40652f13-863e-418a-b792-fc001a25aa13");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "c9b396ac-aa28-4f13-87f6-9012ddc92da3");
        checkProfileEntry(getProfile().SYMMETRIC_PROPERTY(), "SYMMETRIC_PROPERTY");
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "b4dfdc8f-34cb-405b-b2db-cc3e9aa0b874");
        SymmetricProperty p = createOntResource(SymmetricProperty.class, getProfile().SYMMETRIC_PROPERTY(), uri);
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "9e08a2bb-93b4-4e86-93bf-4ff11907a5b2");
        if (functional) {
            writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "cee2a734-c5cb-4445-b170-d823e557cb4d");
            checkProfileEntry(getProfile().FUNCTIONAL_PROPERTY(), "FUNCTIONAL_PROPERTY");
            writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "976915b8-5a68-4629-b00b-52bd7511650a");
            p.addProperty(RDF.type, getProfile().FUNCTIONAL_PROPERTY());
        }
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "e9727c72-e3a9-47fc-aa7f-966d79d7d8dc");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "575cd108-6de3-49b4-8a45-dcbe83e89db7");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "17711156-df01-40e3-8224-33755f4e4026");
        checkProfileEntry(getProfile().INVERSE_FUNCTIONAL_PROPERTY(), "INVERSE_FUNCTIONAL_PROPERTY");
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "881893a2-6f0c-411e-8d14-2bc931da7ae1");
        InverseFunctionalProperty p = createOntResource(InverseFunctionalProperty.class, getProfile().INVERSE_FUNCTIONAL_PROPERTY(), uri);
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "c167d0d5-f288-443b-b9eb-6a6b26074cf9");
        if (functional) {
            writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "88191e3a-a715-44f0-b822-e72ec55b8414");
            checkProfileEntry(getProfile().FUNCTIONAL_PROPERTY(), "FUNCTIONAL_PROPERTY");
            writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "a547fd5a-ca19-4cdf-be8f-51231e0d5b94");
            p.addProperty(RDF.type, getProfile().FUNCTIONAL_PROPERTY());
        }
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "94934e0d-f11c-4cd2-8914-5abc28fd54e9");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "00cc6138-0cb3-4d65-b75e-985d2f0b56d1");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "04f40300-14b2-407a-97df-b77293f31d88");
        checkProfileEntry(getProfile().DATATYPE_PROPERTY(), "DATATYPE_PROPERTY");
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "f6666355-cc7a-42c4-aefe-b6ee98a680dc");
        DatatypeProperty p = createOntResource(DatatypeProperty.class, getProfile().DATATYPE_PROPERTY(), uri);
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "da9e79cb-a8a5-4837-a4a7-4007ddd69324");
        if (functional) {
            writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "7f8cef28-4870-4a14-9308-d3b40596ce56");
            checkProfileEntry(getProfile().FUNCTIONAL_PROPERTY(), "FUNCTIONAL_PROPERTY");
            writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "7d9e43fc-9e1c-4867-be6c-d078de22790f");
            p.addProperty(RDF.type, getProfile().FUNCTIONAL_PROPERTY());
        }
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "c90aef1e-508e-4d90-beda-f4e7efedaf7a");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "9032263c-ab21-4a84-8c53-78a6a0658610");
        checkProfileEntry(getProfile().ANNOTATION_PROPERTY(), "ANNOTATION_PROPERTY");
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "89786435-c316-419b-b4be-1cb23ef9ae24");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "0c9fb4c2-9c11-4fd5-a372-04e61f893802");
        checkProfileEntry(getProfile().CLASS(), "CLASS");
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "7ae02272-acc9-4e17-b293-07989caee174");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "4179b028-b39a-4e6f-80d4-0a1277b8aefe");
        checkProfileEntry(getProfile().CLASS(), "CLASS");
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "dbb81a89-1a35-44ff-b60e-2bb1dcc6afb8");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "207d07d6-d77d-498f-9fe8-2f2f48d0535f");
        checkProfileEntry(getProfile().CLASS(), "CLASS");
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "bb2485a0-2e15-459c-bd02-7607f4fc677c");
        OntClass c = createOntResource(OntClass.class, getProfile().CLASS(), uri);
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "dac795f0-18f0-44e1-9da0-68bcc594d694");
        checkProfileEntry(getProfile().COMPLEMENT_OF(), "COMPLEMENT_OF");
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "9a6040f3-587e-4ee1-a198-447b231d093f");
        // if the class that this class is a complement of is not specified, use owl:nothing or daml:nothing
        c.addProperty(getProfile().COMPLEMENT_OF(), (cls == null) ? getProfile().NOTHING() : cls);
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "c216c6db-a762-4911-8760-3715cf37038b");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "3eb7ad9a-eb15-4f36-a33e-c04e33611636");
        checkProfileEntry(getProfile().CLASS(), "CLASS");
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "40c95b24-206f-443b-88a3-1d13a7d9c68e");
        OntClass c = createOntResource(OntClass.class, getProfile().CLASS(), uri);
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "7b9ba2b2-8e1c-4080-ba25-c900980044b1");
        checkProfileEntry(getProfile().ONE_OF(), "ONE_OF");
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "1152c741-714e-40bc-b234-86cb41726cb1");
        c.addProperty(getProfile().ONE_OF(), (members == null) ? createList() : members);
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "eb0d46cc-c333-462d-ab5d-5094455638f4");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "6a063726-e1d8-40f2-84b1-f365d5d6a21e");
        checkProfileEntry(getProfile().CLASS(), "CLASS");
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "26602bc1-c1a4-4366-8e9a-18f27705fae9");
        OntClass c = createOntResource(OntClass.class, getProfile().CLASS(), uri);
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "85f22a14-6d14-435b-b0c1-ff83b140f550");
        checkProfileEntry(getProfile().UNION_OF(), "UNION_OF");
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "93d96255-64f1-40bd-bc15-3de645678b3e");
        c.addProperty(getProfile().UNION_OF(), (members == null) ? createList() : members);
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "522fd14a-ed76-4202-ba6c-534d678f572b");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "6baee6df-779c-487c-afa4-d517be7d60f7");
        checkProfileEntry(getProfile().CLASS(), "CLASS");
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "b45de2ff-5c51-49b3-92e3-8bbb6193a245");
        OntClass c = createOntResource(OntClass.class, getProfile().CLASS(), uri);
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "ace015dd-63aa-4ceb-99e9-82b300b210ae");
        checkProfileEntry(getProfile().INTERSECTION_OF(), "INTERSECTION_OF");
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "3e3479ce-9674-4a1f-8ae3-d34cbd39cc14");
        c.addProperty(getProfile().INTERSECTION_OF(), (members == null) ? createList() : members);
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "c1140bfe-5c1f-42a2-b7a2-7b43a7915991");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "edef7261-0c78-4088-9e67-06cbb5a920ab");
        checkProfileEntry(getProfile().RESTRICTION(), "RESTRICTION");
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "2d13a6ef-1bad-44b1-8e92-07fd31616d3f");
        Restriction r = createOntResource(Restriction.class, getProfile().RESTRICTION(), null);
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "254354fa-24d9-406f-b4a4-0c824fd76904");
        if (p != null) {
            writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "c5c13e8d-867b-4024-a05d-1f7beb0d4fba");
            r.setOnProperty(p);
        }
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "4fc0907c-77c6-499b-96fa-4513fa4ad024");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "3719386c-4fb7-4454-99cc-5fbd96869eec");
        checkProfileEntry(getProfile().RESTRICTION(), "RESTRICTION");
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "5eb77e4e-10bd-413d-8560-ed7fb142ee90");
        Restriction r = createOntResource(Restriction.class, getProfile().RESTRICTION(), uri);
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "2022bfbc-1e4a-462e-952e-276d9c1f6025");
        if (p != null) {
            writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "bdac2f2b-38a0-4f29-94bc-381cf0e18416");
            r.setOnProperty(p);
        }
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "054d7c0a-9ef9-4f3c-a8d2-13e3c7761615");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "12db1c57-01fa-4135-a4fa-ebd996f029bd");
        checkProfileEntry(getProfile().RESTRICTION(), "RESTRICTION");
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "72e68564-2bc2-44ef-a2bb-27ff450df005");
        Restriction r = createOntResource(Restriction.class, getProfile().RESTRICTION(), uri);
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "e7b7d402-3d21-4ba7-9733-582bb38f8b11");
        if (prop == null || value == null) {
            writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "94a15765-f27b-40f3-98b4-6e8b1fef18d3");
            throw new IllegalArgumentException("Cannot create hasValueRestriction with a null property or value");
        }
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "d30fd19a-5475-4170-a89f-0a0fc516e0c0");
        checkProfileEntry(getProfile().HAS_VALUE(), "HAS_VALUE");
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "e722f938-7093-4d9b-9454-b0e97f4c15d9");
        r.addProperty(getProfile().ON_PROPERTY(), prop);
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "2faac8b3-0eb8-46d5-b99b-b697a6bd3b30");
        r.addProperty(getProfile().HAS_VALUE(), value);
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "3d371ac1-86f3-44f6-9d23-cd222f83a3c5");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "28b18414-9d6c-4f3e-adea-e4bf17cbe3d5");
        checkProfileEntry(getProfile().RESTRICTION(), "RESTRICTION");
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "4edca723-fd41-4f47-823c-6028bf2770a3");
        Restriction r = createOntResource(Restriction.class, getProfile().RESTRICTION(), uri);
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "2497052e-899d-4935-b056-80f533585b3b");
        if (prop == null || cls == null) {
            writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "edcf7582-24a5-4ad9-a38b-1599f07a38b0");
            throw new IllegalArgumentException("Cannot create someValuesFromRestriction with a null property or class");
        }
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "a5b27c8c-d511-463a-9a7b-ac92267c351b");
        checkProfileEntry(getProfile().SOME_VALUES_FROM(), "SOME_VALUES_FROM");
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "1e89817d-2deb-4f36-b383-afc740e2a8be");
        r.addProperty(getProfile().ON_PROPERTY(), prop);
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "0594f20d-e404-4232-a881-364313b908f9");
        r.addProperty(getProfile().SOME_VALUES_FROM(), cls);
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "7c075e50-5b99-4ef3-b938-f58ded2abdc4");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "abaee531-0045-434e-8cc5-e6e385bb23e9");
        checkProfileEntry(getProfile().RESTRICTION(), "RESTRICTION");
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "caa5f373-250b-4e8c-88b3-d2a3835129d3");
        Restriction r = createOntResource(Restriction.class, getProfile().RESTRICTION(), uri);
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "698aff56-c8a0-463e-85cf-01aa9c703d01");
        if (prop == null || cls == null) {
            writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "924998d5-478f-4657-8a54-c11e15b83655");
            throw new IllegalArgumentException("Cannot create allValuesFromRestriction with a null property or class");
        }
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "5651e489-b35c-4314-b4ab-abec0f725a79");
        checkProfileEntry(getProfile().ALL_VALUES_FROM(), "ALL_VALUES_FROM");
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "9ea20539-2263-4036-8d43-ca652a6dc123");
        r.addProperty(getProfile().ON_PROPERTY(), prop);
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "90b31272-83e7-444e-b2e2-6a95c1cde420");
        r.addProperty(getProfile().ALL_VALUES_FROM(), cls);
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "4f29c0ad-1fc6-4653-9520-aeeebcbce711");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "6d811807-6b6c-4906-b208-dbe516ffeb43");
        checkProfileEntry(getProfile().RESTRICTION(), "RESTRICTION");
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "73f215fa-15f6-4dae-8705-ff4a8960ad52");
        Restriction r = createOntResource(Restriction.class, getProfile().RESTRICTION(), uri);
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "b66ebe15-3e71-4607-9ef7-4045a632d2fe");
        if (prop == null) {
            writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "cc922496-51f3-4375-9385-0381ed940644");
            throw new IllegalArgumentException("Cannot create cardinalityRestriction with a null property");
        }
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "1c1c57b1-41c8-4bdf-9283-93631a460322");
        checkProfileEntry(getProfile().CARDINALITY(), "CARDINALITY");
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "51c36b34-9bad-4c22-8941-1ca864c43a63");
        r.addProperty(getProfile().ON_PROPERTY(), prop);
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "406d1bae-1339-4769-87d7-e8fec9b20b4f");
        r.addProperty(getProfile().CARDINALITY(), createTypedLiteral(cardinality));
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "110192bf-e172-4274-974d-61c3998e2bac");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "ba61f02b-1e89-4661-8a5a-737e566fc0a4");
        checkProfileEntry(getProfile().RESTRICTION(), "RESTRICTION");
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "22eac8a1-8b5f-4772-9210-92143ce91bae");
        Restriction r = createOntResource(Restriction.class, getProfile().RESTRICTION(), uri);
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "bcaeacce-ffee-4971-8b64-6e73819cd8a6");
        if (prop == null) {
            writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "48d2c942-d398-4330-8556-0f86d67cbc3a");
            throw new IllegalArgumentException("Cannot create minCardinalityRestriction with a null property");
        }
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "b740102f-bd37-44c6-ac65-ce4ad5235c03");
        checkProfileEntry(getProfile().MIN_CARDINALITY(), "MIN_CARDINALITY");
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "4a8a0506-3f28-4c66-b6f0-aff875ecf3ca");
        r.addProperty(getProfile().ON_PROPERTY(), prop);
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "6be85db5-1229-41da-bbeb-f0b8003399de");
        r.addProperty(getProfile().MIN_CARDINALITY(), createTypedLiteral(cardinality));
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "0cf2c3d8-150f-4959-8337-da7d57268138");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "efd44536-2f82-483f-9a19-4d36b4deece2");
        checkProfileEntry(getProfile().RESTRICTION(), "RESTRICTION");
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "22e10146-584d-4d89-a789-ed53c313fb6d");
        Restriction r = createOntResource(Restriction.class, getProfile().RESTRICTION(), uri);
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "270255a0-4fb6-4a4d-a2e3-88c57ccc01e3");
        if (prop == null) {
            writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "1c9d2569-11e0-408e-bb41-4b9af6bdc5cc");
            throw new IllegalArgumentException("Cannot create maxCardinalityRestriction with a null property");
        }
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "74f19dc8-da5a-4267-a8e2-788491c820f4");
        checkProfileEntry(getProfile().MAX_CARDINALITY(), "MAX_CARDINALITY");
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "a8438991-36be-4c75-ad07-1aeb8caadb90");
        r.addProperty(getProfile().ON_PROPERTY(), prop);
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "3070a82d-0cc7-4b03-ab6e-f273ae4cc4b8");
        r.addProperty(getProfile().MAX_CARDINALITY(), createTypedLiteral(cardinality));
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "33ac41d8-6236-443b-b4df-762439c16305");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "b0fa4489-7612-4a31-b799-be555ddbe927");
        checkProfileEntry(getProfile().RESTRICTION(), "RESTRICTION");
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "a0b9dcf9-881f-4248-bca0-aec0a7aa92d3");
        checkProfileEntry(getProfile().ON_PROPERTY(), "ON_PROPERTY");
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "f4d41f2e-a4a8-4f5b-93a7-28b0d0c83bf8");
        checkProfileEntry(getProfile().MAX_CARDINALITY_Q(), "MAX_CARDINALITY_Q");
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "d70010a7-59ca-427a-a8e1-f6943d0166a8");
        checkProfileEntry(getProfile().HAS_CLASS_Q(), "HAS_CLASS_Q");
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "eaf577ba-d6f4-47e8-a8ce-4c0075e1efac");
        if (prop == null) {
            writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "91b25265-7478-4f61-b01e-b07b8ef971ea");
            throw new IllegalArgumentException("Cannot create MaxCardinalityQRestriction with a null property");
        }
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "15125229-9924-4285-b4ec-dc156a754a16");
        if (cls == null) {
            writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "eb2a9e94-b06a-4d5e-9c6f-50a131d0cd93");
            throw new IllegalArgumentException("Cannot create MaxCardinalityQRestriction with a null class");
        }
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "7472d75c-6f00-4385-bc03-1e7fa6d99f11");
        Restriction r = createOntResource(Restriction.class, getProfile().RESTRICTION(), uri);
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "ea32d42c-0c97-4728-9e69-b8854a31c06e");
        r.addProperty(getProfile().ON_PROPERTY(), prop);
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "dcf9174c-d839-4c27-8469-9cb396c71271");
        r.addProperty(getProfile().MAX_CARDINALITY_Q(), createTypedLiteral(cardinality));
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "ac7ce29f-444d-49aa-bfcc-fc68ed673095");
        r.addProperty(getProfile().HAS_CLASS_Q(), cls);
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "b9965769-f639-4b58-99ef-289b2a339fa5");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "6af46aab-32ce-4fe1-8f4e-0c0cf1772e49");
        checkProfileEntry(getProfile().RESTRICTION(), "RESTRICTION");
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "5ee46128-91ae-4597-a150-65e727f47e87");
        checkProfileEntry(getProfile().ON_PROPERTY(), "ON_PROPERTY");
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "06754f95-13f6-4390-b62c-bfb2fd91d112");
        checkProfileEntry(getProfile().MIN_CARDINALITY_Q(), "MIN_CARDINALITY_Q");
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "fd416cda-8ea5-4408-aaa6-b633660da554");
        checkProfileEntry(getProfile().HAS_CLASS_Q(), "HAS_CLASS_Q");
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "8452c6f7-18bb-42b1-80a7-b21745a117aa");
        if (prop == null) {
            writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "1a38e4cc-593a-43e9-8e37-442f774150d3");
            throw new IllegalArgumentException("Cannot create MinCardinalityQRestriction with a null property");
        }
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "95ff40c8-aad2-4196-b399-86a9b5dcc930");
        if (cls == null) {
            writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "7171cd59-8435-4c6c-892f-1f1b6b174ebf");
            throw new IllegalArgumentException("Cannot create MinCardinalityQRestriction with a null class");
        }
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "3a3a3152-1491-4bca-90dc-e890c072968f");
        Restriction r = createOntResource(Restriction.class, getProfile().RESTRICTION(), uri);
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "58f08644-176d-446b-8796-231d075df9d4");
        r.addProperty(getProfile().ON_PROPERTY(), prop);
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "fa341ff1-d81d-47e3-a5b7-71582b38df96");
        r.addProperty(getProfile().MIN_CARDINALITY_Q(), createTypedLiteral(cardinality));
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "bb6f9b02-320b-4748-aa79-71f67345a780");
        r.addProperty(getProfile().HAS_CLASS_Q(), cls);
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "798cce5a-98d9-4e08-87f4-123be91de5d7");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "b9a208b3-12b4-485e-88d2-9b14d135920a");
        checkProfileEntry(getProfile().RESTRICTION(), "RESTRICTION");
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "26901f4e-2ce1-476a-9bc9-27e7fb01a243");
        checkProfileEntry(getProfile().ON_PROPERTY(), "ON_PROPERTY");
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "f69e2193-f3cf-4683-8e7f-7151145e8514");
        checkProfileEntry(getProfile().CARDINALITY_Q(), "CARDINALITY_Q");
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "3d672fdf-5ce8-49a5-972a-97eaa146645b");
        checkProfileEntry(getProfile().HAS_CLASS_Q(), "HAS_CLASS_Q");
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "fa19fd09-395a-40a8-9a45-4d89aa7e13a1");
        if (prop == null) {
            writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "14923dca-6813-4a46-9d6a-3d4672d26614");
            throw new IllegalArgumentException("Cannot create CardinalityQRestriction with a null property");
        }
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "33b6d182-e29d-47d7-93a4-849d130b2b7a");
        if (cls == null) {
            writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "58b065e7-d6d1-4d51-bb57-9e402a1c38fb");
            throw new IllegalArgumentException("Cannot create CardinalityQRestriction with a null class");
        }
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "6f261dac-e521-49cf-a832-21d15f65764e");
        Restriction r = createOntResource(Restriction.class, getProfile().RESTRICTION(), uri);
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "e2cf5638-1bf4-4ae2-b7e3-9f84e310c7a7");
        r.addProperty(getProfile().ON_PROPERTY(), prop);
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "1438eea4-0df1-433d-804e-80514717e5a1");
        r.addProperty(getProfile().CARDINALITY_Q(), createTypedLiteral(cardinality));
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "37ea731a-8f77-4b1f-b9f1-84be6c3f0a95");
        r.addProperty(getProfile().HAS_CLASS_Q(), cls);
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "739710ec-262f-470b-b44a-2f21409788f9");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "60c9c557-3f27-4067-a4dd-8ed7f03a4664");
        checkProfileEntry(getProfile().DATARANGE(), "DATARANGE");
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "f252cf7a-3896-4c4a-a041-12d222fa7e7a");
        DataRange d = createOntResource(DataRange.class, getProfile().DATARANGE(), null);
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "26611ce0-0950-4617-b681-17cccab66998");
        checkProfileEntry(getProfile().ONE_OF(), "ONE_OF");
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "e7ed3707-eebf-43d0-9270-8d3fb18380a7");
        d.addProperty(getProfile().ONE_OF(), (literals == null) ? createList() : literals);
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "43feb52e-b881-4dd4-a7a1-19949e4753a9");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "0e042ab2-2466-4bb3-9e57-e27e9f8a4710");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "1b983c2b-30b1-4af8-8adc-266d7c019d26");
        checkProfileEntry(getProfile().ALL_DIFFERENT(), "ALL_DIFFERENT");
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "a6f5a232-ca78-41c7-b1c5-3b4238102f91");
        AllDifferent ad = createOntResource(AllDifferent.class, getProfile().ALL_DIFFERENT(), null);
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "f2d20903-7e14-4343-acc6-457fb29c4345");
        ad.setDistinctMembers((differentMembers == null) ? createList() : differentMembers);
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "4f28fff0-db53-455b-9ecf-4633856d5d44");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "de8db0ff-61b1-4506-97a4-976c8cb069de");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "f389f014-9f2f-49fa-b42f-72c214464793");
        return getResource(uri).as(OntResource.class);
    }

    /**
     * <p>Answer a new empty list.  This method overrides the list create method in ModelCom,
     * to allow both DAML and RDFS lists to be created.</p>
     * @return An RDF-encoded list of no elements, using the current language profile
     */
    @Override
    public RDFList createList() {
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "943f1311-1f0c-4cbf-bb98-7a46ee6e5c32");
        Resource list = getResource(getProfile().NIL().getURI());
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "36c9e8c7-09b6-4865-b236-0a3147f8cc1f");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "4a68d658-1667-4491-ae23-1b2d2c94c315");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "28b65001-7d9e-4df0-98ef-f9afd03e973f");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "d055ee61-1a2e-49c2-a938-a78bf484e262");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "dfde4b63-0349-423f-9b88-97a2d2a7867c");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "864cf477-c72c-4bfc-93cc-cc0f2ea014de");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "5e3bbff1-2ace-489c-8944-aad89100e54c");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "2bd86e02-e6b0-4e1c-9320-de96adc1db21");
        Set<String> results = new HashSet<String>();
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "e022900e-e027-4184-8cdf-7f3fbdad6842");
        List<Model> queue = new ArrayList<Model>();
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "a218b8e3-4c7b-4846-9d1c-fc9dfc357123");
        queue.add(getBaseModel());
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "398d3904-6dfb-44eb-a1c9-3bf0586f3873");
        while (!queue.isEmpty()) {
            writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "c2efc922-e6af-480f-b31b-1024f0c1d7ff");
            Model m = queue.remove(0);
            writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "63bf2ad4-2076-4e16-80cf-6979f5bda167");
            // list the ontology nodes
            if (getProfile().ONTOLOGY() != null && getProfile().IMPORTS() != null) {
                writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "282cafa3-be6b-4bd5-8e26-b300f41997c5");
                StmtIterator i = m.listStatements(null, getProfile().IMPORTS(), (RDFNode) null);
                writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "d7a98049-296a-4ca1-8a31-6882d274cc46");
                while (i.hasNext()) {
                    writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "df5cde6a-ddda-465d-b569-d9a167cc16f9");
                    Statement s = i.nextStatement();
                    writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "e1b09d45-684f-4c29-94e3-dee0275684f3");
                    String uri = s.getResource().getURI();
                    writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "2eeb4112-284a-4ce9-b544-2fbfa95a2402");
                    if (!results.contains(uri)) {
                        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "78882463-5443-4cf9-b86b-759299cfa611");
                        // this is a new uri, so we add it
                        results.add(uri);
                        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "f78422ed-9a12-4eaf-8ffd-ee2950b90629");
                        // and push the model on the stack if we know it
                        Model mi = getDocumentManager().getModel(uri);
                        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "7ceb753b-f31c-4556-949f-f2f9c6a134d5");
                        if (closure && mi != null && !queue.contains(mi)) {
                            writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "c6c61ffd-e86e-4865-b4a7-9427d95b5d56");
                            queue.add(mi);
                        }
                    }
                }
            }
        }
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "0ddb6830-f2d0-4ee5-b956-29254c53f28d");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "1204923b-12e4-47a3-ba02-1bc34e5eac79");
        return m_spec.getImportModelMaker();
    }

    /**
     * @deprecated use getImportModelMaker instead.
     */
    @Override
    @Deprecated
    public ModelMaker getModelMaker() {
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "43c130da-a648-4fca-a476-dd4bed507dbe");
        return getImportModelMaker();
    }

    /**
     * <p>Read statements into the model from the given source, and then load
     * imported ontologies (according to the document manager policy).</p>
     * @param uri URI to read from, may be mapped to a local source by the document manager
     */
    @Override
    public Model read(String uri) {
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "a704282b-8d79-4638-b2cc-6487b4fc6f6a");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "06bb9667-8da7-4414-b287-c4d4f8373e6e");
        super.read(reader, base);
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "27c78ff1-df09-40e7-979c-df22fa81dbdc");
        loadImports();
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "fa6518f2-bb57-44d1-909d-e0694450e248");
        rebind();
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "a22356b3-d11e-48ac-b683-724186a5433b");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "11650632-8cfb-43fd-945e-cf988b202b97");
        super.read(reader, base);
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "5a8ffc50-268d-444c-8c39-c5bf771cc39c");
        loadImports();
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "ecfd379a-4ae8-411b-88b0-d675150f41b3");
        rebind();
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "ac0aac4f-cda0-4bcb-ac76-a62f64bec84e");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "5ca477d4-f86e-44c0-a6f8-7b096f4f16c6");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "32082e23-825b-4cae-8a10-26d00fe37910");
        // we don't want to load this document again if imported by one of the imports
        addLoadedImport(uri);
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "f48a306d-3694-4419-8442-449af5190829");
        OntDocumentManager odm = getDocumentManager();
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "069191cd-10b7-41d3-b9d1-1c88470ccccb");
        String sourceURL = odm.doAltURLMapping(uri);
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "c426e6ee-4a0e-4b0d-9ccd-422010aa201d");
        // invoke the read hook from the ODM
        String source = odm.getReadHook().beforeRead(this, sourceURL, odm);
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "8dd59e22-3baa-4257-af13-c9739ea31503");
        if (source == null) {
            writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "fcea6953-803c-40d4-89e6-bbbd68a7b641");
            s_log.warn("ReadHook returned null, so skipping assuming previous value: " + sourceURL);
            writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "6861e7a1-d6b7-47ea-a97d-fba9d957b644");
            source = sourceURL;
        } else {
            writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "2d62195c-bf70-4ebf-bdf3-912aae6aa537");
            // now we can actually do the read, check first if we should use negotiation
            if (// require non-null base
            base == null && // and that negotiation makes sense (don't conneg to file:)
            !ignoreFileURI(source) && // and that we haven't remapped the URI
            source.equals(uri)) {
                writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "71262766-6725-4e95-918d-2ee85648ba0c");
                if (syntax == null) {
                    writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "9469ed95-cf5e-4009-9195-5dc29c8c3423");
                    readDelegate(source);
                } else {
                    writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "6df05604-7670-4c24-a80f-ae5424e919b8");
                    readDelegate(source, syntax);
                }
            } else {
                writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "15194299-2a06-4b68-9691-ac03d748cbf0");
                // if we were given the base, use it ... otherwise default to the base being the source
                readDelegate(source, (base == null ? uri : base), syntax);
            }
        }
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "b5f8004a-7be7-4998-941c-8f83475321dc");
        // the post read hook
        odm.getReadHook().afterRead(this, source, odm);
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "031758e9-307d-48af-8e0a-93e6665b6cc7");
        // cache this model against the public uri (if caching enabled)
        getDocumentManager().addModel(uri, this);
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "e8281774-4e27-42bb-aa6d-5dc50132837c");
        loadImports();
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "4630b92c-227c-43d4-8bdc-e5da9f6e01f7");
        rebind();
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "b7b803de-d3bd-435b-b8a7-fc89caa2407e");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "44a66e0b-04d5-4cb7-80df-23f76c49b9ab");
        super.read(reader, base, syntax);
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "e4a082e2-f7cb-47e6-a373-af127fbf5543");
        loadImports();
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "8636f6c5-3545-4105-8fd0-b7637e7d3cba");
        rebind();
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "1d338085-860d-45e2-8bf6-7d4de4244d1f");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "a538d957-bcb4-4c15-8e7f-d7a582f71f05");
        super.read(reader, base, syntax);
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "51675a61-62d1-4ae9-89c0-e4a405d30607");
        loadImports();
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "89dbe279-3138-48bf-bfa5-428ff7113fb1");
        rebind();
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "db2d1ecb-e5a1-4be8-b51f-99b37aa479c0");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "b6dd054b-b342-4bd5-b303-723a0d50c560");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "8dab7a9a-b4f8-4119-b669-3f7ba23af919");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "bda56844-608c-4956-879e-62f79ad7e48b");
        ExtendedIterator<Graph> i = WrappedIterator.create(getSubGraphs().iterator());
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "349e203f-15e0-4316-965c-bd050899380a");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "d007c046-abd6-4e75-8fa2-28462f1ed94e");
        return listSubModels(false);
    }

    /**
     * <p>Answer the number of sub-models of this model, not including the
     * base model.</p>
     * @return The number of sub-models, &ge; zero.
     */
    @Override
    public int countSubModels() {
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "3258e7c1-c492-452e-8aac-fde8bc0e905f");
        int count = 0;
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "db0b455e-e55d-4e17-a173-52483f22cfb6");
        for (Graph graph1 : getSubGraphs()) {
            writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "9a01e4f0-fe32-4394-b495-149cb11ac2cb");
            count++;
        }
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "5c55481e-9070-4a5a-9776-6a64f8dcf4b7");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "00d25217-295c-49ae-b271-ec49901e6179");
        if (listImportedOntologyURIs(true).contains(uri)) {
            writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "08254e28-63d4-460d-a037-74f83e9dceb7");
            Model mi = getDocumentManager().getModel(uri);
            writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "586225f0-0b84-4b6d-963a-486c27b50e83");
            if (mi != null) {
                writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "1cfd1da3-9d6f-4d9f-9698-543a69e9f113");
                if (mi instanceof OntModel) {
                    writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "ea7ca60f-4a14-4029-8642-9b703a8d363e");
                    // already a suitable ont model
                    return (OntModel) mi;
                } else {
                    writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "e4397708-ee05-495f-9a81-db0f9fc7ab6b");
                    // not in ont-model clothing yet, so re-wrap
                    return ModelFactory.createOntologyModel(m_spec, mi);
                }
            }
        }
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "e1900580-993f-4e11-a820-2fed52245bf0");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "994d8138-1b33-4e3a-8e5b-f555eb73f422");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "993f561e-353d-45b3-9b4e-3e185fc673e3");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "d3e8fab0-474c-466e-a35b-e4d2f37d89b5");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "184985bb-2814-4ba1-856b-5e6485375c1d");
        getUnionGraph().addGraph(model.getGraph());
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "30f37fb7-51c7-423a-b448-1c3b3687a459");
        if (rebind) {
            writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "b0b9c816-96ea-4fe4-a5d9-1df7ba8a9bc9");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "6dbc872c-a52d-4dac-a678-7bbcf5083ad7");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "7937a836-f104-4c20-ba79-afafad376add");
        Graph subG = model.getGraph();
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "9abe4389-8969-4af4-9aa5-81512aa7ad8f");
        getUnionGraph().removeGraph(subG);
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "24a4eea2-1d0b-4dd4-b279-e79c01de508c");
        // originally
        if (subG instanceof MultiUnion) {
            writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "3894f6e6-5b9b-4d7f-a4c2-fcb827948d41");
            // we need to get the base graph when removing a ontmodel
            getUnionGraph().removeGraph(((MultiUnion) subG).getBaseGraph());
        }
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "50a2d88a-813d-4399-9209-fc76f2031a58");
        if (rebind) {
            writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "2a0741ad-6eff-4fbf-b7ff-938c68b78e55");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "49a7e1ce-a18c-46d9-a5a7-1bf9585117b9");
        Node n = node.asNode();
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "a685bcec-7dcb-4e61-9100-736ba8818bdb");
        Graph b = getBaseGraph();
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "61a87e71-808e-447a-9296-fea78c2447b1");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "fe0244ec-69dd-4202-be84-ab2e87fb25c3");
        Node s = stmt.getSubject().asNode();
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "9c4c17b2-9572-453a-b612-e745f61be5ff");
        Node p = stmt.getPredicate().asNode();
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "1fc9fa84-206c-4554-8703-746b32ebb796");
        Node o = stmt.getObject().asNode();
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "d0e13561-8270-425c-a4c8-3ae87c6153c7");
        Graph b = getBaseGraph();
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "90b2254a-9667-49ba-9a48-c2b982402517");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "dc635b5e-9fec-446d-871e-2a81b28e1508");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "d6b18598-a97b-40dc-8c7c-004a9693290d");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "87f32454-9735-4a0d-8943-d081e00c5e7a");
        if (dynamic) {
            writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "e436d0bc-d61d-4d3f-9388-0ca5a4d95d61");
            if (m_importsListener == null) {
                writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "413d25cf-484d-4ea5-aa81-04e83a077cee");
                // turn on dynamic processing
                m_importsListener = new ImportsListener();
                writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "9ecf3d67-bcfe-4c4d-aff3-07fca927ddca");
                register(m_importsListener);
            }
        } else {
            writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "2098711c-b8bb-4385-ae50-38669273bb46");
            if (m_importsListener != null) {
                writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "2dbd0565-d3cc-450f-a516-f4c44ddbd77d");
                // turn off dynamic processing
                unregister(m_importsListener);
                writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "082e2683-9832-4686-be47-f15485ae639d");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "89e8abf1-7d1c-4ef1-920a-dfe06ce158c8");
        return m_importsListener != null;
    }

    /**
     * <p>Answer the ontology model specification that was used to construct this model</p>
     * @return An ont model spec instance.
     */
    @Override
    public OntModelSpec getSpecification() {
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "27416881-e81f-468a-b86a-d826c7278bf5");
        return m_spec;
    }

    // output operations - delegate to base model
    @Override
    public Model write(Writer writer) {
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "0d0e371f-5411-4126-8476-cda4cb46c486");
        return getBaseModel().write(writer);
    }

    @Override
    public Model write(Writer writer, String lang) {
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "59ecf2c8-10e4-455e-bce3-72710250e473");
        return getBaseModel().write(writer, lang);
    }

    @Override
    public Model write(Writer writer, String lang, String base) {
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "94cfc3d9-cae7-4d97-903a-50d54ef67514");
        return getBaseModel().write(writer, lang, base);
    }

    @Override
    public Model write(OutputStream out) {
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "2a97d00d-a562-42be-b8d2-f7aaa4477184");
        return getBaseModel().write(out);
    }

    @Override
    public Model write(OutputStream out, String lang) {
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "80c4c963-ce69-4361-b319-2d62b208dd03");
        return getBaseModel().write(out, lang);
    }

    @Override
    public Model write(OutputStream out, String lang, String base) {
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "971633fa-9c70-4330-b678-299c98bd1e8c");
        return getBaseModel().write(out, lang, base);
    }

    @Override
    public Model writeAll(Writer writer, String lang, String base) {
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "5917bc52-2536-4b2e-9a9f-06f2ffc59020");
        return super.write(writer, lang, base);
    }

    @Override
    public Model writeAll(OutputStream out, String lang, String base) {
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "f00d0c54-0bed-4a06-8d6b-2d56b68e1b46");
        return super.write(out, lang, base);
    }

    @Override
    public Model writeAll(Writer writer, String lang) {
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "075e6543-80dc-4109-83e9-92fc07905ecc");
        return super.write(writer, lang);
    }

    @Override
    public Model writeAll(OutputStream out, String lang) {
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "cf17dec3-5593-48b3-95e9-fbbf7056c73b");
        return super.write(out, lang);
    }

    // Implementation of inf model interface methods
    /**
     * Return the raw RDF model being processed (i.e. the argument
     * to the Reasonder.bind call that created this InfModel).
     */
    @Override
    public Model getRawModel() {
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "5901922f-f788-4110-ab2a-080e841ef7ab");
        return getBaseModel();
    }

    /**
     * Return the Reasoner which is being used to answer queries to this graph.
     */
    @Override
    public Reasoner getReasoner() {
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "ff27b1dd-798f-4e63-bf6c-83153c8914f5");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "3819c629-e705-4761-b62f-a67d965fbe9b");
        if (getGraph() instanceof InfGraph) {
            writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "970b7160-0f25-402c-875e-60978bd3914d");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "f3bd717a-284e-4154-a063-5491616d1632");
        if (getGraph() instanceof InfGraph) {
            writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "cbeeb480-fcdb-4967-9c63-d2c62820b6ef");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "a6a4f41d-20a8-437b-8336-ae637448dac4");
        if (getGraph() instanceof InfGraph) {
            writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "16b22bd6-7396-4afa-8176-d327693d4850");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "d67737d9-e4d7-416e-8c80-cba06337a94f");
        if (m_deductionsModel == null) {
            writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "de4ea527-1b0e-46ab-a736-ff4223d0b35c");
            InfGraph infGraph = getInfGraph();
            writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "930669d0-d475-460d-a9c9-24225b6d2f08");
            if (infGraph != null) {
                writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "6d04a024-dea1-4e8c-8624-37cfa70ac6fc");
                Graph deductionsGraph = infGraph.getDeductionsGraph();
                writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "39fecc99-f7a6-4fb2-884d-a445e32c678a");
                if (deductionsGraph != null) {
                    writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "e9ecda8c-ba72-4b98-871f-3b134120b2f2");
                    m_deductionsModel = ModelFactory.createModelForGraph(deductionsGraph);
                }
            }
        } else {
            writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "2be785e2-5577-4741-b9fa-b34770e40cb0");
            // ensure that the cached model sees the updated changes from the
            // underlying reasoner graph
            getInfGraph().prepare();
        }
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "797bdbe2-ca0a-4e37-9888-c9f1d1effc79");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "153177a9-03ad-4d0d-b1dd-70fe930d3918");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "9aaaa125-f294-498f-a2c6-1545177faf6e");
        if (getGraph() instanceof InfGraph) {
            writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "35e5a996-7253-4d40-b391-1baa212d9087");
            Graph gp = posit == null ? ModelFactory.createDefaultModel().getGraph() : posit.getGraph();
            writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "c05c80e9-77ff-44f7-b1b2-f984d6697287");
            Iterator<Triple> iter = getInfGraph().find(asNode(subject), asNode(predicate), asNode(object), gp);
            writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "09d7f181-d1b6-4d63-85f7-21b56cb577a4");
            return IteratorFactory.asStmtIterator(iter, this);
        } else {
            writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "f03fa670-e995-4031-ae8f-70fd74c16a87");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "b062487f-b0fe-4dce-94e9-9009b5c719b4");
        if (getGraph() instanceof InfGraph) {
            writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "fbadaf12-54c7-413b-9b68-67508af9455c");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "a89ac9ff-38e1-44e3-809b-45c306064e69");
        return (getGraph() instanceof InfGraph) ? ((InfGraph) getGraph()).getDerivation(statement.asTriple()) : null;
    }

    // Internal implementation methods
    // ////////////////////////////////
    private static void initSyntaxCheckerClass() {
        writelineStatic("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "bee03d2e-1900-43d3-b4fb-472fd37cbdd0");
        if (owlSyntaxCheckerClass == null) {
            writelineStatic("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "d87115c4-8c36-4a62-8dba-18c1b8bbd1b6");
            try {
                writelineStatic("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "c0a19d2c-3eed-4543-aebf-e555a3153ee8");
                owlSyntaxCheckerClass = Class.forName(owlSyntaxCheckerClassName);
                writelineStatic("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "4a703d24-67e6-42d9-a165-d901c0525292");
                owlSyntaxCheckerClass.newInstance();
            } catch (Exception e) {
                writelineStatic("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "db975a70-9d17-44f3-9e9b-a57af9b9894c");
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
        writelineStatic("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "4d978493-4da8-4247-b8d6-eca97c359080");
        // create a empty union graph
        MultiUnion u = new MultiUnion();
        writelineStatic("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "373364b0-ebb6-4dc4-8598-05c42d919779");
        u.addGraph(base);
        writelineStatic("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "e12f7e13-b25c-4dc7-aca0-bd6179acc39d");
        u.setBaseGraph(base);
        writelineStatic("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "fbdabb12-4262-446a-8fe1-6f0d9daf8472");
        Reasoner r = spec.getReasoner();
        writelineStatic("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "fcdc70f4-2e40-437d-9a02-3daa7c3d5067");
        // if we have a reasoner in the spec, bind to the union graph and return
        return r == null ? (Graph) u : r.bind(u);
    }

    /**
     * <p>Answer the union graph that contains the imports closure for this ontology</p>
     * @return The union graph
     */
    protected MultiUnion getUnionGraph() {
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "9e5c1765-f315-4747-b111-bcb2a2aa573c");
        return m_union;
    }

    /**
     * Answer the resource with the given URI, if present, as the given facet
     */
    protected <T extends Resource> Resource findByURIAs(String uri, Class<T> asKey) {
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "acd56796-b673-4e9f-b7ef-4e56205b0d24");
        if (uri == null) {
            writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "bc1dcdb4-9bc5-45bc-8097-045b76d03b82");
            throw new IllegalArgumentException("Cannot get() ontology value with a null URI");
        }
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "dcd3bb0d-c5a1-4250-95f1-d7189a109101");
        Node n = NodeFactory.createURI(uri);
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "fadbdff4-57cd-4d4f-aa60-6de0439311fc");
        if (getGraph().contains(n, Node.ANY, Node.ANY)) {
            writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "9facbb78-a01c-436c-a3cd-b61da78354d5");
            // this resource is a subject in the graph
            try {
                writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "c5c165ee-059d-48bd-8edd-7b3533bfd532");
                return getNodeAs(n, asKey);
            } catch (ConversionException ignore) {
            /**/
            }
        }
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "82b01445-4e33-40ac-a059-230eec33dc04");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "3ebe10d4-5e48-44ae-a568-b045f40939f7");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "177f5cac-7f18-4545-9a1c-26d2b8abe445");
        ExtendedIterator<Triple> i = findByType(type);
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "6f4e0413-41c8-4d6a-a5c9-522ea332423c");
        // compose onto i the find iterators for the alternate types
        if (alternates != null) {
            writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "6824ab56-f805-42c1-9903-cb49462a355a");
            while (alternates.hasNext()) {
                writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "c1b40553-99c8-4488-a1d6-dbef29e3d560");
                i = i.andThen(findByType(alternates.next()));
            }
        }
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "a3bc256e-b1ca-48c3-bce9-77f66e117dc7");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "c72c8dc8-3c06-4d19-af1f-d2ba7543e237");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "a3b088f9-4df1-48b3-9535-6b7634d90344");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "15e7baf0-b16b-4bd8-bcf6-a94913f3c17f");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "96a0104e-d493-4098-82e2-0ca6afeb80e6");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "51976d36-bf2f-4032-b6cc-d4f13b9aeb6d");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "c7de44bb-50fe-4908-817f-8271f9836450");
        Resource r = getResource(uri);
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "a9609e42-925a-4157-b1a3-bb5ce70c0c6c");
        if (rdfType != null) {
            writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "6128a403-f582-4bc2-9334-cd70f6072708");
            r.addProperty(RDF.type, rdfType);
        }
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "87a28034-322d-4085-a72b-9953d501695c");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "5f493a97-ed8f-442b-b88f-803cec270f0e");
        Resource r = getResource(uri);
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "6e16687f-374d-4048-b39a-1914e4290750");
        if (containsResource(r)) {
            writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "6473aa1a-4dc3-4ce4-93bb-2007bf68918a");
            return r.as(OntResource.class);
        }
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "19b027c6-6228-42cd-9dc2-4897bcb5f1b4");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "9546775a-c968-489f-bd1e-68b28aecc259");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "f0abd05a-3418-45e0-883e-f7f1283a94ac");
        if (profileTerm == null) {
            writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "ae7aa3ba-34b0-4333-8ed5-defdcbefedfe");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "58b15230-feed-4f44-a76d-4310f59c70d9");
        if (strictMode() && !((Boolean) list.reduce(new RdfTypeTestFn(rdfType), Boolean.TRUE)).booleanValue()) {
            writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "c5eb40af-fc4c-427c-a30e-453843f7cc50");
            // not all of the members of the list are of the given type
            throw new LanguageConsistencyException("The members of the given list are expected to be of rdf:type " + rdfType.toString());
        }
    }

    /**
     * Answer the supplied model, unless it's null, in which case answer a new model
     * constructed as per spec.
     */
    private static Model makeBaseModel(OntModelSpec spec, Model model) {
        writelineStatic("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "0e2c242b-7ee6-4d4e-ab5b-299e73317a02");
        return model == null ? spec.createBaseModel() : model;
    }

    /**
     * <p>Answer the InfGraph that this model is wrapping, or null if this ontology
     * model is not wrapping an inf graph.</p>
     * @return The model's graph as an InfGraph, or null
     */
    private InfGraph getInfGraph() {
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "44fa5762-3ba3-4509-875f-5ca341b3654b");
        return (getGraph() instanceof InfGraph) ? ((InfGraph) getGraph()) : null;
    }

    /**
     * Test for whether we ignore <code>file:</code> URI's when testing for content
     * negotiation.
     * @param source
     * @return
     */
    protected boolean ignoreFileURI(String source) {
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "a4227097-5154-4dd9-a387-5e088475dd6d");
        return source.startsWith("file:");
    }

    /* delegation points to allow unit testing of read operations */
    protected Model readDelegate(String url) {
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "265ef8f9-68ae-44d1-827c-fe69e85698e7");
        return super.read(url);
    }

    protected Model readDelegate(String url, String lang) {
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "d1ef6f95-bc58-451e-af6d-db7b839776e6");
        return super.read(url, lang);
    }

    protected Model readDelegate(String url, String base, String lang) {
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_1_10.coverage", "d9e12f28-b63c-4cc0-a793-557329d48900");
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
