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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "02aaeaac-c373-48c4-a3d7-11670108c066");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "7b1428ce-cdf6-46c0-b27a-7b7778a7a1b3");
        checkProfileEntry(getProfile().ONTOLOGY(), "ONTOLOGY");
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "7b06c1d0-87e2-4d14-98b1-1f5a8bc97c16");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "2cc7bf34-ef5f-4d73-83a5-efd260da6510");
        ExtendedIterator<OntProperty> i = findByTypeAs(RDF.Property, OntProperty.class).filterKeep(new UniqueFilter<OntProperty>());
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "f87c969d-371b-486e-86bf-6a2d21615fcd");
        // if we are in OWL_FULL, the properties should also include the annotation properties
        if (getReasoner() != null && getProfile().equals(ProfileRegistry.getInstance().getProfile(ProfileRegistry.OWL_LANG))) {
            writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "682de86c-e56c-4ed6-a127-c528b08822e1");
            // we are using a reasoner, and in OWL Full
            // so add the annotation properties too
            i = i.andThen(listAnnotationProperties());
        }
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "8b192e87-b8d6-4e30-9e68-fb2976e23ca1");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "369b546c-9027-499d-afa1-5e3fab3c53b5");
        ExtendedIterator<OntProperty> i = findByTypeAs(RDF.Property, OntProperty.class).andThen(listObjectProperties()).andThen(listDatatypeProperties()).andThen(listAnnotationProperties()).andThen(listFunctionalProperties()).andThen(listTransitiveProperties()).andThen(listSymmetricProperties());
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "500b9790-7c85-4623-ae4c-0199703df5b8");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "862ead21-9780-46a4-bf73-b386da975ca5");
        checkProfileEntry(getProfile().OBJECT_PROPERTY(), "OBJECT_PROPERTY");
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "7349ee24-c851-46a5-9547-f8364b8e5cb9");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "3924b936-ee48-401c-897f-638d22789cf6");
        checkProfileEntry(getProfile().DATATYPE_PROPERTY(), "DATATYPE_PROPERTY");
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "2f09f918-36b8-4143-8834-6fa37eb2eb57");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "3eb54085-1099-4ff1-822a-7e8529fcff10");
        checkProfileEntry(getProfile().FUNCTIONAL_PROPERTY(), "FUNCTIONAL_PROPERTY");
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "6efdd752-5d95-4c1c-b54f-8e8f10ac23a2");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "11b45385-415f-46c6-9ee9-22d6aa2a26ea");
        checkProfileEntry(getProfile().TRANSITIVE_PROPERTY(), "TRANSITIVE_PROPERTY");
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "bf08b531-1313-4c78-a977-8f8b9efa91c6");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "c12b05f3-7248-4e1e-bdf8-b9c305c40e72");
        checkProfileEntry(getProfile().SYMMETRIC_PROPERTY(), "SYMMETRIC_PROPERTY");
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "b2894d8a-26b3-44f8-9dc5-d6beca624218");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "72070bb0-66db-41f3-a305-2c5da0b5cee8");
        checkProfileEntry(getProfile().INVERSE_FUNCTIONAL_PROPERTY(), "INVERSE_FUNCTIONAL_PROPERTY");
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "e39162c0-641e-4fb1-92f7-6751df4fea59");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "10a0ceb3-b9e7-422a-9389-f71709d3de44");
        // since the reasoner implements some OWL full functionality for RDF compatibility, we
        // have to decide which strategy to use for identifying individuals depending on whether
        // or not a powerful reasoner (i.e. owl:Thing/daml:Thing aware) is being used with this model
        boolean supportsIndAsThing = false;
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "b24b837c-9d3a-4416-949b-8649e0c042f2");
        if (getGraph() instanceof InfGraph) {
            writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "1cf56836-8ebf-41e5-bc3a-7b0d41c49aad");
            supportsIndAsThing = ((InfGraph) getGraph()).getReasoner().getReasonerCapabilities().contains(null, ReasonerVocabulary.supportsP, ReasonerVocabulary.individualAsThingP);
        }
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "a7772379-1258-4c49-ab0a-0c80cfd7e2ac");
        if (!supportsIndAsThing || (getProfile().THING() == null) || getProfile().CLASS().equals(RDFS.Class)) {
            writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "029b0181-184b-46c3-a27e-fe57cb505c24");
            // no inference, or we are in RDFS land, so we pick things that have rdf:type whose rdf:type is Class
            // it's tricky to make this efficient and cover all possible cases. I've changed the code to
            // make use of the isIndividual() test on OntResource, at the expense of some redundant queries
            // to the model, which could become expensive in the case of a DB model - ijd Apr-23-09
            Set<Individual> results = new HashSet<Individual>();
            writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "65e17a49-c3f2-4b9b-9292-524a1e8f9c19");
            for (Iterator<Statement> i = listStatements(null, RDF.type, (RDFNode) null); i.hasNext(); ) {
                writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "8ce056ce-6ffa-4f76-8177-3cccd9d41185");
                OntResource r = i.next().getSubject().as(OntResource.class);
                writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "0efd0444-123e-46b2-be95-ba9bd3108ac5");
                if (r.isIndividual()) {
                    writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "c01d3115-924e-4ddc-a293-3f4e07d6abae");
                    results.add(r.as(Individual.class));
                }
            }
            writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "eaec3e01-1c71-4408-847e-aae1e5b225f3");
            return WrappedIterator.create(results.iterator());
        } else {
            writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "aad56580-c647-4012-a554-be28d8fb16e7");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "9f203f80-c62f-48eb-87f7-7651252e9dc1");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "da08674f-99ae-4521-b2c4-91ab10adefc4");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "ec3f6f74-c51a-42c6-a780-604cf5ef02d8");
        // look for the shortcut of using direct subClass on :Thing
        if (getReasoner() != null) {
            writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "c0d44f1b-8967-4f22-9ddc-cb62fd724afc");
            Model conf = getReasoner().getReasonerCapabilities();
            writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "c201f07d-e0b3-41a7-9800-d9017649e61f");
            if (conf != null && conf.contains(null, ReasonerVocabulary.supportsP, ReasonerVocabulary.directSubClassOf) && getProfile().THING() != null) {
                writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "2f6cd495-25c5-4feb-9410-626adf0b9980");
                // we have have both direct sub-class of and a :Thing class to test against
                return listStatements(null, ReasonerVocabulary.directSubClassOf, getProfile().THING()).mapWith(s -> s.getSubject().as(OntClass.class));
            }
        }
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "d8d57446-4aaa-4f92-abfe-a328719e490e");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "bed02419-8647-49c6-95ea-4f0e054a64ad");
        checkProfileEntry(getProfile().ONE_OF(), "ONE_OF");
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "2c173b03-cfd1-43ca-a94d-8e88f5e79412");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "66449219-c58e-4a6e-aa24-f85426888f89");
        checkProfileEntry(getProfile().UNION_OF(), "UNION_OF");
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "332259d5-c51b-411f-963d-7c24e2b029d5");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "eb8a5d75-eafb-41cc-bdec-50ac73073588");
        checkProfileEntry(getProfile().COMPLEMENT_OF(), "COMPLEMENT_OF");
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "9c1e805d-6fd8-4290-85a2-6fa1e759b697");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "64c01977-5da3-46b8-a1eb-e4722c1cbdbc");
        checkProfileEntry(getProfile().INTERSECTION_OF(), "INTERSECTION_OF");
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "3d27ebc7-0ecf-448a-98ec-f8bf845c82ca");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "bb249933-241d-402d-b00f-a5ead4c4dca3");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "33ae7381-b96d-43c3-a962-05e3f1a66eb9");
        checkProfileEntry(getProfile().RESTRICTION(), "RESTRICTION");
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "60849534-40d8-41ee-a60b-9e42362a4dce");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "0db4e754-4269-4a83-aad1-b9da26667228");
        checkProfileEntry(getProfile().ALL_DIFFERENT(), "ALL_DIFFERENT");
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "bdb3867d-ee7a-4ccd-a0a5-ab038c5f54a0");
        return findByTypeAs(getProfile().ALL_DIFFERENT(), AllDifferent.class).filterKeep(new UniqueFilter<AllDifferent>());
    }

    /**
     * <p>Answer an iterator over the DataRange objects in this ontology, if there
     * are any.</p>
     * @return An iterator, whose values are {@link DataRange} objects.
     */
    @Override
    public ExtendedIterator<DataRange> listDataRanges() {
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "476006b3-37f9-4e6c-a249-94222e787852");
        checkProfileEntry(getProfile().DATARANGE(), "DATARANGE");
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "a404d540-9dd2-45d6-8857-9162f7980801");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "d5ead40e-a205-43b2-bd99-2ffa61ef2a48");
        checkProfileEntry(getProfile().ANNOTATION_PROPERTY(), "ANNOTATION_PROPERTY");
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "3ad8d6bc-2158-4a82-b371-2781640c2574");
        Resource r = getProfile().ANNOTATION_PROPERTY();
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "ea74ab0e-bd6c-4dbc-b4ac-5a5e07b53c27");
        if (r == null) {
            writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "85f58b9a-9b6e-498a-93e6-bde194f1c9e8");
            return new NullIterator<AnnotationProperty>();
        } else {
            writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "394484d8-096a-4b26-aee2-ad7f54a287dc");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "10e6c05d-0abd-45f6-996a-84df675f30df");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "da6c2d19-71cf-4fe0-a9b1-6f4f532df42c");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "afab6914-be74-41d1-b6f3-b961b1174c87");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "f7148ed4-17e0-43ee-a136-ae41e271cafd");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "7332f217-50e5-45c5-a33b-f6d2c426f297");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "1bc7d8bd-5fa1-4a08-8d0c-337f5b2e8227");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "84edfcfe-0e8c-4891-83f2-a6de626a3fd6");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "b84f7dad-41fb-4b00-928a-1a019325444d");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "e6d0773e-dc99-4fe9-bdde-4e22244f4350");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "5ccf9ec0-4fac-4fb2-9e2f-f7c1f15ce316");
        OntClass c = (OntClass) findByURIAs(uri, OntClass.class);
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "05abf084-84b2-413a-be12-58c9f902aa20");
        // special case for nothing and thing
        if (c == null) {
            writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "5877fbe8-0058-457f-9eb5-3d4ee170a6f5");
            Resource thing = getProfile().THING();
            writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "4c0f204b-27d4-4f48-9327-968571ddfe61");
            if (thing != null && thing.getURI().equals(uri)) {
                writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "526de539-8fd7-4860-9a68-d63a4e5ce899");
                c = thing.inModel(this).as(OntClass.class);
            }
            writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "1cc58d65-4874-4e3b-adeb-f6da1c793679");
            Resource nothing = getProfile().NOTHING();
            writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "286f5fe2-6316-4288-a715-5df8a6d46d07");
            if (nothing != null && nothing.getURI().equals(uri)) {
                writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "e30cec64-a924-4d1c-9075-87b01ef88c0d");
                c = nothing.inModel(this).as(OntClass.class);
            }
        }
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "fa29fde7-2ad5-4996-9c53-dfad68fab5e4");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "53957177-ae72-4a8f-9cb4-fdb4d75ca306");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "3c0f7ed4-f9d1-4ae9-becd-b6cf6363b4a5");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "26e07ce1-350b-41e6-8128-164b01854f8a");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "b98277ec-c6db-40c3-8f5a-46302d82b85d");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "d841ceaa-db33-49c7-9a0d-50b1903cb406");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "175682cf-a8b6-4b4a-8d46-f5d1247c114d");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "8ad13575-5674-46eb-a45c-ae9113783103");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "fb755a5c-78ce-422d-b024-867d8897d290");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "62e49912-9bb3-4277-8a9f-6ccfe2f5da87");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "81bcdcc0-a2d5-4152-b10b-44cd5d51326a");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "fe231ef0-804a-46fe-81be-703159045b52");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "8b335af0-9a68-4f6e-98e2-4e35a2448242");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "a084fccf-3e93-45d4-b358-be58a5fd1e00");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "60a7d353-7166-47b6-a578-c325f4886b06");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "f423058d-b126-4789-b891-8741b969fe56");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "fd8b9473-7e06-458e-b551-7d4bc08f01b8");
        checkProfileEntry(getProfile().ONTOLOGY(), "ONTOLOGY");
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "f5a4bed8-b29c-4d6c-acab-04e8c8d166c3");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "3c50b677-bce0-4b3b-98ac-5a38d6745229");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "de2c7c17-27f4-477e-8341-3e827b9c6423");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "f0afc160-fe5d-4118-ae8e-b8fc8d5d901f");
        Property p = createProperty(uri);
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "19e85c47-3a43-4a81-898f-096c1da5463f");
        p.addProperty(RDF.type, getProfile().PROPERTY());
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "6e91805e-3f41-4f5f-87d4-3d5966b4da2c");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "f51a983b-4e0c-4d47-854d-74966a0524fd");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "2d42f0c0-3354-4d0c-89e7-d9cdf17afac3");
        checkProfileEntry(getProfile().OBJECT_PROPERTY(), "OBJECT_PROPERTY");
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "4209d623-c281-473a-8749-16694958dcc0");
        ObjectProperty p = createOntResource(ObjectProperty.class, getProfile().OBJECT_PROPERTY(), uri);
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "34d4e17a-ca03-4e0b-b01f-22a766cfe05a");
        if (functional) {
            writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "ac446508-bd59-47d9-a5b0-9f39476e0309");
            checkProfileEntry(getProfile().FUNCTIONAL_PROPERTY(), "FUNCTIONAL_PROPERTY");
            writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "2e7cfb00-ecc5-4d97-9337-e4132a5022b4");
            p.addProperty(RDF.type, getProfile().FUNCTIONAL_PROPERTY());
        }
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "f179fae5-364b-48b3-8caf-835e410676b5");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "ae8acfa3-05b3-4903-ad5d-e9e9f8d31c70");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "e47afe2e-775f-43e2-942b-c5e9c00a4845");
        checkProfileEntry(getProfile().TRANSITIVE_PROPERTY(), "TRANSITIVE_PROPERTY");
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "4a069256-c0a4-449c-87d0-222a2f20bee2");
        TransitiveProperty p = createOntResource(TransitiveProperty.class, getProfile().TRANSITIVE_PROPERTY(), uri);
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "fae555b9-5221-46e4-8de0-e88202af14ba");
        if (functional) {
            writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "10efc7a8-4d78-4f08-9b58-fbd374ab3ab3");
            checkProfileEntry(getProfile().FUNCTIONAL_PROPERTY(), "FUNCTIONAL_PROPERTY");
            writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "c9289451-8e7a-4f43-be4f-4cfaa45ab130");
            p.addProperty(RDF.type, getProfile().FUNCTIONAL_PROPERTY());
        }
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "035dc577-984a-4ce7-8c67-0cc4f88bfea0");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "467dd116-d4bd-49a7-a586-3c4728ab6397");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "5cbe424f-a15b-431e-978a-324f95d06b03");
        checkProfileEntry(getProfile().SYMMETRIC_PROPERTY(), "SYMMETRIC_PROPERTY");
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "b93ed571-f736-47f9-a515-f707b83d0de4");
        SymmetricProperty p = createOntResource(SymmetricProperty.class, getProfile().SYMMETRIC_PROPERTY(), uri);
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "ed9ad596-e075-4c05-832e-76c484e5679b");
        if (functional) {
            writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "e5d26910-0b11-48da-a8cd-ac50e68342c0");
            checkProfileEntry(getProfile().FUNCTIONAL_PROPERTY(), "FUNCTIONAL_PROPERTY");
            writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "acac27e4-e703-4fec-ba2d-b0fd87f31435");
            p.addProperty(RDF.type, getProfile().FUNCTIONAL_PROPERTY());
        }
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "575af4b8-6f36-405f-8494-2164a00fd8fa");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "1d9bd427-f0f4-43ba-a980-3ad85dc483ff");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "c95a6b1d-bed4-417d-8f14-b4f9134105bb");
        checkProfileEntry(getProfile().INVERSE_FUNCTIONAL_PROPERTY(), "INVERSE_FUNCTIONAL_PROPERTY");
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "7f856073-1df3-4207-97db-48a09994edd0");
        InverseFunctionalProperty p = createOntResource(InverseFunctionalProperty.class, getProfile().INVERSE_FUNCTIONAL_PROPERTY(), uri);
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "86cb80b0-57ef-4d83-87f9-582ad66994d9");
        if (functional) {
            writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "5eb9e169-e937-40fd-a441-0a5150457fda");
            checkProfileEntry(getProfile().FUNCTIONAL_PROPERTY(), "FUNCTIONAL_PROPERTY");
            writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "1947cf8c-c803-4e43-af8f-98079b1f2a6b");
            p.addProperty(RDF.type, getProfile().FUNCTIONAL_PROPERTY());
        }
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "10ea06e9-56c9-4c3c-b54f-bb10c89787fa");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "624cd752-c4b7-4f49-a6cd-7366b99a9c1b");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "7b7b313a-e5d8-4c31-81c9-f052baab39c9");
        checkProfileEntry(getProfile().DATATYPE_PROPERTY(), "DATATYPE_PROPERTY");
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "a4dbdf77-ce7f-474f-9eac-14c4f54585f4");
        DatatypeProperty p = createOntResource(DatatypeProperty.class, getProfile().DATATYPE_PROPERTY(), uri);
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "4574aead-f757-482c-be82-38c5fa7fc13c");
        if (functional) {
            writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "1c751bb3-0caa-4b9c-81b2-3b9d50cbcb7c");
            checkProfileEntry(getProfile().FUNCTIONAL_PROPERTY(), "FUNCTIONAL_PROPERTY");
            writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "80ed1f7e-9c75-4508-82a9-7f5a2c8d786e");
            p.addProperty(RDF.type, getProfile().FUNCTIONAL_PROPERTY());
        }
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "19bbfab4-96b7-472e-a866-c542c085cad7");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "11d0f9f8-cb27-47c2-ac79-4ff98d5d5b35");
        checkProfileEntry(getProfile().ANNOTATION_PROPERTY(), "ANNOTATION_PROPERTY");
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "cb862eed-5b45-4442-8648-71d2c32f4f80");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "c196fee8-1397-42f4-9c42-d68ab141c933");
        checkProfileEntry(getProfile().CLASS(), "CLASS");
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "a00154cf-2f85-46fa-b166-bdabe9ee9620");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "c269d20f-fb57-472d-87c4-c6703f34274e");
        checkProfileEntry(getProfile().CLASS(), "CLASS");
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "50d59e84-48b5-470b-bfa4-80865bf3b291");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "a8105c33-24d6-4874-82f3-c5bfc94bbd7c");
        checkProfileEntry(getProfile().CLASS(), "CLASS");
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "f8f67053-440d-43b0-ad97-a9009cc06ab5");
        OntClass c = createOntResource(OntClass.class, getProfile().CLASS(), uri);
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "c0357075-ff9d-430c-be83-1590d3b42dd8");
        checkProfileEntry(getProfile().COMPLEMENT_OF(), "COMPLEMENT_OF");
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "b137f21e-dc67-4fb7-920b-60512f1e8bbd");
        // if the class that this class is a complement of is not specified, use owl:nothing or daml:nothing
        c.addProperty(getProfile().COMPLEMENT_OF(), (cls == null) ? getProfile().NOTHING() : cls);
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "7ee106cc-ac8f-461c-badc-170752d7c850");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "dfb89803-8660-4a52-a4bd-6d6df1548d1b");
        checkProfileEntry(getProfile().CLASS(), "CLASS");
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "290fba99-cc69-412d-987d-de0a52f29da5");
        OntClass c = createOntResource(OntClass.class, getProfile().CLASS(), uri);
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "78c4463f-11df-43c4-9ce2-8e5e00eee16c");
        checkProfileEntry(getProfile().ONE_OF(), "ONE_OF");
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "b09cb910-0bf4-46df-930d-ef41252df9c4");
        c.addProperty(getProfile().ONE_OF(), (members == null) ? createList() : members);
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "ea9ea179-d13c-439d-b8a7-af4dea14f2c7");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "2ab63607-853a-4aef-9927-54f1e217522f");
        checkProfileEntry(getProfile().CLASS(), "CLASS");
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "dc22af15-c999-45d6-a16a-7082636d438d");
        OntClass c = createOntResource(OntClass.class, getProfile().CLASS(), uri);
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "182f32ff-7470-43fb-a343-3eeb149c5528");
        checkProfileEntry(getProfile().UNION_OF(), "UNION_OF");
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "73bb107d-8eb2-415c-b708-f0b6aa73ee34");
        c.addProperty(getProfile().UNION_OF(), (members == null) ? createList() : members);
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "4643b532-3d0e-4929-8b5d-46362b8a6641");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "88c006f7-be3a-4315-a8bf-d48954984c64");
        checkProfileEntry(getProfile().CLASS(), "CLASS");
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "d5b06300-a168-43e0-b0cd-a40bc72ebb80");
        OntClass c = createOntResource(OntClass.class, getProfile().CLASS(), uri);
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "b13b159e-22db-43de-9112-cea1f5a705d7");
        checkProfileEntry(getProfile().INTERSECTION_OF(), "INTERSECTION_OF");
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "46b8e3ca-3964-410f-b7ea-789690259791");
        c.addProperty(getProfile().INTERSECTION_OF(), (members == null) ? createList() : members);
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "c6cbe9be-017f-4fda-ba46-e198acdec163");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "44166d89-b44c-449e-8a53-d1795e1bfbae");
        checkProfileEntry(getProfile().RESTRICTION(), "RESTRICTION");
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "cbe73e82-a3e4-49f3-a467-9a09c6ba8ef7");
        Restriction r = createOntResource(Restriction.class, getProfile().RESTRICTION(), null);
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "2d065f7c-f193-43fe-95eb-c83885547fca");
        if (p != null) {
            writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "6d64b14f-8166-4506-8d2f-cdde8329a94c");
            r.setOnProperty(p);
        }
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "29e7b767-a2d0-492e-b74f-3a792cacfe68");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "fc3dd264-123e-4ff3-8fab-954129b16a74");
        checkProfileEntry(getProfile().RESTRICTION(), "RESTRICTION");
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "412751a7-6af5-41ca-9dd8-3ce67e0ca994");
        Restriction r = createOntResource(Restriction.class, getProfile().RESTRICTION(), uri);
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "a350c427-e8c2-4037-8703-0a1b92b92a1b");
        if (p != null) {
            writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "18d93096-40cc-4049-a6e8-73ab44629398");
            r.setOnProperty(p);
        }
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "ece9d587-80dc-4598-887d-f24433e2b66d");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "0c553bf9-25f8-4604-b583-97fa08c62207");
        checkProfileEntry(getProfile().RESTRICTION(), "RESTRICTION");
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "708f807a-871b-434e-88ff-e61c0fd9e8fc");
        Restriction r = createOntResource(Restriction.class, getProfile().RESTRICTION(), uri);
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "98bcd126-4450-47f9-b636-afc9e835f4b7");
        if (prop == null || value == null) {
            writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "be60391a-e332-4883-98f9-37dc97fb2758");
            throw new IllegalArgumentException("Cannot create hasValueRestriction with a null property or value");
        }
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "5dbc6693-363f-452c-ae36-81838be2961f");
        checkProfileEntry(getProfile().HAS_VALUE(), "HAS_VALUE");
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "63969856-e6dd-4769-93c1-5e2483cc086e");
        r.addProperty(getProfile().ON_PROPERTY(), prop);
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "ae8bbaa8-f448-4dae-ba4d-d22bd72b59ff");
        r.addProperty(getProfile().HAS_VALUE(), value);
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "3750b321-264f-45a7-9fba-f227f10a0c41");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "9f7a3a1b-667a-4643-af37-9b142634ec50");
        checkProfileEntry(getProfile().RESTRICTION(), "RESTRICTION");
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "a5a89bfb-1731-4e6e-a943-8b8043e0d481");
        Restriction r = createOntResource(Restriction.class, getProfile().RESTRICTION(), uri);
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "3e8a3fbe-4795-4906-9d3c-bc1a6520b779");
        if (prop == null || cls == null) {
            writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "d44c9e31-6681-418b-9993-5283baa58456");
            throw new IllegalArgumentException("Cannot create someValuesFromRestriction with a null property or class");
        }
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "9276ed21-fb70-4edf-b5df-862fd8f21bd2");
        checkProfileEntry(getProfile().SOME_VALUES_FROM(), "SOME_VALUES_FROM");
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "440307a3-360c-40e5-8dd5-31e2d0d113c3");
        r.addProperty(getProfile().ON_PROPERTY(), prop);
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "9175492a-05f1-4d39-8d20-150009cbb5b8");
        r.addProperty(getProfile().SOME_VALUES_FROM(), cls);
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "9d687a58-9417-4819-9174-537c224a5b3b");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "3434863d-975e-41a0-b74d-b44d2c695b7d");
        checkProfileEntry(getProfile().RESTRICTION(), "RESTRICTION");
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "6a65698f-1752-4761-9028-357366b8be92");
        Restriction r = createOntResource(Restriction.class, getProfile().RESTRICTION(), uri);
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "2ee5d08a-c789-4794-bc4c-caa9726b95be");
        if (prop == null || cls == null) {
            writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "9e4aa0b8-89eb-4e34-8577-d828bcaa088b");
            throw new IllegalArgumentException("Cannot create allValuesFromRestriction with a null property or class");
        }
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "cdfa98ff-23fd-42d5-88d4-0274a8d058d1");
        checkProfileEntry(getProfile().ALL_VALUES_FROM(), "ALL_VALUES_FROM");
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "d379f595-6896-436f-8acb-e92cb810497e");
        r.addProperty(getProfile().ON_PROPERTY(), prop);
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "c59eaee3-068f-4f3d-ac9a-4eca6fce78a3");
        r.addProperty(getProfile().ALL_VALUES_FROM(), cls);
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "b0978191-9740-42cb-969e-cf2c36938c6d");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "acb55317-8d90-4fdf-8505-82f17f2aba08");
        checkProfileEntry(getProfile().RESTRICTION(), "RESTRICTION");
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "a5e09d07-bf95-4526-974b-5303a73b4576");
        Restriction r = createOntResource(Restriction.class, getProfile().RESTRICTION(), uri);
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "2c9f654a-dcb1-4a41-bcf9-f1d3c051a29d");
        if (prop == null) {
            writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "30ffec62-e452-44c8-8e04-8d1de338e8e2");
            throw new IllegalArgumentException("Cannot create cardinalityRestriction with a null property");
        }
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "473cf522-5c37-4f50-8617-52ad8798c402");
        checkProfileEntry(getProfile().CARDINALITY(), "CARDINALITY");
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "cc31a6c2-afb4-417c-aaf1-2c46459eaef9");
        r.addProperty(getProfile().ON_PROPERTY(), prop);
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "7f4c9cae-1cae-4e77-b2d3-31213c53e5f2");
        r.addProperty(getProfile().CARDINALITY(), createTypedLiteral(cardinality));
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "9b671fa6-4f98-4d49-ae7e-e85f108f981b");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "efbbfd6c-27e9-4410-b79f-b7198e96e7e8");
        checkProfileEntry(getProfile().RESTRICTION(), "RESTRICTION");
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "02215fd4-70b9-4e5c-acdd-7b4516f10fc4");
        Restriction r = createOntResource(Restriction.class, getProfile().RESTRICTION(), uri);
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "00e871c0-e558-4c2d-9874-0b9528212c1d");
        if (prop == null) {
            writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "d297c611-c22e-4696-81ae-9e8382c6fa3c");
            throw new IllegalArgumentException("Cannot create minCardinalityRestriction with a null property");
        }
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "920caf28-5b2f-42de-9872-93233bc8abad");
        checkProfileEntry(getProfile().MIN_CARDINALITY(), "MIN_CARDINALITY");
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "1508e941-ba22-41bc-9cb8-d4c9ccbbd50f");
        r.addProperty(getProfile().ON_PROPERTY(), prop);
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "c98ddbb2-3c61-4a34-8730-e15a37487841");
        r.addProperty(getProfile().MIN_CARDINALITY(), createTypedLiteral(cardinality));
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "3ecb2d61-8f2d-44bb-9135-54c8526c105a");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "740ca0ee-1450-4d2c-a08f-9c36a2a033cf");
        checkProfileEntry(getProfile().RESTRICTION(), "RESTRICTION");
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "599bf08a-188c-4c8a-b6f8-eebf4d0fd532");
        Restriction r = createOntResource(Restriction.class, getProfile().RESTRICTION(), uri);
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "5806a087-35f8-4c8e-b9b6-39b4cd861a6c");
        if (prop == null) {
            writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "87852379-426c-47b0-baf9-8e07bffe244d");
            throw new IllegalArgumentException("Cannot create maxCardinalityRestriction with a null property");
        }
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "fc42a4ae-1de0-436c-b006-89e1817103a9");
        checkProfileEntry(getProfile().MAX_CARDINALITY(), "MAX_CARDINALITY");
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "3bb782df-e03c-498d-859a-48779d2541d2");
        r.addProperty(getProfile().ON_PROPERTY(), prop);
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "7039550d-19d5-4ed6-8a39-6f87da5f9b5e");
        r.addProperty(getProfile().MAX_CARDINALITY(), createTypedLiteral(cardinality));
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "f055e6ba-a8bc-4c81-92e1-898202dd1061");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "6f0e816b-e28a-4289-a7a0-7ff0a5cb0ba6");
        checkProfileEntry(getProfile().RESTRICTION(), "RESTRICTION");
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "fec391d4-eb5d-41e3-829f-65aa0ef1abda");
        checkProfileEntry(getProfile().ON_PROPERTY(), "ON_PROPERTY");
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "e49e5244-a04f-4097-b5e2-99b2c6d65171");
        checkProfileEntry(getProfile().MAX_CARDINALITY_Q(), "MAX_CARDINALITY_Q");
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "74e42315-7ef9-46b3-88d3-958dbfe8e831");
        checkProfileEntry(getProfile().HAS_CLASS_Q(), "HAS_CLASS_Q");
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "215fa2d5-0c84-4692-959b-2f9bcb53a42d");
        if (prop == null) {
            writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "d3a98339-4503-41e1-bec1-525d857aaa33");
            throw new IllegalArgumentException("Cannot create MaxCardinalityQRestriction with a null property");
        }
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "66bf802f-d5ad-4c8f-a9a3-8e22c1f1b739");
        if (cls == null) {
            writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "d51cc914-28de-470d-b46e-30a91fc097e4");
            throw new IllegalArgumentException("Cannot create MaxCardinalityQRestriction with a null class");
        }
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "a14dd665-d7e0-4039-8e58-5d4a12e19f87");
        Restriction r = createOntResource(Restriction.class, getProfile().RESTRICTION(), uri);
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "0b5fad04-4eba-4ff1-8d2d-90df6e09f64c");
        r.addProperty(getProfile().ON_PROPERTY(), prop);
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "013b50a5-5558-4e19-aaa2-39493eba5a2f");
        r.addProperty(getProfile().MAX_CARDINALITY_Q(), createTypedLiteral(cardinality));
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "22a8bdf2-c690-4b92-9932-80fc8e7dd9a7");
        r.addProperty(getProfile().HAS_CLASS_Q(), cls);
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "c39ba161-f51d-4577-8dc0-a163d2eb1f07");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "7f92e0a1-f725-44bf-81b5-e893426bab57");
        checkProfileEntry(getProfile().RESTRICTION(), "RESTRICTION");
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "5402a9cc-b608-497c-9a7b-3b69bcf01871");
        checkProfileEntry(getProfile().ON_PROPERTY(), "ON_PROPERTY");
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "1c4fea09-36ed-46f7-9ee4-f9f31b84980c");
        checkProfileEntry(getProfile().MIN_CARDINALITY_Q(), "MIN_CARDINALITY_Q");
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "2f6b0f96-9a18-4b3e-99a2-415df332d53e");
        checkProfileEntry(getProfile().HAS_CLASS_Q(), "HAS_CLASS_Q");
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "9bf800a5-f926-4d34-94a4-73912836310b");
        if (prop == null) {
            writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "c160ba36-5dc6-4258-9e68-94fc7e3af54a");
            throw new IllegalArgumentException("Cannot create MinCardinalityQRestriction with a null property");
        }
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "a6f76c47-ec02-4792-94c2-3688e46db64e");
        if (cls == null) {
            writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "9df26aec-a860-40dd-96b0-c69fc54579f7");
            throw new IllegalArgumentException("Cannot create MinCardinalityQRestriction with a null class");
        }
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "11cefdcf-b93b-4aec-97a7-90db70f4b452");
        Restriction r = createOntResource(Restriction.class, getProfile().RESTRICTION(), uri);
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "181e0efb-a9b5-4eaf-a00f-c1f165421a5a");
        r.addProperty(getProfile().ON_PROPERTY(), prop);
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "2423d02c-b86f-492a-bb34-b5bd4e74a4e2");
        r.addProperty(getProfile().MIN_CARDINALITY_Q(), createTypedLiteral(cardinality));
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "c0872219-6ff3-477f-ab91-3e3660089252");
        r.addProperty(getProfile().HAS_CLASS_Q(), cls);
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "dd1794a9-e218-4ccf-af6c-7a2a55489d0e");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "4b16f78b-a79c-4313-8b9a-1ad215070636");
        checkProfileEntry(getProfile().RESTRICTION(), "RESTRICTION");
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "704bd002-28f8-4cd3-a5af-ff880df3183c");
        checkProfileEntry(getProfile().ON_PROPERTY(), "ON_PROPERTY");
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "d6695b0d-f0db-408d-a470-766e780e0e16");
        checkProfileEntry(getProfile().CARDINALITY_Q(), "CARDINALITY_Q");
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "9a080fb7-31f9-49c5-8df7-a91c0525f745");
        checkProfileEntry(getProfile().HAS_CLASS_Q(), "HAS_CLASS_Q");
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "6ec33c8a-b20a-448d-957d-d8b0a03bb571");
        if (prop == null) {
            writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "fac6e59c-0203-453e-8fd8-24188b749af2");
            throw new IllegalArgumentException("Cannot create CardinalityQRestriction with a null property");
        }
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "460645aa-cccf-49d5-94ff-842f03dc4a44");
        if (cls == null) {
            writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "d4b4e9ef-846c-4fe4-957c-c7c6cfaa8d61");
            throw new IllegalArgumentException("Cannot create CardinalityQRestriction with a null class");
        }
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "079e684f-c60e-47f4-ac00-23f2a79baddf");
        Restriction r = createOntResource(Restriction.class, getProfile().RESTRICTION(), uri);
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "95bf0862-676c-4e6f-a3d9-c1e980679bc9");
        r.addProperty(getProfile().ON_PROPERTY(), prop);
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "aba49bf8-17ef-4f84-8404-9f4df0ca9ef6");
        r.addProperty(getProfile().CARDINALITY_Q(), createTypedLiteral(cardinality));
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "b8a424f1-d175-443c-ab66-e63a41d213bc");
        r.addProperty(getProfile().HAS_CLASS_Q(), cls);
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "0e22ef1c-9eb5-4d39-b28a-dd9caa493412");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "77068015-6f95-465d-8d5b-86dfeea92d23");
        checkProfileEntry(getProfile().DATARANGE(), "DATARANGE");
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "d955cdb2-24fd-468c-a964-75c85406a005");
        DataRange d = createOntResource(DataRange.class, getProfile().DATARANGE(), null);
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "3caf8c28-0f2a-4741-839c-5f83fdf972b7");
        checkProfileEntry(getProfile().ONE_OF(), "ONE_OF");
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "29768886-3713-4323-ae2a-da19a1ff560c");
        d.addProperty(getProfile().ONE_OF(), (literals == null) ? createList() : literals);
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "2b18e77c-2851-4414-b148-a8cda698c235");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "16926de0-4c2a-4180-a017-57d5a7896231");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "23f48ba8-5538-45b5-95f0-8af84d5717ea");
        checkProfileEntry(getProfile().ALL_DIFFERENT(), "ALL_DIFFERENT");
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "5b6ad2d1-5698-4eff-a810-bcbcb7e44ce8");
        AllDifferent ad = createOntResource(AllDifferent.class, getProfile().ALL_DIFFERENT(), null);
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "25deca93-455e-46df-8dd6-fecd8199c3c8");
        ad.setDistinctMembers((differentMembers == null) ? createList() : differentMembers);
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "e78f6e23-130b-422a-8b07-48bdc75c1b5c");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "f65045f5-cfc5-49d1-b784-c5685173ea6c");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "7be15f71-bffc-44db-8d1c-40a5dc1e2d15");
        return getResource(uri).as(OntResource.class);
    }

    /**
     * <p>Answer a new empty list.  This method overrides the list create method in ModelCom,
     * to allow both DAML and RDFS lists to be created.</p>
     * @return An RDF-encoded list of no elements, using the current language profile
     */
    @Override
    public RDFList createList() {
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "4a8240c4-536f-4cab-90f6-c98ff03265f2");
        Resource list = getResource(getProfile().NIL().getURI());
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "66a7ed87-b505-4b4c-bcb3-2c5d3987bfd2");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "60042b2f-7cb0-47db-8415-b92339e2546e");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "aadbd1d8-7a91-47b9-8e24-1c6660b4868a");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "3fd3c5ff-2755-4b16-977d-f3828eaca8e4");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "1b90a1f6-061c-4cd6-9133-b3fb234ee3d1");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "3dc03940-5d2b-4c16-9a8c-c4b0f978a68e");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "c711d07b-e1c9-488f-9526-8d6d775238b6");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "3373dcdd-2257-434a-beeb-bca546eace66");
        Set<String> results = new HashSet<String>();
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "6827f7c5-9bb7-43a8-ae84-beb8499f766f");
        List<Model> queue = new ArrayList<Model>();
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "160ab8e7-b898-4437-a71e-2f711926bbfc");
        queue.add(getBaseModel());
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "279a40e0-6c46-43bc-ba3c-3aa019be1613");
        while (!queue.isEmpty()) {
            writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "9b851130-77ed-4330-af65-df8a8e18ac44");
            Model m = queue.remove(0);
            writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "5778eb49-e916-4c95-a916-4a82685c0353");
            // list the ontology nodes
            if (getProfile().ONTOLOGY() != null && getProfile().IMPORTS() != null) {
                writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "9669c9b9-0bb4-4621-bbd3-55c306a49ee3");
                StmtIterator i = m.listStatements(null, getProfile().IMPORTS(), (RDFNode) null);
                writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "e37b5a87-a669-4d42-a832-d38b799c6c0c");
                while (i.hasNext()) {
                    writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "2a0f0e40-ae9f-4b29-9b05-e6a32916605c");
                    Statement s = i.nextStatement();
                    writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "bb7b89ca-f5cf-478c-bef7-883d3c8852e7");
                    String uri = s.getResource().getURI();
                    writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "5200952f-d368-4e13-8ca3-464eaaac74c1");
                    if (!results.contains(uri)) {
                        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "fdd47b52-7792-4950-8a74-63a59e0061b2");
                        // this is a new uri, so we add it
                        results.add(uri);
                        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "2e308e83-7d43-4ebf-8e47-e159cef02ef1");
                        // and push the model on the stack if we know it
                        Model mi = getDocumentManager().getModel(uri);
                        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "83ac959f-9ac4-41d2-8606-ddfabfa2bd83");
                        if (closure && mi != null && !queue.contains(mi)) {
                            writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "5b849dd7-0dca-4413-86cd-a2c8fac8e071");
                            queue.add(mi);
                        }
                    }
                }
            }
        }
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "e76c8028-6413-4ed6-a612-21cd1489d09a");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "dcd146c3-3603-430a-8a99-dd4c7c0d85a6");
        return m_spec.getImportModelMaker();
    }

    /**
     * @deprecated use getImportModelMaker instead.
     */
    @Override
    @Deprecated
    public ModelMaker getModelMaker() {
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "4287b5c4-a5b6-46fb-9dfa-49cd698fdf32");
        return getImportModelMaker();
    }

    /**
     * <p>Read statements into the model from the given source, and then load
     * imported ontologies (according to the document manager policy).</p>
     * @param uri URI to read from, may be mapped to a local source by the document manager
     */
    @Override
    public Model read(String uri) {
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "f201d008-05fe-4592-9041-00d0490b50a6");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "44aa86c8-5ee6-4aae-a98d-14cc316539a6");
        super.read(reader, base);
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "01e6c43d-2f38-4d8a-92ac-a0913e3b8cd2");
        loadImports();
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "7ac1fa18-c438-43fd-b614-1e8aab01de3c");
        rebind();
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "3eb2da13-2b93-4a8c-a4ca-9ae1ad411615");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "1be96f5a-2e08-4b69-82ef-444e4c577df6");
        super.read(reader, base);
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "93a74b3f-f261-4c4f-95f1-e2e719a9731a");
        loadImports();
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "34343a4f-14ca-4fed-89d4-c92c3a546369");
        rebind();
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "b27f7ddc-dabb-47c9-8647-a7160f6babda");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "1357a99a-6c0c-42cf-84a6-974cfb190357");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "80a7587a-1664-4937-895e-8ebea9cd680f");
        // we don't want to load this document again if imported by one of the imports
        addLoadedImport(uri);
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "8bb7a598-c4e8-474b-9f2c-3bcf2e261783");
        OntDocumentManager odm = getDocumentManager();
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "52efddb7-4cb9-4d3a-9ab0-b2168acca302");
        String sourceURL = odm.doAltURLMapping(uri);
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "c226d0e5-42e1-4877-9760-375e9454d561");
        // invoke the read hook from the ODM
        String source = odm.getReadHook().beforeRead(this, sourceURL, odm);
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "a698118d-b1e8-4d8d-be4c-92eb4e72b5b3");
        if (source == null) {
            writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "20816993-295f-4f95-9ba6-6c8082024fcc");
            s_log.warn("ReadHook returned null, so skipping assuming previous value: " + sourceURL);
            writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "fa78e93d-01d7-431e-b6ca-4844cf759afd");
            source = sourceURL;
        } else {
            writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "c2d65d7f-7ee4-436c-a8ba-9d310daa4de3");
            // now we can actually do the read, check first if we should use negotiation
            if (// require non-null base
            base == null && // and that negotiation makes sense (don't conneg to file:)
            !ignoreFileURI(source) && // and that we haven't remapped the URI
            source.equals(uri)) {
                writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "92e30f1c-032b-43a5-8a0b-b1b163a4f4bc");
                if (syntax == null) {
                    writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "b310bf10-427f-4bf9-a103-a72bfe1d3614");
                    readDelegate(source);
                } else {
                    writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "d3e0e7e5-021f-438c-a16a-05cd54c8fce5");
                    readDelegate(source, syntax);
                }
            } else {
                writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "826c3e51-e847-495a-a695-cb9667a837f5");
                // if we were given the base, use it ... otherwise default to the base being the source
                readDelegate(source, (base == null ? uri : base), syntax);
            }
        }
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "f1d78570-b115-4684-a39d-5e5291ba9e43");
        // the post read hook
        odm.getReadHook().afterRead(this, source, odm);
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "b8a4c79f-5929-48ad-9291-ca05bb1b47d1");
        // cache this model against the public uri (if caching enabled)
        getDocumentManager().addModel(uri, this);
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "1fed8d2f-c369-4dd4-8461-efff8fbbfed5");
        loadImports();
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "e50ec600-2d29-4c59-a9d8-93db51b72d0f");
        rebind();
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "4a076aa2-9858-4d9d-8235-45e6a64535f2");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "d81c15ba-8c39-4664-a3c7-a1faa74e5982");
        super.read(reader, base, syntax);
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "a920ad65-6ec1-4244-85bd-4762a407ecf1");
        loadImports();
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "9a5f335d-596c-488f-854f-b136b32527ee");
        rebind();
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "524d886c-9e02-471c-9152-0862586a449d");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "77f8b712-7dc1-4632-b3e9-bd6c8bc1c594");
        super.read(reader, base, syntax);
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "591161bb-c2a0-46a9-9f27-70bebe4741b9");
        loadImports();
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "d4ecfb9d-fa65-4b11-a93b-5edb897eb988");
        rebind();
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "acef39aa-3946-4767-b96c-d88c11e71e99");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "4ccde0f1-c546-47f0-948d-a559fc6e13ff");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "8395d376-f446-4a38-8c5b-af29c428255a");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "c8fb9755-6e36-48df-8188-751ffac41b5a");
        ExtendedIterator<Graph> i = WrappedIterator.create(getSubGraphs().iterator());
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "b07d6ac7-0430-49c6-a00a-389a9171e3c3");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "d5deaaf1-047b-47c0-8677-fd3d8073e6c9");
        return listSubModels(false);
    }

    /**
     * <p>Answer the number of sub-models of this model, not including the
     * base model.</p>
     * @return The number of sub-models, &ge; zero.
     */
    @Override
    public int countSubModels() {
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "88012a67-9ab1-4020-bafb-6cb49968447b");
        int count = 0;
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "8a82e0c6-52ff-4304-a8cd-2c861eaa677a");
        for (Graph graph1 : getSubGraphs()) {
            writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "e45fd0a4-ecc9-4e8f-ab16-a9bf4846df1b");
            count++;
        }
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "abd3b577-63de-4ef3-8767-324f23ceb8aa");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "ce42512a-8270-435d-9578-767abe534c9c");
        if (listImportedOntologyURIs(true).contains(uri)) {
            writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "926dbded-3621-425a-96a1-7051aa68cdbd");
            Model mi = getDocumentManager().getModel(uri);
            writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "acd3273f-5b3f-4f98-958b-547d0e8ad78a");
            if (mi != null) {
                writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "0a6801be-c358-445b-aa65-60479e8bcc1e");
                if (mi instanceof OntModel) {
                    writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "c94efa46-f188-4e62-8d97-82754bf54d04");
                    // already a suitable ont model
                    return (OntModel) mi;
                } else {
                    writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "c9856cce-20af-460c-ae70-ade84395f136");
                    // not in ont-model clothing yet, so re-wrap
                    return ModelFactory.createOntologyModel(m_spec, mi);
                }
            }
        }
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "4adda29b-7b76-4137-8317-1b30fbd8d1ce");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "84f08eee-2e91-46fd-a337-3fb485ed890b");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "6d8e594f-43dc-4cf4-abc4-544bc96ffe0c");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "ad726c99-22ad-45b3-94db-b2b2dfdcb889");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "83471bcb-ce42-4f70-ba0d-edabab597793");
        getUnionGraph().addGraph(model.getGraph());
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "b4872bf7-7fc2-4221-a4cc-668b0c2099e1");
        if (rebind) {
            writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "f7faf4db-6fc4-4962-a1c7-64a5bc0005d8");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "14c0468b-3bf0-4649-866d-a0137a2e1b77");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "1061ba7e-073b-4425-83b1-85fb4528a003");
        Graph subG = model.getGraph();
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "078f6315-af22-4860-b6a5-9b11e578f665");
        getUnionGraph().removeGraph(subG);
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "70fa8aaf-5425-4815-bc35-cac9e11cc8e2");
        // originally
        if (subG instanceof MultiUnion) {
            writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "e16dc4ec-11f9-489e-9932-d1c58ca521f6");
            // we need to get the base graph when removing a ontmodel
            getUnionGraph().removeGraph(((MultiUnion) subG).getBaseGraph());
        }
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "b305195f-ff3d-401d-bed1-68ba33716282");
        if (rebind) {
            writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "93117310-93d1-408a-9bec-704eab64af97");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "72473f40-6351-4bcd-9960-fc01887883c0");
        Node n = node.asNode();
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "bc66a43a-cba8-4f67-9b5a-d7f270f88bcc");
        Graph b = getBaseGraph();
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "34a8e360-6040-47aa-94cc-362483af3cbc");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "5a7eabf6-83b1-4cb2-94ca-e4a50c36d664");
        Node s = stmt.getSubject().asNode();
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "7574c383-b8b6-4d2c-b192-5601e56a2eae");
        Node p = stmt.getPredicate().asNode();
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "3c6da3d7-ca4f-40a7-994e-acdbd6755598");
        Node o = stmt.getObject().asNode();
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "a6bffc82-42c1-4b65-8a7d-af3b28697bec");
        Graph b = getBaseGraph();
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "774a0748-6c93-4bec-844e-f542ad22f3bf");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "22ebe196-8c3c-48d4-9a9a-476fc371237a");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "a7d4b4df-c647-4fe5-aa47-e311fd136fed");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "bbd36e28-4e93-4121-a175-93f88fbd9a22");
        if (dynamic) {
            writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "1212333c-ae2b-46c3-be70-74c3f59983f7");
            if (m_importsListener == null) {
                writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "b61e1f52-7d29-490c-9e16-937f05a20a65");
                // turn on dynamic processing
                m_importsListener = new ImportsListener();
                writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "cf0f20f2-4fd1-4439-a52f-7fc2f13f2ae1");
                register(m_importsListener);
            }
        } else {
            writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "0b898fe4-0606-4d48-8326-727db903476a");
            if (m_importsListener != null) {
                writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "486fe726-0870-4ed3-acc2-58712dcd18db");
                // turn off dynamic processing
                unregister(m_importsListener);
                writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "ffffa674-7b4f-4270-9893-41874e6181cb");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "5d3e2c71-2792-4ff4-9a98-e1df97307078");
        return m_importsListener != null;
    }

    /**
     * <p>Answer the ontology model specification that was used to construct this model</p>
     * @return An ont model spec instance.
     */
    @Override
    public OntModelSpec getSpecification() {
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "a9d92691-5394-442e-9dde-7350f562e912");
        return m_spec;
    }

    // output operations - delegate to base model
    @Override
    public Model write(Writer writer) {
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "938fed10-07dd-450b-91fd-04dcba234eec");
        return getBaseModel().write(writer);
    }

    @Override
    public Model write(Writer writer, String lang) {
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "a09c3007-8fc1-4995-b349-990495757b0e");
        return getBaseModel().write(writer, lang);
    }

    @Override
    public Model write(Writer writer, String lang, String base) {
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "3fdab408-c2bd-45eb-95e1-e9da262f2532");
        return getBaseModel().write(writer, lang, base);
    }

    @Override
    public Model write(OutputStream out) {
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "d9c36061-192d-4131-8b7d-dc006b0c5502");
        return getBaseModel().write(out);
    }

    @Override
    public Model write(OutputStream out, String lang) {
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "3bea4f51-5ce5-4525-8193-42a6ac1097e5");
        return getBaseModel().write(out, lang);
    }

    @Override
    public Model write(OutputStream out, String lang, String base) {
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "54d25626-309c-492f-b80d-23154baa1095");
        return getBaseModel().write(out, lang, base);
    }

    @Override
    public Model writeAll(Writer writer, String lang, String base) {
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "50941819-29c6-463a-a134-674e6dce8ce3");
        return super.write(writer, lang, base);
    }

    @Override
    public Model writeAll(OutputStream out, String lang, String base) {
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "be6d5a2c-c84e-440b-b88a-eddb3bc70f71");
        return super.write(out, lang, base);
    }

    @Override
    public Model writeAll(Writer writer, String lang) {
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "a94a8817-795d-44fe-b6b8-2ce264376c45");
        return super.write(writer, lang);
    }

    @Override
    public Model writeAll(OutputStream out, String lang) {
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "4719faed-de98-42ce-81d3-a1116efb6868");
        return super.write(out, lang);
    }

    // Implementation of inf model interface methods
    /**
     * Return the raw RDF model being processed (i.e. the argument
     * to the Reasonder.bind call that created this InfModel).
     */
    @Override
    public Model getRawModel() {
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "4b185dd9-babb-4ae0-8fbb-fa7e41b2fb6b");
        return getBaseModel();
    }

    /**
     * Return the Reasoner which is being used to answer queries to this graph.
     */
    @Override
    public Reasoner getReasoner() {
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "0374fe34-04e5-4ccf-88cf-ca311e44826d");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "ee0b66f6-2935-4382-95f2-fbf60c2a7147");
        if (getGraph() instanceof InfGraph) {
            writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "f898668e-d2ff-4965-b971-bf4178952046");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "8090a950-4b45-4283-9910-23b096f70754");
        if (getGraph() instanceof InfGraph) {
            writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "8f2b1fff-bd52-4c2b-937a-b41c5a049598");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "6f8a4900-f0f5-4830-8833-cc04ac121808");
        if (getGraph() instanceof InfGraph) {
            writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "01213ca7-ca43-4378-b691-5f0d6168d984");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "d9f315ed-5002-48ed-809c-6fea691a32f5");
        if (m_deductionsModel == null) {
            writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "9dc198bb-328e-4f7d-a35e-f88650627558");
            InfGraph infGraph = getInfGraph();
            writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "8040ec37-905e-4b5f-8404-e1c1095ba9bf");
            if (infGraph != null) {
                writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "6eed715c-4e64-4f15-af8f-dd610e4c3a60");
                Graph deductionsGraph = infGraph.getDeductionsGraph();
                writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "abc1a421-caeb-4a43-905f-e42b166b6164");
                if (deductionsGraph != null) {
                    writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "58980152-08a8-4c96-b25b-a5bc8ec7f42a");
                    m_deductionsModel = ModelFactory.createModelForGraph(deductionsGraph);
                }
            }
        } else {
            writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "1ebea818-92ae-4884-b15a-c9c5383d8d72");
            // ensure that the cached model sees the updated changes from the
            // underlying reasoner graph
            getInfGraph().prepare();
        }
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "b49e4612-d673-4728-ba27-353589dee465");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "4450aa39-e4b0-4156-8edb-aefb0aa5e63d");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "5d37ff82-5f94-49bf-bf64-6eb26c7e2bb9");
        if (getGraph() instanceof InfGraph) {
            writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "73c0ff40-a7e4-47f9-8b32-0dcc4231eb0e");
            Graph gp = posit == null ? ModelFactory.createDefaultModel().getGraph() : posit.getGraph();
            writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "322c1636-fa20-48d8-9042-b9d98bc21a08");
            Iterator<Triple> iter = getInfGraph().find(asNode(subject), asNode(predicate), asNode(object), gp);
            writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "5020d117-7fa4-4b7d-b1e8-49533a0edea5");
            return IteratorFactory.asStmtIterator(iter, this);
        } else {
            writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "c17c5d02-ed85-41ab-9fe0-2cb3f60bf51d");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "c8261f29-c671-43dd-b5ee-86c2bb739ea5");
        if (getGraph() instanceof InfGraph) {
            writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "aec9fcbb-a036-48d1-803a-d10ef590ba4a");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "e6f796b6-f1de-47c8-b4ad-c5d65ccf9108");
        return (getGraph() instanceof InfGraph) ? ((InfGraph) getGraph()).getDerivation(statement.asTriple()) : null;
    }

    // Internal implementation methods
    // ////////////////////////////////
    private static void initSyntaxCheckerClass() {
        writelineStatic("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "f4f2ffff-4e62-4617-8367-5de9b4e64326");
        if (owlSyntaxCheckerClass == null) {
            writelineStatic("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "204c04fa-5db2-42e6-82ab-2b4077529150");
            try {
                writelineStatic("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "500f8ffd-0616-4443-9137-7c024d4b22c1");
                owlSyntaxCheckerClass = Class.forName(owlSyntaxCheckerClassName);
                writelineStatic("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "979ad536-7d5a-4210-a957-5dbfd7539322");
                owlSyntaxCheckerClass.newInstance();
            } catch (Exception e) {
                writelineStatic("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "720cfb9d-a101-40cf-8122-783a25255960");
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
        writelineStatic("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "0295cac8-c07e-415d-a94d-cfe8afcf0e3e");
        // create a empty union graph
        MultiUnion u = new MultiUnion();
        writelineStatic("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "fa7bba87-0e17-4893-82f8-9608c09353a8");
        u.addGraph(base);
        writelineStatic("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "de9f33b0-3aaf-4c08-ae37-50d40ccd4b74");
        u.setBaseGraph(base);
        writelineStatic("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "dbc8de3f-e3dd-45c0-83b8-7380047e273c");
        Reasoner r = spec.getReasoner();
        writelineStatic("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "e4ac4aae-657f-48e0-b352-3783c6fcc576");
        // if we have a reasoner in the spec, bind to the union graph and return
        return r == null ? (Graph) u : r.bind(u);
    }

    /**
     * <p>Answer the union graph that contains the imports closure for this ontology</p>
     * @return The union graph
     */
    protected MultiUnion getUnionGraph() {
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "5564428d-4ad3-4144-88b7-5c0a26399669");
        return m_union;
    }

    /**
     * Answer the resource with the given URI, if present, as the given facet
     */
    protected <T extends Resource> Resource findByURIAs(String uri, Class<T> asKey) {
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "1b89c6f4-aca3-4656-8c83-8f6df7283429");
        if (uri == null) {
            writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "df20e616-8997-43d1-a70c-abf769a1d42b");
            throw new IllegalArgumentException("Cannot get() ontology value with a null URI");
        }
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "c6c8c47e-ccfa-4927-9e89-25636ebbcb95");
        Node n = NodeFactory.createURI(uri);
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "053a51d7-da1e-4830-bd37-f12748a4c14f");
        if (getGraph().contains(n, Node.ANY, Node.ANY)) {
            writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "476b9cfe-bda4-47b3-8ab9-4db453ab1c6b");
            // this resource is a subject in the graph
            try {
                writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "81bf4852-0168-4f08-9c17-85d72512d1c7");
                return getNodeAs(n, asKey);
            } catch (ConversionException ignore) {
            /**/
            }
        }
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "31776e64-8799-45a9-8fb2-613ad53180c5");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "9da0aba7-e790-4443-a23c-4587c5c69977");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "457d2cfb-01f3-42b6-a1e7-47b4a5a2bee2");
        ExtendedIterator<Triple> i = findByType(type);
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "2c11d83b-64b2-4f71-9836-e1b1898b26a0");
        // compose onto i the find iterators for the alternate types
        if (alternates != null) {
            writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "8fe940f2-cd6b-468c-b51f-62bcc6f48c0e");
            while (alternates.hasNext()) {
                writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "cb4fc086-3f1e-4291-94ec-12eff4a71f4d");
                i = i.andThen(findByType(alternates.next()));
            }
        }
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "390f3cfe-89aa-420a-bda1-044da81dba6e");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "9e011e7d-aa7b-440b-af91-e4740444139d");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "e2dcb0cd-ba08-4137-9e6c-ca8cb850d8c4");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "bd1bfe1d-f805-4acc-a2ed-cbf83f84580a");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "bef029b8-8476-4537-9e6a-ac5396487ef7");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "8816b820-53bd-4ef1-85bf-e187a6145a35");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "0b33ce6f-347d-44ca-982b-b3f4c3683781");
        Resource r = getResource(uri);
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "0182c624-db22-4093-9c8b-9c6d0986d26d");
        if (rdfType != null) {
            writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "4dd19ed8-75a6-47af-adbf-b5d49d0ff32d");
            r.addProperty(RDF.type, rdfType);
        }
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "ee978b7d-8a08-4e56-995e-dc87479bbc6c");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "93f3a4d2-c7e3-4c75-97a6-f6610773f8b0");
        Resource r = getResource(uri);
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "4ab37a2e-5f7b-465a-8db0-eb4f1ce2c6bc");
        if (containsResource(r)) {
            writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "18029286-c540-4622-b9ad-20cb85602495");
            return r.as(OntResource.class);
        }
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "35f352f9-a78f-48cf-a705-2724a6e2e3f4");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "e1c91219-6965-48a0-8fcb-b4206786cb45");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "6d87232d-4080-4918-a73d-bc9d658059cd");
        if (profileTerm == null) {
            writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "d3fea17c-6fb5-4883-bdcb-7df996b699fe");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "d5b07e64-6d80-4d6f-a9ab-f267bc9d9085");
        if (strictMode() && !((Boolean) list.reduce(new RdfTypeTestFn(rdfType), Boolean.TRUE)).booleanValue()) {
            writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "377ab32e-6803-45e2-9c21-ce726c45ada5");
            // not all of the members of the list are of the given type
            throw new LanguageConsistencyException("The members of the given list are expected to be of rdf:type " + rdfType.toString());
        }
    }

    /**
     * Answer the supplied model, unless it's null, in which case answer a new model
     * constructed as per spec.
     */
    private static Model makeBaseModel(OntModelSpec spec, Model model) {
        writelineStatic("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "c6b9c538-0f59-42eb-ae58-7b6af4db29be");
        return model == null ? spec.createBaseModel() : model;
    }

    /**
     * <p>Answer the InfGraph that this model is wrapping, or null if this ontology
     * model is not wrapping an inf graph.</p>
     * @return The model's graph as an InfGraph, or null
     */
    private InfGraph getInfGraph() {
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "8a4d3ff7-2d60-47a0-a601-1b01e85cf555");
        return (getGraph() instanceof InfGraph) ? ((InfGraph) getGraph()) : null;
    }

    /**
     * Test for whether we ignore <code>file:</code> URI's when testing for content
     * negotiation.
     * @param source
     * @return
     */
    protected boolean ignoreFileURI(String source) {
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "88731671-76a5-418c-af71-4532992a1ecf");
        return source.startsWith("file:");
    }

    /* delegation points to allow unit testing of read operations */
    protected Model readDelegate(String url) {
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "6b6403ae-e3e5-4cfa-9217-3d3a486e1871");
        return super.read(url);
    }

    protected Model readDelegate(String url, String lang) {
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "eab05271-ad45-4870-970a-4c691f223ab0");
        return super.read(url, lang);
    }

    protected Model readDelegate(String url, String base, String lang) {
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_7_10.coverage", "b8874b7f-bffe-4b8d-8eb4-d284a8a8ea8f");
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
