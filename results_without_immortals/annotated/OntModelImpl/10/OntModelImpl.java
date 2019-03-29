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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "4bcca71d-2d1c-417c-afe7-276f32a17571");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "0735609c-d557-450e-b4d1-f58caa3b7442");
        checkProfileEntry(getProfile().ONTOLOGY(), "ONTOLOGY");
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "4ab9cdf1-c568-49a8-9eba-879b622e0fec");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "2492d4fd-88d6-4930-bc10-44e6945063c0");
        ExtendedIterator<OntProperty> i = findByTypeAs(RDF.Property, OntProperty.class).filterKeep(new UniqueFilter<OntProperty>());
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "6a302b11-738a-46c9-a85f-c39937468541");
        // if we are in OWL_FULL, the properties should also include the annotation properties
        if (getReasoner() != null && getProfile().equals(ProfileRegistry.getInstance().getProfile(ProfileRegistry.OWL_LANG))) {
            writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "8886ac6c-be8e-4c8e-b55c-c480b30f2469");
            // we are using a reasoner, and in OWL Full
            // so add the annotation properties too
            i = i.andThen(listAnnotationProperties());
        }
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "974fa2ae-050e-4ccf-a19a-abb679c003d2");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "6e300066-bef3-4400-9e79-b211c7206264");
        ExtendedIterator<OntProperty> i = findByTypeAs(RDF.Property, OntProperty.class).andThen(listObjectProperties()).andThen(listDatatypeProperties()).andThen(listAnnotationProperties()).andThen(listFunctionalProperties()).andThen(listTransitiveProperties()).andThen(listSymmetricProperties());
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "e760ca63-6842-4321-8ec7-cd45e8cc54bf");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "194acc7d-f3bc-4057-bbff-7bdf692471da");
        checkProfileEntry(getProfile().OBJECT_PROPERTY(), "OBJECT_PROPERTY");
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "4400fa2d-6c51-4f9d-8793-3cdd35ecdda2");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "36cfe4ed-d7bb-4e78-82d5-b8ef92587dc9");
        checkProfileEntry(getProfile().DATATYPE_PROPERTY(), "DATATYPE_PROPERTY");
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "b656630f-345c-4f3d-b719-c6a9d35b5081");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "009b76bf-149f-4237-acd5-ae9d4bf49e5e");
        checkProfileEntry(getProfile().FUNCTIONAL_PROPERTY(), "FUNCTIONAL_PROPERTY");
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "103d83d6-e397-42dd-8faf-6b6e76f0e930");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "2868041d-0ea4-4d44-9f47-8fc0daf85636");
        checkProfileEntry(getProfile().TRANSITIVE_PROPERTY(), "TRANSITIVE_PROPERTY");
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "46936e1a-68c9-43d6-a2c6-b6cf62f3a340");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "c57c8c66-e654-45a4-8e01-8d689e9a15a8");
        checkProfileEntry(getProfile().SYMMETRIC_PROPERTY(), "SYMMETRIC_PROPERTY");
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "7841aa0c-d228-4ef7-bedc-847d9bb61327");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "7a3071b7-ce1b-49fd-bab5-af6bf2b7eafa");
        checkProfileEntry(getProfile().INVERSE_FUNCTIONAL_PROPERTY(), "INVERSE_FUNCTIONAL_PROPERTY");
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "45755ec5-64ea-4790-892a-0ee8e69e472d");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "1061a4ec-7c64-45d7-a16e-7658c97094fd");
        // since the reasoner implements some OWL full functionality for RDF compatibility, we
        // have to decide which strategy to use for identifying individuals depending on whether
        // or not a powerful reasoner (i.e. owl:Thing/daml:Thing aware) is being used with this model
        boolean supportsIndAsThing = false;
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "4d627f2a-7a72-4a46-9375-50f1c1f8e943");
        if (getGraph() instanceof InfGraph) {
            writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "31c4bd8d-495d-4805-8312-090341966676");
            supportsIndAsThing = ((InfGraph) getGraph()).getReasoner().getReasonerCapabilities().contains(null, ReasonerVocabulary.supportsP, ReasonerVocabulary.individualAsThingP);
        }
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "be8272c8-ff81-469e-af94-6fa1231d7e69");
        if (!supportsIndAsThing || (getProfile().THING() == null) || getProfile().CLASS().equals(RDFS.Class)) {
            writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "015e8ed1-a33a-449a-913b-b53a72a24794");
            // no inference, or we are in RDFS land, so we pick things that have rdf:type whose rdf:type is Class
            // it's tricky to make this efficient and cover all possible cases. I've changed the code to
            // make use of the isIndividual() test on OntResource, at the expense of some redundant queries
            // to the model, which could become expensive in the case of a DB model - ijd Apr-23-09
            Set<Individual> results = new HashSet<Individual>();
            writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "f0fb1690-143c-4afe-ab3a-4c37494c5d56");
            for (Iterator<Statement> i = listStatements(null, RDF.type, (RDFNode) null); i.hasNext(); ) {
                writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "73887f21-1c47-44db-bdf5-124ffa936240");
                OntResource r = i.next().getSubject().as(OntResource.class);
                writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "fc7c67e7-806b-4907-9e8c-a1f50baa5e5c");
                if (r.isIndividual()) {
                    writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "6937ea42-9f26-4ff4-a10f-b10cf5375e16");
                    results.add(r.as(Individual.class));
                }
            }
            writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "9939f588-e19f-4912-a608-e6ff0506f010");
            return WrappedIterator.create(results.iterator());
        } else {
            writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "33a69b53-9daf-4bb1-ba08-289186129dc2");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "a82b622e-5546-4cbd-83a3-1880bfe9dd17");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "0c1241a9-eff5-40ee-bc7d-6dce615b46c6");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "b6482c3d-633b-45fa-bc9e-b5bd9320b578");
        // look for the shortcut of using direct subClass on :Thing
        if (getReasoner() != null) {
            writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "03674dec-b2eb-4d23-b646-86e41128be71");
            Model conf = getReasoner().getReasonerCapabilities();
            writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "874684ee-b961-4b5a-a079-8a47ebc0f730");
            if (conf != null && conf.contains(null, ReasonerVocabulary.supportsP, ReasonerVocabulary.directSubClassOf) && getProfile().THING() != null) {
                writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "4adff71a-2315-4a4f-8f5d-d14e3db0ea2e");
                // we have have both direct sub-class of and a :Thing class to test against
                return listStatements(null, ReasonerVocabulary.directSubClassOf, getProfile().THING()).mapWith(s -> s.getSubject().as(OntClass.class));
            }
        }
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "9e66eda0-2465-4667-84d6-494e1fd1f541");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "18f09874-71f8-4217-b3fe-8be1f04539fd");
        checkProfileEntry(getProfile().ONE_OF(), "ONE_OF");
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "7ae6f65f-a051-442d-ba42-3c783819c805");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "123bbdc7-1c84-47d5-872c-8feab2e8898c");
        checkProfileEntry(getProfile().UNION_OF(), "UNION_OF");
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "7728712c-9ced-4984-b53c-c77581aa2a36");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "80b2d13b-2d83-4994-8418-0fbb52a25f35");
        checkProfileEntry(getProfile().COMPLEMENT_OF(), "COMPLEMENT_OF");
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "3b60de5d-3767-49f9-a622-02fe4dd9a6ac");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "2fefaeb7-f024-427a-952f-51b0e749e65d");
        checkProfileEntry(getProfile().INTERSECTION_OF(), "INTERSECTION_OF");
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "1f8980f1-298c-4e4a-b41e-beb61de1c2ed");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "1a81799d-b854-4c1f-a6fc-552395b72178");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "069ec7ea-bffa-475d-b09d-8657394ca6c6");
        checkProfileEntry(getProfile().RESTRICTION(), "RESTRICTION");
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "54bcf4d3-44f5-46c4-a517-d2ef9a519ffc");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "1cdd93bc-c363-4d50-a182-b60c238920e3");
        checkProfileEntry(getProfile().ALL_DIFFERENT(), "ALL_DIFFERENT");
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "68143a87-7260-479e-8843-32763d7d6295");
        return findByTypeAs(getProfile().ALL_DIFFERENT(), AllDifferent.class).filterKeep(new UniqueFilter<AllDifferent>());
    }

    /**
     * <p>Answer an iterator over the DataRange objects in this ontology, if there
     * are any.</p>
     * @return An iterator, whose values are {@link DataRange} objects.
     */
    @Override
    public ExtendedIterator<DataRange> listDataRanges() {
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "55993782-6fe3-4c71-9343-4a2de4440290");
        checkProfileEntry(getProfile().DATARANGE(), "DATARANGE");
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "aec45775-b3b4-4111-891f-b028da1d6fb8");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "f3569b47-e526-481c-a4a4-2aa9504cef11");
        checkProfileEntry(getProfile().ANNOTATION_PROPERTY(), "ANNOTATION_PROPERTY");
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "e28424b2-a7ed-47ad-8e67-a05941baf9c2");
        Resource r = getProfile().ANNOTATION_PROPERTY();
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "e57dcb4f-7696-4c0a-8447-5d75d760b367");
        if (r == null) {
            writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "f054e935-2e7f-4ed8-b4a3-db1b04db240f");
            return new NullIterator<AnnotationProperty>();
        } else {
            writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "a99952ed-ce71-4e1a-b627-4e514dea064b");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "66e16e26-42a6-470d-8d24-18bd899f62be");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "bd95244c-6ae9-4af5-99c4-f9c9307876b7");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "5b83cf25-dd90-48c0-9217-66aa6e40a252");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "4f2fc82f-9aa3-470b-8c62-4c7e8ada43ec");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "5d5301a6-3326-4187-968d-eadf66b287de");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "ca928ada-2318-4f66-908a-b43c9ab7222e");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "cff7c8b3-a3c3-4b1b-9295-cbedec1d450a");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "035a3f21-ea2d-438a-a594-d094aa6f06ed");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "ce48d0cc-a6eb-476c-8cb6-ec02dfecc2f7");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "dbfa5f3c-7673-4f90-9791-4d7c4de521ee");
        OntClass c = (OntClass) findByURIAs(uri, OntClass.class);
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "883ed9de-9802-4934-a422-43aa5c20863c");
        // special case for nothing and thing
        if (c == null) {
            writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "a136451d-7dff-4ceb-b837-57cf460072bb");
            Resource thing = getProfile().THING();
            writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "8fbb4668-e459-4ae9-80e7-7687eb1da7ce");
            if (thing != null && thing.getURI().equals(uri)) {
                writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "40f7e632-0df0-4370-b431-3f89db0d9fbc");
                c = thing.inModel(this).as(OntClass.class);
            }
            writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "e7dbef74-97a2-4815-ac5e-c840e65f555d");
            Resource nothing = getProfile().NOTHING();
            writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "210d9bdb-15ed-4cb9-9f8c-31623dbc3843");
            if (nothing != null && nothing.getURI().equals(uri)) {
                writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "edb27452-b1b1-4c68-9856-d650e8d775e0");
                c = nothing.inModel(this).as(OntClass.class);
            }
        }
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "dca6e5e9-c978-4d51-9f30-4d2ac07edef7");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "50bee892-f146-456a-a360-e4103533845d");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "501cb72f-871c-4339-8c96-ee5812697dfc");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "1b4fa858-382a-48cf-be1f-5be819b92cc7");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "a1d6062d-d921-45d1-962f-27764a71ac74");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "9a9f4e12-25f2-4be0-9ecb-9f2543284633");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "cd4611d2-a9e9-4390-962d-0941ed931119");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "2ce0cfb1-3a68-4d04-89a0-7949be4f4f3b");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "dd690fa7-25c2-41e6-9836-5153ebe04ac0");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "d7906133-fb6e-48e4-a95d-d85886b10d92");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "4228f658-8eac-4ace-b400-6c0fdd1ed2c7");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "e329f5ac-5c01-4510-b09c-1ce002ae41e7");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "68bbb151-222b-434b-9d65-defa2ee00db6");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "69a5193f-7c7b-4f33-8d3a-3961eb75e225");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "2841fd41-c85a-43a2-a54d-d30be2c538c2");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "50b50249-a53b-40c1-b802-8f50fe680adf");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "88a2f9fc-1b19-43b5-9cb6-600b579360ab");
        checkProfileEntry(getProfile().ONTOLOGY(), "ONTOLOGY");
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "e357c3c4-2d55-44d3-b89b-91629ed5f82c");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "487370d1-6d6e-4f5f-92b6-441f4be8f0fe");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "e5515813-9779-4917-a78d-d5bf731b1e51");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "b7827d9d-870e-474e-9b63-5318808123cf");
        Property p = createProperty(uri);
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "e20d6dd7-2bad-48ca-97d4-f015834a4447");
        p.addProperty(RDF.type, getProfile().PROPERTY());
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "8206764a-f4cd-4bf7-a9c9-f34cac47657d");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "d0b6e75d-7db1-4374-be9c-dbb276b8b78b");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "f28972fd-c781-4757-bd73-11a5d5a8c622");
        checkProfileEntry(getProfile().OBJECT_PROPERTY(), "OBJECT_PROPERTY");
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "3a21a17d-4991-4c75-831e-4eea930586f1");
        ObjectProperty p = createOntResource(ObjectProperty.class, getProfile().OBJECT_PROPERTY(), uri);
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "e46b2667-2500-4805-ab26-87f3dcffec4e");
        if (functional) {
            writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "4734c469-5d2f-4ec5-84f9-8023518703f0");
            checkProfileEntry(getProfile().FUNCTIONAL_PROPERTY(), "FUNCTIONAL_PROPERTY");
            writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "1ca397d5-a695-4461-a69e-8c1da4bf2f51");
            p.addProperty(RDF.type, getProfile().FUNCTIONAL_PROPERTY());
        }
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "dd3474f7-6be2-4864-bd29-41a42e3e25f3");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "5923bd53-c196-4901-a82d-621b411a193f");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "d7d35507-584d-462a-add1-8a61c4625705");
        checkProfileEntry(getProfile().TRANSITIVE_PROPERTY(), "TRANSITIVE_PROPERTY");
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "32e28778-9e48-4505-bb06-26daee604ad0");
        TransitiveProperty p = createOntResource(TransitiveProperty.class, getProfile().TRANSITIVE_PROPERTY(), uri);
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "4f75dce9-be88-4b22-aba0-83ac0706e2b5");
        if (functional) {
            writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "6cb53f77-3b8e-4e18-bd72-2715df6926de");
            checkProfileEntry(getProfile().FUNCTIONAL_PROPERTY(), "FUNCTIONAL_PROPERTY");
            writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "f93b5bdc-ae16-4651-8f96-5d0ce604423c");
            p.addProperty(RDF.type, getProfile().FUNCTIONAL_PROPERTY());
        }
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "bb0dd819-970c-44c0-99de-bc22cd640b62");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "652b84b2-fb9c-4abb-9ecf-3b6297a9e6f9");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "52dd9563-4050-4b82-8cf4-833e27c37623");
        checkProfileEntry(getProfile().SYMMETRIC_PROPERTY(), "SYMMETRIC_PROPERTY");
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "b458712a-76f4-4f28-a6b8-1b31e0d47eda");
        SymmetricProperty p = createOntResource(SymmetricProperty.class, getProfile().SYMMETRIC_PROPERTY(), uri);
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "8406f8fd-5ae8-4561-a6f2-863c82bba98f");
        if (functional) {
            writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "2d5aa4b5-b4d4-4c42-b8be-d8396bb387fb");
            checkProfileEntry(getProfile().FUNCTIONAL_PROPERTY(), "FUNCTIONAL_PROPERTY");
            writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "134632db-ae07-441e-b7a8-a703b93de8aa");
            p.addProperty(RDF.type, getProfile().FUNCTIONAL_PROPERTY());
        }
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "c3d0895d-c03b-41c9-b5e8-67b61ff9a811");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "c9d03dba-2e4a-4ebd-8912-c2640bd1d631");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "5b6d3f77-35a9-4c3d-b04f-bd4aec2ff93b");
        checkProfileEntry(getProfile().INVERSE_FUNCTIONAL_PROPERTY(), "INVERSE_FUNCTIONAL_PROPERTY");
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "ce49dccb-8160-4f1d-95a8-b6eaf957958a");
        InverseFunctionalProperty p = createOntResource(InverseFunctionalProperty.class, getProfile().INVERSE_FUNCTIONAL_PROPERTY(), uri);
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "8bd22a64-17c3-4dde-a5c5-d2607e37adbf");
        if (functional) {
            writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "b32492b2-107f-450d-8d5b-f0e0d74da702");
            checkProfileEntry(getProfile().FUNCTIONAL_PROPERTY(), "FUNCTIONAL_PROPERTY");
            writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "a1deee8a-0f5d-4dfb-afaf-ef5f92ee58a8");
            p.addProperty(RDF.type, getProfile().FUNCTIONAL_PROPERTY());
        }
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "507357ce-f064-40de-95ca-27d40e333517");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "86d86294-7140-47c3-8d8a-b8c94fbd95b5");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "7a203d00-59ce-4183-a129-67681c11cac5");
        checkProfileEntry(getProfile().DATATYPE_PROPERTY(), "DATATYPE_PROPERTY");
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "c8dedbcb-03b3-4617-b05b-0b59ee3fdfce");
        DatatypeProperty p = createOntResource(DatatypeProperty.class, getProfile().DATATYPE_PROPERTY(), uri);
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "4fb5e012-e4cf-4477-96d1-6448234721a4");
        if (functional) {
            writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "6adfb1d7-fac5-4f3c-aa00-71fe66399b24");
            checkProfileEntry(getProfile().FUNCTIONAL_PROPERTY(), "FUNCTIONAL_PROPERTY");
            writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "1c05e0d0-7c97-4b6a-ab90-02f2acf848d3");
            p.addProperty(RDF.type, getProfile().FUNCTIONAL_PROPERTY());
        }
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "51aaa584-c5f5-4f85-9d03-16ff51a18030");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "bab7c112-acd5-498c-bdf6-a24e16f18638");
        checkProfileEntry(getProfile().ANNOTATION_PROPERTY(), "ANNOTATION_PROPERTY");
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "c5fd2fd9-afb9-421b-9634-7fdcc6092d48");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "89056602-bc27-4f6c-b891-20b16d397046");
        checkProfileEntry(getProfile().CLASS(), "CLASS");
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "7e72ff13-8556-4edb-b096-1317d9b09003");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "0d95b186-b58b-4aa0-a3c3-74dadd55b1e1");
        checkProfileEntry(getProfile().CLASS(), "CLASS");
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "70b93fbb-e74d-4f54-825c-a17f0769063e");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "ecee5661-e09f-49b6-bcc7-e0909cf5789e");
        checkProfileEntry(getProfile().CLASS(), "CLASS");
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "818c5fa3-9772-4ec0-acf4-85970a4ee447");
        OntClass c = createOntResource(OntClass.class, getProfile().CLASS(), uri);
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "7fa1af38-d4f3-4d9f-b518-8a8943f286e7");
        checkProfileEntry(getProfile().COMPLEMENT_OF(), "COMPLEMENT_OF");
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "711aeec1-bf22-4d6f-a0b1-df4edd630934");
        // if the class that this class is a complement of is not specified, use owl:nothing or daml:nothing
        c.addProperty(getProfile().COMPLEMENT_OF(), (cls == null) ? getProfile().NOTHING() : cls);
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "b54a4755-7910-4aaa-a6dd-e474adc6eff2");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "327c6fbd-646e-48a4-85bc-93badf634680");
        checkProfileEntry(getProfile().CLASS(), "CLASS");
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "cbd5005e-820f-4637-be20-00c62fd1e07f");
        OntClass c = createOntResource(OntClass.class, getProfile().CLASS(), uri);
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "0d7fab29-8178-4144-8a8c-f936a8026d04");
        checkProfileEntry(getProfile().ONE_OF(), "ONE_OF");
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "af6c59a1-ca47-4287-95fb-86937eae7094");
        c.addProperty(getProfile().ONE_OF(), (members == null) ? createList() : members);
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "58b35c43-3725-4817-a143-d7dff015a682");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "55faa420-35ca-4487-aeed-f02ca34c400f");
        checkProfileEntry(getProfile().CLASS(), "CLASS");
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "f7478508-f05b-40a2-8094-f534ee0e7db5");
        OntClass c = createOntResource(OntClass.class, getProfile().CLASS(), uri);
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "64d92da6-5fc7-4c14-b11a-bb091859bb14");
        checkProfileEntry(getProfile().UNION_OF(), "UNION_OF");
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "20b2caff-f4f3-421f-aae6-aba13ab3171c");
        c.addProperty(getProfile().UNION_OF(), (members == null) ? createList() : members);
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "ceb0e219-8ae9-48b4-a656-b87978f7cc59");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "8b9fd4e4-46fa-403b-8abb-4fc7986b3e08");
        checkProfileEntry(getProfile().CLASS(), "CLASS");
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "3902764d-fd8f-40d8-a26c-12774750d3dd");
        OntClass c = createOntResource(OntClass.class, getProfile().CLASS(), uri);
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "49a58ab6-a9bd-4dfa-9fec-d1090c56ae50");
        checkProfileEntry(getProfile().INTERSECTION_OF(), "INTERSECTION_OF");
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "93dca8a7-aa68-48fc-87f4-97b24ffc1f73");
        c.addProperty(getProfile().INTERSECTION_OF(), (members == null) ? createList() : members);
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "c2d91192-d84c-4f91-92d2-7ec271804b6a");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "81e9b222-b223-4674-a1da-6c118fa3bf8c");
        checkProfileEntry(getProfile().RESTRICTION(), "RESTRICTION");
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "1c9c117b-fadb-45ba-afbb-50ba6c8d5450");
        Restriction r = createOntResource(Restriction.class, getProfile().RESTRICTION(), null);
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "e82a1b99-baa7-4e3e-a05c-3075f5b206a5");
        if (p != null) {
            writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "3ea38301-af77-4d30-871e-af42b665ed9b");
            r.setOnProperty(p);
        }
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "5376f9f5-33fd-4cff-a29c-a9a73a33d7b4");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "ca0c4070-d276-4482-a740-7744bd1a12e0");
        checkProfileEntry(getProfile().RESTRICTION(), "RESTRICTION");
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "f8eb0198-3f2e-492e-8528-c42beb58c167");
        Restriction r = createOntResource(Restriction.class, getProfile().RESTRICTION(), uri);
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "32f03c86-1159-4ad1-bc31-669c589da597");
        if (p != null) {
            writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "4ad5962c-2173-4572-bf13-51ac1a70efb7");
            r.setOnProperty(p);
        }
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "d5602885-dbdd-465c-b6c6-270d3abe7add");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "981720f9-79f3-4fc0-b5d8-b49d250bb05c");
        checkProfileEntry(getProfile().RESTRICTION(), "RESTRICTION");
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "c518439c-ed5a-40b9-9799-5ce63fb71cec");
        Restriction r = createOntResource(Restriction.class, getProfile().RESTRICTION(), uri);
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "a996831a-7d63-4e45-ae73-b243a4dd76d8");
        if (prop == null || value == null) {
            writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "c2bbb074-898c-472a-b1b5-53938fbd9cea");
            throw new IllegalArgumentException("Cannot create hasValueRestriction with a null property or value");
        }
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "509c170a-fc0b-4051-a7ec-8ac0e3d9ab8f");
        checkProfileEntry(getProfile().HAS_VALUE(), "HAS_VALUE");
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "f2278212-ceff-4480-b6b2-2e77adb9ef03");
        r.addProperty(getProfile().ON_PROPERTY(), prop);
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "6b443466-e4ac-4db7-9157-86a045792b5e");
        r.addProperty(getProfile().HAS_VALUE(), value);
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "c0828288-8509-4042-b7de-e7cc90651bfe");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "10ce2367-c41c-4c41-91b1-2b98ccdbf284");
        checkProfileEntry(getProfile().RESTRICTION(), "RESTRICTION");
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "d297490b-2d71-427f-8f4f-1388a11ddba0");
        Restriction r = createOntResource(Restriction.class, getProfile().RESTRICTION(), uri);
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "ad01c181-7cde-4873-808b-f9ca6347acbb");
        if (prop == null || cls == null) {
            writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "203b8c0e-25f2-4829-9788-d7b1ebc899aa");
            throw new IllegalArgumentException("Cannot create someValuesFromRestriction with a null property or class");
        }
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "a50acc90-583f-4309-ade1-de5198018058");
        checkProfileEntry(getProfile().SOME_VALUES_FROM(), "SOME_VALUES_FROM");
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "17e0c646-9bff-4709-b4d0-8a0341a26c6f");
        r.addProperty(getProfile().ON_PROPERTY(), prop);
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "703bcf86-fb05-45ea-9f1f-4d0ce9e1a6ad");
        r.addProperty(getProfile().SOME_VALUES_FROM(), cls);
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "6101e358-f714-4b64-aaec-8549d8be534f");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "15be29a5-1e9a-483e-b95a-3188d1f23dc6");
        checkProfileEntry(getProfile().RESTRICTION(), "RESTRICTION");
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "23a31b7d-cf91-4059-9017-71535e334bcc");
        Restriction r = createOntResource(Restriction.class, getProfile().RESTRICTION(), uri);
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "a4343263-eaad-4849-8d99-4485f93b3190");
        if (prop == null || cls == null) {
            writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "34617924-0973-4e08-a3a7-7fab90d3e42b");
            throw new IllegalArgumentException("Cannot create allValuesFromRestriction with a null property or class");
        }
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "f530c3ad-4fc0-439e-a339-9237e75cdcb1");
        checkProfileEntry(getProfile().ALL_VALUES_FROM(), "ALL_VALUES_FROM");
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "9f0b8430-a765-421a-8cf8-845447053d2e");
        r.addProperty(getProfile().ON_PROPERTY(), prop);
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "67052ea5-0fa4-4aeb-a020-fca7cddab5b7");
        r.addProperty(getProfile().ALL_VALUES_FROM(), cls);
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "1881b2e7-3a1d-4307-a058-891d8b4ead30");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "8e3156cb-ae0f-4410-ade7-36cf00f44d47");
        checkProfileEntry(getProfile().RESTRICTION(), "RESTRICTION");
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "d9d3f1d1-b6ac-4a41-b935-13ed94cc8f4a");
        Restriction r = createOntResource(Restriction.class, getProfile().RESTRICTION(), uri);
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "0046baa1-f909-487a-9bb4-ff19654e8e75");
        if (prop == null) {
            writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "d03cbff8-8fd4-4453-bdda-a1954cec37e3");
            throw new IllegalArgumentException("Cannot create cardinalityRestriction with a null property");
        }
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "860c45ea-545a-481f-ad82-8dc515c2cc45");
        checkProfileEntry(getProfile().CARDINALITY(), "CARDINALITY");
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "f44af24e-af55-4fbe-a912-12306d1ed8e3");
        r.addProperty(getProfile().ON_PROPERTY(), prop);
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "1b2131ae-36a8-4598-97e3-7580ab0f4189");
        r.addProperty(getProfile().CARDINALITY(), createTypedLiteral(cardinality));
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "60fbc4b9-3b95-4274-b0bb-43d4bb1c7112");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "d64d979f-98f0-495c-b499-dc8b1906ef80");
        checkProfileEntry(getProfile().RESTRICTION(), "RESTRICTION");
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "0c91a554-d792-46e3-8e87-d1a2f669e3b6");
        Restriction r = createOntResource(Restriction.class, getProfile().RESTRICTION(), uri);
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "e1b5aed9-213a-40bd-896b-439c8d5074cd");
        if (prop == null) {
            writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "5f8d8f99-2160-4129-a4f6-7133081c3f70");
            throw new IllegalArgumentException("Cannot create minCardinalityRestriction with a null property");
        }
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "82ceeaec-cf63-463d-ac93-c78a762317eb");
        checkProfileEntry(getProfile().MIN_CARDINALITY(), "MIN_CARDINALITY");
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "9fdebd42-24f7-46dd-b0a9-cb7a86b753dc");
        r.addProperty(getProfile().ON_PROPERTY(), prop);
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "0af02c0d-1306-42f5-8ae5-cd31c882c255");
        r.addProperty(getProfile().MIN_CARDINALITY(), createTypedLiteral(cardinality));
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "1b6297a1-84e6-4634-8ba2-c0d67ea4d477");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "829cd3aa-6ec7-41fd-aa28-8f3607ebf6ba");
        checkProfileEntry(getProfile().RESTRICTION(), "RESTRICTION");
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "aa3a671f-2262-4237-8ba6-a59159fc399c");
        Restriction r = createOntResource(Restriction.class, getProfile().RESTRICTION(), uri);
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "172c79ee-3000-4bfd-b835-8c85e9a7811d");
        if (prop == null) {
            writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "12fb78bf-f224-4665-a206-deac148a5eb4");
            throw new IllegalArgumentException("Cannot create maxCardinalityRestriction with a null property");
        }
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "701755ba-f1c7-40e9-845b-061939129012");
        checkProfileEntry(getProfile().MAX_CARDINALITY(), "MAX_CARDINALITY");
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "10ff3fc7-f2ac-473c-935a-1e0193a98467");
        r.addProperty(getProfile().ON_PROPERTY(), prop);
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "04fb45b5-6757-4159-87ff-3638ca29b504");
        r.addProperty(getProfile().MAX_CARDINALITY(), createTypedLiteral(cardinality));
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "da9ef6bf-2e07-4113-96ad-59145d7609e5");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "5e067574-0ac5-4f7c-929e-a56614b3708c");
        checkProfileEntry(getProfile().RESTRICTION(), "RESTRICTION");
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "0397753d-8b52-488b-bf61-587e1d87fa0c");
        checkProfileEntry(getProfile().ON_PROPERTY(), "ON_PROPERTY");
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "8505070d-2d7e-4a65-b8bc-9e2363b66ec0");
        checkProfileEntry(getProfile().MAX_CARDINALITY_Q(), "MAX_CARDINALITY_Q");
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "f0bfdf12-dccf-446a-8ead-fa6020b7f6fb");
        checkProfileEntry(getProfile().HAS_CLASS_Q(), "HAS_CLASS_Q");
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "4c3d02f7-0362-4069-a8a2-ef27c760795e");
        if (prop == null) {
            writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "908e4b2d-6cf3-482d-a915-8a4e2a5ea23f");
            throw new IllegalArgumentException("Cannot create MaxCardinalityQRestriction with a null property");
        }
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "bbc0a4e7-a244-4ca2-ae34-230399f48329");
        if (cls == null) {
            writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "bcdc54f0-e5f2-4b80-ba78-01cacb8710e1");
            throw new IllegalArgumentException("Cannot create MaxCardinalityQRestriction with a null class");
        }
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "e1b10388-b0ac-4377-a244-c61600d2ed0e");
        Restriction r = createOntResource(Restriction.class, getProfile().RESTRICTION(), uri);
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "a430f9ef-45eb-45e6-99b4-e027acf418e3");
        r.addProperty(getProfile().ON_PROPERTY(), prop);
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "d6973120-3ba5-405d-bd59-cc4fbfd299fd");
        r.addProperty(getProfile().MAX_CARDINALITY_Q(), createTypedLiteral(cardinality));
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "53567a8f-f181-4a5d-af03-0c713b3e75d9");
        r.addProperty(getProfile().HAS_CLASS_Q(), cls);
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "965de39d-10ae-49c6-adbb-cd9776cfe5a5");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "134b5213-31f0-42ee-a526-6101bb5a091f");
        checkProfileEntry(getProfile().RESTRICTION(), "RESTRICTION");
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "39296f0e-19cb-4bb7-8dd5-3da00421a1ba");
        checkProfileEntry(getProfile().ON_PROPERTY(), "ON_PROPERTY");
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "36359a3b-1ca3-4393-95a2-73d6c0fa0a88");
        checkProfileEntry(getProfile().MIN_CARDINALITY_Q(), "MIN_CARDINALITY_Q");
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "cf7500d5-e08d-4571-8d08-c57efae66b6e");
        checkProfileEntry(getProfile().HAS_CLASS_Q(), "HAS_CLASS_Q");
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "bf503772-7bfd-458e-85b9-bf9378c443c9");
        if (prop == null) {
            writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "29de058e-fde8-4fed-bf7b-ccc2fe03fc42");
            throw new IllegalArgumentException("Cannot create MinCardinalityQRestriction with a null property");
        }
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "f528b5c0-ccff-4a05-ac0d-ad47fb58f61b");
        if (cls == null) {
            writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "2eb89409-67c4-42f0-9d31-cc97ddc030cf");
            throw new IllegalArgumentException("Cannot create MinCardinalityQRestriction with a null class");
        }
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "4d08bcc8-d001-4567-9f20-a944827c9ca6");
        Restriction r = createOntResource(Restriction.class, getProfile().RESTRICTION(), uri);
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "5ad00a51-f65f-41bf-a799-5aeeebdb7c55");
        r.addProperty(getProfile().ON_PROPERTY(), prop);
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "c1eeefc7-0018-4f7c-923a-638b51e7cb8c");
        r.addProperty(getProfile().MIN_CARDINALITY_Q(), createTypedLiteral(cardinality));
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "18fa4965-a08b-4308-9a85-ce451f7eea83");
        r.addProperty(getProfile().HAS_CLASS_Q(), cls);
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "39b701ad-71b7-49fa-8672-f84160ae4629");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "0e924cde-5cb7-4b3b-9460-96b03cef7a19");
        checkProfileEntry(getProfile().RESTRICTION(), "RESTRICTION");
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "dc7d28c9-cc81-471f-bb6a-40086d15c314");
        checkProfileEntry(getProfile().ON_PROPERTY(), "ON_PROPERTY");
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "82e6247e-7e2e-4151-a266-231ce896430b");
        checkProfileEntry(getProfile().CARDINALITY_Q(), "CARDINALITY_Q");
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "4f6bcd01-2093-4c3f-bb35-3d6761b9215f");
        checkProfileEntry(getProfile().HAS_CLASS_Q(), "HAS_CLASS_Q");
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "8531254d-6833-476c-8885-fdfa3fd198fe");
        if (prop == null) {
            writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "e5d8ba60-7aec-4f40-a90f-2eec20320a25");
            throw new IllegalArgumentException("Cannot create CardinalityQRestriction with a null property");
        }
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "d2ddc3ba-b5e2-4af3-bdbb-3448ed91d9e0");
        if (cls == null) {
            writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "67c8c0fa-dbdb-4e75-b533-93da126dbc74");
            throw new IllegalArgumentException("Cannot create CardinalityQRestriction with a null class");
        }
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "693b89ca-ddb7-4699-9cd9-ecf04224c635");
        Restriction r = createOntResource(Restriction.class, getProfile().RESTRICTION(), uri);
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "5605622b-5419-45c7-ba97-4b1b64b8a8fd");
        r.addProperty(getProfile().ON_PROPERTY(), prop);
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "959b32f9-5705-4cac-a1bc-573bbdb7e9ed");
        r.addProperty(getProfile().CARDINALITY_Q(), createTypedLiteral(cardinality));
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "ed3ad6d4-1a14-424c-bfc8-f8cba5ab5f83");
        r.addProperty(getProfile().HAS_CLASS_Q(), cls);
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "d06aead6-b174-41cf-880a-f0f739ed37b7");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "4871f39d-4643-4ac2-81d1-4f761fb9e46c");
        checkProfileEntry(getProfile().DATARANGE(), "DATARANGE");
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "1a82e884-1018-4c65-9887-38ab0a3bafdf");
        DataRange d = createOntResource(DataRange.class, getProfile().DATARANGE(), null);
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "74ea9ba5-1343-4f79-8dc1-3520adf4b03a");
        checkProfileEntry(getProfile().ONE_OF(), "ONE_OF");
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "7731581d-2a8f-4ba3-a49a-72bf4a1216f4");
        d.addProperty(getProfile().ONE_OF(), (literals == null) ? createList() : literals);
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "f3b3289c-40cf-4dd5-acea-9a079e24ca2c");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "a2c76f6a-0850-4571-9283-3add9b36c24d");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "542c53df-01a3-4e74-8bc3-dc505c1dcc80");
        checkProfileEntry(getProfile().ALL_DIFFERENT(), "ALL_DIFFERENT");
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "a263b743-9128-48ef-8330-2b24d256b7b2");
        AllDifferent ad = createOntResource(AllDifferent.class, getProfile().ALL_DIFFERENT(), null);
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "bf2cf45a-0b4d-4fe7-9619-27293a66b1ea");
        ad.setDistinctMembers((differentMembers == null) ? createList() : differentMembers);
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "59bd4d0a-1484-49bd-8d23-f3260ffec6d0");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "6e109d6f-f18c-44d0-a71c-fca542a486ea");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "e5d65757-4e46-41cb-8064-8cf53bc3091b");
        return getResource(uri).as(OntResource.class);
    }

    /**
     * <p>Answer a new empty list.  This method overrides the list create method in ModelCom,
     * to allow both DAML and RDFS lists to be created.</p>
     * @return An RDF-encoded list of no elements, using the current language profile
     */
    @Override
    public RDFList createList() {
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "1619a536-81b4-4c51-a3d3-fe363c9fe0d2");
        Resource list = getResource(getProfile().NIL().getURI());
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "558a6052-20ad-4bb2-a902-d9bef47bdf3a");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "3f996c87-96f6-48a7-8951-64b94c6f1560");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "17aa3474-5e64-4333-9b30-32c1bf604a44");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "f67e4c17-16b7-43af-a4ef-e1ce61693840");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "88189873-03ad-4616-a1bf-1eabd2c53793");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "1647e89a-de90-4b02-907d-0c8eab6cfa2c");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "42f6b43c-78a2-466f-9e63-4086068d8674");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "5475cfcc-7907-4233-9cf2-399b684ca663");
        Set<String> results = new HashSet<String>();
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "4ebc39d9-3799-4187-aa4d-b884b02538d1");
        List<Model> queue = new ArrayList<Model>();
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "1e6bc87b-944c-4986-af59-e74ee97f937f");
        queue.add(getBaseModel());
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "fdf1e973-b187-4f23-8bfb-df1d7fe0820e");
        while (!queue.isEmpty()) {
            writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "802d7b75-1399-4f5b-a1fc-db1925c79308");
            Model m = queue.remove(0);
            writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "02458190-2d1f-4570-86eb-f4f927673148");
            // list the ontology nodes
            if (getProfile().ONTOLOGY() != null && getProfile().IMPORTS() != null) {
                writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "de606834-7f78-4606-b6b8-3fe002ebd7ed");
                StmtIterator i = m.listStatements(null, getProfile().IMPORTS(), (RDFNode) null);
                writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "fa4e8483-5c21-41d7-bbf3-5293950b6688");
                while (i.hasNext()) {
                    writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "4a34ee1f-ce9b-41d7-b428-5d51d6fef7aa");
                    Statement s = i.nextStatement();
                    writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "02dfb89c-8c7a-4168-8694-784c2046d080");
                    String uri = s.getResource().getURI();
                    writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "cbc906c9-9801-4595-ba62-f455e7c720f9");
                    if (!results.contains(uri)) {
                        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "ada4ffa6-1329-4cac-8d42-059bab276bc6");
                        // this is a new uri, so we add it
                        results.add(uri);
                        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "e3b6ecc2-4c5a-427f-997b-3051334b29da");
                        // and push the model on the stack if we know it
                        Model mi = getDocumentManager().getModel(uri);
                        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "2378f9c2-a6e7-4dc3-bcc5-3d8c3ab15932");
                        if (closure && mi != null && !queue.contains(mi)) {
                            writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "b7484af0-3f85-4f64-9c37-26109e6ab0d9");
                            queue.add(mi);
                        }
                    }
                }
            }
        }
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "68401df2-a73a-47e3-a3a0-1219477419e9");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "de8b25ef-afdf-416a-b5fa-1357d6a44aeb");
        return m_spec.getImportModelMaker();
    }

    /**
     * @deprecated use getImportModelMaker instead.
     */
    @Override
    @Deprecated
    public ModelMaker getModelMaker() {
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "5fb927f2-faa6-42a6-ac54-222947029b19");
        return getImportModelMaker();
    }

    /**
     * <p>Read statements into the model from the given source, and then load
     * imported ontologies (according to the document manager policy).</p>
     * @param uri URI to read from, may be mapped to a local source by the document manager
     */
    @Override
    public Model read(String uri) {
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "8f2cc32c-ec65-4f71-a2e1-6551366fbb2f");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "95255a58-710b-4b92-b44c-b7881f69d02a");
        super.read(reader, base);
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "19ed6d4a-3fb8-4283-9e1e-130d76209929");
        loadImports();
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "811ee44a-4818-4908-b361-7ed1b8f1a4b3");
        rebind();
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "7e73f445-fecf-4c2b-b1a2-88f355ea5495");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "3c1e32af-a8a6-4dd0-9f68-33e8ea74e569");
        super.read(reader, base);
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "7c80e137-b2d4-4a86-b2b2-09d07a47584e");
        loadImports();
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "61b1d730-bee1-4f27-be39-11f1dd1af012");
        rebind();
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "8b864685-a4f5-4e26-bcd8-d1a8fef48f57");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "e4518969-0560-471c-b1c9-8475c7097305");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "91bbd305-37bd-459c-91d9-5fdca2daa61e");
        // we don't want to load this document again if imported by one of the imports
        addLoadedImport(uri);
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "bf984c12-6546-47d4-a2c1-a9d6eac7fe3b");
        OntDocumentManager odm = getDocumentManager();
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "294d25b0-59e8-43b5-bf1b-29334e951001");
        String sourceURL = odm.doAltURLMapping(uri);
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "efbfa112-1fce-42f2-aedc-555f8949b20e");
        // invoke the read hook from the ODM
        String source = odm.getReadHook().beforeRead(this, sourceURL, odm);
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "ad62a810-1f22-40fe-ab22-c11a23a05869");
        if (source == null) {
            writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "d745ea5f-9d72-4a40-a422-f05d8c894be6");
            s_log.warn("ReadHook returned null, so skipping assuming previous value: " + sourceURL);
            writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "563f5879-bbcc-423d-980e-98cac6ff8f31");
            source = sourceURL;
        } else {
            writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "6e9ee2c4-e647-4029-b012-51a66377791c");
            // now we can actually do the read, check first if we should use negotiation
            if (// require non-null base
            base == null && // and that negotiation makes sense (don't conneg to file:)
            !ignoreFileURI(source) && // and that we haven't remapped the URI
            source.equals(uri)) {
                writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "c6459581-c955-4a59-b35b-fe3f72392291");
                if (syntax == null) {
                    writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "c824e2f1-fc28-41f1-b4d3-84ebe39df1ec");
                    readDelegate(source);
                } else {
                    writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "89c0612b-5ccc-4d30-8328-3a333eba05d5");
                    readDelegate(source, syntax);
                }
            } else {
                writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "bf950664-f878-4276-a45d-147963a0d712");
                // if we were given the base, use it ... otherwise default to the base being the source
                readDelegate(source, (base == null ? uri : base), syntax);
            }
        }
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "7741cab5-4fb1-4d47-9e49-fb604a306b9f");
        // the post read hook
        odm.getReadHook().afterRead(this, source, odm);
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "37780e56-a7dd-4c1f-b1f5-f91297377a00");
        // cache this model against the public uri (if caching enabled)
        getDocumentManager().addModel(uri, this);
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "87b62a0c-e345-4f2f-becf-ab2148c6ac2f");
        loadImports();
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "3f53813a-e04d-4e3e-9ce5-b43c06cf1976");
        rebind();
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "fc5be48d-480f-4986-bd19-468128a151d7");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "2c11994d-fd71-4e14-a5a3-49924ef13287");
        super.read(reader, base, syntax);
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "701c5e24-84ed-4a92-8a69-e6346b3558bb");
        loadImports();
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "abce8242-ebcd-4f54-90a2-0cffd155409a");
        rebind();
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "f48cf4c9-4e38-41a0-a8ad-6d3f3da81704");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "3e606b6d-d0e3-4201-9229-06a3e4162ef4");
        super.read(reader, base, syntax);
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "804f2f3c-2be3-4b9f-b953-9b238fcc0937");
        loadImports();
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "5620be46-9569-45ca-86b7-9a73412c360d");
        rebind();
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "3003ddbd-3275-4854-85ef-aad88b06c25b");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "02e95f10-87e1-48c0-b8e3-46036fa94d23");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "aa5e2130-88c2-433e-9a51-0e487fbaaf24");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "6a08171a-ff5f-4993-9c57-2a3b25d07de2");
        ExtendedIterator<Graph> i = WrappedIterator.create(getSubGraphs().iterator());
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "f1954038-831c-4deb-b6ad-4ca875094819");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "83b2f214-f4b8-4e99-a3b8-f79aa55ed9fa");
        return listSubModels(false);
    }

    /**
     * <p>Answer the number of sub-models of this model, not including the
     * base model.</p>
     * @return The number of sub-models, &ge; zero.
     */
    @Override
    public int countSubModels() {
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "c2374a52-b300-4613-ad4c-182d42319927");
        int count = 0;
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "4e911b9b-e3ce-4e53-a840-f8619b1c92b2");
        for (Graph graph1 : getSubGraphs()) {
            writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "b03b73b8-f6d8-4dd6-9381-753880d6ddc9");
            count++;
        }
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "bdb2b0e0-787a-4d27-bb95-ce5617236905");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "a6107eda-cbfb-408c-94d0-8d28cb46b5a3");
        if (listImportedOntologyURIs(true).contains(uri)) {
            writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "6ee0dd82-c5be-447d-a84f-6c2d6464f25c");
            Model mi = getDocumentManager().getModel(uri);
            writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "b9b82c5a-ae7e-4a7c-a903-9f97533d0fcc");
            if (mi != null) {
                writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "0edc5c18-2160-432f-b12e-744b37a8a04c");
                if (mi instanceof OntModel) {
                    writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "49f4d684-62c0-4749-9162-26a6497ae1b8");
                    // already a suitable ont model
                    return (OntModel) mi;
                } else {
                    writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "73b08da9-1f37-44ab-85aa-d2e1da615371");
                    // not in ont-model clothing yet, so re-wrap
                    return ModelFactory.createOntologyModel(m_spec, mi);
                }
            }
        }
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "30f2edf2-77bf-4c78-86ae-ee164b59351f");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "ae7ec573-019b-4737-8ef2-7208e5c613c5");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "2e1e90ad-0e5b-4bb5-b1d4-1fbaf3591c82");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "0bf0251a-a91e-44e9-a86b-2890d787fba4");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "34f90ce7-309a-48b4-92da-6d902413e19e");
        getUnionGraph().addGraph(model.getGraph());
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "9a002fa0-b02b-4239-9040-3c3691ea22b9");
        if (rebind) {
            writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "8af56590-4e34-4cd3-8e0b-bf794141d084");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "03e4cf53-fb2f-4cf9-af80-83c9a876e9cd");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "61335b14-661d-4305-8ea8-1735175a9b53");
        Graph subG = model.getGraph();
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "1edf4205-5229-4517-a4f3-0af19ac81f6c");
        getUnionGraph().removeGraph(subG);
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "18c9cd8f-5987-4a4c-9c09-c2cefbddf783");
        // originally
        if (subG instanceof MultiUnion) {
            writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "30af0a28-eb04-42bc-b66f-32cc99b00c95");
            // we need to get the base graph when removing a ontmodel
            getUnionGraph().removeGraph(((MultiUnion) subG).getBaseGraph());
        }
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "223ac020-0922-4c55-8353-1cd1bbd56eee");
        if (rebind) {
            writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "c2408c55-b131-45de-acec-5a5bdd10798b");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "7f09b791-14db-426e-bff1-e4e304ccf7ae");
        Node n = node.asNode();
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "6a936d74-27f7-4c3e-b422-9025996a915f");
        Graph b = getBaseGraph();
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "706efa7d-0216-4f6e-adeb-96c2bfee2b24");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "a5fb6585-900f-4b11-94b0-e1986ab37022");
        Node s = stmt.getSubject().asNode();
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "81f17b18-4056-4e5f-a246-d957d6c60fd8");
        Node p = stmt.getPredicate().asNode();
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "bfaff2ef-185f-4acc-bd04-ba61206ba58a");
        Node o = stmt.getObject().asNode();
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "e77aa86b-7230-48b3-a956-d64b94f454fb");
        Graph b = getBaseGraph();
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "96c99845-d8e0-4f9b-8773-8ebbfa028c3c");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "07ecdf25-7f11-4e70-ba9c-60cd4a318713");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "be54ccc1-40bc-4901-a2cf-de3dce261dc0");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "c67f65f0-019f-4b7e-9306-b8346b16a600");
        if (dynamic) {
            writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "80d06ddb-4f6a-453b-823d-ea7219056997");
            if (m_importsListener == null) {
                writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "cfc27fed-5ba2-46e3-9277-d570c426848b");
                // turn on dynamic processing
                m_importsListener = new ImportsListener();
                writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "fe9d4593-5320-4c9e-80eb-a089db497bfb");
                register(m_importsListener);
            }
        } else {
            writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "93f41d47-9895-4c0a-842f-422239a22115");
            if (m_importsListener != null) {
                writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "473443d3-c27c-483d-97dd-c9fa9292c8cb");
                // turn off dynamic processing
                unregister(m_importsListener);
                writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "8ba5a5b2-2041-47c4-b3d4-d6ba3c13ad92");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "d72ec435-83c5-4137-afd3-e747778b1a3a");
        return m_importsListener != null;
    }

    /**
     * <p>Answer the ontology model specification that was used to construct this model</p>
     * @return An ont model spec instance.
     */
    @Override
    public OntModelSpec getSpecification() {
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "7c235c03-5a24-4a2c-a6fa-7d4df63b3de5");
        return m_spec;
    }

    // output operations - delegate to base model
    @Override
    public Model write(Writer writer) {
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "cb49574f-e713-4e06-9e7c-bb464bc9717f");
        return getBaseModel().write(writer);
    }

    @Override
    public Model write(Writer writer, String lang) {
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "0ed358d3-a228-4e2e-aef7-9b3681c34648");
        return getBaseModel().write(writer, lang);
    }

    @Override
    public Model write(Writer writer, String lang, String base) {
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "9a354c8e-d97b-4155-b3d5-452f1fb414fe");
        return getBaseModel().write(writer, lang, base);
    }

    @Override
    public Model write(OutputStream out) {
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "99ca48d0-1b06-4628-a37d-f8228c749a3f");
        return getBaseModel().write(out);
    }

    @Override
    public Model write(OutputStream out, String lang) {
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "ff0af653-8225-4e7f-9de8-ef436d77a4f9");
        return getBaseModel().write(out, lang);
    }

    @Override
    public Model write(OutputStream out, String lang, String base) {
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "b644d3e9-c0ec-425d-ba44-d3ef73879766");
        return getBaseModel().write(out, lang, base);
    }

    @Override
    public Model writeAll(Writer writer, String lang, String base) {
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "541edabd-081c-442c-82d7-20271ca556e3");
        return super.write(writer, lang, base);
    }

    @Override
    public Model writeAll(OutputStream out, String lang, String base) {
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "96f72fbf-64f5-40ce-a8c8-859e589d595a");
        return super.write(out, lang, base);
    }

    @Override
    public Model writeAll(Writer writer, String lang) {
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "3aef3e12-ad29-4bfb-8b98-80df00ab0a4b");
        return super.write(writer, lang);
    }

    @Override
    public Model writeAll(OutputStream out, String lang) {
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "bba385da-a7e4-4d5e-ad98-9541d0e1407e");
        return super.write(out, lang);
    }

    // Implementation of inf model interface methods
    /**
     * Return the raw RDF model being processed (i.e. the argument
     * to the Reasonder.bind call that created this InfModel).
     */
    @Override
    public Model getRawModel() {
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "1d3a8bdb-78ad-4c72-8aa1-905d0a773e08");
        return getBaseModel();
    }

    /**
     * Return the Reasoner which is being used to answer queries to this graph.
     */
    @Override
    public Reasoner getReasoner() {
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "5e38a4ab-eee7-4349-a992-17346ac93911");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "63fd535f-1b7c-4687-9597-3cb5513cf216");
        if (getGraph() instanceof InfGraph) {
            writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "a818816f-a768-4bd6-8acf-5624f5dad26a");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "266ba8eb-3fb5-4a2b-8833-6d179e866c0f");
        if (getGraph() instanceof InfGraph) {
            writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "6448508c-0acf-490b-a1db-6ef6424c24b6");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "188fb8cd-fa3a-46ee-ae6b-b877ccd2bb4e");
        if (getGraph() instanceof InfGraph) {
            writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "6f79428b-ed0f-4588-8e01-c040ea1d2591");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "5545382f-93d9-4f4f-a977-240679a98936");
        if (m_deductionsModel == null) {
            writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "caf4f1a3-a220-4c09-bae6-4e1594745587");
            InfGraph infGraph = getInfGraph();
            writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "06abd355-3908-471e-9b43-c279fc1c3f75");
            if (infGraph != null) {
                writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "b9c6b342-7598-4b46-b41c-7f79c5ab2cf3");
                Graph deductionsGraph = infGraph.getDeductionsGraph();
                writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "e33bd5c7-7d60-4645-bb46-31a293e24984");
                if (deductionsGraph != null) {
                    writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "a6e3f00d-fc1f-4c62-a4f1-f4d023adfa21");
                    m_deductionsModel = ModelFactory.createModelForGraph(deductionsGraph);
                }
            }
        } else {
            writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "88aac62c-5a70-4dcc-8252-59434b5f81c6");
            // ensure that the cached model sees the updated changes from the
            // underlying reasoner graph
            getInfGraph().prepare();
        }
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "a40fc4fa-00ef-497d-b573-29521ae0feb1");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "aa4ade60-d7a7-4b0e-ac41-10dfc5a37ebc");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "4d3129a3-b0f3-47ba-84c4-2a9d803a34f8");
        if (getGraph() instanceof InfGraph) {
            writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "f8bbe522-82f5-44b7-b203-15db78a91edc");
            Graph gp = posit == null ? ModelFactory.createDefaultModel().getGraph() : posit.getGraph();
            writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "d5317e98-bd36-4ae3-86ab-7e87c0987b1b");
            Iterator<Triple> iter = getInfGraph().find(asNode(subject), asNode(predicate), asNode(object), gp);
            writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "b4799d57-1355-4c2c-abe4-2b3dff820373");
            return IteratorFactory.asStmtIterator(iter, this);
        } else {
            writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "5c60f71c-17e6-4ce3-8d59-ebfc1a944961");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "80539818-694c-48d2-b85d-9b2850e4bf64");
        if (getGraph() instanceof InfGraph) {
            writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "f6155437-014b-4dec-b49b-f785da1afd08");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "8911e84c-0d06-4dbb-b56f-1874078d4033");
        return (getGraph() instanceof InfGraph) ? ((InfGraph) getGraph()).getDerivation(statement.asTriple()) : null;
    }

    // Internal implementation methods
    // ////////////////////////////////
    private static void initSyntaxCheckerClass() {
        writelineStatic("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "9ddfcb18-313c-44a2-84e4-8cede5f5f9e3");
        if (owlSyntaxCheckerClass == null) {
            writelineStatic("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "0c538cbf-0827-4eaa-8623-662992f2be62");
            try {
                writelineStatic("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "45c2d5f1-a781-46c7-8e72-52e2200ebdaf");
                owlSyntaxCheckerClass = Class.forName(owlSyntaxCheckerClassName);
                writelineStatic("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "0dbf2aa5-f2af-42aa-86f5-8bf73ac04f99");
                owlSyntaxCheckerClass.newInstance();
            } catch (Exception e) {
                writelineStatic("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "3f3c08d2-c754-4f82-abbe-0a317a39517d");
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
        writelineStatic("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "b14d8bff-7768-411e-88e0-a239d536f797");
        // create a empty union graph
        MultiUnion u = new MultiUnion();
        writelineStatic("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "db484207-c478-40f1-9e66-767ad4ab9fdf");
        u.addGraph(base);
        writelineStatic("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "959488d0-5e21-45f0-8e9a-149a4771a5a0");
        u.setBaseGraph(base);
        writelineStatic("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "e352e6e6-b292-4ce1-ae97-a2b1e80a2160");
        Reasoner r = spec.getReasoner();
        writelineStatic("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "80ef7768-63a9-478f-a0b8-68ced6f0d6aa");
        // if we have a reasoner in the spec, bind to the union graph and return
        return r == null ? (Graph) u : r.bind(u);
    }

    /**
     * <p>Answer the union graph that contains the imports closure for this ontology</p>
     * @return The union graph
     */
    protected MultiUnion getUnionGraph() {
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "d6e2b2e9-f16c-4d6f-bb8f-e3b393a643e7");
        return m_union;
    }

    /**
     * Answer the resource with the given URI, if present, as the given facet
     */
    protected <T extends Resource> Resource findByURIAs(String uri, Class<T> asKey) {
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "6b9f1adc-7779-4414-af9c-48b9bc79b8ec");
        if (uri == null) {
            writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "ec6648c7-cf3b-4eec-a778-845cd0fd9577");
            throw new IllegalArgumentException("Cannot get() ontology value with a null URI");
        }
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "265ca6ec-5ed1-41d1-ba4f-67d6b27d849f");
        Node n = NodeFactory.createURI(uri);
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "15064f10-c394-434a-b11f-16f18bd0911a");
        if (getGraph().contains(n, Node.ANY, Node.ANY)) {
            writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "808c24ae-7a6b-4810-8e98-bdef95272db0");
            // this resource is a subject in the graph
            try {
                writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "33984843-a161-43da-9d46-8db81b20f301");
                return getNodeAs(n, asKey);
            } catch (ConversionException ignore) {
            /**/
            }
        }
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "32d4548d-9195-4d95-b306-1e51d6d63fef");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "df64531d-73fd-4115-9ea1-79f36ac11da4");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "ff42ed15-3fa6-4d0c-8a57-bd0eee75907d");
        ExtendedIterator<Triple> i = findByType(type);
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "29d61f26-b10f-4b98-8c75-4108900835bc");
        // compose onto i the find iterators for the alternate types
        if (alternates != null) {
            writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "0b4dfc65-17b3-4f42-bd23-9428604a4aff");
            while (alternates.hasNext()) {
                writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "84010594-50a2-41a7-9894-a2582286de49");
                i = i.andThen(findByType(alternates.next()));
            }
        }
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "124fc637-7ada-4564-94f6-660a8cdb6706");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "0a8cad88-d650-40e9-b4ae-234522f6a414");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "bb761a17-294b-4dc2-820c-925d10acd4dd");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "109098b5-1eaa-4ec8-8c3b-122e2bbdff6d");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "987ae517-8215-4f25-bc30-828bcedba2ee");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "ffac537b-e1c8-4ef8-918b-c2bc3eae681b");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "e87cfdbc-29de-4e21-a8dc-9469848b2a40");
        Resource r = getResource(uri);
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "53dde035-411b-4669-8fb0-8314f7ebfec2");
        if (rdfType != null) {
            writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "f763dbca-4535-40fc-8875-c5eab71fc8fb");
            r.addProperty(RDF.type, rdfType);
        }
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "8bc7b038-eb92-4069-b1fc-73c94b8d2bde");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "0ab98d68-e727-40f5-a52c-780f1aa2479f");
        Resource r = getResource(uri);
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "d6d3895b-a365-4088-ba50-31b120fff5fd");
        if (containsResource(r)) {
            writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "818ef77c-e177-4ba7-bbb3-97eb7ca44985");
            return r.as(OntResource.class);
        }
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "d043d2db-d353-49ea-85cb-3d7a48ed9362");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "e5af3932-9fce-4ce6-8d70-99979f707f8a");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "19a98b16-2fb0-4365-a024-13e0ae400f6c");
        if (profileTerm == null) {
            writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "47afa8b6-adbd-4e7f-a870-10c99c89a890");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "c624b4e5-2362-41b9-8ef4-da175c1e1a6a");
        if (strictMode() && !((Boolean) list.reduce(new RdfTypeTestFn(rdfType), Boolean.TRUE)).booleanValue()) {
            writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "60d1ac49-2525-4399-b9db-823e25c7eece");
            // not all of the members of the list are of the given type
            throw new LanguageConsistencyException("The members of the given list are expected to be of rdf:type " + rdfType.toString());
        }
    }

    /**
     * Answer the supplied model, unless it's null, in which case answer a new model
     * constructed as per spec.
     */
    private static Model makeBaseModel(OntModelSpec spec, Model model) {
        writelineStatic("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "0222919e-125b-42b1-92af-f6cc724aa7de");
        return model == null ? spec.createBaseModel() : model;
    }

    /**
     * <p>Answer the InfGraph that this model is wrapping, or null if this ontology
     * model is not wrapping an inf graph.</p>
     * @return The model's graph as an InfGraph, or null
     */
    private InfGraph getInfGraph() {
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "83112a7e-5065-48c0-83ef-9ca311a658c9");
        return (getGraph() instanceof InfGraph) ? ((InfGraph) getGraph()) : null;
    }

    /**
     * Test for whether we ignore <code>file:</code> URI's when testing for content
     * negotiation.
     * @param source
     * @return
     */
    protected boolean ignoreFileURI(String source) {
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "3262358d-5b56-4525-877a-3d2799e8c30e");
        return source.startsWith("file:");
    }

    /* delegation points to allow unit testing of read operations */
    protected Model readDelegate(String url) {
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "1378c5fb-a72f-41d1-9cfa-dfaec7ce5b37");
        return super.read(url);
    }

    protected Model readDelegate(String url, String lang) {
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "744d4d6b-2a85-479b-8201-a2c886add601");
        return super.read(url, lang);
    }

    protected Model readDelegate(String url, String base, String lang) {
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_10_10.coverage", "59121bc5-d30f-4de9-9e4e-68df8e4ff2b0");
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
