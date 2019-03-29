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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "067e56ab-936c-499d-8a91-989cd951e175");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "62aab677-21c4-41cd-8f75-398c2e221393");
        checkProfileEntry(getProfile().ONTOLOGY(), "ONTOLOGY");
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "32c0ce2c-9844-4f09-99de-e6709ffc4f78");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "67d2e7fb-e7b5-4b62-9deb-d7a8992bbc2e");
        ExtendedIterator<OntProperty> i = findByTypeAs(RDF.Property, OntProperty.class).filterKeep(new UniqueFilter<OntProperty>());
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "78a3f02e-45c3-4f3c-a43d-1acbda9211e9");
        // if we are in OWL_FULL, the properties should also include the annotation properties
        if (getReasoner() != null && getProfile().equals(ProfileRegistry.getInstance().getProfile(ProfileRegistry.OWL_LANG))) {
            writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "527017a9-35d8-4b42-92bf-b3eb9c197128");
            // we are using a reasoner, and in OWL Full
            // so add the annotation properties too
            i = i.andThen(listAnnotationProperties());
        }
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "6834db6e-accb-4361-b1fb-9a215c9eb8f5");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "1b073e81-6925-43ec-bd80-c1f2b82dd112");
        ExtendedIterator<OntProperty> i = findByTypeAs(RDF.Property, OntProperty.class).andThen(listObjectProperties()).andThen(listDatatypeProperties()).andThen(listAnnotationProperties()).andThen(listFunctionalProperties()).andThen(listTransitiveProperties()).andThen(listSymmetricProperties());
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "f979c190-bd72-4455-b83d-622e6f638291");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "6b40523e-2bef-4563-801e-e64d1e347d3c");
        checkProfileEntry(getProfile().OBJECT_PROPERTY(), "OBJECT_PROPERTY");
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "4b257f93-c53c-4c03-9ad2-325ca0492f7b");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "8479807e-bafa-4062-adca-0032d36a69fc");
        checkProfileEntry(getProfile().DATATYPE_PROPERTY(), "DATATYPE_PROPERTY");
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "54c13617-8634-44c0-b51a-ec12c26c2758");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "2e9aedb7-cccd-4f97-8ab9-e65a60aa20e3");
        checkProfileEntry(getProfile().FUNCTIONAL_PROPERTY(), "FUNCTIONAL_PROPERTY");
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "83ba47f0-8b19-4617-a4bb-fa7b12b203d0");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "fa9bbda4-05bc-4b59-8868-2697706c7c5b");
        checkProfileEntry(getProfile().TRANSITIVE_PROPERTY(), "TRANSITIVE_PROPERTY");
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "29913b79-bec1-4aaa-a011-d777b8f2602a");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "1ff8e337-0799-468c-8afc-ee20651694fa");
        checkProfileEntry(getProfile().SYMMETRIC_PROPERTY(), "SYMMETRIC_PROPERTY");
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "c0f555dc-c369-45e5-ae4d-89fe4b3e9eb4");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "117615c0-696b-40cd-9504-10f00c8a5e7e");
        checkProfileEntry(getProfile().INVERSE_FUNCTIONAL_PROPERTY(), "INVERSE_FUNCTIONAL_PROPERTY");
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "80247683-e4c9-4beb-9d5c-5cc8bf6017d4");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "6fbc1388-3bba-42fe-ba1b-0d207cdd60b5");
        // since the reasoner implements some OWL full functionality for RDF compatibility, we
        // have to decide which strategy to use for identifying individuals depending on whether
        // or not a powerful reasoner (i.e. owl:Thing/daml:Thing aware) is being used with this model
        boolean supportsIndAsThing = false;
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "66da0068-2c7f-4ddc-beb1-833e926c9006");
        if (getGraph() instanceof InfGraph) {
            writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "36d52efe-7a6c-457c-a7f5-85c76137af66");
            supportsIndAsThing = ((InfGraph) getGraph()).getReasoner().getReasonerCapabilities().contains(null, ReasonerVocabulary.supportsP, ReasonerVocabulary.individualAsThingP);
        }
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "19102c3f-98cc-488b-bb00-2e5275d86754");
        if (!supportsIndAsThing || (getProfile().THING() == null) || getProfile().CLASS().equals(RDFS.Class)) {
            writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "42bd8856-2bd2-4f40-8452-70288911725f");
            // no inference, or we are in RDFS land, so we pick things that have rdf:type whose rdf:type is Class
            // it's tricky to make this efficient and cover all possible cases. I've changed the code to
            // make use of the isIndividual() test on OntResource, at the expense of some redundant queries
            // to the model, which could become expensive in the case of a DB model - ijd Apr-23-09
            Set<Individual> results = new HashSet<Individual>();
            writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "f84d09f1-4004-4326-b610-ead3773379b1");
            for (Iterator<Statement> i = listStatements(null, RDF.type, (RDFNode) null); i.hasNext(); ) {
                writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "a38da76d-997c-47cd-b3be-e4172d6f3fdf");
                OntResource r = i.next().getSubject().as(OntResource.class);
                writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "7e696a59-7400-4f85-aa8b-cf5200ddb320");
                if (r.isIndividual()) {
                    writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "bba1bb76-acbb-450f-9283-c21b44c5ab51");
                    results.add(r.as(Individual.class));
                }
            }
            writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "27a8bdf3-2357-4dd9-8b32-d54b77a81543");
            return WrappedIterator.create(results.iterator());
        } else {
            writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "a043f4a0-e4ca-49b5-b29a-f484e451811c");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "8ec67791-1543-4747-aa35-c573cac74039");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "74dfb648-f920-4ff4-a186-0fd68e7fb3f6");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "e877d940-ef17-4615-b471-9d40a571c721");
        // look for the shortcut of using direct subClass on :Thing
        if (getReasoner() != null) {
            writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "d32dd925-0d8c-4354-a561-2d1e540c71c4");
            Model conf = getReasoner().getReasonerCapabilities();
            writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "6c8a91d6-b69b-4533-8e38-4cc71af49456");
            if (conf != null && conf.contains(null, ReasonerVocabulary.supportsP, ReasonerVocabulary.directSubClassOf) && getProfile().THING() != null) {
                writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "99d597c5-7cac-4db1-99b6-29184da9c84e");
                // we have have both direct sub-class of and a :Thing class to test against
                return listStatements(null, ReasonerVocabulary.directSubClassOf, getProfile().THING()).mapWith(s -> s.getSubject().as(OntClass.class));
            }
        }
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "978f040a-b1fa-4172-9870-3ab40fa5fb40");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "d2e2cfc0-e5cc-4361-b595-130ca73bf89c");
        checkProfileEntry(getProfile().ONE_OF(), "ONE_OF");
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "e13d8d41-b238-4b17-92ad-59e7350016ef");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "654e0035-77f1-4718-9d34-ff34b5e6ca62");
        checkProfileEntry(getProfile().UNION_OF(), "UNION_OF");
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "c73659cf-2e08-4b7b-96bc-06b4fc5a3fa9");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "adab6f6f-8b74-4c42-b758-50830171cb10");
        checkProfileEntry(getProfile().COMPLEMENT_OF(), "COMPLEMENT_OF");
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "d7dad550-4fe4-4ef8-82e8-6dd9463dc1b8");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "7eb28558-97eb-47f8-b532-e0cea1cfcb8d");
        checkProfileEntry(getProfile().INTERSECTION_OF(), "INTERSECTION_OF");
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "5c3c1278-bbe5-4593-88c7-ed19a29c8d8f");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "af704a2a-1ffd-4b07-9ddb-eecb47aa639e");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "24baa489-93e9-45fa-9ed5-d427d03ee9be");
        checkProfileEntry(getProfile().RESTRICTION(), "RESTRICTION");
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "c6443d10-144d-44e5-91f8-aeca40aeba7d");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "1d3c682d-4f45-4d94-8821-73deea058df9");
        checkProfileEntry(getProfile().ALL_DIFFERENT(), "ALL_DIFFERENT");
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "e03ce402-79bb-45c0-a7e0-c03ab9a0e340");
        return findByTypeAs(getProfile().ALL_DIFFERENT(), AllDifferent.class).filterKeep(new UniqueFilter<AllDifferent>());
    }

    /**
     * <p>Answer an iterator over the DataRange objects in this ontology, if there
     * are any.</p>
     * @return An iterator, whose values are {@link DataRange} objects.
     */
    @Override
    public ExtendedIterator<DataRange> listDataRanges() {
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "192f11c1-5a12-4d6e-b6ae-7201ca7f69bb");
        checkProfileEntry(getProfile().DATARANGE(), "DATARANGE");
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "07b44eb2-2f3e-491c-9016-56da7341de68");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "232dfc42-dc2e-4048-9334-34cd01194089");
        checkProfileEntry(getProfile().ANNOTATION_PROPERTY(), "ANNOTATION_PROPERTY");
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "51306299-6741-4b3e-834f-c44544695224");
        Resource r = getProfile().ANNOTATION_PROPERTY();
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "996169be-e2b9-431a-b58f-a6241921a686");
        if (r == null) {
            writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "d4b17900-9630-4a96-946d-9a3241b2089f");
            return new NullIterator<AnnotationProperty>();
        } else {
            writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "967986b3-01ff-4b19-8974-dddae6134166");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "6a7c6cea-2c7a-4a24-9c44-1ff15c530a40");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "f14e6e1c-c258-429e-bbe4-34b1d933029f");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "92024914-1e7f-4476-966d-00b6847e5a46");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "e83af179-3293-49a4-bee0-b6b6eb0f8e78");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "008cfcb7-c6c8-4082-9444-8f98f71abb90");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "fc66d454-9739-43b6-bc47-2242d9c1625f");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "a2a6acdf-fca3-4dbe-b402-f74bb52175e3");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "05bee3f2-195b-47cc-a7b1-464c9b23a94a");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "cf631a97-0411-4cfd-a505-30c469f4401d");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "e68ac4dc-a629-44bc-93b8-0bc3897870e0");
        OntClass c = (OntClass) findByURIAs(uri, OntClass.class);
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "9127c866-df8f-401f-8a1e-0a3960f1e25a");
        // special case for nothing and thing
        if (c == null) {
            writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "08c6b7a9-321a-49e3-bde6-d4cab7668b05");
            Resource thing = getProfile().THING();
            writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "411caaa8-e6de-40bf-a38f-a85a99a774dc");
            if (thing != null && thing.getURI().equals(uri)) {
                writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "44221026-3b5a-4654-a265-9c8610849574");
                c = thing.inModel(this).as(OntClass.class);
            }
            writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "6cdc2f07-5361-45ab-9f39-1c3460401999");
            Resource nothing = getProfile().NOTHING();
            writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "f4f60b57-5036-4095-b7f3-9775d09d3f16");
            if (nothing != null && nothing.getURI().equals(uri)) {
                writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "2e0dfe0e-da32-486c-b76b-45a9bea29c75");
                c = nothing.inModel(this).as(OntClass.class);
            }
        }
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "4d997c89-ab46-4290-9891-1f60c114a949");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "9c145176-4d32-4af5-8e60-0845be8023a3");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "0316b128-ffe5-4a0b-9e07-35a6d49f349c");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "e0c97bbb-e7c9-4e88-a601-b600d42efe27");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "72b7afd9-bd72-409b-b8c0-72354291314b");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "bf9eb4bb-1b5c-45c2-a4d7-d4e18d9a8240");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "f8544ec3-4a0d-42a5-b961-a04d47d74075");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "0acfc80a-3e3d-47f9-91a1-781266bfcb9f");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "a80807ad-2a3e-4ce1-853a-92aee85e90a8");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "d0a8f125-ccf8-4a67-8a2d-31873c584958");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "06ea7d5b-6573-45d4-a7df-b5e0abcd257a");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "244e165f-2160-456c-847f-f48e5551aee7");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "227a423b-caeb-4da8-8b6b-5bbd942464bb");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "85d55e28-da62-4f57-9707-77ad68fd6176");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "cf2f075b-30de-42fb-8b9a-b03f174be05d");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "1c3754ea-695c-4ec2-af6c-d5bb9a6bec90");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "8b117f4a-7d3c-4594-8b63-43c57aac9825");
        checkProfileEntry(getProfile().ONTOLOGY(), "ONTOLOGY");
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "36a89b17-bb52-4fbf-a729-d7366e8aaf74");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "026024a8-59c3-439f-9f9f-822805de8405");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "4708932e-ef3c-47ea-a0f7-57875e130520");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "bb52f81a-2524-4380-a6ee-981557865e7f");
        Property p = createProperty(uri);
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "c54bb25e-ce67-4347-813f-4d70ab1e6ec8");
        p.addProperty(RDF.type, getProfile().PROPERTY());
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "84600376-d02a-4c03-95a9-f20e0bbdd884");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "ea6719c9-d9f9-49e4-9225-cb70c4bad39f");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "c79fe533-12eb-47a1-b566-52ebd8d4272f");
        checkProfileEntry(getProfile().OBJECT_PROPERTY(), "OBJECT_PROPERTY");
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "17aeaaac-b65e-424a-b470-0324eded2ad0");
        ObjectProperty p = createOntResource(ObjectProperty.class, getProfile().OBJECT_PROPERTY(), uri);
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "a724a4e6-39aa-4df7-a373-7fc77ee7ea4d");
        if (functional) {
            writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "faa50078-5520-4dde-8953-caf654819ccf");
            checkProfileEntry(getProfile().FUNCTIONAL_PROPERTY(), "FUNCTIONAL_PROPERTY");
            writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "9d31fc1a-7f69-4180-8817-dc1e66abba6b");
            p.addProperty(RDF.type, getProfile().FUNCTIONAL_PROPERTY());
        }
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "98123e49-9b22-406a-b685-28bc0398874a");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "edd9cc24-36a5-4ec7-8a25-a907e683726e");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "98a0fc6c-2c7b-4af3-860e-44613edd3865");
        checkProfileEntry(getProfile().TRANSITIVE_PROPERTY(), "TRANSITIVE_PROPERTY");
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "6126f21a-d329-4895-9cbb-6642e2a6b503");
        TransitiveProperty p = createOntResource(TransitiveProperty.class, getProfile().TRANSITIVE_PROPERTY(), uri);
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "9a05cce8-654b-4a18-98b5-f95ab71def78");
        if (functional) {
            writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "98b6bf91-260a-490d-b29e-9ae747af2c7e");
            checkProfileEntry(getProfile().FUNCTIONAL_PROPERTY(), "FUNCTIONAL_PROPERTY");
            writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "f44ae589-052d-4f7e-a8b7-f5eca8849d13");
            p.addProperty(RDF.type, getProfile().FUNCTIONAL_PROPERTY());
        }
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "f200d6c3-b256-48e9-871e-aab21a1dc717");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "8426c221-7543-4c8b-ab78-cdff29d59351");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "43110e49-a703-4864-9358-067733c01ff9");
        checkProfileEntry(getProfile().SYMMETRIC_PROPERTY(), "SYMMETRIC_PROPERTY");
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "983e6a7a-f8e1-413a-b024-28075cfae170");
        SymmetricProperty p = createOntResource(SymmetricProperty.class, getProfile().SYMMETRIC_PROPERTY(), uri);
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "aea2a888-6a7d-466a-8838-7290a3fe41a1");
        if (functional) {
            writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "f2a58486-ad31-45fd-8695-31dda3986ec7");
            checkProfileEntry(getProfile().FUNCTIONAL_PROPERTY(), "FUNCTIONAL_PROPERTY");
            writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "e6e9a28a-0291-4242-a89c-c6d5cfa53156");
            p.addProperty(RDF.type, getProfile().FUNCTIONAL_PROPERTY());
        }
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "4b35a46f-76c4-4ce7-8576-e14e471d2ed9");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "59e42c56-208f-479e-adfe-cd1281735358");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "a2f4b825-5184-45ba-ba5d-5974edb828df");
        checkProfileEntry(getProfile().INVERSE_FUNCTIONAL_PROPERTY(), "INVERSE_FUNCTIONAL_PROPERTY");
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "47c6d7a5-e2c4-49b1-bfd0-c22121514353");
        InverseFunctionalProperty p = createOntResource(InverseFunctionalProperty.class, getProfile().INVERSE_FUNCTIONAL_PROPERTY(), uri);
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "54addc9e-f816-4842-92f7-0e60343e7ccf");
        if (functional) {
            writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "e330c135-a95e-4272-b45c-6c1dc6c55a37");
            checkProfileEntry(getProfile().FUNCTIONAL_PROPERTY(), "FUNCTIONAL_PROPERTY");
            writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "67b20fd3-7c44-4dc7-b3f4-98d5930042a4");
            p.addProperty(RDF.type, getProfile().FUNCTIONAL_PROPERTY());
        }
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "45d8535e-6d0f-498c-a52e-52b68daa7536");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "c5c87761-cf55-4282-a92c-ae2267a8de4a");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "94c3121d-6e1b-40cd-9b6e-266cd97b1f46");
        checkProfileEntry(getProfile().DATATYPE_PROPERTY(), "DATATYPE_PROPERTY");
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "271bac23-44e7-4753-9f56-b8d4ebbc53df");
        DatatypeProperty p = createOntResource(DatatypeProperty.class, getProfile().DATATYPE_PROPERTY(), uri);
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "3be85bde-5b51-43de-8b25-8d49b83699ab");
        if (functional) {
            writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "a2a2fce5-2eab-43d7-92ce-92089455d6e6");
            checkProfileEntry(getProfile().FUNCTIONAL_PROPERTY(), "FUNCTIONAL_PROPERTY");
            writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "258c61bc-8800-4981-8460-c1077b49961a");
            p.addProperty(RDF.type, getProfile().FUNCTIONAL_PROPERTY());
        }
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "35bf53ec-c832-4758-af05-4c548bcbcaeb");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "7ab5c93a-23cf-40d1-9b74-a852123b65f8");
        checkProfileEntry(getProfile().ANNOTATION_PROPERTY(), "ANNOTATION_PROPERTY");
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "944ed94e-fab5-4593-a3ee-b46b3b215c56");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "a7e23058-91fa-4728-a52f-74e3f998a723");
        checkProfileEntry(getProfile().CLASS(), "CLASS");
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "c72fa316-2ab2-44ac-a9f6-1fc720e4cb0d");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "e66e71cd-9f28-4d9c-8e10-ba7032d188d7");
        checkProfileEntry(getProfile().CLASS(), "CLASS");
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "8f723376-dbef-47d2-b045-a92916124738");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "0a15164b-b43f-44eb-9e72-15bfc4b5e7b1");
        checkProfileEntry(getProfile().CLASS(), "CLASS");
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "0b5847f7-075d-42b2-ba6c-b32f216ab506");
        OntClass c = createOntResource(OntClass.class, getProfile().CLASS(), uri);
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "fcf3256c-8d23-4ba8-974d-6e2ab785c64a");
        checkProfileEntry(getProfile().COMPLEMENT_OF(), "COMPLEMENT_OF");
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "6d5f426f-5acd-48db-84d1-c1d85c34cb64");
        // if the class that this class is a complement of is not specified, use owl:nothing or daml:nothing
        c.addProperty(getProfile().COMPLEMENT_OF(), (cls == null) ? getProfile().NOTHING() : cls);
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "eaf30486-13eb-4238-8b84-36effe7e07fb");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "05929453-7150-4376-82fc-1ace422c0c3f");
        checkProfileEntry(getProfile().CLASS(), "CLASS");
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "227acdb2-3d95-40a5-9a84-94757bdcd532");
        OntClass c = createOntResource(OntClass.class, getProfile().CLASS(), uri);
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "1dbc89b6-57a2-4f28-81c8-d81a34c2bd9a");
        checkProfileEntry(getProfile().ONE_OF(), "ONE_OF");
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "62c35cee-2ff0-49de-bf67-f0760659be8e");
        c.addProperty(getProfile().ONE_OF(), (members == null) ? createList() : members);
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "0b619d14-9500-4f85-9bea-0c6a41a9346f");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "a16dd9cf-c854-48c0-85b0-3ea0f4cf1f7f");
        checkProfileEntry(getProfile().CLASS(), "CLASS");
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "ab5dc47b-d2da-475f-af7e-d0e1d652d756");
        OntClass c = createOntResource(OntClass.class, getProfile().CLASS(), uri);
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "91034465-2854-4bd6-aace-d784759c2d6b");
        checkProfileEntry(getProfile().UNION_OF(), "UNION_OF");
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "dd5ecb35-b4e3-4e2d-9c1d-2413727fe7e4");
        c.addProperty(getProfile().UNION_OF(), (members == null) ? createList() : members);
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "a2336156-0a48-43e1-a0ab-6b774dea16ce");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "eb940bd5-d9f5-4c45-a078-feb8f1b19002");
        checkProfileEntry(getProfile().CLASS(), "CLASS");
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "bc0afe2a-b3db-4dba-853b-5890b57b5883");
        OntClass c = createOntResource(OntClass.class, getProfile().CLASS(), uri);
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "e0c14cee-a785-4d51-bc36-62e121f2548a");
        checkProfileEntry(getProfile().INTERSECTION_OF(), "INTERSECTION_OF");
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "aa503b80-b261-4274-b807-9f151aee6bea");
        c.addProperty(getProfile().INTERSECTION_OF(), (members == null) ? createList() : members);
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "8e228992-d099-4b93-8186-5717f00a21c7");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "df10a686-a654-4b48-8990-c95fd8d54eb4");
        checkProfileEntry(getProfile().RESTRICTION(), "RESTRICTION");
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "cbfe46ea-7195-4ea3-a320-fffd553d5be9");
        Restriction r = createOntResource(Restriction.class, getProfile().RESTRICTION(), null);
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "4089ef96-d626-4aca-9dae-5aeb67123c9b");
        if (p != null) {
            writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "717be3d5-4c36-4e45-b9ca-e8fa11fea0d8");
            r.setOnProperty(p);
        }
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "a1eef96a-fe0a-48a4-889a-c1fdb3f2a79f");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "d9d2629d-0726-4e7e-a016-fdab1f919a58");
        checkProfileEntry(getProfile().RESTRICTION(), "RESTRICTION");
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "833a23cd-6210-4d5d-b4ca-269e90dccd1b");
        Restriction r = createOntResource(Restriction.class, getProfile().RESTRICTION(), uri);
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "ee1a80cf-6ede-43bb-bd9e-99768a6a06cb");
        if (p != null) {
            writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "40d18bf1-4001-4cea-beea-4052f04f9caf");
            r.setOnProperty(p);
        }
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "c300bcec-b675-42cc-afd6-22afac1d6a2f");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "34abc4b6-d0d2-42f4-acad-444a6963fc40");
        checkProfileEntry(getProfile().RESTRICTION(), "RESTRICTION");
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "d0566f78-4bf0-4123-b71a-2c903437cd04");
        Restriction r = createOntResource(Restriction.class, getProfile().RESTRICTION(), uri);
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "83dd81fe-b718-4268-8227-720fcfbb4c5a");
        if (prop == null || value == null) {
            writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "d97a86cf-dda1-4e7a-bc4d-543f95b4e3e0");
            throw new IllegalArgumentException("Cannot create hasValueRestriction with a null property or value");
        }
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "164da771-f9bf-4eb3-956e-97b9444001ea");
        checkProfileEntry(getProfile().HAS_VALUE(), "HAS_VALUE");
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "02b0c387-4f5c-43d9-98ee-bd74e7d5daba");
        r.addProperty(getProfile().ON_PROPERTY(), prop);
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "297f3f79-2e4f-4ce5-b4e7-7dd4cdecb66d");
        r.addProperty(getProfile().HAS_VALUE(), value);
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "28bdfcd3-5fdb-426e-a9bc-82dc7d787ee8");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "4a15db1d-1553-4142-a1a0-3a3b230b2736");
        checkProfileEntry(getProfile().RESTRICTION(), "RESTRICTION");
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "ea7f347c-ae27-4795-ab71-413a056e503e");
        Restriction r = createOntResource(Restriction.class, getProfile().RESTRICTION(), uri);
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "2f053a21-89ee-4554-a7bb-4f34513f909d");
        if (prop == null || cls == null) {
            writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "151d903d-5086-45bc-a004-f15a2df21251");
            throw new IllegalArgumentException("Cannot create someValuesFromRestriction with a null property or class");
        }
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "cd26fcb9-f8a3-4bd0-966f-55627cb19a06");
        checkProfileEntry(getProfile().SOME_VALUES_FROM(), "SOME_VALUES_FROM");
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "1c1fd385-1575-4bb5-b96a-54e1bbd07781");
        r.addProperty(getProfile().ON_PROPERTY(), prop);
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "7d69eebd-0dc5-4a4d-837d-621863b390dc");
        r.addProperty(getProfile().SOME_VALUES_FROM(), cls);
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "adf2bdb0-af3e-4a3b-a0f0-d47041b83ef8");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "a81ac8bd-3cca-437f-b367-f09cf3e86390");
        checkProfileEntry(getProfile().RESTRICTION(), "RESTRICTION");
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "05b26b3f-6325-44c8-a249-55a47a259559");
        Restriction r = createOntResource(Restriction.class, getProfile().RESTRICTION(), uri);
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "29ff05c1-9fb5-4f52-abca-b6ae9b737779");
        if (prop == null || cls == null) {
            writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "62ecba2a-8dd3-4c20-bc4c-33b5464f14b9");
            throw new IllegalArgumentException("Cannot create allValuesFromRestriction with a null property or class");
        }
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "024595f8-61b1-4a7f-b379-8057c09444c5");
        checkProfileEntry(getProfile().ALL_VALUES_FROM(), "ALL_VALUES_FROM");
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "2ea719a5-4061-46fd-9d1f-9c0d5908f85e");
        r.addProperty(getProfile().ON_PROPERTY(), prop);
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "9b32cfd4-94a4-48e6-aad2-294ee7293cd3");
        r.addProperty(getProfile().ALL_VALUES_FROM(), cls);
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "886b5812-138a-4891-aeb8-c3d3513e3c2e");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "9f929612-bf9e-4da4-af83-106c88ee505c");
        checkProfileEntry(getProfile().RESTRICTION(), "RESTRICTION");
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "91048421-cf7c-40d7-b66a-26268094c604");
        Restriction r = createOntResource(Restriction.class, getProfile().RESTRICTION(), uri);
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "72444cba-137b-43bb-b901-f141e6640142");
        if (prop == null) {
            writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "ee14a952-d8be-40f6-8904-38777b768cf1");
            throw new IllegalArgumentException("Cannot create cardinalityRestriction with a null property");
        }
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "002f8e9b-55b8-4fa8-9b65-f665e0829938");
        checkProfileEntry(getProfile().CARDINALITY(), "CARDINALITY");
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "627f7a1a-b9a1-4df3-90be-226ceef92191");
        r.addProperty(getProfile().ON_PROPERTY(), prop);
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "b56acace-dcae-4e88-8229-b5b2dca711c3");
        r.addProperty(getProfile().CARDINALITY(), createTypedLiteral(cardinality));
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "385ebb15-5072-4999-b547-781041808667");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "61764c31-1359-402b-ab5e-1ed15213c22f");
        checkProfileEntry(getProfile().RESTRICTION(), "RESTRICTION");
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "07c77f48-5fed-4455-9d81-18e69fcbf124");
        Restriction r = createOntResource(Restriction.class, getProfile().RESTRICTION(), uri);
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "bc1841c9-3d5e-48b1-82b7-c1d84ea6c120");
        if (prop == null) {
            writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "9276427d-f430-4dea-94c6-4de9087a29fb");
            throw new IllegalArgumentException("Cannot create minCardinalityRestriction with a null property");
        }
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "383bbf19-ebee-4124-aef4-463c98f7d911");
        checkProfileEntry(getProfile().MIN_CARDINALITY(), "MIN_CARDINALITY");
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "a5272949-ff2a-409a-a853-4b3dc068046a");
        r.addProperty(getProfile().ON_PROPERTY(), prop);
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "068c652b-4b18-49f7-b912-1e4ca197aa61");
        r.addProperty(getProfile().MIN_CARDINALITY(), createTypedLiteral(cardinality));
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "d2047aa5-b620-4483-a94f-5d4003e92da6");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "fb49e7d4-dc1d-4ea6-9cce-b6407f5289ba");
        checkProfileEntry(getProfile().RESTRICTION(), "RESTRICTION");
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "17454f0a-86a8-4130-8a95-cef8c56a60cf");
        Restriction r = createOntResource(Restriction.class, getProfile().RESTRICTION(), uri);
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "270df34a-8165-42ca-9949-b850701b6d33");
        if (prop == null) {
            writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "191caa44-75e2-4673-8d33-f238fba20825");
            throw new IllegalArgumentException("Cannot create maxCardinalityRestriction with a null property");
        }
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "8171adbe-d1fe-4910-b0d0-29bb5be66a07");
        checkProfileEntry(getProfile().MAX_CARDINALITY(), "MAX_CARDINALITY");
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "14b65940-e328-40ed-9e58-45676303267b");
        r.addProperty(getProfile().ON_PROPERTY(), prop);
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "3af3294e-2713-4507-8492-00e512ad7b65");
        r.addProperty(getProfile().MAX_CARDINALITY(), createTypedLiteral(cardinality));
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "b7f529a1-8d5f-45a9-9d22-e159de7c1de7");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "eadcade0-b512-4005-9c43-f746f498f40c");
        checkProfileEntry(getProfile().RESTRICTION(), "RESTRICTION");
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "17607f47-3486-461a-a531-18acb4cdb618");
        checkProfileEntry(getProfile().ON_PROPERTY(), "ON_PROPERTY");
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "f78f0678-5601-44f7-b252-73a11481d8a2");
        checkProfileEntry(getProfile().MAX_CARDINALITY_Q(), "MAX_CARDINALITY_Q");
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "4debb1c1-d10f-4900-98f0-7893d879d234");
        checkProfileEntry(getProfile().HAS_CLASS_Q(), "HAS_CLASS_Q");
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "0159c006-c385-46b5-992f-a4dcae4d0cfb");
        if (prop == null) {
            writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "788958bf-a925-454d-adad-df7ddccfbb27");
            throw new IllegalArgumentException("Cannot create MaxCardinalityQRestriction with a null property");
        }
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "c031b438-86b4-44b9-be94-edca3c094a3f");
        if (cls == null) {
            writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "16e968af-ca22-4c10-82a7-ed570ed1e694");
            throw new IllegalArgumentException("Cannot create MaxCardinalityQRestriction with a null class");
        }
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "fdae25e9-e7a3-4b59-a5d0-fbf28efc5e44");
        Restriction r = createOntResource(Restriction.class, getProfile().RESTRICTION(), uri);
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "49d80cd6-a82e-4ea3-913f-d5b6c2ad20c6");
        r.addProperty(getProfile().ON_PROPERTY(), prop);
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "bc4129aa-a948-464a-81b0-e30d3f273da5");
        r.addProperty(getProfile().MAX_CARDINALITY_Q(), createTypedLiteral(cardinality));
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "56d797f9-2d03-4ae9-a7be-62dd1de4ea34");
        r.addProperty(getProfile().HAS_CLASS_Q(), cls);
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "eac50c4d-166a-46a6-9a3a-93f4cbde62ab");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "167bef04-d476-4c72-ab0f-2e7e62345329");
        checkProfileEntry(getProfile().RESTRICTION(), "RESTRICTION");
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "42c5f060-454b-439b-97f1-780d141ce133");
        checkProfileEntry(getProfile().ON_PROPERTY(), "ON_PROPERTY");
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "8e891352-1aea-4d84-97b1-6a2ea13b40fc");
        checkProfileEntry(getProfile().MIN_CARDINALITY_Q(), "MIN_CARDINALITY_Q");
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "59e1357e-ae39-4e61-98a6-345dacc677c9");
        checkProfileEntry(getProfile().HAS_CLASS_Q(), "HAS_CLASS_Q");
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "0751b2d3-77b3-4244-befa-a53993e69ab2");
        if (prop == null) {
            writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "103c5c0c-3dc3-4bf6-844d-2e38d9cd6035");
            throw new IllegalArgumentException("Cannot create MinCardinalityQRestriction with a null property");
        }
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "71d68bf3-2619-4ad3-a64b-c8274105f4a1");
        if (cls == null) {
            writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "558f6527-db4e-4eeb-a495-a34e9d1772ca");
            throw new IllegalArgumentException("Cannot create MinCardinalityQRestriction with a null class");
        }
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "167a2380-3359-4ddc-a7b2-dafab597734a");
        Restriction r = createOntResource(Restriction.class, getProfile().RESTRICTION(), uri);
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "db5a7007-3f8a-4318-986c-f8d4c46e500e");
        r.addProperty(getProfile().ON_PROPERTY(), prop);
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "d9314e9a-1a80-4abc-8604-007559f4ef8c");
        r.addProperty(getProfile().MIN_CARDINALITY_Q(), createTypedLiteral(cardinality));
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "dc5dfd57-9096-451a-b416-7b7501141fc9");
        r.addProperty(getProfile().HAS_CLASS_Q(), cls);
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "f6c1b8c5-567e-4e28-bab7-188839decfcb");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "e3847493-83cd-4095-afa2-21ab9843e058");
        checkProfileEntry(getProfile().RESTRICTION(), "RESTRICTION");
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "4b7cd88e-0d7d-4838-8559-d10acabd913f");
        checkProfileEntry(getProfile().ON_PROPERTY(), "ON_PROPERTY");
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "ba49185b-3189-4d05-8315-5a0bceb8470d");
        checkProfileEntry(getProfile().CARDINALITY_Q(), "CARDINALITY_Q");
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "f70397df-368b-4e6e-8d4a-02bdd52c44e4");
        checkProfileEntry(getProfile().HAS_CLASS_Q(), "HAS_CLASS_Q");
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "6aac2572-f7f1-4733-9489-710d46453e27");
        if (prop == null) {
            writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "910bb020-32cb-49a1-bb48-6d8b35e60dcb");
            throw new IllegalArgumentException("Cannot create CardinalityQRestriction with a null property");
        }
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "49d16176-bab8-4452-aa97-383f57d0a27f");
        if (cls == null) {
            writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "c54a382d-3a7d-4de8-b1e0-8212b3ad29b2");
            throw new IllegalArgumentException("Cannot create CardinalityQRestriction with a null class");
        }
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "eee73b31-b913-4a69-b3e1-f849790624d6");
        Restriction r = createOntResource(Restriction.class, getProfile().RESTRICTION(), uri);
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "3f3beaf7-0740-4023-8921-0f11e4de3398");
        r.addProperty(getProfile().ON_PROPERTY(), prop);
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "8dc0a4ed-bcf1-4651-bb9e-cd75158cafe2");
        r.addProperty(getProfile().CARDINALITY_Q(), createTypedLiteral(cardinality));
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "0eb3ea7e-4e06-4cda-b717-a175178a9ffd");
        r.addProperty(getProfile().HAS_CLASS_Q(), cls);
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "95a01d8a-c525-4e9f-8b09-d8aa522c34b7");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "1ecf977b-5832-4512-86c0-25e1efd575cc");
        checkProfileEntry(getProfile().DATARANGE(), "DATARANGE");
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "558292fe-b397-43bc-b50d-ee925955a091");
        DataRange d = createOntResource(DataRange.class, getProfile().DATARANGE(), null);
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "64a7a99f-814c-486d-aa0a-e5a1751b5fab");
        checkProfileEntry(getProfile().ONE_OF(), "ONE_OF");
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "1a97a87d-fc08-4b08-b435-b816cd4c2760");
        d.addProperty(getProfile().ONE_OF(), (literals == null) ? createList() : literals);
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "410d3526-aa5a-452c-ad42-0c7b7667204a");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "8c43641f-97fc-4198-bc93-eabce62a94f7");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "6b161c99-3c48-4caf-9318-ef777dcf275f");
        checkProfileEntry(getProfile().ALL_DIFFERENT(), "ALL_DIFFERENT");
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "ed220409-b8ab-4736-8608-2887d729a6f7");
        AllDifferent ad = createOntResource(AllDifferent.class, getProfile().ALL_DIFFERENT(), null);
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "c149b652-1bdf-44f2-8f5f-cefe2697749c");
        ad.setDistinctMembers((differentMembers == null) ? createList() : differentMembers);
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "43fcd48c-6487-48bd-9fd9-31b07166bee2");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "6f5f966a-f828-4296-875e-283a271e199c");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "e748d4ee-2d4d-4b0b-881d-538a4b1044e3");
        return getResource(uri).as(OntResource.class);
    }

    /**
     * <p>Answer a new empty list.  This method overrides the list create method in ModelCom,
     * to allow both DAML and RDFS lists to be created.</p>
     * @return An RDF-encoded list of no elements, using the current language profile
     */
    @Override
    public RDFList createList() {
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "d5060ba3-3506-4c1e-9df1-1ea05821d832");
        Resource list = getResource(getProfile().NIL().getURI());
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "2e5835f4-068b-4b2a-ba4f-3fea483be485");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "4dcdb03c-307a-4615-b52b-8fa1a90fb967");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "d2c87354-c456-45f4-815d-e83c9062beed");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "6088f3e1-40f5-4d40-8c98-fc652f6cf56d");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "91334f6a-4aae-4e60-9407-5898697ae091");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "e61ee178-12c3-4787-9e3b-9d4adf9b6815");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "9baf2a4a-3bba-423e-ab4f-0bb5eeac16f0");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "ebc861b2-3450-4884-9d87-bbb5370b338b");
        Set<String> results = new HashSet<String>();
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "1671a482-0caa-4224-bab7-64a3cc430318");
        List<Model> queue = new ArrayList<Model>();
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "6041ad3d-7f73-41c5-8819-b5925ede6e22");
        queue.add(getBaseModel());
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "38ff1400-3c24-4c7e-be9a-c32f4135076d");
        while (!queue.isEmpty()) {
            writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "d4275c39-219e-4e5d-943a-f55708f7089d");
            Model m = queue.remove(0);
            writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "2a02089e-4c24-4c42-b100-abc135eee855");
            // list the ontology nodes
            if (getProfile().ONTOLOGY() != null && getProfile().IMPORTS() != null) {
                writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "345486fa-2e2d-42b9-9994-ae5338dd45cb");
                StmtIterator i = m.listStatements(null, getProfile().IMPORTS(), (RDFNode) null);
                writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "54d876f6-0301-45b9-a21f-0381721bcb55");
                while (i.hasNext()) {
                    writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "ac36d616-d606-4277-8823-3e9c2148d286");
                    Statement s = i.nextStatement();
                    writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "eb878334-7f26-4d98-8616-e8b9d7592b40");
                    String uri = s.getResource().getURI();
                    writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "3978c70a-2b45-4abb-ba53-d23aad44eb85");
                    if (!results.contains(uri)) {
                        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "39dc032a-73f6-4a1c-b920-d5cbaf365cf1");
                        // this is a new uri, so we add it
                        results.add(uri);
                        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "b78a5ee8-e6b3-4f9a-b612-5951ab7ba7b3");
                        // and push the model on the stack if we know it
                        Model mi = getDocumentManager().getModel(uri);
                        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "2bc42833-7f33-4b3a-b55c-cce4493dda73");
                        if (closure && mi != null && !queue.contains(mi)) {
                            writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "c6eb9a53-1646-45d4-b7b3-e8f58afaba6d");
                            queue.add(mi);
                        }
                    }
                }
            }
        }
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "a95f4cfa-3085-47bd-bf71-525dbc94a2be");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "0a265107-d7dc-4de2-8124-5d82f5e7a824");
        return m_spec.getImportModelMaker();
    }

    /**
     * @deprecated use getImportModelMaker instead.
     */
    @Override
    @Deprecated
    public ModelMaker getModelMaker() {
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "baf30adf-255c-4081-9b94-bac596a24517");
        return getImportModelMaker();
    }

    /**
     * <p>Read statements into the model from the given source, and then load
     * imported ontologies (according to the document manager policy).</p>
     * @param uri URI to read from, may be mapped to a local source by the document manager
     */
    @Override
    public Model read(String uri) {
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "175a81b0-aaf7-465d-987b-787135717df6");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "a75b48c6-6d2c-488d-9233-339a4c580ffc");
        super.read(reader, base);
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "a00d7f3e-040f-4099-8c4e-540d12c61b0b");
        loadImports();
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "f201d76a-11bc-4a33-a8ce-94f8690763cd");
        rebind();
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "7ca06025-6781-4916-9d2e-358080130a3d");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "8062800e-ffab-4f1d-9c49-20adc480dd19");
        super.read(reader, base);
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "1199e96c-54e8-4606-8b9c-1d1cb620eeb7");
        loadImports();
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "5c6ac55c-3fad-49f9-9307-366c9a8c307a");
        rebind();
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "f5354a15-00ca-4123-9759-bfed6967ed07");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "7b1f2be5-c851-4f96-b95d-268c0383dc4e");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "b8cc8136-037b-428a-b8ae-a49bdb85170c");
        // we don't want to load this document again if imported by one of the imports
        addLoadedImport(uri);
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "f40b1a27-7750-4846-9e7b-a83c27680b8f");
        OntDocumentManager odm = getDocumentManager();
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "a1fd16d9-e451-45af-86dd-95fb6167b02c");
        String sourceURL = odm.doAltURLMapping(uri);
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "ed3e09dd-902f-406e-82fe-c8018bedd9c3");
        // invoke the read hook from the ODM
        String source = odm.getReadHook().beforeRead(this, sourceURL, odm);
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "944e1600-0d6a-4ff7-b430-d110e80543e3");
        if (source == null) {
            writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "c075588e-91fe-4026-b1c4-2138cfea4b24");
            s_log.warn("ReadHook returned null, so skipping assuming previous value: " + sourceURL);
            writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "e126e587-071a-483b-a80a-900dabb4e436");
            source = sourceURL;
        } else {
            writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "4c8ff66a-7678-4a59-a7dd-a9f42f2ba133");
            // now we can actually do the read, check first if we should use negotiation
            if (// require non-null base
            base == null && // and that negotiation makes sense (don't conneg to file:)
            !ignoreFileURI(source) && // and that we haven't remapped the URI
            source.equals(uri)) {
                writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "8d507970-4cc1-48d3-9b9b-31a3b1230a24");
                if (syntax == null) {
                    writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "1116facb-ca29-48d9-b8a5-474b534c89eb");
                    readDelegate(source);
                } else {
                    writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "7c2708f6-b595-44e0-8e03-baf4c250f8a0");
                    readDelegate(source, syntax);
                }
            } else {
                writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "c8e02d65-65ba-42d7-bbd6-e40eb2313704");
                // if we were given the base, use it ... otherwise default to the base being the source
                readDelegate(source, (base == null ? uri : base), syntax);
            }
        }
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "a180fccb-35dd-430c-8bd0-d84d702c5a54");
        // the post read hook
        odm.getReadHook().afterRead(this, source, odm);
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "87cbc605-d997-4614-a1f3-495d509b90e1");
        // cache this model against the public uri (if caching enabled)
        getDocumentManager().addModel(uri, this);
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "17de28d1-a122-4651-8eb5-b4e564cd68db");
        loadImports();
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "57877047-08f9-4b9d-a848-a7d66bdf37b3");
        rebind();
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "bcf78c78-9e75-407b-979a-2d6d72cb1a61");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "a19f1750-06ad-483e-9c5d-1f4f415beae1");
        super.read(reader, base, syntax);
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "90eac78c-89fc-46b8-9e53-21552d493b6e");
        loadImports();
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "cdba9a47-fe46-41af-aeb7-555483d44c58");
        rebind();
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "5d4b1302-a3d2-4f46-aeed-25c8e912c179");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "b12de506-6e8c-49a8-a950-8f5786da0e16");
        super.read(reader, base, syntax);
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "bc9646e3-ffe4-4377-be1c-829e7ec6690d");
        loadImports();
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "5aaecddc-ad34-4c56-bcd7-fc1063011667");
        rebind();
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "4166cef7-1833-43c1-93db-e927b843af32");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "e0706390-4fc6-4fba-acbf-f64ff927765d");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "cc801dc8-a3a1-4971-abc0-79658dec3ad1");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "433cef95-1fc0-4bb3-aa94-611073916c1f");
        ExtendedIterator<Graph> i = WrappedIterator.create(getSubGraphs().iterator());
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "ce1177f0-7803-4458-98c3-f9f0c8382cfe");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "059f6ae6-34d4-4b17-b6f9-426e0ed28143");
        return listSubModels(false);
    }

    /**
     * <p>Answer the number of sub-models of this model, not including the
     * base model.</p>
     * @return The number of sub-models, &ge; zero.
     */
    @Override
    public int countSubModels() {
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "5fd9e9cd-747c-4f4d-a906-a96af146a134");
        int count = 0;
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "c9a149bd-1361-461a-af12-0d771195a1ec");
        for (Graph graph1 : getSubGraphs()) {
            writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "8a9c86bb-3b47-470c-9f3c-0cce4803dd11");
            count++;
        }
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "38276ddf-dc2f-4005-8012-38bac37604ec");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "7fe605e6-e756-41a3-afcb-3fd7d98f73a7");
        if (listImportedOntologyURIs(true).contains(uri)) {
            writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "10cebf06-bdd6-4b45-910c-294bd0cc0426");
            Model mi = getDocumentManager().getModel(uri);
            writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "81b1d7b2-4893-4410-8100-91c50e789a8b");
            if (mi != null) {
                writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "541b6626-84b7-4400-b4b4-5b7ecbfd9003");
                if (mi instanceof OntModel) {
                    writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "64033d78-1b96-48a1-899b-6f11cc73e5fe");
                    // already a suitable ont model
                    return (OntModel) mi;
                } else {
                    writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "dd6b55f9-d200-49f1-9bc2-bf5e0f613a08");
                    // not in ont-model clothing yet, so re-wrap
                    return ModelFactory.createOntologyModel(m_spec, mi);
                }
            }
        }
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "c47a8db5-64b2-4ac6-9190-e7e02e69b547");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "0bf13f55-1755-47a7-947b-3f7aec05b794");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "d7feece7-048a-4135-9ca6-2c64a86aa7c5");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "cf8f4bb4-89a5-44ad-87a2-ebd9637ba036");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "7679cb86-f36a-431c-9ff3-a318d20c581f");
        getUnionGraph().addGraph(model.getGraph());
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "8a1680a6-eac3-486d-b0bd-aa69c4d70b56");
        if (rebind) {
            writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "10a974f1-757a-4e9e-b3a4-6469ce1f5846");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "d5ff6c18-1252-49c2-b240-badeb9416393");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "4e37dc9a-4a87-4616-9918-97ecf73e48c2");
        Graph subG = model.getGraph();
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "6ec9d06c-4808-4276-813d-ee53a8f7de99");
        getUnionGraph().removeGraph(subG);
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "3c998fea-ae32-48f8-ae77-a13d0b7764cf");
        // originally
        if (subG instanceof MultiUnion) {
            writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "7865d816-6d58-4b2c-9bd5-64e4fbc7a26e");
            // we need to get the base graph when removing a ontmodel
            getUnionGraph().removeGraph(((MultiUnion) subG).getBaseGraph());
        }
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "965be763-ed77-4f3a-888c-ecc4029e91cc");
        if (rebind) {
            writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "fa59dff2-45ef-4711-bdf7-e046df41fbca");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "18c0ea87-f9b0-4272-bfeb-b56f28576d7d");
        Node n = node.asNode();
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "6667aadf-0d1f-4beb-b64d-545dbbd2219f");
        Graph b = getBaseGraph();
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "61c2df20-8938-4cfe-b079-9ed1b745a010");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "76df5b6a-d620-452f-a648-3884a26a27d7");
        Node s = stmt.getSubject().asNode();
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "e3a8e3b5-8ec4-4445-b3bc-37396500a8cd");
        Node p = stmt.getPredicate().asNode();
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "8c25b87a-d4ac-4390-a427-659e2a663119");
        Node o = stmt.getObject().asNode();
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "7ab0c295-c647-4eb2-9c74-f341318e1850");
        Graph b = getBaseGraph();
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "11e81abe-6e5f-45ad-b05b-225ea216b159");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "33dac9e1-667f-4ddd-b7fc-02b0edc9bd52");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "eaad1c1b-31d2-4e52-9e4a-c10563f1a1c3");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "5a170eed-1954-4fb3-9867-dd00a5e8960c");
        if (dynamic) {
            writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "aba83a6f-061f-48d8-9f41-ec16148aa774");
            if (m_importsListener == null) {
                writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "3ce7972e-37da-42b2-aea1-93b123c1acb0");
                // turn on dynamic processing
                m_importsListener = new ImportsListener();
                writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "46191473-1a02-4e0e-bc6b-0a4a73f0ccba");
                register(m_importsListener);
            }
        } else {
            writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "ea422716-e274-49ed-9eaf-db068f85ae4b");
            if (m_importsListener != null) {
                writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "f6309d8d-754d-4056-9ee8-69f3ef4268b7");
                // turn off dynamic processing
                unregister(m_importsListener);
                writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "86eaf0c5-b833-4d03-a8ac-635404dcbb12");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "4270de0a-e236-4395-8b19-3c7629d976d0");
        return m_importsListener != null;
    }

    /**
     * <p>Answer the ontology model specification that was used to construct this model</p>
     * @return An ont model spec instance.
     */
    @Override
    public OntModelSpec getSpecification() {
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "c14815cd-7aae-4987-9990-8e4d5ee7e692");
        return m_spec;
    }

    // output operations - delegate to base model
    @Override
    public Model write(Writer writer) {
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "50a25460-67f2-43b6-a149-fab087e00795");
        return getBaseModel().write(writer);
    }

    @Override
    public Model write(Writer writer, String lang) {
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "7f44eafb-ae53-43a5-ac29-27fec4302435");
        return getBaseModel().write(writer, lang);
    }

    @Override
    public Model write(Writer writer, String lang, String base) {
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "43d786b1-2b80-4ff4-bac2-36bd874c8ce5");
        return getBaseModel().write(writer, lang, base);
    }

    @Override
    public Model write(OutputStream out) {
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "a025ddc1-6a82-46a6-ab06-eb5d46f6aa5f");
        return getBaseModel().write(out);
    }

    @Override
    public Model write(OutputStream out, String lang) {
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "111d3bd6-616b-4ec4-8ebf-44fa042bbf7c");
        return getBaseModel().write(out, lang);
    }

    @Override
    public Model write(OutputStream out, String lang, String base) {
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "1241e2e0-50db-4c4d-8827-c571ca5c1546");
        return getBaseModel().write(out, lang, base);
    }

    @Override
    public Model writeAll(Writer writer, String lang, String base) {
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "51d4ebb8-8179-48e6-b15b-8b1522cfef61");
        return super.write(writer, lang, base);
    }

    @Override
    public Model writeAll(OutputStream out, String lang, String base) {
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "c06d48f8-f4b3-4617-aa30-c10355c0ff41");
        return super.write(out, lang, base);
    }

    @Override
    public Model writeAll(Writer writer, String lang) {
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "d4b8a4ad-4db6-4cc0-a108-424d0e7e4cd2");
        return super.write(writer, lang);
    }

    @Override
    public Model writeAll(OutputStream out, String lang) {
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "0ec41a8c-2147-4b7e-9d0c-498e582535a9");
        return super.write(out, lang);
    }

    // Implementation of inf model interface methods
    /**
     * Return the raw RDF model being processed (i.e. the argument
     * to the Reasonder.bind call that created this InfModel).
     */
    @Override
    public Model getRawModel() {
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "b164b84c-195a-48bb-bd03-7c2029ef24a0");
        return getBaseModel();
    }

    /**
     * Return the Reasoner which is being used to answer queries to this graph.
     */
    @Override
    public Reasoner getReasoner() {
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "e86bbccf-be44-4b64-bbdc-912c9116dd8a");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "b85a09da-d4a8-4006-8383-6d4ef18d8897");
        if (getGraph() instanceof InfGraph) {
            writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "66f6636d-58da-457a-a352-e904e7704590");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "255c7e95-0b0f-4c26-8e49-a2233bf346d0");
        if (getGraph() instanceof InfGraph) {
            writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "52b10e3b-d440-49d3-8387-89176159d37c");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "c6a24981-1b73-4ab6-bc24-7fdceb14ed32");
        if (getGraph() instanceof InfGraph) {
            writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "5e1135a3-4d47-4adb-b5d0-ce8203624774");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "9343c063-1690-47e0-9527-44aa7b1dccd6");
        if (m_deductionsModel == null) {
            writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "8f33db08-fe26-401f-92a5-fbff315b6d72");
            InfGraph infGraph = getInfGraph();
            writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "3293ec09-d08d-4402-b4bd-32561a994518");
            if (infGraph != null) {
                writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "94b37da1-bcd8-4f72-8aa2-c35f95ce727d");
                Graph deductionsGraph = infGraph.getDeductionsGraph();
                writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "d573144d-19f2-4df3-85d5-c4de8c47afec");
                if (deductionsGraph != null) {
                    writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "bd596c33-cfae-4e01-8930-bc26b8c912df");
                    m_deductionsModel = ModelFactory.createModelForGraph(deductionsGraph);
                }
            }
        } else {
            writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "22bb0a26-3b4d-4fbd-91a8-c7152c41450b");
            // ensure that the cached model sees the updated changes from the
            // underlying reasoner graph
            getInfGraph().prepare();
        }
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "67f877aa-0c8c-4743-8c4c-0843bd11ace2");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "6e747459-8942-4ba1-aacc-10694f8ddbe4");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "a7b856a8-f466-49a9-b5eb-1ad8d9302580");
        if (getGraph() instanceof InfGraph) {
            writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "d945cc40-5dea-49d5-9062-c1562c2df86c");
            Graph gp = posit == null ? ModelFactory.createDefaultModel().getGraph() : posit.getGraph();
            writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "fe8f699a-21bc-4fbc-98c0-711e57fdd840");
            Iterator<Triple> iter = getInfGraph().find(asNode(subject), asNode(predicate), asNode(object), gp);
            writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "d5edfa61-5164-4145-b82b-cd216005e0eb");
            return IteratorFactory.asStmtIterator(iter, this);
        } else {
            writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "4f0388e5-3a41-4971-a6d7-d07a33382a11");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "b9dbd50d-e495-4c33-b08d-2537ebaffa2e");
        if (getGraph() instanceof InfGraph) {
            writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "b72a06a2-68c1-45a5-833c-b4b9837cee86");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "a4c80842-50bd-43d7-896c-df160dff55fe");
        return (getGraph() instanceof InfGraph) ? ((InfGraph) getGraph()).getDerivation(statement.asTriple()) : null;
    }

    // Internal implementation methods
    // ////////////////////////////////
    private static void initSyntaxCheckerClass() {
        writelineStatic("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "c49db1d3-656c-475a-b898-53854bd8d032");
        if (owlSyntaxCheckerClass == null) {
            writelineStatic("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "d42cd24e-597d-4fe6-beb2-0e59bac016c8");
            try {
                writelineStatic("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "1851af97-6cfb-44a7-ae26-57dcc1b26d3f");
                owlSyntaxCheckerClass = Class.forName(owlSyntaxCheckerClassName);
                writelineStatic("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "680b38b6-1658-4940-a429-5c189dc63892");
                owlSyntaxCheckerClass.newInstance();
            } catch (Exception e) {
                writelineStatic("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "28e7a225-4cfb-4cdb-a4e4-93241b0b29a6");
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
        writelineStatic("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "59f2bd71-db99-4681-a970-5f7687914e91");
        // create a empty union graph
        MultiUnion u = new MultiUnion();
        writelineStatic("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "aea020e8-ae98-4f9a-b4aa-719c8d39eedb");
        u.addGraph(base);
        writelineStatic("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "0d191a66-c945-4649-bad7-5808eb37c727");
        u.setBaseGraph(base);
        writelineStatic("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "694ed980-245c-4a07-9446-972641db90f7");
        Reasoner r = spec.getReasoner();
        writelineStatic("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "36020fba-23e4-4554-ab56-8c9ddb9ea8b7");
        // if we have a reasoner in the spec, bind to the union graph and return
        return r == null ? (Graph) u : r.bind(u);
    }

    /**
     * <p>Answer the union graph that contains the imports closure for this ontology</p>
     * @return The union graph
     */
    protected MultiUnion getUnionGraph() {
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "3aa303ad-a8ac-4b69-922a-820aad254a2e");
        return m_union;
    }

    /**
     * Answer the resource with the given URI, if present, as the given facet
     */
    protected <T extends Resource> Resource findByURIAs(String uri, Class<T> asKey) {
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "15a3f6ec-95e2-4a47-a00d-b8ff535d9b2c");
        if (uri == null) {
            writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "ff764466-1db2-427c-bf52-c68bf009246a");
            throw new IllegalArgumentException("Cannot get() ontology value with a null URI");
        }
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "8eac36ff-f0d8-4566-9d60-9adbfbba5db1");
        Node n = NodeFactory.createURI(uri);
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "4d1da5c7-c8e2-495e-9ceb-fbbf2b8d4e67");
        if (getGraph().contains(n, Node.ANY, Node.ANY)) {
            writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "187c2718-c2ed-4b87-825f-0ccccfa03ec3");
            // this resource is a subject in the graph
            try {
                writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "c67e665b-453f-4108-9752-79852e2e84d1");
                return getNodeAs(n, asKey);
            } catch (ConversionException ignore) {
            /**/
            }
        }
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "02c6a9b8-ed4a-4fb3-8b10-4425bd0e3191");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "b158911d-ecd4-433d-915b-0fea0dc3bf7b");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "b9cd7f8a-7e78-497d-8770-fa3b2a75d8a2");
        ExtendedIterator<Triple> i = findByType(type);
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "d42fe175-0d0b-48ee-9729-717280e503e3");
        // compose onto i the find iterators for the alternate types
        if (alternates != null) {
            writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "c22c22d0-727d-4db9-9faa-996c3aab3e17");
            while (alternates.hasNext()) {
                writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "d56986b7-8693-40ce-914e-73327b7bc51b");
                i = i.andThen(findByType(alternates.next()));
            }
        }
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "5e582fc0-2546-422d-a370-d3ba9c982971");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "c6f525de-7db8-4afd-83e2-3463257fcc11");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "4df14601-0e08-4db1-8c57-88d2529572f6");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "21e11465-64d5-4dc9-a560-2d63acc83b52");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "3b46ede8-28d7-47a5-b324-f9455da14d55");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "d7fc3af5-19d0-4048-bdbd-5d12e95bcafc");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "6c625c10-7e07-4471-a277-3faf026dcbbe");
        Resource r = getResource(uri);
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "5709b3e5-fe44-47e8-9e2c-0126f9230c8c");
        if (rdfType != null) {
            writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "1176eb4b-1687-4e42-8a8d-9ccd0bac31b3");
            r.addProperty(RDF.type, rdfType);
        }
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "7e1e061b-a99b-41a6-b2f0-2a2b208120cd");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "728037af-0f3d-440e-bced-793996489344");
        Resource r = getResource(uri);
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "bff93cac-6bbc-47f4-a494-01efaeb1da6d");
        if (containsResource(r)) {
            writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "cd16dcd7-4501-4274-a8c6-d578c19f1690");
            return r.as(OntResource.class);
        }
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "c82baae7-fe43-4002-b534-61f2e15c8676");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "53164c6f-f93c-47fa-bdc8-da2d92a881fb");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "4fac1f38-ae08-40c5-a8d2-ea1280ae4bab");
        if (profileTerm == null) {
            writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "7a4cd0c4-a57d-4452-bece-e8893c3bff79");
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
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "423f2152-0614-49c7-b118-34610be37e5c");
        if (strictMode() && !((Boolean) list.reduce(new RdfTypeTestFn(rdfType), Boolean.TRUE)).booleanValue()) {
            writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "82268ae6-fad7-4ac9-a4ff-7426cdf8a713");
            // not all of the members of the list are of the given type
            throw new LanguageConsistencyException("The members of the given list are expected to be of rdf:type " + rdfType.toString());
        }
    }

    /**
     * Answer the supplied model, unless it's null, in which case answer a new model
     * constructed as per spec.
     */
    private static Model makeBaseModel(OntModelSpec spec, Model model) {
        writelineStatic("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "a48b788d-35a1-4651-aa40-cf068eb79fe7");
        return model == null ? spec.createBaseModel() : model;
    }

    /**
     * <p>Answer the InfGraph that this model is wrapping, or null if this ontology
     * model is not wrapping an inf graph.</p>
     * @return The model's graph as an InfGraph, or null
     */
    private InfGraph getInfGraph() {
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "ce8e7d96-3c15-4de6-a7d2-3815aaa22697");
        return (getGraph() instanceof InfGraph) ? ((InfGraph) getGraph()) : null;
    }

    /**
     * Test for whether we ignore <code>file:</code> URI's when testing for content
     * negotiation.
     * @param source
     * @return
     */
    protected boolean ignoreFileURI(String source) {
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "b03cc071-07dc-46d4-b8dd-a9e38ec2823e");
        return source.startsWith("file:");
    }

    /* delegation points to allow unit testing of read operations */
    protected Model readDelegate(String url) {
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "5c6897e5-7dcd-4a08-bd5a-3663cd62f86a");
        return super.read(url);
    }

    protected Model readDelegate(String url, String lang) {
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "96fb4af3-6217-488b-8393-84affbccd8a1");
        return super.read(url, lang);
    }

    protected Model readDelegate(String url, String base, String lang) {
        writeline("/home/ubuntu/results/coverage/OntModelImpl/OntModelImpl_3_10.coverage", "1a68f6e9-a0ae-4ece-aeb1-6231d5ec6d87");
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
