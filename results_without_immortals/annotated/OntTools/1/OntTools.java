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
package org.apache.jena.ontology;

// /////////////
import java.util.*;
import java.util.function.Predicate;
import org.apache.jena.rdf.model.*;
import org.apache.jena.shared.JenaException;
import java.io.*;

/**
 * <p>
 * Some general utilities and algorithms to support developers working with the
 * general classes in the Jena ontology API. <strong>Warning</strong> these
 * utilities are <strong>experimental</strong>. Extensive testing has not yet
 * occurred (see {@link org.apache.jena.ontology.impl.TestOntTools} in the
 * test area for basic unit tests),
 * and in particular performance testing has not been carried out yet.
 * Users are advised to exercise caution before relying on these utilities in
 * production code. Please send any comments or suggestions to the
 * <a href="http://tech.groups.yahoo.com/group/jena-dev">Jena support email list</a>.
 * </p>
 */
public class OntTools {

    // Constants
    // ////////////////////////////////
    // Static variables
    // ////////////////////////////////
    // static private Logger log = LoggerFactory.getLogger( OntTools.class );
    // Instance variables
    // ////////////////////////////////
    // Constructors
    // ////////////////////////////////
    // External signature methods
    // ////////////////////////////////
    /**
     * <p>Answer the lowest common ancestor of two classes in a given ontology. This
     * is the class that is farthest from the root concept (defaulting to
     * <code>owl:Thing</code> which is a super-class of both <code>u</code>
     * and <code>v</code>. The algorithm is based on
     * <a href="http://en.wikipedia.org/wiki/Tarjan's_off-line_least_common_ancestors_algorithm">Tarjan's
     * off-line LCA</a>. The current implementation expects that the given model:
     * </p>
     * <ul>
     * <li>is transitively closed over the <code>subClassOf</code> relation</li>
     * <li>can cheaply determine <em>direct sub-class</em> relations</li>
     * </ul>
     * <p>Both of these conditions are true of the built-in Jena OWL reasoners,
     * such as {@link OntModelSpec#OWL_MEM_MICRO_RULE_INF}, and external DL
     * reasoners such as Pellet.</p>
     *
     * @param m The ontology model being queried to find the LCA, which should conform
     * to the reasoner capabilities described above
     * @param u An ontology class
     * @param v An ontology class
     * @return The LCA of <code>u</code> and <code>v</code>
     * @exception JenaException if the language profile of the given model does not
     * define a top concept (e.g. <code>owl:Thing</code>)
     */
    public static OntClass getLCA(OntModel m, OntClass u, OntClass v) {
        writelineStatic("/home/ubuntu/results/coverage/OntTools/OntTools_1_10.coverage", "27648cf3-f0ef-4fb6-8cbd-91666c60a753");
        Resource root = m.getProfile().THING();
        writelineStatic("/home/ubuntu/results/coverage/OntTools/OntTools_1_10.coverage", "30fbbd2b-da08-4f05-85cd-0d99cd4d1232");
        if (root == null) {
            writelineStatic("/home/ubuntu/results/coverage/OntTools/OntTools_1_10.coverage", "48ae5f2c-f631-4c3c-8d92-5f102b391a1e");
            throw new JenaException("The given OntModel has a language profile that does not define a generic root class (such as owl:Thing)");
        }
        writelineStatic("/home/ubuntu/results/coverage/OntTools/OntTools_1_10.coverage", "2cfdfb0b-8f40-4ac1-b93a-3696839030ce");
        root = root.inModel(m);
        writelineStatic("/home/ubuntu/results/coverage/OntTools/OntTools_1_10.coverage", "46d34267-c69d-434f-9723-1c27794aea88");
        return getLCA(m, root.as(OntClass.class), u, v);
    }

    /**
     * Answer the lowest common ancestor of two classes, assuming that the given
     * class is the root concept to start searching from. See {@link #getLCA(OntModel, OntClass, OntClass)}
     * for details.
     *
     * @param m The ontology model being queried to find the LCA, which should conform
     * to the reasoner capabilities described above
     * @param root The root concept, which will be the starting point for the algorithm
     * @param u An ontology class
     * @param v An ontology class
     * @return The LCA of <code>u</code> and <code>v</code>
     * @exception JenaException if the language profile of the given model does not
     * define a top concept (e.g. <code>owl:Thing</code>)
     */
    public static OntClass getLCA(OntModel m, OntClass root, OntClass u, OntClass v) {
        writelineStatic("/home/ubuntu/results/coverage/OntTools/OntTools_1_10.coverage", "141d5ad3-6636-405c-a04e-bf830cc0f3ec");
        // check some common cases first
        if (u.equals(root) || v.equals(root)) {
            writelineStatic("/home/ubuntu/results/coverage/OntTools/OntTools_1_10.coverage", "359a2d66-77d3-4ce2-bca5-88f6f7f7e784");
            return root;
        }
        writelineStatic("/home/ubuntu/results/coverage/OntTools/OntTools_1_10.coverage", "61b76389-7d9b-4bfc-a6f7-5c7084950af8");
        if (u.hasSubClass(v)) {
            writelineStatic("/home/ubuntu/results/coverage/OntTools/OntTools_1_10.coverage", "cb4a92ac-f2dc-403b-835f-127d47334ec6");
            return u;
        }
        writelineStatic("/home/ubuntu/results/coverage/OntTools/OntTools_1_10.coverage", "cb01fbb3-ac4a-4c10-9cff-d5213e12ab90");
        if (v.hasSubClass(u)) {
            writelineStatic("/home/ubuntu/results/coverage/OntTools/OntTools_1_10.coverage", "d2b76244-a161-4fd2-afa5-0c419b142688");
            return v;
        }
        writelineStatic("/home/ubuntu/results/coverage/OntTools/OntTools_1_10.coverage", "27356a31-aa25-403c-b239-fd5dda566168");
        // not a common case, so apply Tarjan's LCA algorithm
        LCAIndex index = new LCAIndex();
        writelineStatic("/home/ubuntu/results/coverage/OntTools/OntTools_1_10.coverage", "5b993ef1-a77d-4dcc-abb8-878f0d2ada8d");
        lca(root, u, v, index);
        writelineStatic("/home/ubuntu/results/coverage/OntTools/OntTools_1_10.coverage", "89a0958e-7cc3-4033-9f0d-badbdd3be760");
        return (OntClass) index.getLCA(u, v);
    }

    /**
     * <p>Answer the shortest path from the <code>start</code> resource to the <code>end</code> RDF node,
     * such that every step on the path is accepted by the given filter. A path is a {@link List}
     * of RDF {@link Statement}s. The subject of the first statement in the list is <code>start</code>,
     * and the object of the last statement in the list is <code>end</code>.</p>
     * <p>The <code>onPath</code> argument is a {@link Predicate}, which accepts a statement and returns
     * true if the statement should be considered to be on the path. To search for an unconstrained
     * path, pass <code>()->true</code> as an argument. To search for a path whose predicates match a
     * fixed restricted set of property names, pass an instance of {@link PredicatesFilter}.</p>
     * <p>If there is more than one path of minimal length from <code>start</code> to <code>end</code>,
     * this method returns an arbitrary one. The algorithm is blind breadth-first search,
     * with loop detection.</p>
     *
     * @param m The model in which we are seeking a path
     * @param start The starting resource
     * @param end The end, or goal, node
     * @param onPath A filter which determines whether a given statement can be considered part
     * of the path
     * @return A path, consisting of a list of statements whose first subject is <code>start</code>,
     * and whose last object is <code>end</code>, or null if no such path exists.
     */
    public static Path findShortestPath(Model m, Resource start, RDFNode end, Predicate<Statement> onPath) {
        writelineStatic("/home/ubuntu/results/coverage/OntTools/OntTools_1_10.coverage", "c98aaea7-30a0-4b3c-a41c-ae3e9117f8d1");
        List<Path> bfs = new LinkedList<Path>();
        writelineStatic("/home/ubuntu/results/coverage/OntTools/OntTools_1_10.coverage", "f93b36e4-151c-4bb5-8a4f-8eefac125495");
        Set<Resource> seen = new HashSet<Resource>();
        writelineStatic("/home/ubuntu/results/coverage/OntTools/OntTools_1_10.coverage", "1a567b12-4a96-4c3c-bafe-2e73bb58ac8d");
        // initialise the paths
        for (Iterator<Statement> i = m.listStatements(start, null, (RDFNode) null).filterKeep(onPath); i.hasNext(); ) {
            writelineStatic("/home/ubuntu/results/coverage/OntTools/OntTools_1_10.coverage", "fae52e14-ac95-4f86-95f8-866398bb2ff6");
            bfs.add(new Path().append(i.next()));
        }
        writelineStatic("/home/ubuntu/results/coverage/OntTools/OntTools_1_10.coverage", "0c9534f6-b02f-4dea-bb79-3c92339fc2d2");
        // search
        Path solution = null;
        writelineStatic("/home/ubuntu/results/coverage/OntTools/OntTools_1_10.coverage", "90739251-aeee-4f8b-82d2-c30fef3645ab");
        while (solution == null && !bfs.isEmpty()) {
            writelineStatic("/home/ubuntu/results/coverage/OntTools/OntTools_1_10.coverage", "956b0c8d-09eb-4e14-9da6-78e83f957e17");
            Path candidate = bfs.remove(0);
            writelineStatic("/home/ubuntu/results/coverage/OntTools/OntTools_1_10.coverage", "3c0c6ab5-75c7-4671-a99d-b4016be1f1ee");
            if (candidate.hasTerminus(end)) {
                writelineStatic("/home/ubuntu/results/coverage/OntTools/OntTools_1_10.coverage", "cef22f28-300d-4867-b4eb-aa579ba55bbd");
                solution = candidate;
            } else {
                writelineStatic("/home/ubuntu/results/coverage/OntTools/OntTools_1_10.coverage", "3a1d24dc-e558-42e1-b720-1ca1163bf44c");
                Resource terminus = candidate.getTerminalResource();
                writelineStatic("/home/ubuntu/results/coverage/OntTools/OntTools_1_10.coverage", "57060ee5-8692-49b5-8041-c16d7cd5c0b4");
                if (terminus != null) {
                    writelineStatic("/home/ubuntu/results/coverage/OntTools/OntTools_1_10.coverage", "d298b624-e250-4b3a-8f7a-15415ac36630");
                    seen.add(terminus);
                    writelineStatic("/home/ubuntu/results/coverage/OntTools/OntTools_1_10.coverage", "f6e1aeb3-11cf-4f95-ad28-9a6f800e2ddf");
                    // breadth-first expansion
                    for (Iterator<Statement> i = terminus.listProperties().filterKeep(onPath); i.hasNext(); ) {
                        writelineStatic("/home/ubuntu/results/coverage/OntTools/OntTools_1_10.coverage", "b5b343a4-bbc8-4b7f-a92a-ff559d64d2f5");
                        Statement link = i.next();
                        writelineStatic("/home/ubuntu/results/coverage/OntTools/OntTools_1_10.coverage", "027ccb31-e493-4fe3-80db-2165aa7691ab");
                        // no looping allowed, so we skip this link if it takes us to a node we've seen
                        if (!seen.contains(link.getObject())) {
                            writelineStatic("/home/ubuntu/results/coverage/OntTools/OntTools_1_10.coverage", "d4a8a13a-b9bc-4672-9baa-f41e6d245ff6");
                            bfs.add(candidate.append(link));
                        }
                    }
                }
            }
        }
        writelineStatic("/home/ubuntu/results/coverage/OntTools/OntTools_1_10.coverage", "e87467d3-6b6b-4b86-ba6d-9354a58fd283");
        return solution;
    }

    /**
     * Answer a list of the named hierarchy roots of a given {@link OntModel}. This
     * will be similar to the results of {@link OntModel#listHierarchyRootClasses()},
     * with the added constraint that every member of the returned iterator will be a
     * named class, not an anonymous class expression. The named root classes are
     * calculated from the root classes, by recursively replacing every anonymous class
     * with its direct sub-classes. Thus it can be seen that the values in the list
     * consists of the shallowest fringe of named classes in the hierarchy.
     * @param m An ontology model
     * @return A list of classes whose members are the named root classes of the
     * class hierarchy in <code>m</code>
     */
    public static List<OntClass> namedHierarchyRoots(OntModel m) {
        writelineStatic("/home/ubuntu/results/coverage/OntTools/OntTools_1_10.coverage", "06700b1e-055b-4868-a8ef-b9a8555ef763");
        // named roots
        List<OntClass> nhr = new ArrayList<OntClass>();
        writelineStatic("/home/ubuntu/results/coverage/OntTools/OntTools_1_10.coverage", "0f247be5-2c07-4388-88d9-1cd9e4e79b2b");
        // anon roots
        List<OntClass> ahr = new ArrayList<OntClass>();
        writelineStatic("/home/ubuntu/results/coverage/OntTools/OntTools_1_10.coverage", "eeec3a3b-0a2e-48c0-b4e8-0146f6c98a5c");
        // do the initial partition of the root classes
        partitionByNamed(m.listHierarchyRootClasses(), nhr, ahr);
        writelineStatic("/home/ubuntu/results/coverage/OntTools/OntTools_1_10.coverage", "ca47f1e1-2685-49b5-a5c6-cfe0f1b8b7ef");
        // now push the fringe down until we have only named classes
        while (!ahr.isEmpty()) {
            writelineStatic("/home/ubuntu/results/coverage/OntTools/OntTools_1_10.coverage", "0b1b4da0-58aa-4ae4-a51c-b28834f6ae00");
            OntClass c = ahr.remove(0);
            writelineStatic("/home/ubuntu/results/coverage/OntTools/OntTools_1_10.coverage", "a8389edb-e006-4b44-b0de-ce7135e5e84b");
            partitionByNamed(c.listSubClasses(true), nhr, ahr);
        }
        writelineStatic("/home/ubuntu/results/coverage/OntTools/OntTools_1_10.coverage", "d46771ce-b99f-4f3b-86c9-b0e704bf71e3");
        return nhr;
    }

    // Internal implementation methods
    // ////////////////////////////////
    /**
     * Compute the LCA disjoint set at <code>cls</code>, noting that we are
     * searching for the LCA of <code>uCls</code> and <code>vCls</code>.
     * @param cls The class we are testing (this is 'u' in the Wiki article)
     * @param uCls One of the two classes we are searching for the LCA of. We
     * have simplified the set P of pairs to the unity set {uCls,vCls}
     * @param vCls One of the two classes we are searching for the LCA of. We
     * have simplified the set P of pairs to the unity set {uCls,vCls}
     * @param index A data structure mapping resources to disjoint sets (since
     * we can't side-effect Jena resources), and which is used to record the
     * LCA pairs
     */
    protected static DisjointSet lca(OntClass cls, OntClass uCls, OntClass vCls, LCAIndex index) {
        writelineStatic("/home/ubuntu/results/coverage/OntTools/OntTools_1_10.coverage", "af8fe30f-45cd-441c-89c1-b048e9006aa6");
        // log.debug( "Entering lca(), cls = " + cls );
        DisjointSet clsSet = index.getSet(cls);
        writelineStatic("/home/ubuntu/results/coverage/OntTools/OntTools_1_10.coverage", "5aa79018-97f4-4618-a0cd-1f9236760d00");
        if (clsSet.isBlack()) {
            writelineStatic("/home/ubuntu/results/coverage/OntTools/OntTools_1_10.coverage", "6ffc415b-9a63-456c-ab1b-bcf274f0a235");
            // already visited
            return clsSet;
        }
        writelineStatic("/home/ubuntu/results/coverage/OntTools/OntTools_1_10.coverage", "149c8811-1a2d-49b1-9857-631d45e9f13f");
        // not visited yet
        clsSet.setAncestor(clsSet);
        writelineStatic("/home/ubuntu/results/coverage/OntTools/OntTools_1_10.coverage", "0d4e1a2d-d93d-4f21-992d-fc6b5371a437");
        // for each child of cls
        for (Iterator<OntClass> i = cls.listSubClasses(true); i.hasNext(); ) {
            writelineStatic("/home/ubuntu/results/coverage/OntTools/OntTools_1_10.coverage", "cc323c25-bcf1-4ef5-946b-c66a296ee2a7");
            OntClass child = i.next();
            writelineStatic("/home/ubuntu/results/coverage/OntTools/OntTools_1_10.coverage", "a67e6ef1-3699-47b5-831f-bd2d9461a2b4");
            if (child.equals(cls) || child.equals(cls.getProfile().NOTHING())) {
                writelineStatic("/home/ubuntu/results/coverage/OntTools/OntTools_1_10.coverage", "f714acb9-3b61-47ea-a962-d5fb9152dcc1");
                // we ignore the reflexive case and bottom
                continue;
            }
            writelineStatic("/home/ubuntu/results/coverage/OntTools/OntTools_1_10.coverage", "52ec972a-6941-4eee-bc37-7e4deddbe656");
            // compute the LCA of the sub-tree
            DisjointSet v = lca(child, uCls, vCls, index);
            writelineStatic("/home/ubuntu/results/coverage/OntTools/OntTools_1_10.coverage", "a36f7355-edf6-4fb1-a35b-67b09d2ff42e");
            // union the two disjoint sets together
            clsSet.union(v);
            writelineStatic("/home/ubuntu/results/coverage/OntTools/OntTools_1_10.coverage", "5fb5b274-436e-47a6-8738-9369c3013d88");
            // propagate the distinguished member
            clsSet.find().setAncestor(clsSet);
        }
        writelineStatic("/home/ubuntu/results/coverage/OntTools/OntTools_1_10.coverage", "4e3f11f0-6dcc-4753-87b8-a9e69e77968f");
        // this node is done
        clsSet.setBlack();
        writelineStatic("/home/ubuntu/results/coverage/OntTools/OntTools_1_10.coverage", "c4b3ae43-30bf-45ed-b99d-9e960abc9a24");
        // are we inspecting one of the elements we're interested in?
        if (cls.equals(uCls)) {
            writelineStatic("/home/ubuntu/results/coverage/OntTools/OntTools_1_10.coverage", "d4de2bf9-4063-49e1-8fdd-629a00d15109");
            checkSolution(uCls, vCls, index);
        } else if (cls.equals(vCls)) {
            writelineStatic("/home/ubuntu/results/coverage/OntTools/OntTools_1_10.coverage", "aae146b3-819b-4646-973f-cc563444260a");
            checkSolution(vCls, uCls, index);
        }
        writelineStatic("/home/ubuntu/results/coverage/OntTools/OntTools_1_10.coverage", "0ef3523a-599c-47b4-8ab4-5ca1744f7ec5");
        return clsSet;
    }

    /**
     * Check to see if we have found a solution to the problem.
     * TODO: we could throw an exception to simulate a non-local exit
     * here, since we've assumed that P is the unity set.
     * @param uCls
     * @param vCls
     * @param index
     */
    protected static void checkSolution(OntClass uCls, OntClass vCls, LCAIndex index) {
        writelineStatic("/home/ubuntu/results/coverage/OntTools/OntTools_1_10.coverage", "ec61aaa4-a070-4042-aab2-c82ff7ce8ee9");
        DisjointSet vSet = index.getSet(vCls);
        writelineStatic("/home/ubuntu/results/coverage/OntTools/OntTools_1_10.coverage", "1adc5b15-0d08-425f-bc24-0cfd0847e249");
        DisjointSet uSet = index.getSet(uCls);
        writelineStatic("/home/ubuntu/results/coverage/OntTools/OntTools_1_10.coverage", "8dabe957-f199-4f8f-a85b-fc7d5ed57ddf");
        if (vSet != null && vSet.isBlack() && !vSet.used() && uSet != null && uSet.isBlack() && !uSet.used()) {
            writelineStatic("/home/ubuntu/results/coverage/OntTools/OntTools_1_10.coverage", "ced8fe0b-2cf3-47fc-8bfa-715fba0d4aa2");
            vSet.setUsed();
            writelineStatic("/home/ubuntu/results/coverage/OntTools/OntTools_1_10.coverage", "fef0a5dc-05ce-4ddc-9618-962f30cc11b7");
            uSet.setUsed();
            writelineStatic("/home/ubuntu/results/coverage/OntTools/OntTools_1_10.coverage", "0bb09a76-67de-41a0-8c96-09b8aa67b4b8");
            // log.debug( "Found LCA: u = " + uCls + ", v = " + vCls  );
            OntClass lca = (OntClass) vSet.find().getAncestor().getNode();
            writelineStatic("/home/ubuntu/results/coverage/OntTools/OntTools_1_10.coverage", "0d11de3f-76b5-4d46-924c-4b258718e3cf");
            // log.debug( "Found LCA: lca = " + lca );
            index.setLCA(uCls, vCls, lca);
        }
    }

    /**
     * Partition the members of an iterator into two lists, according to whether
     * they are named or anonymous classes
     * @param i An iterator to partition
     * @param named A list of named classes
     * @param anon A list of anonymous classes
     */
    protected static void partitionByNamed(Iterator<? extends OntClass> i, List<OntClass> named, List<OntClass> anon) {
        writelineStatic("/home/ubuntu/results/coverage/OntTools/OntTools_1_10.coverage", "0058b7eb-3913-4dff-b890-0bf259944d7d");
        while (i.hasNext()) {
            writelineStatic("/home/ubuntu/results/coverage/OntTools/OntTools_1_10.coverage", "7e3ea2bd-9cec-41b1-bc39-7a77f28af4ec");
            OntClass c = i.next();
            writelineStatic("/home/ubuntu/results/coverage/OntTools/OntTools_1_10.coverage", "14d8c5a9-1bd0-4bfc-89e9-295179e1616c");
            boolean ignore = false;
            writelineStatic("/home/ubuntu/results/coverage/OntTools/OntTools_1_10.coverage", "ceb6d50f-d2cd-4e24-97a9-a052efe3e75a");
            // duplicate check: we ignore this class if we've already got it
            if (named.contains(c)) {
                writelineStatic("/home/ubuntu/results/coverage/OntTools/OntTools_1_10.coverage", "83ab3631-5799-45b8-be68-24dd31ea4ecb");
                ignore = true;
            }
            writelineStatic("/home/ubuntu/results/coverage/OntTools/OntTools_1_10.coverage", "abbfb40f-2fe2-4744-8eae-7d224b942389");
            // subsumption check: c must have only anon classes or Thing
            // as super-classes to still qualify as a root class
            Resource thing = c.getProfile().THING();
            writelineStatic("/home/ubuntu/results/coverage/OntTools/OntTools_1_10.coverage", "15959a29-9ab3-49b0-abaf-0839844a722f");
            for (Iterator<OntClass> j = c.listSuperClasses(); !ignore && j.hasNext(); ) {
                writelineStatic("/home/ubuntu/results/coverage/OntTools/OntTools_1_10.coverage", "23059512-74ef-46d9-bf79-7f0f4311ee49");
                OntClass sup = j.next();
                writelineStatic("/home/ubuntu/results/coverage/OntTools/OntTools_1_10.coverage", "8aa5203e-f363-41b1-954b-0e0da5a8ffe6");
                if (!((thing != null && sup.equals(thing)) || sup.isAnon() || sup.equals(c))) {
                    writelineStatic("/home/ubuntu/results/coverage/OntTools/OntTools_1_10.coverage", "96263b08-4961-42f1-83b2-fa52731ebd6d");
                    ignore = true;
                }
            }
            writelineStatic("/home/ubuntu/results/coverage/OntTools/OntTools_1_10.coverage", "36a221ba-df4f-465d-9982-b5e619ab44e3");
            if (!ignore) {
                writelineStatic("/home/ubuntu/results/coverage/OntTools/OntTools_1_10.coverage", "a40ebe7b-7065-4676-bc77-12c4f1f4f3bf");
                // place the class in the appropriate partition
                (c.isAnon() ? anon : named).add(c);
            }
        }
    }

    /**
     * A simple representation of disjoint sets
     */
    public static class DisjointSet {

        /**
         * The resource this set represents
         */
        private Resource m_node;

        /**
         * The parent set in a union
         */
        private DisjointSet m_parent;

        /**
         * Heuristic used to build balanced unions
         */
        private int m_rank;

        /**
         * The link to the distinguished member set
         */
        private DisjointSet m_ancestor;

        /**
         * Set to true when the node has been processed
         */
        private boolean m_black = false;

        /**
         * Set to true when we've inspected a black set, since the result is only
         * correct just after both of the sets for u and v have been marked black
         */
        private boolean m_used = false;

        public DisjointSet(Resource node) {
            m_node = node;
            m_rank = 0;
            m_parent = this;
        }

        public Resource getNode() {
            return m_node;
        }

        public DisjointSet getParent() {
            return m_parent;
        }

        public void setParent(DisjointSet parent) {
            m_parent = parent;
        }

        public int getRank() {
            return m_rank;
        }

        public void incrementRank() {
            m_rank++;
        }

        public DisjointSet getAncestor() {
            return m_ancestor;
        }

        public void setAncestor(DisjointSet anc) {
            m_ancestor = anc;
        }

        public void setBlack() {
            m_black = true;
        }

        public boolean isBlack() {
            return m_black;
        }

        public boolean used() {
            return m_used;
        }

        public void setUsed() {
            m_used = true;
        }

        /**
         * The find operation collapses the pointer to the root parent, which is
         * one of Tarjan's standard optimisations.
         * @return The representative of the union containing this set
         */
        public DisjointSet find() {
            DisjointSet root;
            if (getParent() == this) {
                // the representative of the set
                root = this;
            } else {
                // otherwise, seek the representative of my parent and save it
                root = getParent().find();
                setParent(root);
            }
            return root;
        }

        /**
         * The union of two sets
         * @param y
         */
        public void union(DisjointSet y) {
            DisjointSet xRoot = find();
            DisjointSet yRoot = y.find();
            if (xRoot.getRank() > yRoot.getRank()) {
                yRoot.setParent(xRoot);
            } else if (yRoot.getRank() > xRoot.getRank()) {
                xRoot.setParent(yRoot);
            } else if (xRoot != yRoot) {
                yRoot.setParent(xRoot);
                xRoot.incrementRank();
            }
        }

        /**
         * @see java.lang.Object#toString()
         * @return A string representation of this set for debugging
         */
        @Override
        public String toString() {
            StringBuilder buf = new StringBuilder();
            buf.append("DisjointSet{node=");
            buf.append(m_node);
            buf.append(",anc=");
            buf.append((getAncestor() == this) ? "self" : (getAncestor() == null ? "null" : getAncestor().toShortString()));
            buf.append(",parent=");
            buf.append((getParent() == this) ? "self" : (getParent() == null ? "null" : getParent().toShortString()));
            buf.append(",rank=");
            buf.append(getRank());
            buf.append(m_black ? ",black" : ",white");
            buf.append("}");
            return buf.toString();
        }

        public String toShortString() {
            StringBuilder buf = new StringBuilder();
            buf.append("DisjointSet{node=");
            buf.append(m_node);
            buf.append(",parent=");
            buf.append((getParent() == this) ? "self" : (getParent() == null ? "null" : getParent().toShortString()));
            buf.append("...}");
            return buf.toString();
        }
    }

    /**
     * Simple data structure mapping RDF nodes to disjoint sets, and
     * pairs of resources to their LCA.
     */
    public static class LCAIndex {

        private Map<Resource, DisjointSet> m_setIndex = new HashMap<Resource, DisjointSet>();

        private Map<Resource, Map<Resource, Resource>> m_lcaIndex = new HashMap<Resource, Map<Resource, Resource>>();

        public Resource getLCA(Resource u, Resource v) {
            Map<Resource, Resource> map = m_lcaIndex.get(u);
            Resource lca = (map == null) ? null : (Resource) map.get(v);
            if (lca == null) {
                map = m_lcaIndex.get(v);
                lca = (map == null) ? null : (Resource) map.get(u);
            }
            return lca;
        }

        public void setLCA(Resource u, Resource v, Resource lca) {
            Map<Resource, Resource> uMap = m_lcaIndex.get(u);
            if (uMap == null) {
                uMap = new HashMap<Resource, Resource>();
                m_lcaIndex.put(u, uMap);
            }
            uMap.put(v, lca);
        }

        public DisjointSet getSet(Resource r) {
            DisjointSet s = m_setIndex.get(r);
            if (s == null) {
                // log.debug( "Generating new set for " + r );
                s = new DisjointSet(r);
                m_setIndex.put(r, s);
            } else {
            // log.debug( "Retrieving old set for " + r );
            }
            return s;
        }
    }

    /**
     * A path is an application of {@link java.util.List} containing only {@link Statement}
     * objects, and in which for all adjacent elements <code>S<sub>i-1</sub></code>
     * and  <code>S<sub>i</sub></code>, where <code>i &gt; 0</code>, it is true that:
     * <code><pre>S<sub>i-1</sub>.getObject().equals( S<sub>i</sub>.getSubject() )</pre></code>
     */
    public static class Path extends ArrayList<Statement> {

        public Path() {
            super();
        }

        public Path(Path basePath) {
            super(basePath);
        }

        public Statement getStatement(int i) {
            return get(i);
        }

        /**
         * Answer a new Path whose elements are this Path with <code>s</code> added at the end
         */
        public Path append(Statement s) {
            Path newPath = new Path(this);
            newPath.add(s);
            return newPath;
        }

        /**
         * Answer true if the last link on the path has object equal to <code>n</code>
         */
        public boolean hasTerminus(RDFNode n) {
            return n != null && n.equals(getTerminal());
        }

        /**
         * Answer the RDF node at the end of the path, if defined, or null
         */
        public RDFNode getTerminal() {
            return size() > 0 ? get(size() - 1).getObject() : null;
        }

        /**
         * Answer the resource at the end of the path, if defined, or null
         */
        public Resource getTerminalResource() {
            RDFNode n = getTerminal();
            return (n != null && n.isResource()) ? (Resource) n : null;
        }
    }

    /**
     * A filter which accepts statements whose predicate matches one of a collection
     * of predicates held by the filter object.
     */
    public static class PredicatesFilter implements Predicate<Statement> {

        public Collection<Property> m_preds;

        /**
         * Accept statements with any predicate from <code>preds</code>
         */
        public PredicatesFilter(Collection<Property> preds) {
            m_preds = preds;
        }

        /**
         * Accept statements with any predicate from <code>preds</code>
         */
        public PredicatesFilter(Property[] preds) {
            m_preds = new HashSet<Property>();
            for (Property pred : preds) {
                m_preds.add(pred);
            }
        }

        /**
         * Accept statements with predicate <code>pred</code>
         */
        public PredicatesFilter(Property pred) {
            m_preds = new HashSet<Property>();
            m_preds.add(pred);
        }

        @Override
        public boolean test(Statement s) {
            return m_preds.contains(s.getPredicate());
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
