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
        writelineStatic("/home/ubuntu/results/coverage/OntTools/OntTools_2_10.coverage", "4e26e9ee-1ce8-45ba-9a19-dbceaa064138");
        Resource root = m.getProfile().THING();
        writelineStatic("/home/ubuntu/results/coverage/OntTools/OntTools_2_10.coverage", "e17ffc2a-0677-4e18-9b02-8514ea2f1d6f");
        if (root == null) {
            writelineStatic("/home/ubuntu/results/coverage/OntTools/OntTools_2_10.coverage", "ed5ba7c1-03b8-4c0d-a5c6-2e8116c07253");
            throw new JenaException("The given OntModel has a language profile that does not define a generic root class (such as owl:Thing)");
        }
        writelineStatic("/home/ubuntu/results/coverage/OntTools/OntTools_2_10.coverage", "38324fe5-1f23-4578-bceb-76b49aa03ff3");
        root = root.inModel(m);
        writelineStatic("/home/ubuntu/results/coverage/OntTools/OntTools_2_10.coverage", "2b0e7598-dcd1-44f9-833e-c8e063b82c3b");
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
        writelineStatic("/home/ubuntu/results/coverage/OntTools/OntTools_2_10.coverage", "431d9558-3180-402f-959d-31eb594eb59a");
        // check some common cases first
        if (u.equals(root) || v.equals(root)) {
            writelineStatic("/home/ubuntu/results/coverage/OntTools/OntTools_2_10.coverage", "efcadf16-552f-4289-837f-28928f8f6f8a");
            return root;
        }
        writelineStatic("/home/ubuntu/results/coverage/OntTools/OntTools_2_10.coverage", "38b73c39-8dc1-4d7f-9cee-4a1144eb9732");
        if (u.hasSubClass(v)) {
            writelineStatic("/home/ubuntu/results/coverage/OntTools/OntTools_2_10.coverage", "e455a323-e6d3-4737-9b01-c6df363a210d");
            return u;
        }
        writelineStatic("/home/ubuntu/results/coverage/OntTools/OntTools_2_10.coverage", "56646660-e45e-4932-ba93-35048b2f76b6");
        if (v.hasSubClass(u)) {
            writelineStatic("/home/ubuntu/results/coverage/OntTools/OntTools_2_10.coverage", "c27ac481-6218-45b4-9ee9-b8efbc0d51c4");
            return v;
        }
        writelineStatic("/home/ubuntu/results/coverage/OntTools/OntTools_2_10.coverage", "03c32ec0-faa3-430b-b935-5f3e04c3e3ec");
        // not a common case, so apply Tarjan's LCA algorithm
        LCAIndex index = new LCAIndex();
        writelineStatic("/home/ubuntu/results/coverage/OntTools/OntTools_2_10.coverage", "fbff023f-b942-40a2-b1df-427ab84709db");
        lca(root, u, v, index);
        writelineStatic("/home/ubuntu/results/coverage/OntTools/OntTools_2_10.coverage", "bb88760e-0588-4fa7-b80c-3782bfdcc465");
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
        writelineStatic("/home/ubuntu/results/coverage/OntTools/OntTools_2_10.coverage", "b9c39800-2701-4b35-9b7c-11343ed60767");
        List<Path> bfs = new LinkedList<Path>();
        writelineStatic("/home/ubuntu/results/coverage/OntTools/OntTools_2_10.coverage", "61af1205-bcf7-45ea-b0a3-aad85f1d4e73");
        Set<Resource> seen = new HashSet<Resource>();
        writelineStatic("/home/ubuntu/results/coverage/OntTools/OntTools_2_10.coverage", "8f7b0f64-3014-42f0-9c44-cd114985a0e2");
        // initialise the paths
        for (Iterator<Statement> i = m.listStatements(start, null, (RDFNode) null).filterKeep(onPath); i.hasNext(); ) {
            writelineStatic("/home/ubuntu/results/coverage/OntTools/OntTools_2_10.coverage", "70aa7237-a5cd-4c15-ab32-df8342d8ce95");
            bfs.add(new Path().append(i.next()));
        }
        writelineStatic("/home/ubuntu/results/coverage/OntTools/OntTools_2_10.coverage", "93d85aac-87f7-47e0-8302-699d41869de0");
        // search
        Path solution = null;
        writelineStatic("/home/ubuntu/results/coverage/OntTools/OntTools_2_10.coverage", "11b2aa89-ea1a-4580-9f8b-4fba42ca7f8f");
        while (solution == null && !bfs.isEmpty()) {
            writelineStatic("/home/ubuntu/results/coverage/OntTools/OntTools_2_10.coverage", "d069dc57-569c-418e-88e6-31ce1d5c9e78");
            Path candidate = bfs.remove(0);
            writelineStatic("/home/ubuntu/results/coverage/OntTools/OntTools_2_10.coverage", "287bd231-6b17-4c35-8f86-17ba3b9faf07");
            if (candidate.hasTerminus(end)) {
                writelineStatic("/home/ubuntu/results/coverage/OntTools/OntTools_2_10.coverage", "b2d3e4cb-3d27-4854-bd04-a4e0128843b2");
                solution = candidate;
            } else {
                writelineStatic("/home/ubuntu/results/coverage/OntTools/OntTools_2_10.coverage", "7c66e882-8843-470d-aebb-12d2eda90513");
                Resource terminus = candidate.getTerminalResource();
                writelineStatic("/home/ubuntu/results/coverage/OntTools/OntTools_2_10.coverage", "4fcfd461-dc23-41e5-afa0-24ff4f176469");
                if (terminus != null) {
                    writelineStatic("/home/ubuntu/results/coverage/OntTools/OntTools_2_10.coverage", "c8c48df8-aa04-4944-8325-1737c40f7cac");
                    seen.add(terminus);
                    writelineStatic("/home/ubuntu/results/coverage/OntTools/OntTools_2_10.coverage", "756af445-5934-4fda-af68-9b9b2de90771");
                    // breadth-first expansion
                    for (Iterator<Statement> i = terminus.listProperties().filterKeep(onPath); i.hasNext(); ) {
                        writelineStatic("/home/ubuntu/results/coverage/OntTools/OntTools_2_10.coverage", "d0cf7775-77de-40a0-9716-dc50b9f23a37");
                        Statement link = i.next();
                        writelineStatic("/home/ubuntu/results/coverage/OntTools/OntTools_2_10.coverage", "82f7a0bd-5247-4263-89ef-d911150800da");
                        // no looping allowed, so we skip this link if it takes us to a node we've seen
                        if (!seen.contains(link.getObject())) {
                            writelineStatic("/home/ubuntu/results/coverage/OntTools/OntTools_2_10.coverage", "eba95715-c86e-4531-bf29-19f8fd0bfa28");
                            bfs.add(candidate.append(link));
                        }
                    }
                }
            }
        }
        writelineStatic("/home/ubuntu/results/coverage/OntTools/OntTools_2_10.coverage", "9c3de4f6-d997-4981-a868-c29474ab3bc0");
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
        writelineStatic("/home/ubuntu/results/coverage/OntTools/OntTools_2_10.coverage", "afd49dcf-2f88-4c10-9f6e-fa17e44962e5");
        // named roots
        List<OntClass> nhr = new ArrayList<OntClass>();
        writelineStatic("/home/ubuntu/results/coverage/OntTools/OntTools_2_10.coverage", "dc830a98-fa1f-49d1-b4ef-2a477cfb7e76");
        // anon roots
        List<OntClass> ahr = new ArrayList<OntClass>();
        writelineStatic("/home/ubuntu/results/coverage/OntTools/OntTools_2_10.coverage", "f6a4dd45-92b1-43b9-a9d7-5ce4e6224780");
        // do the initial partition of the root classes
        partitionByNamed(m.listHierarchyRootClasses(), nhr, ahr);
        writelineStatic("/home/ubuntu/results/coverage/OntTools/OntTools_2_10.coverage", "2a310a56-3b8d-4d8a-8927-2aa2502a6ccc");
        // now push the fringe down until we have only named classes
        while (!ahr.isEmpty()) {
            writelineStatic("/home/ubuntu/results/coverage/OntTools/OntTools_2_10.coverage", "70afb2f5-830b-4a5d-9efa-2c8d298feb4d");
            OntClass c = ahr.remove(0);
            writelineStatic("/home/ubuntu/results/coverage/OntTools/OntTools_2_10.coverage", "4d1564fc-64a3-4c50-ae91-8f138e04caa0");
            partitionByNamed(c.listSubClasses(true), nhr, ahr);
        }
        writelineStatic("/home/ubuntu/results/coverage/OntTools/OntTools_2_10.coverage", "e6f15a68-97ef-400f-a421-1dad99d2dda4");
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
        writelineStatic("/home/ubuntu/results/coverage/OntTools/OntTools_2_10.coverage", "8bfd792f-1340-415e-a66e-832c8c355bc7");
        // log.debug( "Entering lca(), cls = " + cls );
        DisjointSet clsSet = index.getSet(cls);
        writelineStatic("/home/ubuntu/results/coverage/OntTools/OntTools_2_10.coverage", "8eb5707c-32f4-4d7f-bd7a-ed9c4a1860c7");
        if (clsSet.isBlack()) {
            writelineStatic("/home/ubuntu/results/coverage/OntTools/OntTools_2_10.coverage", "46463031-6893-4fe3-b6c4-c9cdd5c0c1db");
            // already visited
            return clsSet;
        }
        writelineStatic("/home/ubuntu/results/coverage/OntTools/OntTools_2_10.coverage", "1af21c90-3430-443f-957d-7a2d68775c8b");
        // not visited yet
        clsSet.setAncestor(clsSet);
        writelineStatic("/home/ubuntu/results/coverage/OntTools/OntTools_2_10.coverage", "36f8a906-3a9d-4c42-822b-df1778545942");
        // for each child of cls
        for (Iterator<OntClass> i = cls.listSubClasses(true); i.hasNext(); ) {
            writelineStatic("/home/ubuntu/results/coverage/OntTools/OntTools_2_10.coverage", "cc1dd134-9714-4d6b-a9c6-415563711198");
            OntClass child = i.next();
            writelineStatic("/home/ubuntu/results/coverage/OntTools/OntTools_2_10.coverage", "41825183-409b-463e-9698-128c46f44823");
            if (child.equals(cls) || child.equals(cls.getProfile().NOTHING())) {
                writelineStatic("/home/ubuntu/results/coverage/OntTools/OntTools_2_10.coverage", "b92ec0c4-2d76-418d-885c-c5c05d756bdc");
                // we ignore the reflexive case and bottom
                continue;
            }
            writelineStatic("/home/ubuntu/results/coverage/OntTools/OntTools_2_10.coverage", "fc48b9ed-cd03-42fe-b180-7cb1472f4857");
            // compute the LCA of the sub-tree
            DisjointSet v = lca(child, uCls, vCls, index);
            writelineStatic("/home/ubuntu/results/coverage/OntTools/OntTools_2_10.coverage", "f2b09a97-3747-45fe-9100-e8aff8b0792e");
            // union the two disjoint sets together
            clsSet.union(v);
            writelineStatic("/home/ubuntu/results/coverage/OntTools/OntTools_2_10.coverage", "4a7f4cfe-2d4b-4ecc-9e3d-3f4b105df89f");
            // propagate the distinguished member
            clsSet.find().setAncestor(clsSet);
        }
        writelineStatic("/home/ubuntu/results/coverage/OntTools/OntTools_2_10.coverage", "00acd08a-b989-4bee-8f11-128edae36a7a");
        // this node is done
        clsSet.setBlack();
        writelineStatic("/home/ubuntu/results/coverage/OntTools/OntTools_2_10.coverage", "524e29fb-b060-4ed8-959e-2467f9cc19e5");
        // are we inspecting one of the elements we're interested in?
        if (cls.equals(uCls)) {
            writelineStatic("/home/ubuntu/results/coverage/OntTools/OntTools_2_10.coverage", "20440cf5-9214-403b-988d-086ec8b5fa6f");
            checkSolution(uCls, vCls, index);
        } else if (cls.equals(vCls)) {
            writelineStatic("/home/ubuntu/results/coverage/OntTools/OntTools_2_10.coverage", "b5ca228c-919f-483a-88f9-5659c8dfb859");
            checkSolution(vCls, uCls, index);
        }
        writelineStatic("/home/ubuntu/results/coverage/OntTools/OntTools_2_10.coverage", "a01a44a9-8008-4c98-8301-e36ef8f2e504");
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
        writelineStatic("/home/ubuntu/results/coverage/OntTools/OntTools_2_10.coverage", "ae0a1f1d-c040-4af9-b33a-a0b703fa11df");
        DisjointSet vSet = index.getSet(vCls);
        writelineStatic("/home/ubuntu/results/coverage/OntTools/OntTools_2_10.coverage", "724aa442-48a4-4700-a4cf-582595105098");
        DisjointSet uSet = index.getSet(uCls);
        writelineStatic("/home/ubuntu/results/coverage/OntTools/OntTools_2_10.coverage", "0219c959-b857-445f-b77b-ffb4472cf891");
        if (vSet != null && vSet.isBlack() && !vSet.used() && uSet != null && uSet.isBlack() && !uSet.used()) {
            writelineStatic("/home/ubuntu/results/coverage/OntTools/OntTools_2_10.coverage", "67029258-d70e-4e08-9e04-8ea2763e9e84");
            vSet.setUsed();
            writelineStatic("/home/ubuntu/results/coverage/OntTools/OntTools_2_10.coverage", "d34f24b4-4c52-44a9-9812-dfdc2b5e0e0f");
            uSet.setUsed();
            writelineStatic("/home/ubuntu/results/coverage/OntTools/OntTools_2_10.coverage", "d7c0ee6f-10f3-4f07-aaee-abe088ec54fc");
            // log.debug( "Found LCA: u = " + uCls + ", v = " + vCls  );
            OntClass lca = (OntClass) vSet.find().getAncestor().getNode();
            writelineStatic("/home/ubuntu/results/coverage/OntTools/OntTools_2_10.coverage", "9dad79f9-31a3-4e13-aa09-2b00b2dc05fb");
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
        writelineStatic("/home/ubuntu/results/coverage/OntTools/OntTools_2_10.coverage", "1a3676e4-ee0f-45a5-a377-eb3ebbb6ef4b");
        while (i.hasNext()) {
            writelineStatic("/home/ubuntu/results/coverage/OntTools/OntTools_2_10.coverage", "7bc0f7e0-de3f-4293-858a-f7f3a257b128");
            OntClass c = i.next();
            writelineStatic("/home/ubuntu/results/coverage/OntTools/OntTools_2_10.coverage", "05341632-95bc-49ee-939c-60b8b1bd7213");
            boolean ignore = false;
            writelineStatic("/home/ubuntu/results/coverage/OntTools/OntTools_2_10.coverage", "709ae38c-d982-4c69-ba0e-e60c76e4cd0b");
            // duplicate check: we ignore this class if we've already got it
            if (named.contains(c)) {
                writelineStatic("/home/ubuntu/results/coverage/OntTools/OntTools_2_10.coverage", "2c2c8bee-4159-4c4b-8d96-3a869be29fdb");
                ignore = true;
            }
            writelineStatic("/home/ubuntu/results/coverage/OntTools/OntTools_2_10.coverage", "b98eca7a-5f15-491a-ae46-34adf1267d81");
            // subsumption check: c must have only anon classes or Thing
            // as super-classes to still qualify as a root class
            Resource thing = c.getProfile().THING();
            writelineStatic("/home/ubuntu/results/coverage/OntTools/OntTools_2_10.coverage", "afe2a9bf-ec66-4c7e-afdd-d809b2494855");
            for (Iterator<OntClass> j = c.listSuperClasses(); !ignore && j.hasNext(); ) {
                writelineStatic("/home/ubuntu/results/coverage/OntTools/OntTools_2_10.coverage", "53e6aa81-3c12-4787-8a89-3d64319fa98b");
                OntClass sup = j.next();
                writelineStatic("/home/ubuntu/results/coverage/OntTools/OntTools_2_10.coverage", "d068af5e-9b0d-4f3d-9df9-58336e04f933");
                if (!((thing != null && sup.equals(thing)) || sup.isAnon() || sup.equals(c))) {
                    writelineStatic("/home/ubuntu/results/coverage/OntTools/OntTools_2_10.coverage", "765c267f-2dc0-49f7-87dd-45cee1e66322");
                    ignore = true;
                }
            }
            writelineStatic("/home/ubuntu/results/coverage/OntTools/OntTools_2_10.coverage", "7652133c-19c5-451c-a1e6-d49f53189ebd");
            if (!ignore) {
                writelineStatic("/home/ubuntu/results/coverage/OntTools/OntTools_2_10.coverage", "8f762458-92cd-426b-9bd8-bcb9dfea4e83");
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
