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
        writelineStatic("/home/ubuntu/results/coverage/OntTools/OntTools_8_10.coverage", "de9b8667-ff01-427e-b9ca-51466e0b1dcc");
        Resource root = m.getProfile().THING();
        writelineStatic("/home/ubuntu/results/coverage/OntTools/OntTools_8_10.coverage", "bc44a652-6a48-4025-834d-6aebcec831e7");
        if (root == null) {
            writelineStatic("/home/ubuntu/results/coverage/OntTools/OntTools_8_10.coverage", "e38bc77c-26ad-43db-9dfc-1a433beb33f0");
            throw new JenaException("The given OntModel has a language profile that does not define a generic root class (such as owl:Thing)");
        }
        writelineStatic("/home/ubuntu/results/coverage/OntTools/OntTools_8_10.coverage", "3b4aa6ca-faef-4e44-98a7-c3d0566327fa");
        root = root.inModel(m);
        writelineStatic("/home/ubuntu/results/coverage/OntTools/OntTools_8_10.coverage", "87c772b8-15e5-4a81-ad2d-dee14d921e6d");
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
        writelineStatic("/home/ubuntu/results/coverage/OntTools/OntTools_8_10.coverage", "2a55ba81-63ea-4539-a772-ae6fccbcd148");
        // check some common cases first
        if (u.equals(root) || v.equals(root)) {
            writelineStatic("/home/ubuntu/results/coverage/OntTools/OntTools_8_10.coverage", "f19cf0c2-d1e0-45f1-b4e0-5365898269bc");
            return root;
        }
        writelineStatic("/home/ubuntu/results/coverage/OntTools/OntTools_8_10.coverage", "5a414942-516b-4a25-8026-1e59ede453e1");
        if (u.hasSubClass(v)) {
            writelineStatic("/home/ubuntu/results/coverage/OntTools/OntTools_8_10.coverage", "39d4d788-a64f-4cbe-bfc8-e563164fc5a7");
            return u;
        }
        writelineStatic("/home/ubuntu/results/coverage/OntTools/OntTools_8_10.coverage", "f1673f53-f4bc-40f7-98f1-c5d62b285393");
        if (v.hasSubClass(u)) {
            writelineStatic("/home/ubuntu/results/coverage/OntTools/OntTools_8_10.coverage", "4a33117e-8456-44e7-9547-37795b56044b");
            return v;
        }
        writelineStatic("/home/ubuntu/results/coverage/OntTools/OntTools_8_10.coverage", "3eabd91e-bfbd-482b-93bd-ab605889fa26");
        // not a common case, so apply Tarjan's LCA algorithm
        LCAIndex index = new LCAIndex();
        writelineStatic("/home/ubuntu/results/coverage/OntTools/OntTools_8_10.coverage", "53b677cb-0d3d-4dc4-96ef-321569bd1c7b");
        lca(root, u, v, index);
        writelineStatic("/home/ubuntu/results/coverage/OntTools/OntTools_8_10.coverage", "afd64aa6-c24c-4081-9a7d-c284855fda8e");
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
        writelineStatic("/home/ubuntu/results/coverage/OntTools/OntTools_8_10.coverage", "92fce124-f419-488f-983c-d496ecbed4f4");
        List<Path> bfs = new LinkedList<Path>();
        writelineStatic("/home/ubuntu/results/coverage/OntTools/OntTools_8_10.coverage", "926b7415-66f6-4716-97e7-2faa1919a4c1");
        Set<Resource> seen = new HashSet<Resource>();
        writelineStatic("/home/ubuntu/results/coverage/OntTools/OntTools_8_10.coverage", "c62e4f1b-5d0b-4471-bfa3-4629ac221364");
        // initialise the paths
        for (Iterator<Statement> i = m.listStatements(start, null, (RDFNode) null).filterKeep(onPath); i.hasNext(); ) {
            writelineStatic("/home/ubuntu/results/coverage/OntTools/OntTools_8_10.coverage", "5ede2746-bdd2-435a-99a0-e6a64246fa7c");
            bfs.add(new Path().append(i.next()));
        }
        writelineStatic("/home/ubuntu/results/coverage/OntTools/OntTools_8_10.coverage", "0674899e-77e6-49c3-8238-d1a8e0477f23");
        // search
        Path solution = null;
        writelineStatic("/home/ubuntu/results/coverage/OntTools/OntTools_8_10.coverage", "45490764-6c00-451e-8073-cd2c7e546346");
        while (solution == null && !bfs.isEmpty()) {
            writelineStatic("/home/ubuntu/results/coverage/OntTools/OntTools_8_10.coverage", "fbcd8c13-c99f-478c-a887-5f3451ac66ae");
            Path candidate = bfs.remove(0);
            writelineStatic("/home/ubuntu/results/coverage/OntTools/OntTools_8_10.coverage", "cdfb5cf9-b3ed-47e2-bd56-291d5593bed8");
            if (candidate.hasTerminus(end)) {
                writelineStatic("/home/ubuntu/results/coverage/OntTools/OntTools_8_10.coverage", "bdbe1262-0fed-407d-ad50-f78d615b06ad");
                solution = candidate;
            } else {
                writelineStatic("/home/ubuntu/results/coverage/OntTools/OntTools_8_10.coverage", "906e36bd-1558-47a2-bf5c-1b3ffc244101");
                Resource terminus = candidate.getTerminalResource();
                writelineStatic("/home/ubuntu/results/coverage/OntTools/OntTools_8_10.coverage", "009063ee-cfac-4ad2-878c-cb44077c3abf");
                if (terminus != null) {
                    writelineStatic("/home/ubuntu/results/coverage/OntTools/OntTools_8_10.coverage", "9ec15caa-5004-4667-b485-ca36e9570610");
                    seen.add(terminus);
                    writelineStatic("/home/ubuntu/results/coverage/OntTools/OntTools_8_10.coverage", "ca98368d-68b8-4676-b315-a2243ddd7824");
                    // breadth-first expansion
                    for (Iterator<Statement> i = terminus.listProperties().filterKeep(onPath); i.hasNext(); ) {
                        writelineStatic("/home/ubuntu/results/coverage/OntTools/OntTools_8_10.coverage", "3285eb84-61df-42f5-960c-496d78cb6507");
                        Statement link = i.next();
                        writelineStatic("/home/ubuntu/results/coverage/OntTools/OntTools_8_10.coverage", "0ed866cf-90d7-40cc-8b8d-83061feb90e7");
                        // no looping allowed, so we skip this link if it takes us to a node we've seen
                        if (!seen.contains(link.getObject())) {
                            writelineStatic("/home/ubuntu/results/coverage/OntTools/OntTools_8_10.coverage", "0021d049-fd19-42e5-8cb9-69ff9280496c");
                            bfs.add(candidate.append(link));
                        }
                    }
                }
            }
        }
        writelineStatic("/home/ubuntu/results/coverage/OntTools/OntTools_8_10.coverage", "a866f318-3541-4020-beca-c1b07a982fda");
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
        writelineStatic("/home/ubuntu/results/coverage/OntTools/OntTools_8_10.coverage", "4874e8b4-8cf0-4f57-b628-8bae1ca3106f");
        // named roots
        List<OntClass> nhr = new ArrayList<OntClass>();
        writelineStatic("/home/ubuntu/results/coverage/OntTools/OntTools_8_10.coverage", "83346ccb-ab70-4402-9f85-302cd0d14249");
        // anon roots
        List<OntClass> ahr = new ArrayList<OntClass>();
        writelineStatic("/home/ubuntu/results/coverage/OntTools/OntTools_8_10.coverage", "c7122077-dc15-49aa-9842-66a8bf01dbe1");
        // do the initial partition of the root classes
        partitionByNamed(m.listHierarchyRootClasses(), nhr, ahr);
        writelineStatic("/home/ubuntu/results/coverage/OntTools/OntTools_8_10.coverage", "db756c7f-a821-4aed-973c-cc1fe227691b");
        // now push the fringe down until we have only named classes
        while (!ahr.isEmpty()) {
            writelineStatic("/home/ubuntu/results/coverage/OntTools/OntTools_8_10.coverage", "1e3a9baa-1f0f-401e-8030-ef727d15f8bb");
            OntClass c = ahr.remove(0);
            writelineStatic("/home/ubuntu/results/coverage/OntTools/OntTools_8_10.coverage", "311d6d01-dd68-43b8-9f6b-ad94393d03cd");
            partitionByNamed(c.listSubClasses(true), nhr, ahr);
        }
        writelineStatic("/home/ubuntu/results/coverage/OntTools/OntTools_8_10.coverage", "bacccdaa-b47b-4160-847d-9afc734af8df");
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
        writelineStatic("/home/ubuntu/results/coverage/OntTools/OntTools_8_10.coverage", "ed11fbb0-10db-439b-a58b-44dc44d9d5f7");
        // log.debug( "Entering lca(), cls = " + cls );
        DisjointSet clsSet = index.getSet(cls);
        writelineStatic("/home/ubuntu/results/coverage/OntTools/OntTools_8_10.coverage", "792dd4a3-f5d7-4de2-93c2-5eade1b10701");
        if (clsSet.isBlack()) {
            writelineStatic("/home/ubuntu/results/coverage/OntTools/OntTools_8_10.coverage", "c267c225-3473-4d2b-ae10-59edbabf5e5d");
            // already visited
            return clsSet;
        }
        writelineStatic("/home/ubuntu/results/coverage/OntTools/OntTools_8_10.coverage", "2b0ca7e0-221e-4506-83b5-57c79e3cb85f");
        // not visited yet
        clsSet.setAncestor(clsSet);
        writelineStatic("/home/ubuntu/results/coverage/OntTools/OntTools_8_10.coverage", "da5d8946-8789-4196-8bcd-27975ddd9560");
        // for each child of cls
        for (Iterator<OntClass> i = cls.listSubClasses(true); i.hasNext(); ) {
            writelineStatic("/home/ubuntu/results/coverage/OntTools/OntTools_8_10.coverage", "11e8db50-554b-4294-8c95-684ef04bc019");
            OntClass child = i.next();
            writelineStatic("/home/ubuntu/results/coverage/OntTools/OntTools_8_10.coverage", "142602c1-b3c8-4960-bad1-e09f6b6c3f6c");
            if (child.equals(cls) || child.equals(cls.getProfile().NOTHING())) {
                writelineStatic("/home/ubuntu/results/coverage/OntTools/OntTools_8_10.coverage", "101005e3-de00-4f86-bb3e-746667aa20df");
                // we ignore the reflexive case and bottom
                continue;
            }
            writelineStatic("/home/ubuntu/results/coverage/OntTools/OntTools_8_10.coverage", "6d997982-96ed-422d-8268-367dac31fe16");
            // compute the LCA of the sub-tree
            DisjointSet v = lca(child, uCls, vCls, index);
            writelineStatic("/home/ubuntu/results/coverage/OntTools/OntTools_8_10.coverage", "70440083-650a-4d2f-b047-3501b223fe76");
            // union the two disjoint sets together
            clsSet.union(v);
            writelineStatic("/home/ubuntu/results/coverage/OntTools/OntTools_8_10.coverage", "b2d22a2a-7c04-454c-8493-470fa416b118");
            // propagate the distinguished member
            clsSet.find().setAncestor(clsSet);
        }
        writelineStatic("/home/ubuntu/results/coverage/OntTools/OntTools_8_10.coverage", "ce9a34d2-3b87-49e1-a0ff-0360b2d57022");
        // this node is done
        clsSet.setBlack();
        writelineStatic("/home/ubuntu/results/coverage/OntTools/OntTools_8_10.coverage", "698b0acc-fff4-452e-8a3e-ac58b870ac6c");
        // are we inspecting one of the elements we're interested in?
        if (cls.equals(uCls)) {
            writelineStatic("/home/ubuntu/results/coverage/OntTools/OntTools_8_10.coverage", "fa5fdd93-f3b5-4373-af60-3d472ff29ee4");
            checkSolution(uCls, vCls, index);
        } else if (cls.equals(vCls)) {
            writelineStatic("/home/ubuntu/results/coverage/OntTools/OntTools_8_10.coverage", "7ccc61ee-b2cf-4537-be81-69c1e8ff3825");
            checkSolution(vCls, uCls, index);
        }
        writelineStatic("/home/ubuntu/results/coverage/OntTools/OntTools_8_10.coverage", "42e996af-d231-4212-af89-819f1ce1727b");
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
        writelineStatic("/home/ubuntu/results/coverage/OntTools/OntTools_8_10.coverage", "9847746f-ad74-49ab-a399-8b81a2729ace");
        DisjointSet vSet = index.getSet(vCls);
        writelineStatic("/home/ubuntu/results/coverage/OntTools/OntTools_8_10.coverage", "4334ceaa-061a-4cdf-9a4b-991c76aca7d8");
        DisjointSet uSet = index.getSet(uCls);
        writelineStatic("/home/ubuntu/results/coverage/OntTools/OntTools_8_10.coverage", "6b491130-84ef-4483-a997-5562a8ea3402");
        if (vSet != null && vSet.isBlack() && !vSet.used() && uSet != null && uSet.isBlack() && !uSet.used()) {
            writelineStatic("/home/ubuntu/results/coverage/OntTools/OntTools_8_10.coverage", "f2376227-6fb5-48a4-9a9e-061d18bf64e0");
            vSet.setUsed();
            writelineStatic("/home/ubuntu/results/coverage/OntTools/OntTools_8_10.coverage", "261ed487-5529-4538-bdd3-8d99d10a4b15");
            uSet.setUsed();
            writelineStatic("/home/ubuntu/results/coverage/OntTools/OntTools_8_10.coverage", "5e215aa6-85ea-49c7-bfe7-beb8e841d6c4");
            // log.debug( "Found LCA: u = " + uCls + ", v = " + vCls  );
            OntClass lca = (OntClass) vSet.find().getAncestor().getNode();
            writelineStatic("/home/ubuntu/results/coverage/OntTools/OntTools_8_10.coverage", "f7095c4f-a43d-488a-a584-a28254216c64");
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
        writelineStatic("/home/ubuntu/results/coverage/OntTools/OntTools_8_10.coverage", "cb6a9875-6802-40e0-9886-fa40357db034");
        while (i.hasNext()) {
            writelineStatic("/home/ubuntu/results/coverage/OntTools/OntTools_8_10.coverage", "f920ab30-e885-4218-9f90-7b7328199765");
            OntClass c = i.next();
            writelineStatic("/home/ubuntu/results/coverage/OntTools/OntTools_8_10.coverage", "f46dad67-ef23-46d5-bbf9-9242ab54bd4c");
            boolean ignore = false;
            writelineStatic("/home/ubuntu/results/coverage/OntTools/OntTools_8_10.coverage", "849ce857-9f91-410f-af48-c60825f729d4");
            // duplicate check: we ignore this class if we've already got it
            if (named.contains(c)) {
                writelineStatic("/home/ubuntu/results/coverage/OntTools/OntTools_8_10.coverage", "fdbdb0c6-8e8e-49e6-931b-4f7348249627");
                ignore = true;
            }
            writelineStatic("/home/ubuntu/results/coverage/OntTools/OntTools_8_10.coverage", "eeabda8b-5650-4147-adb2-9b3137f393c0");
            // subsumption check: c must have only anon classes or Thing
            // as super-classes to still qualify as a root class
            Resource thing = c.getProfile().THING();
            writelineStatic("/home/ubuntu/results/coverage/OntTools/OntTools_8_10.coverage", "7e12e6fe-1a7d-4f89-ae4b-70872756a740");
            for (Iterator<OntClass> j = c.listSuperClasses(); !ignore && j.hasNext(); ) {
                writelineStatic("/home/ubuntu/results/coverage/OntTools/OntTools_8_10.coverage", "0ba3dd79-4e65-4fb4-9367-f2fa21063615");
                OntClass sup = j.next();
                writelineStatic("/home/ubuntu/results/coverage/OntTools/OntTools_8_10.coverage", "554240c6-6e4a-418d-95c0-8de4780f7769");
                if (!((thing != null && sup.equals(thing)) || sup.isAnon() || sup.equals(c))) {
                    writelineStatic("/home/ubuntu/results/coverage/OntTools/OntTools_8_10.coverage", "7c4bec37-0ed6-482a-b450-e0cc2b04c797");
                    ignore = true;
                }
            }
            writelineStatic("/home/ubuntu/results/coverage/OntTools/OntTools_8_10.coverage", "ee07bed2-58e5-4eca-a570-1af72e81ccfb");
            if (!ignore) {
                writelineStatic("/home/ubuntu/results/coverage/OntTools/OntTools_8_10.coverage", "8cead925-d627-46af-bf64-61dd86f2a5a9");
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
