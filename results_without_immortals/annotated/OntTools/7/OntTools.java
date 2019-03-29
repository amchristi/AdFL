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
        writelineStatic("/home/ubuntu/results/coverage/OntTools/OntTools_7_10.coverage", "ec67469a-e3e3-4fb6-8dd2-a5994b6264c3");
        Resource root = m.getProfile().THING();
        writelineStatic("/home/ubuntu/results/coverage/OntTools/OntTools_7_10.coverage", "a1d10057-24bf-42b7-af58-7ce474d778f1");
        if (root == null) {
            writelineStatic("/home/ubuntu/results/coverage/OntTools/OntTools_7_10.coverage", "6706a796-7a15-4efd-8363-a45becca0e58");
            throw new JenaException("The given OntModel has a language profile that does not define a generic root class (such as owl:Thing)");
        }
        writelineStatic("/home/ubuntu/results/coverage/OntTools/OntTools_7_10.coverage", "18372794-04f8-41cb-b49b-53da33e67fc6");
        root = root.inModel(m);
        writelineStatic("/home/ubuntu/results/coverage/OntTools/OntTools_7_10.coverage", "3b27d39e-8cdc-44f7-aa9f-652c840168a1");
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
        writelineStatic("/home/ubuntu/results/coverage/OntTools/OntTools_7_10.coverage", "e639f24e-a98c-4ec7-bcb3-a449532d7353");
        // check some common cases first
        if (u.equals(root) || v.equals(root)) {
            writelineStatic("/home/ubuntu/results/coverage/OntTools/OntTools_7_10.coverage", "9a068b02-6608-4d43-9185-ce51b4ebab92");
            return root;
        }
        writelineStatic("/home/ubuntu/results/coverage/OntTools/OntTools_7_10.coverage", "2e22ea8d-8935-4949-9c8e-dca37e863ea6");
        if (u.hasSubClass(v)) {
            writelineStatic("/home/ubuntu/results/coverage/OntTools/OntTools_7_10.coverage", "d7594ad8-341f-4a48-bc37-1772846e7a2c");
            return u;
        }
        writelineStatic("/home/ubuntu/results/coverage/OntTools/OntTools_7_10.coverage", "34d163a5-c521-448b-b285-5538cf8bb502");
        if (v.hasSubClass(u)) {
            writelineStatic("/home/ubuntu/results/coverage/OntTools/OntTools_7_10.coverage", "daffe425-afc0-47d9-a278-06b14147fb97");
            return v;
        }
        writelineStatic("/home/ubuntu/results/coverage/OntTools/OntTools_7_10.coverage", "c9874482-0e4e-4c58-8ba7-7b6e845d221d");
        // not a common case, so apply Tarjan's LCA algorithm
        LCAIndex index = new LCAIndex();
        writelineStatic("/home/ubuntu/results/coverage/OntTools/OntTools_7_10.coverage", "6db069b2-faed-4e2d-a911-30815b3d7fe7");
        lca(root, u, v, index);
        writelineStatic("/home/ubuntu/results/coverage/OntTools/OntTools_7_10.coverage", "c0bdb1a4-0bd7-47ad-afdd-5f27bb1a1b4f");
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
        writelineStatic("/home/ubuntu/results/coverage/OntTools/OntTools_7_10.coverage", "9ee709ed-687a-43e0-9249-3ac9ef6da006");
        List<Path> bfs = new LinkedList<Path>();
        writelineStatic("/home/ubuntu/results/coverage/OntTools/OntTools_7_10.coverage", "f5417023-793e-4972-a867-6725ca6336a9");
        Set<Resource> seen = new HashSet<Resource>();
        writelineStatic("/home/ubuntu/results/coverage/OntTools/OntTools_7_10.coverage", "3b952ba6-9738-45c9-bee2-00b84b91dd2a");
        // initialise the paths
        for (Iterator<Statement> i = m.listStatements(start, null, (RDFNode) null).filterKeep(onPath); i.hasNext(); ) {
            writelineStatic("/home/ubuntu/results/coverage/OntTools/OntTools_7_10.coverage", "d4742e20-1963-4eee-8d90-47557cf5fd97");
            bfs.add(new Path().append(i.next()));
        }
        writelineStatic("/home/ubuntu/results/coverage/OntTools/OntTools_7_10.coverage", "f102d861-fbb2-44a8-8f0f-ca1ccfe92b46");
        // search
        Path solution = null;
        writelineStatic("/home/ubuntu/results/coverage/OntTools/OntTools_7_10.coverage", "cf75dc8a-58fa-4630-9dd1-9181dcd1774c");
        while (solution == null && !bfs.isEmpty()) {
            writelineStatic("/home/ubuntu/results/coverage/OntTools/OntTools_7_10.coverage", "37d2fc27-5a74-4d22-956e-83ca0225dbd4");
            Path candidate = bfs.remove(0);
            writelineStatic("/home/ubuntu/results/coverage/OntTools/OntTools_7_10.coverage", "a01b9d9f-3c08-46c7-ab1f-e8ce1e8f500d");
            if (candidate.hasTerminus(end)) {
                writelineStatic("/home/ubuntu/results/coverage/OntTools/OntTools_7_10.coverage", "86f05f2d-98d6-4cd0-b185-eb73c4f0fd25");
                solution = candidate;
            } else {
                writelineStatic("/home/ubuntu/results/coverage/OntTools/OntTools_7_10.coverage", "92849519-7948-4b70-a088-4939c9574fd9");
                Resource terminus = candidate.getTerminalResource();
                writelineStatic("/home/ubuntu/results/coverage/OntTools/OntTools_7_10.coverage", "829f335d-53e0-4031-8498-0886b2c94e86");
                if (terminus != null) {
                    writelineStatic("/home/ubuntu/results/coverage/OntTools/OntTools_7_10.coverage", "46158810-8e0e-4631-9198-8d7959c88d80");
                    seen.add(terminus);
                    writelineStatic("/home/ubuntu/results/coverage/OntTools/OntTools_7_10.coverage", "68061b77-ad65-4d8f-81fb-c69491776697");
                    // breadth-first expansion
                    for (Iterator<Statement> i = terminus.listProperties().filterKeep(onPath); i.hasNext(); ) {
                        writelineStatic("/home/ubuntu/results/coverage/OntTools/OntTools_7_10.coverage", "5402faf4-ad2a-447f-a7e9-5e0485049a4a");
                        Statement link = i.next();
                        writelineStatic("/home/ubuntu/results/coverage/OntTools/OntTools_7_10.coverage", "e2462013-1c7d-4183-9f16-a912e42cc9ad");
                        // no looping allowed, so we skip this link if it takes us to a node we've seen
                        if (!seen.contains(link.getObject())) {
                            writelineStatic("/home/ubuntu/results/coverage/OntTools/OntTools_7_10.coverage", "5c008e86-1ca9-4d08-bc12-6ffaafeb9cb5");
                            bfs.add(candidate.append(link));
                        }
                    }
                }
            }
        }
        writelineStatic("/home/ubuntu/results/coverage/OntTools/OntTools_7_10.coverage", "57b08b8d-8afa-464a-94ba-8c70a1678d54");
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
        writelineStatic("/home/ubuntu/results/coverage/OntTools/OntTools_7_10.coverage", "323dc6cd-4434-48ea-b44f-6c6e888c1601");
        // named roots
        List<OntClass> nhr = new ArrayList<OntClass>();
        writelineStatic("/home/ubuntu/results/coverage/OntTools/OntTools_7_10.coverage", "7ab10b83-71d7-4477-97f7-03b2138cebbc");
        // anon roots
        List<OntClass> ahr = new ArrayList<OntClass>();
        writelineStatic("/home/ubuntu/results/coverage/OntTools/OntTools_7_10.coverage", "e35b2c9d-e364-46ba-be5c-746badb77a09");
        // do the initial partition of the root classes
        partitionByNamed(m.listHierarchyRootClasses(), nhr, ahr);
        writelineStatic("/home/ubuntu/results/coverage/OntTools/OntTools_7_10.coverage", "3df4b3e8-226a-43f7-9ca3-60e1e7d57e49");
        // now push the fringe down until we have only named classes
        while (!ahr.isEmpty()) {
            writelineStatic("/home/ubuntu/results/coverage/OntTools/OntTools_7_10.coverage", "12ca0627-9e54-430f-8f9f-b5eed68e094e");
            OntClass c = ahr.remove(0);
            writelineStatic("/home/ubuntu/results/coverage/OntTools/OntTools_7_10.coverage", "298cc95d-7d21-479d-808d-f7e5583b1619");
            partitionByNamed(c.listSubClasses(true), nhr, ahr);
        }
        writelineStatic("/home/ubuntu/results/coverage/OntTools/OntTools_7_10.coverage", "9c5c0187-f217-4b8e-8083-e81d155d2c4b");
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
        writelineStatic("/home/ubuntu/results/coverage/OntTools/OntTools_7_10.coverage", "fa0d363a-a998-4615-933b-538ff787b270");
        // log.debug( "Entering lca(), cls = " + cls );
        DisjointSet clsSet = index.getSet(cls);
        writelineStatic("/home/ubuntu/results/coverage/OntTools/OntTools_7_10.coverage", "0f5d3e0f-446d-44f8-b139-65f0566ab144");
        if (clsSet.isBlack()) {
            writelineStatic("/home/ubuntu/results/coverage/OntTools/OntTools_7_10.coverage", "1b96d6a0-7ca7-4628-93ea-43b115b2b4ad");
            // already visited
            return clsSet;
        }
        writelineStatic("/home/ubuntu/results/coverage/OntTools/OntTools_7_10.coverage", "b3736295-285e-4b29-9425-7bd3f2b01053");
        // not visited yet
        clsSet.setAncestor(clsSet);
        writelineStatic("/home/ubuntu/results/coverage/OntTools/OntTools_7_10.coverage", "d51ff3b0-2d83-4f33-b87e-d9e7ec432f30");
        // for each child of cls
        for (Iterator<OntClass> i = cls.listSubClasses(true); i.hasNext(); ) {
            writelineStatic("/home/ubuntu/results/coverage/OntTools/OntTools_7_10.coverage", "6acf171e-5dad-4783-8d3c-79545163ed18");
            OntClass child = i.next();
            writelineStatic("/home/ubuntu/results/coverage/OntTools/OntTools_7_10.coverage", "c7a10cce-4e15-444d-8c67-378e8ab898a6");
            if (child.equals(cls) || child.equals(cls.getProfile().NOTHING())) {
                writelineStatic("/home/ubuntu/results/coverage/OntTools/OntTools_7_10.coverage", "f600f9db-c5cc-4323-886d-f902c22686b4");
                // we ignore the reflexive case and bottom
                continue;
            }
            writelineStatic("/home/ubuntu/results/coverage/OntTools/OntTools_7_10.coverage", "ad72c4f1-0a89-4fe0-8c67-86117d110fb6");
            // compute the LCA of the sub-tree
            DisjointSet v = lca(child, uCls, vCls, index);
            writelineStatic("/home/ubuntu/results/coverage/OntTools/OntTools_7_10.coverage", "c6af9be8-3043-41ae-ad93-3de2b918b914");
            // union the two disjoint sets together
            clsSet.union(v);
            writelineStatic("/home/ubuntu/results/coverage/OntTools/OntTools_7_10.coverage", "b3e9b897-0c4d-4dfa-b327-9a62b17237f3");
            // propagate the distinguished member
            clsSet.find().setAncestor(clsSet);
        }
        writelineStatic("/home/ubuntu/results/coverage/OntTools/OntTools_7_10.coverage", "d1f1738a-933d-4443-9e64-5d371ff7cc10");
        // this node is done
        clsSet.setBlack();
        writelineStatic("/home/ubuntu/results/coverage/OntTools/OntTools_7_10.coverage", "50e687e3-9f31-4008-ac6e-2620e4ee8c7c");
        // are we inspecting one of the elements we're interested in?
        if (cls.equals(uCls)) {
            writelineStatic("/home/ubuntu/results/coverage/OntTools/OntTools_7_10.coverage", "7b6865af-96cf-42ac-a7e7-8df163e727b9");
            checkSolution(uCls, vCls, index);
        } else if (cls.equals(vCls)) {
            writelineStatic("/home/ubuntu/results/coverage/OntTools/OntTools_7_10.coverage", "e72b5128-a03a-41c0-85eb-224d01b5b2fe");
            checkSolution(vCls, uCls, index);
        }
        writelineStatic("/home/ubuntu/results/coverage/OntTools/OntTools_7_10.coverage", "43c31804-192e-447d-b63f-53c03ebda615");
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
        writelineStatic("/home/ubuntu/results/coverage/OntTools/OntTools_7_10.coverage", "c1eabb69-14b6-47f1-8015-6fa63ffd3c59");
        DisjointSet vSet = index.getSet(vCls);
        writelineStatic("/home/ubuntu/results/coverage/OntTools/OntTools_7_10.coverage", "a04692ff-96fd-4d33-8000-ba678ffef8fc");
        DisjointSet uSet = index.getSet(uCls);
        writelineStatic("/home/ubuntu/results/coverage/OntTools/OntTools_7_10.coverage", "88d010ee-04ad-42f5-aa08-c3b634e30fe9");
        if (vSet != null && vSet.isBlack() && !vSet.used() && uSet != null && uSet.isBlack() && !uSet.used()) {
            writelineStatic("/home/ubuntu/results/coverage/OntTools/OntTools_7_10.coverage", "893e5266-da98-4d8e-98d9-b34ae43a8a54");
            vSet.setUsed();
            writelineStatic("/home/ubuntu/results/coverage/OntTools/OntTools_7_10.coverage", "65800589-bb08-4bf3-996c-e84eb1c17622");
            uSet.setUsed();
            writelineStatic("/home/ubuntu/results/coverage/OntTools/OntTools_7_10.coverage", "e9bdf13b-15d0-4313-b134-86f4626bfe10");
            // log.debug( "Found LCA: u = " + uCls + ", v = " + vCls  );
            OntClass lca = (OntClass) vSet.find().getAncestor().getNode();
            writelineStatic("/home/ubuntu/results/coverage/OntTools/OntTools_7_10.coverage", "18e6dc94-66ec-42d6-acff-09cf79fb5726");
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
        writelineStatic("/home/ubuntu/results/coverage/OntTools/OntTools_7_10.coverage", "bbdbcbf5-01f0-4da1-acdb-960d16c33847");
        while (i.hasNext()) {
            writelineStatic("/home/ubuntu/results/coverage/OntTools/OntTools_7_10.coverage", "a6dcc89c-dd1b-4878-9e84-de7d883cb20e");
            OntClass c = i.next();
            writelineStatic("/home/ubuntu/results/coverage/OntTools/OntTools_7_10.coverage", "8e012724-869a-4f36-9883-17cb56700a7b");
            boolean ignore = false;
            writelineStatic("/home/ubuntu/results/coverage/OntTools/OntTools_7_10.coverage", "bb7620bf-bfdc-44a8-9c96-e21fa9f76727");
            // duplicate check: we ignore this class if we've already got it
            if (named.contains(c)) {
                writelineStatic("/home/ubuntu/results/coverage/OntTools/OntTools_7_10.coverage", "8aa4679f-bdef-4551-89d2-53ba5b65c628");
                ignore = true;
            }
            writelineStatic("/home/ubuntu/results/coverage/OntTools/OntTools_7_10.coverage", "c9c4cc9e-9d47-41ef-8e43-a25e5c15e4f6");
            // subsumption check: c must have only anon classes or Thing
            // as super-classes to still qualify as a root class
            Resource thing = c.getProfile().THING();
            writelineStatic("/home/ubuntu/results/coverage/OntTools/OntTools_7_10.coverage", "1d7e9dee-0e50-4a74-9767-d1a67c71c000");
            for (Iterator<OntClass> j = c.listSuperClasses(); !ignore && j.hasNext(); ) {
                writelineStatic("/home/ubuntu/results/coverage/OntTools/OntTools_7_10.coverage", "801b68d2-1f75-4011-bb48-32f289f46d05");
                OntClass sup = j.next();
                writelineStatic("/home/ubuntu/results/coverage/OntTools/OntTools_7_10.coverage", "e0dfaaa2-664a-4dec-bff1-83fcc4525dae");
                if (!((thing != null && sup.equals(thing)) || sup.isAnon() || sup.equals(c))) {
                    writelineStatic("/home/ubuntu/results/coverage/OntTools/OntTools_7_10.coverage", "3dd0c5cb-958b-408a-be78-4b04eec1b917");
                    ignore = true;
                }
            }
            writelineStatic("/home/ubuntu/results/coverage/OntTools/OntTools_7_10.coverage", "2efa8b39-a63b-4ad2-83a4-fabcc97591bf");
            if (!ignore) {
                writelineStatic("/home/ubuntu/results/coverage/OntTools/OntTools_7_10.coverage", "8542af64-957e-40b3-a0d8-f7109c539642");
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
