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
        writelineStatic("/home/ubuntu/results/coverage/OntTools/OntTools_5_10.coverage", "741b37a8-57bb-4901-8aec-580f4006d2ad");
        Resource root = m.getProfile().THING();
        writelineStatic("/home/ubuntu/results/coverage/OntTools/OntTools_5_10.coverage", "0a2f9a19-1ad6-43a3-ba26-7725e13cf6a4");
        if (root == null) {
            writelineStatic("/home/ubuntu/results/coverage/OntTools/OntTools_5_10.coverage", "44c79e30-2feb-421a-b4bb-762a86fb501e");
            throw new JenaException("The given OntModel has a language profile that does not define a generic root class (such as owl:Thing)");
        }
        writelineStatic("/home/ubuntu/results/coverage/OntTools/OntTools_5_10.coverage", "a91e6987-5461-46be-b451-5f2af04524ef");
        root = root.inModel(m);
        writelineStatic("/home/ubuntu/results/coverage/OntTools/OntTools_5_10.coverage", "ec2c2510-131c-4c79-94e6-5d30f12478ba");
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
        writelineStatic("/home/ubuntu/results/coverage/OntTools/OntTools_5_10.coverage", "94bd8ac7-13c1-4d12-87d2-4282cb830e87");
        // check some common cases first
        if (u.equals(root) || v.equals(root)) {
            writelineStatic("/home/ubuntu/results/coverage/OntTools/OntTools_5_10.coverage", "9f4440ee-1a84-4134-8fcb-6d6e20c00d48");
            return root;
        }
        writelineStatic("/home/ubuntu/results/coverage/OntTools/OntTools_5_10.coverage", "fae9c3bd-72bc-453f-811f-18c9aa615d62");
        if (u.hasSubClass(v)) {
            writelineStatic("/home/ubuntu/results/coverage/OntTools/OntTools_5_10.coverage", "a85e8b59-0120-494e-b7ec-fbaa3307c41b");
            return u;
        }
        writelineStatic("/home/ubuntu/results/coverage/OntTools/OntTools_5_10.coverage", "691e5905-8ea9-4835-a032-5b4f8deb4c1a");
        if (v.hasSubClass(u)) {
            writelineStatic("/home/ubuntu/results/coverage/OntTools/OntTools_5_10.coverage", "96c415ce-f8fb-443d-98ac-bde92121f268");
            return v;
        }
        writelineStatic("/home/ubuntu/results/coverage/OntTools/OntTools_5_10.coverage", "556d77e4-8b48-41ed-94ed-c86e150b0bdb");
        // not a common case, so apply Tarjan's LCA algorithm
        LCAIndex index = new LCAIndex();
        writelineStatic("/home/ubuntu/results/coverage/OntTools/OntTools_5_10.coverage", "113d353b-d333-4370-84a4-c29e2175cd92");
        lca(root, u, v, index);
        writelineStatic("/home/ubuntu/results/coverage/OntTools/OntTools_5_10.coverage", "dd6f4125-f156-42a9-84b9-bd5cad82f36f");
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
        writelineStatic("/home/ubuntu/results/coverage/OntTools/OntTools_5_10.coverage", "406a9f3c-8083-4428-a654-27e309b8b444");
        List<Path> bfs = new LinkedList<Path>();
        writelineStatic("/home/ubuntu/results/coverage/OntTools/OntTools_5_10.coverage", "c7d9d808-341c-437c-a2cd-095594eebbd4");
        Set<Resource> seen = new HashSet<Resource>();
        writelineStatic("/home/ubuntu/results/coverage/OntTools/OntTools_5_10.coverage", "d47cf508-889f-4d0f-9f62-a851a296c98c");
        // initialise the paths
        for (Iterator<Statement> i = m.listStatements(start, null, (RDFNode) null).filterKeep(onPath); i.hasNext(); ) {
            writelineStatic("/home/ubuntu/results/coverage/OntTools/OntTools_5_10.coverage", "18915cd5-c027-4670-9cb6-bd90d1b74ada");
            bfs.add(new Path().append(i.next()));
        }
        writelineStatic("/home/ubuntu/results/coverage/OntTools/OntTools_5_10.coverage", "b50c2eee-b074-45e9-bd05-e303bfd142de");
        // search
        Path solution = null;
        writelineStatic("/home/ubuntu/results/coverage/OntTools/OntTools_5_10.coverage", "3cea8729-e706-48a6-a08d-c0e5c3ddd056");
        while (solution == null && !bfs.isEmpty()) {
            writelineStatic("/home/ubuntu/results/coverage/OntTools/OntTools_5_10.coverage", "f4050adb-7df4-402d-b205-4093e58d5960");
            Path candidate = bfs.remove(0);
            writelineStatic("/home/ubuntu/results/coverage/OntTools/OntTools_5_10.coverage", "b15298d9-6e8c-4d05-b655-98956ff35e2d");
            if (candidate.hasTerminus(end)) {
                writelineStatic("/home/ubuntu/results/coverage/OntTools/OntTools_5_10.coverage", "9e9e4dd5-5b32-4db2-b4a7-83ff32764d87");
                solution = candidate;
            } else {
                writelineStatic("/home/ubuntu/results/coverage/OntTools/OntTools_5_10.coverage", "248a9e03-4d7e-487d-84f0-c17704eb887c");
                Resource terminus = candidate.getTerminalResource();
                writelineStatic("/home/ubuntu/results/coverage/OntTools/OntTools_5_10.coverage", "2dbcc047-91b8-4444-a26d-5493bcd0c30e");
                if (terminus != null) {
                    writelineStatic("/home/ubuntu/results/coverage/OntTools/OntTools_5_10.coverage", "36796115-4de5-434c-9743-17a82cce089f");
                    seen.add(terminus);
                    writelineStatic("/home/ubuntu/results/coverage/OntTools/OntTools_5_10.coverage", "ee44f2f9-da8c-4b9e-857d-f838ff6a57a4");
                    // breadth-first expansion
                    for (Iterator<Statement> i = terminus.listProperties().filterKeep(onPath); i.hasNext(); ) {
                        writelineStatic("/home/ubuntu/results/coverage/OntTools/OntTools_5_10.coverage", "da8e059d-5578-47d9-b6f7-02cc9ffff1be");
                        Statement link = i.next();
                        writelineStatic("/home/ubuntu/results/coverage/OntTools/OntTools_5_10.coverage", "fd18272e-e4dc-4bac-b9a8-b81a98c57d51");
                        // no looping allowed, so we skip this link if it takes us to a node we've seen
                        if (!seen.contains(link.getObject())) {
                            writelineStatic("/home/ubuntu/results/coverage/OntTools/OntTools_5_10.coverage", "ca01c571-a5f5-431c-99b8-1215e548571a");
                            bfs.add(candidate.append(link));
                        }
                    }
                }
            }
        }
        writelineStatic("/home/ubuntu/results/coverage/OntTools/OntTools_5_10.coverage", "c1a4dca1-73cc-45b2-b9f0-0ea92669a109");
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
        writelineStatic("/home/ubuntu/results/coverage/OntTools/OntTools_5_10.coverage", "cfdc9b34-635c-473b-bba0-d6a42832f26d");
        // named roots
        List<OntClass> nhr = new ArrayList<OntClass>();
        writelineStatic("/home/ubuntu/results/coverage/OntTools/OntTools_5_10.coverage", "87ee72c8-cf43-445e-b658-c91f678bf1cb");
        // anon roots
        List<OntClass> ahr = new ArrayList<OntClass>();
        writelineStatic("/home/ubuntu/results/coverage/OntTools/OntTools_5_10.coverage", "10d69d41-0599-489c-9b47-e129f27a4ee7");
        // do the initial partition of the root classes
        partitionByNamed(m.listHierarchyRootClasses(), nhr, ahr);
        writelineStatic("/home/ubuntu/results/coverage/OntTools/OntTools_5_10.coverage", "66bcd0de-b63c-4e64-97d7-67a619ee6caa");
        // now push the fringe down until we have only named classes
        while (!ahr.isEmpty()) {
            writelineStatic("/home/ubuntu/results/coverage/OntTools/OntTools_5_10.coverage", "4b7a3391-981f-4481-acc2-98a33fae5a6b");
            OntClass c = ahr.remove(0);
            writelineStatic("/home/ubuntu/results/coverage/OntTools/OntTools_5_10.coverage", "1c66fd75-1e96-4960-bfdb-ec8ad777e0f4");
            partitionByNamed(c.listSubClasses(true), nhr, ahr);
        }
        writelineStatic("/home/ubuntu/results/coverage/OntTools/OntTools_5_10.coverage", "3d6cee15-0144-4f62-a567-3afd34f6f910");
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
        writelineStatic("/home/ubuntu/results/coverage/OntTools/OntTools_5_10.coverage", "3559d336-ca05-44d8-ae5a-e0201125a628");
        // log.debug( "Entering lca(), cls = " + cls );
        DisjointSet clsSet = index.getSet(cls);
        writelineStatic("/home/ubuntu/results/coverage/OntTools/OntTools_5_10.coverage", "6c3cb9a5-34d4-48f2-889e-eed4f475fd1e");
        if (clsSet.isBlack()) {
            writelineStatic("/home/ubuntu/results/coverage/OntTools/OntTools_5_10.coverage", "c2162804-1369-4a9b-b9c3-5a2d56ef134f");
            // already visited
            return clsSet;
        }
        writelineStatic("/home/ubuntu/results/coverage/OntTools/OntTools_5_10.coverage", "66db5aea-b76f-4344-af97-2adef91f306b");
        // not visited yet
        clsSet.setAncestor(clsSet);
        writelineStatic("/home/ubuntu/results/coverage/OntTools/OntTools_5_10.coverage", "81509866-ab3d-4704-a5f3-f014327c5b27");
        // for each child of cls
        for (Iterator<OntClass> i = cls.listSubClasses(true); i.hasNext(); ) {
            writelineStatic("/home/ubuntu/results/coverage/OntTools/OntTools_5_10.coverage", "17e7eb73-5a01-4971-ad20-bd797c31f7fa");
            OntClass child = i.next();
            writelineStatic("/home/ubuntu/results/coverage/OntTools/OntTools_5_10.coverage", "8de78dc9-442a-4b50-81fd-25337a47db51");
            if (child.equals(cls) || child.equals(cls.getProfile().NOTHING())) {
                writelineStatic("/home/ubuntu/results/coverage/OntTools/OntTools_5_10.coverage", "c63b22e3-693e-4e85-a15f-0a4821b96d5e");
                // we ignore the reflexive case and bottom
                continue;
            }
            writelineStatic("/home/ubuntu/results/coverage/OntTools/OntTools_5_10.coverage", "32506424-e195-4ef0-aabb-b03ecebbc25d");
            // compute the LCA of the sub-tree
            DisjointSet v = lca(child, uCls, vCls, index);
            writelineStatic("/home/ubuntu/results/coverage/OntTools/OntTools_5_10.coverage", "263a8088-31f2-44f7-9cfb-0ae401639528");
            // union the two disjoint sets together
            clsSet.union(v);
            writelineStatic("/home/ubuntu/results/coverage/OntTools/OntTools_5_10.coverage", "88fb9c88-37cd-42dc-93a4-b9c1f759ca34");
            // propagate the distinguished member
            clsSet.find().setAncestor(clsSet);
        }
        writelineStatic("/home/ubuntu/results/coverage/OntTools/OntTools_5_10.coverage", "e358a957-11da-47d6-ad56-e5ffbe4c99b1");
        // this node is done
        clsSet.setBlack();
        writelineStatic("/home/ubuntu/results/coverage/OntTools/OntTools_5_10.coverage", "1c76a8c6-4f1a-4fdf-9917-0728362ec065");
        // are we inspecting one of the elements we're interested in?
        if (cls.equals(uCls)) {
            writelineStatic("/home/ubuntu/results/coverage/OntTools/OntTools_5_10.coverage", "ed936b3c-1d5a-41d3-ba7b-2849b91ca1f1");
            checkSolution(uCls, vCls, index);
        } else if (cls.equals(vCls)) {
            writelineStatic("/home/ubuntu/results/coverage/OntTools/OntTools_5_10.coverage", "c832c54d-a7d4-416f-966b-fdd396292e91");
            checkSolution(vCls, uCls, index);
        }
        writelineStatic("/home/ubuntu/results/coverage/OntTools/OntTools_5_10.coverage", "721920e4-9ecf-41d1-9069-1deb9024f061");
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
        writelineStatic("/home/ubuntu/results/coverage/OntTools/OntTools_5_10.coverage", "ffef904b-0244-435b-b3eb-00ab280515e5");
        DisjointSet vSet = index.getSet(vCls);
        writelineStatic("/home/ubuntu/results/coverage/OntTools/OntTools_5_10.coverage", "f06f6de5-707b-46ed-9998-803b5c6ff458");
        DisjointSet uSet = index.getSet(uCls);
        writelineStatic("/home/ubuntu/results/coverage/OntTools/OntTools_5_10.coverage", "8d7dd9bf-8524-4dec-a8c7-8b53c39c1dc1");
        if (vSet != null && vSet.isBlack() && !vSet.used() && uSet != null && uSet.isBlack() && !uSet.used()) {
            writelineStatic("/home/ubuntu/results/coverage/OntTools/OntTools_5_10.coverage", "97f57cc9-e35a-482c-ae0e-aac41aea7321");
            vSet.setUsed();
            writelineStatic("/home/ubuntu/results/coverage/OntTools/OntTools_5_10.coverage", "21327e97-7b23-4692-9d14-85686d3bd271");
            uSet.setUsed();
            writelineStatic("/home/ubuntu/results/coverage/OntTools/OntTools_5_10.coverage", "fdba1650-02b4-4b98-b07c-5229cbdb00f1");
            // log.debug( "Found LCA: u = " + uCls + ", v = " + vCls  );
            OntClass lca = (OntClass) vSet.find().getAncestor().getNode();
            writelineStatic("/home/ubuntu/results/coverage/OntTools/OntTools_5_10.coverage", "a36c650c-6a68-456b-a871-89e539698647");
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
        writelineStatic("/home/ubuntu/results/coverage/OntTools/OntTools_5_10.coverage", "d8f7b697-08b4-4f32-9b05-0dc985a20403");
        while (i.hasNext()) {
            writelineStatic("/home/ubuntu/results/coverage/OntTools/OntTools_5_10.coverage", "d6ef39ec-7231-43ea-89fb-26d8f1bc47ae");
            OntClass c = i.next();
            writelineStatic("/home/ubuntu/results/coverage/OntTools/OntTools_5_10.coverage", "7fbd3619-5751-4922-a056-dbf68b2e15e2");
            boolean ignore = false;
            writelineStatic("/home/ubuntu/results/coverage/OntTools/OntTools_5_10.coverage", "3c8aa4f3-24ea-490c-9068-da6d59e89b51");
            // duplicate check: we ignore this class if we've already got it
            if (named.contains(c)) {
                writelineStatic("/home/ubuntu/results/coverage/OntTools/OntTools_5_10.coverage", "0a647804-5904-4bb7-b4e0-3a0814f39e18");
                ignore = true;
            }
            writelineStatic("/home/ubuntu/results/coverage/OntTools/OntTools_5_10.coverage", "a76d3a29-f95b-4a1b-a1c2-91599f7086f5");
            // subsumption check: c must have only anon classes or Thing
            // as super-classes to still qualify as a root class
            Resource thing = c.getProfile().THING();
            writelineStatic("/home/ubuntu/results/coverage/OntTools/OntTools_5_10.coverage", "1291c78c-1929-4f4c-a2ac-ceadce3e7017");
            for (Iterator<OntClass> j = c.listSuperClasses(); !ignore && j.hasNext(); ) {
                writelineStatic("/home/ubuntu/results/coverage/OntTools/OntTools_5_10.coverage", "64f2c7b1-a6b6-42ab-a6dd-d91291d92aff");
                OntClass sup = j.next();
                writelineStatic("/home/ubuntu/results/coverage/OntTools/OntTools_5_10.coverage", "d4d1ea9e-1f1e-4f3d-ad72-2ec3a2a98a37");
                if (!((thing != null && sup.equals(thing)) || sup.isAnon() || sup.equals(c))) {
                    writelineStatic("/home/ubuntu/results/coverage/OntTools/OntTools_5_10.coverage", "2c51779c-5c26-459d-95c0-51581747363b");
                    ignore = true;
                }
            }
            writelineStatic("/home/ubuntu/results/coverage/OntTools/OntTools_5_10.coverage", "8b4a16c8-f04e-49c2-85b9-a9f6d95655a3");
            if (!ignore) {
                writelineStatic("/home/ubuntu/results/coverage/OntTools/OntTools_5_10.coverage", "164fab0f-3a5e-4dd2-ab19-61437c21667e");
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
