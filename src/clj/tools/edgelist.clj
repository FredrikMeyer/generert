(ns tools.edgelist)

(defprotocol IDoubleConnectedEdgeList
  (vertices [this]))

(deftype DoubleConnectedEdgeList [vertices]
  IDoubleConnectedEdgeList
  (vertices [_]
    vertices))

(defn init-dcel []
  (->DoubleConnectedEdgeList (java.util.ArrayList)))



(deftype Face [outer-component inner-components])


(defprotocol IHalfEdge
  (twin [this])
  (set-twin! [this twin])
  (next-edge [this])
  (set-next-edge! [this new-next])
  (previous-edge [this])
  (set-previous-edge! [this new-previous])
  (origin [this]))

(deftype HalfEdge [^:unsynchronized-mutable twin
                   origin
                   ^:unsynchronized-mutable next-edge
                   ^:unsynchronized-mutable previous-edge]
  IHalfEdge
  (twin [this]
    (.-twin this))
  (set-twin! [this twin]
    (set! (.-twin this) twin))
  (origin [this]
    origin)

  (next-edge [this]
    (.-next-edge this))

  (set-next-edge! [this new-next]
    (set! next-edge new-next))

  (previous-edge [this]
    (.-previous-edge this))

  (set-previous-edge! [this new-previous]
    (set! previous-edge new-previous))

  Object
  (toString [this]
    (str "HalfEdge" "TWIN" origin)))


(defprotocol IVertex
  (coordinate [this])
  (connect [this vertex])
  (edge [this])
  (set-edge! [this edge]))

(deftype Vertex [x y ^:unsynchronized-mutable edge]
  IVertex
  (coordinate [this]
    [x y])

  (connect [this vertex]
    (let [new-edge (->HalfEdge nil this nil nil)
          other-edge (->HalfEdge new-edge vertex nil nil)]
      (set-twin! new-edge other-edge)
      (if (nil? (.-edge this))
        (do
          (set-edge! this new-edge)
          (set-edge! vertex other-edge))
        (do
          (set-previous-edge! edge other-edge)
          (set-edge! vertex other-edge)
          (set-next-edge! other-edge edge)

          (let [incoming-edge (twin edge)]
            (set-next-edge! incoming-edge new-edge))
          ))))

  (edge [this]
    (.-edge this))

  (set-edge! [this edge]
    (set! (.-edge this) edge))
  Object
  (toString [_]
    (str "Vertex <" x ", " y "> ")))

"
A doubly connected edge list (DCEL) is a data structure used to represent planar graphs, particularly useful in computational geometry for representing subdivisions of the plane. It allows for efficient traversal and manipulation of the graph. Here are some common operations performed on a DCEL:

### 1. **Traversal Operations**
-  **Vertex Traversal**: Iterate over all vertices in the DCEL.
-  **Edge Traversal**: Iterate over all edges in the DCEL.
-  **Face Traversal**: Iterate over all faces in the DCEL.

### 2. **Query Operations**
-  **Find Incident Edges**: Given a vertex, find all edges incident to it.
-  **Find Adjacent Vertices**: Given a vertex, find all vertices adjacent to it.
-  **Find Face**: Given an edge, find the face to its left or right.

### 3. **Modification Operations**
-  **Add Vertex**: Add a new vertex to the DCEL.
-  **Add Edge**: Add a new edge to the DCEL, updating the relevant vertices and faces.
-  **Remove Edge**: Remove an edge from the DCEL, updating the connectivity of the graph.
-  **Split Face**: Add an edge that splits a face into two, requiring updates to the face records.

### 4. **Geometric Operations**
-  **Locate Point**: Determine which face contains a given point.
-  **Intersect Edges**: Find intersections between edges and update the DCEL accordingly.

### 5. **Topological Operations**
-  **Merge Faces**: Merge two adjacent faces by removing the edge between them.
-  **Split Vertex**: Split a vertex into two, typically by adding an edge between two incident edges.

### Key Components of a DCEL
-  **Vertex Record**: Stores the coordinates of the vertex and a reference to one of its incident half-edges.
-  **Half-Edge Record**: Stores references to its twin half-edge, its origin vertex, the face it bounds, and the next and previous half-edges in the face's boundary.
-  **Face Record**: Stores a reference to one of the half-edges on its boundary.

These operations and components make the DCEL a powerful tool for efficiently handling planar subdivisions and performing complex geometric algorithms.
"
2
