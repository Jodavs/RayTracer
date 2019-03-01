module KDMeshTree

open Types
open Point
open Vector
open Shape
open BaseShape
open Texture

/// The split that defines which axis a node splits along, and at which value of that given axis
type Split = int * float

/// A node of the KDTree
type KDTreeNode =
    /// Defines a Split, a left- and a right subtree Node
    | Node of Split * KDTreeNode * KDTreeNode
    /// Leaf node containing a list of Shapes
    | Leaf of Triangle list

/// KDTree with a root node and a surrounding bounding box
type KDTree = KDTreeNode * BoundingBox

/// Given an axis and a Point, return the value of the axis from the Point
val private getPointAxis : axis:int -> point:Point -> float

/// Given an axis and a Vector, return the value of the axis from the Vector
val private getVectorAxis : axis:int -> vector:Vector -> float

/// Given an axis and the BoundingBoxes to consider in the split, define the splitvalue to split at along the axis
val private findSplitValue : axis:int -> boundingBoxes:BoundingBox list -> float

/// Append the given shape to left, right or both subtrees based on the axis and splitValue.
/// The subtreePair therefore represents a tuple where the input shape is Some in either or both tuple values, but never None in both
val private makeSubtreePair : axis:int -> splitValue:float -> shape:Triangle -> Triangle option * Triangle option

/// Finds the surrounding bounding box from a list of boundingboxes
val private getSurroundingBoundingBox : boundingBoxes: BoundingBox list -> BoundingBox

/// Determines the best split axis based on the dimensions of a given list of boundingboxes
val private getSplitAxis : axis:int -> int

/// Given a direction d, and two lists, keep the order of the lists if d is positive, but swap the order if d is negative
val private order : d:float * left: 'a * right: 'a -> 'a * 'a

/// Search recursively from a Node in the KDTree and return the hit of a Shape, if it hits any within the tree
val private search : node:KDTreeNode -> ray:Ray -> t:float -> t':float -> Texture ->(Vector -> Vector) -> Hit option

val private searchNode : KDTreeNode -> Ray -> float -> float -> Texture ->(Vector -> Vector) -> Hit option

/// Traverse the tree with the ray and return the hit of a Shape, if it hits any within the tree
val private traverse : tree:KDTree -> ray:Ray -> Texture ->(Vector -> Vector) -> Hit option

/// Given a KDTree, convert it into a BaseShape
val private mkTreeShape : kdTree:KDTree -> BaseShape

/// Generate a KDTree based on a list of Shapes
val public mkMeshKDTree : shapesInput:Triangle list -> BaseShape
