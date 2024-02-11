See <https://math.stackexchange.com/questions/1106870/can-i-find-the-connected-components-of-a-graph-using-matrix-operations-on-the-gr>

Perhaps the easiest way is to obtain the Laplacian matrix and find a basis of its kernel.

In words, call A your adjacency matrix. Obtain the diagonal matrix D of the degrees of each vertex. Set L=D−A

Now dim ker L = number of connected components. Moreover, the kernel of L is spanned by vectors constant on each connected component.

For example, a block diagonal matrix A=diag(A1,…,An)
, with blocks representing the connected components of your graph, will have an associated Laplacian matrix L
 with kernel spanned by vectors vi=(0,…,0,1,…,1,0,…,0)
 where the string of ones is as long as the number of vertices in Ai
, and specifically in the entries corresponding to the vertices of that connected component.
