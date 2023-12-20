module PT8
import StdEnv

//Write your name and neptune code here: < Abaidulla  ,   >

/*---------------------------------- TASK: --------------------------------------

Given a binary tree that contains only 0s and 1s, remove every subtree that does not contain
node with value 1.

example:
     
     INPUT:
		1
	  /   \
	 0     1
	/ \     \
   1   0     0
  /   /     / \
 0 	 0     0   0
 
 
     OUTPUT:
		1
	  /   \
	 0     1
	/      
   1         
            

*/

:: Tree a = Node a (Tree a) (Tree a) | Leaf
tree1 = (Node 1 (Node 0 (Node 0 Leaf Leaf) (Node 0 Leaf Leaf))(Node 1 (Node 0 Leaf Leaf) (Node 1 Leaf Leaf)))
tree2 = (Node 0 (Node 0 (Node 1 Leaf Leaf) (Node 0 Leaf Leaf))(Node 0 (Node 0 Leaf Leaf) (Node 0 Leaf Leaf)))
tree3 = (Node 1 (Node 0 (Node 0 Leaf Leaf) (Node 1 Leaf Leaf)) Leaf)

notContain1 :: (Tree Int) -> Bool
notContain1 Leaf = True
notContain1 (Node x le ri)
| x == 0 = notContain1 le && notContain1 ri
= False
 
removeSubtree :: (Tree Int) -> (Tree Int)
removeSubtree Leaf = Leaf
removeSubtree (Node x le ri)
| notContain1 (Node x le ri) = Leaf
= Node x (removeSubtree le) (removeSubtree ri) 

//Start = removeSubtree (Leaf) // Leaf
//Start = removeSubtree tree1 // (Node 1 Leaf (Node 1 Leaf (Node 1 Leaf Leaf)))
//Start = removeSubtree tree2 // (Node 0 (Node 0 (Node 1 Leaf Leaf) Leaf) Leaf
Start = removeSubtree tree3 // (Node 1 (Node 0 Leaf (Node 1 Leaf Leaf))Leaf)









