import sys
import traversal

####### Binary Tree and its methods

class BinaryTreeNode:
    def __init__(self, data):
        self.data = data
        self.left_child = None
        self.right_child = None
#     #set data
#     def set_data(self, data):
#         self.data = data
#         return self.data
#     #get data
#     def get_data(self):
#         return self.data
#     #get left child of node
#     def get_left_child(self):
#         return self.left_child
#     #get right child of a node
#     def get_right_child(self):
#         return self.right_child

new_tree = BinaryTreeNode("Drinks")
leftchild = BinaryTreeNode("Hot")
tea = BinaryTreeNode("Tea")
coffee =  BinaryTreeNode("Coffee")
leftchild.left_child = tea

leftchild.right_child  = coffee
rightchild = BinaryTreeNode("Cold")

new_tree.left_child = leftchild
new_tree.right_child = rightchild


print(
traversal.pre_order_recursive(new_tree, [])
)

print(traversal.pre_order_iterative(new_tree, []))

print(traversal.in_order_recurssive(new_tree, []))

print(traversal.in_order_iterative(new_tree, []))

print(traversal.post_order_traversal(new_tree, []))


































