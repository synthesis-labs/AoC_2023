####### Binary Tree and its methods

class BinaryTreeNode:
    def __init__(self, data):
        self.data = data
        self.left = None
        self.right = None
    #set data
    def set_data(self, data):
        self.data = data
    #get data
    def get_data(self):
        return self.data
    #get left child of node
    def get_left(self):
        return self.left
    #get right child of a node
    def get_right_node(self):
        return self.right


binary_tree = BinaryTreeNode("A")
 
binary_tree.left  = "C"
binary_tree.right = "D"

print(binary_tree.get_data())
print(binary_tree.get_left())
print(binary_tree.get_right_node())