
########### Pre-order Traversals ############

def pre_order_recursive(root, result):
    if not root:
        return
    result.append(root.data)
    pre_order_recursive(root.left_child, result)
    pre_order_recursive(root.right_child, result)
    return result
# Non-Recurssive

def pre_order_iterative(root, result):
    if not root:
        return
        stack = []
        stack.append(root)
        while stack:
            node = stack.pop()
            result.append(node.data)
            if node.right_child: stack.append(node.right_child)
            if node.left_child: stack.append(node.left_child9)