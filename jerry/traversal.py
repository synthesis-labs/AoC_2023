
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
        if node.left_child: stack.append(node.left_child)
    return result

    ####### In-order Traversal ########

def in_order_recurssive(root, result):
    if not root:
        return
    in_order_recurssive(root.left_child, result)
    result.append(root.data)
    in_order_recurssive(root.right_child, result)
    return result

def in_order_iterative(root, result):
    if not root:
        return
    stack = []
    node = root
    while stack or node:
        if node:    
            stack.append(node)
            node = node.left_child
        else:
            node = stack.pop()
            result.append(node.data)
            node = node.right_child
    return result

###### Post-Order Reversal ######

def post_order_traversal(root, result):
    if not root:
        return
    post_order_traversal(root.left_child, result)
    post_order_traversal(root.right_child, result)
    result.append(root.data)
    return result