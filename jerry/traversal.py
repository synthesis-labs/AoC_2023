def pre_order_traversal(root_node):
    if not root_node:
        return
    pre_order_traversal(root_node.left_child)
    pre_order_traversal(root_node)

    