#[derive(Debug)]
struct Node {
    name: String,
    left: Option<Box<Node>>,
    right: Option<Box<Node>>,
}

impl Node {

    fn new(name: &str, left: Node, right: Node) -> Node {
        Node {
            name: name.to_string(),
            left: Some(Box::new(left)),
            right: Some(Box::new(right)),
        }
    }

    fn set_left(&mut self, left: Node) {
        self.left = Some(Box::new(left));
    }

    fn set_right(&mut self, right: Node) {
        self.right = Some(Box::new(right));
    }

    fn go_left(&self) -> Option<&Node> {
        self.left.as_deref()
    }

    fn go_right(&self) -> Option<&Node> {
        self.right.as_deref()
    }
}

