#[derive(Debug)]
pub struct List<T> {
    head: Option<Box<Node<T>>>,
}
#[derive(Debug)]
struct Node<T> {
    value: T,
    next: Option<Box<Node<T>>>,
}

impl<T> List<T> {
    pub fn new() -> Self {
        Self { head: None }
    }
    pub fn push(&mut self, value: T) {
        let head = Some(Box::new(Node {
            value,
            next: std::mem::replace(&mut self.head, None),
        }));
        self.head = head;
    }
    pub fn pop(&mut self) -> Option<T> {
        match std::mem::replace(&mut self.head, None) {
            None => None,
            Some(node) => {
                self.head = node.next;
                Some(node.value)
            }
        }
    }
}

impl<T> Drop for List<T> {
    fn drop(&mut self) {
        let mut head = std::mem::replace(&mut self.head, None);
        while let Some(mut node) = head {
            head = std::mem::replace(&mut node.next, None);
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;
    #[test]
    fn test() {
        let mut l = List::new();
        assert_eq!(l.pop(), None);
        l.push(1);
        assert_eq!(l.pop(), Some(1));
        l.push(2);
        l.push(3);
        assert_eq!(l.pop(), Some(3));
        assert_eq!(l.pop(), Some(2));
        assert_eq!(l.pop(), None);
    }
}
