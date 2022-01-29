use std::rc::Rc;

struct List<T> {
    head: Option<Rc<Node<T>>>,
}
struct Node<T> {
    value: T,
    next: Option<Rc<Node<T>>>,
}

impl<T> List<T> {
    fn new() -> List<T> {
        List { head: None }
    }
    fn prepend(&self, value: T) -> List<T> {
        List {
            head: Some(Rc::new(Node {
                value,
                next: self.head.clone(),
            })),
        }
    }
    fn tail(&self) -> List<T> {
        List {
            head: self.head.as_ref().and_then(|node| node.next.clone()),
        }
    }
    fn head(&self) -> Option<&T> {
        self.head.as_ref().map(|node| &node.value)
    }
    fn iter(&self) -> impl Iterator<Item = &T> {
        Iter(self.head.as_deref())
    }
}
impl<T> Drop for List<T> {
    fn drop(&mut self) {
        let mut head = self.head.take();
        while let Some(node) = head {
            if let Ok(mut node) = Rc::try_unwrap(node) {
                head = node.next.take();
            } else {
                break;
            }
        }
    }
}
struct Iter<'a, T>(Option<&'a Node<T>>);
impl<'a, T> Iterator for Iter<'a, T> {
    type Item = &'a T;
    fn next(&mut self) -> Option<Self::Item> {
        self.0.map(|node| {
            self.0 = node.next.as_deref();
            &node.value
        })
    }
}

#[cfg(test)]
mod test {
    use super::*;
    #[test]
    fn test() {
        let l = List::new();
        let l = l.prepend(1);
        let l = l.prepend(2);
        assert_eq!(l.head(), Some(&2));
        assert_eq!(l.tail().head(), Some(&1));
        assert_eq!(l.tail().tail().head(), None);
        assert_eq!(l.tail().tail().tail().head(), None);
    }
}
