pub struct List<T> {
    head: Option<Box<Node<T>>>,
}

struct Node<T> {
    value: T,
    next: Option<Box<Node<T>>>,
}

impl<T> List<T> {
    pub fn new() -> List<T> {
        List { head: None }
    }
    pub fn push(&mut self, value: T) {
        let next = self.head.take();
        self.head = Some(Box::new(Node { value, next }))
    }
    pub fn pop(&mut self) -> Option<T> {
        let head = self.head.take();
        head.map(|node| {
            self.head = node.next;
            node.value
        })
    }
    pub fn peek(&self) -> Option<&T> {
        self.head.as_ref().map(|node| &node.value)
    }
    pub fn peek_mut(&mut self) -> Option<&mut T> {
        self.head.as_mut().map(|node| &mut node.value)
    }
    pub fn into_iter(self) -> impl Iterator<Item = T> {
        IntoIter(self)
    }
    pub fn iter(&self) -> impl Iterator<Item = &T> {
        Iter(self.head.as_deref())
    }
    pub fn iter_mut(&mut self) -> impl Iterator<Item = &mut T> {
        IterMut(self.head.as_deref_mut())
    }
}
impl<T> Drop for List<T> {
    fn drop(&mut self) {
        let mut head = self.head.take();
        while let Some(mut node) = head {
            head = node.next.take();
        }
    }
}

struct IntoIter<T>(List<T>);
impl<T> Iterator for IntoIter<T> {
    type Item = T;
    fn next(&mut self) -> Option<Self::Item> {
        self.0.pop()
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
struct IterMut<'a, T>(Option<&'a mut Node<T>>);
impl<'a, T> Iterator for IterMut<'a, T> {
    type Item = &'a mut T;
    fn next(&mut self) -> Option<Self::Item> {
        self.0.take().map(|node| {
            self.0 = node.next.as_deref_mut();
            &mut node.value
        })
    }
}

#[cfg(test)]
mod test {
    use super::*;
    #[test]
    fn test() {
        let mut l = List::new();
        assert_eq!(l.pop(), None);
        assert_eq!(l.peek(), None);
        l.push(1);
        assert_eq!(l.peek(), Some(&1));
        l.peek_mut().map(|x| *x = 2);
        assert_eq!(l.pop(), Some(2));
        l.push(2);
        l.push(3);
        assert_eq!(l.pop(), Some(3));
        assert_eq!(l.pop(), Some(2));
        assert_eq!(l.pop(), None);
    }
    #[test]
    fn into_iter() {
        let mut l = List::new();
        l.push(1);
        l.push(2);

        let mut i = l.into_iter();
        assert_eq!(i.next(), Some(2));
        assert_eq!(i.next(), Some(1));
        assert_eq!(i.next(), None);
    }

    #[test]
    fn iter() {
        let mut l = List::new();
        l.push(1);
        l.push(2);

        let mut i = l.iter();
        assert_eq!(i.next(), Some(&2));
        assert_eq!(i.next(), Some(&1));
        assert_eq!(i.next(), None);
    }

    #[test]
    fn iter_mut() {
        let mut l = List::new();
        l.push(1);
        l.push(2);

        {
            let mut i = l.iter_mut();
            assert_eq!(i.next(), Some(&mut 2));
            {
                if let Some(x) = i.next() {
                    *x = 3;
                }
            }
            assert_eq!(i.next(), None);
        }

        let mut i = l.iter();
        assert_eq!(i.next(), Some(&2));
        assert_eq!(i.next(), Some(&3));
    }
}
