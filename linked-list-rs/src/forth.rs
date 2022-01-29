use std::cell::RefCell;
use std::rc::Rc;

struct List<T> {
    head: Link<T>,
    last: Link<T>,
}

type Link<T> = Option<Rc<RefCell<Node<T>>>>;

struct Node<T> {
    value: T,
    next: Link<T>,
    prev: Link<T>,
}

impl<T> List<T> {
    fn new() -> Self {
        List {
            head: None,
            last: None,
        }
    }
    fn push_front(&mut self, value: T) {
        let new_head = Rc::new(RefCell::new(Node {
            value,
            next: None,
            prev: None,
        }));
        match self.head.take() {
            None => {
                self.head = Some(new_head.clone());
                self.last = Some(new_head);
            }
            Some(old_head) => {
                old_head.borrow_mut().prev = Some(new_head.clone());
                new_head.borrow_mut().next = Some(old_head);
                self.head = Some(new_head);
            }
        }
    }
    fn push_back(&mut self, value: T) {
        let new_last = Rc::new(RefCell::new(Node {
            value,
            next: None,
            prev: None,
        }));
        match self.last.take() {
            None => {
                self.head = Some(new_last.clone());
                self.last = Some(new_last);
            }
            Some(old_last) => {
                old_last.borrow_mut().next = Some(new_last.clone());
                new_last.borrow_mut().prev = Some(old_last);
                self.last = Some(new_last);
            }
        }
    }
    fn pop_front(&mut self) -> Option<T> {
        self.head.take().map(|old_head| {
            match old_head.borrow_mut().next.take() {
                Some(new_head) => {
                    new_head.borrow_mut().prev = None;
                    self.head = Some(new_head);
                }
                None => {
                    self.last = None;
                }
            }
            Rc::try_unwrap(old_head).ok().unwrap().into_inner().value
        })
    }
    fn pop_back(&mut self) -> Option<T> {
        self.last.take().map(|old_last| {
            match old_last.borrow_mut().prev.take() {
                Some(new_last) => {
                    new_last.borrow_mut().next = None;
                    self.last = Some(new_last);
                }
                None => {
                    self.head = None;
                }
            }
            Rc::try_unwrap(old_last).ok().unwrap().into_inner().value
        })
    }
    fn peek_front(&self) -> Option<std::cell::Ref<T>> {
        self.head
            .as_ref()
            .map(|node| std::cell::Ref::map(node.borrow(), |n| &n.value))
    }
    fn peek_back(&self) -> Option<std::cell::Ref<T>> {
        self.last
            .as_ref()
            .map(|node| std::cell::Ref::map(node.borrow(), |n| &n.value))
    }
}
impl<T> Drop for List<T> {
    fn drop(&mut self) {
        while self.pop_front().is_some() {}
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test() {
        let mut l = List::new();

        assert_eq!(l.pop_front(), None);
        assert_eq!(l.pop_back(), None);

        l.push_front(1);
        l.push_back(2);
        l.push_front(0);
        l.push_back(3);

        assert_eq!(l.peek_front().as_deref(), Some(&0));
        assert_eq!(l.peek_back().as_deref(), Some(&3));

        assert_eq!(l.pop_front(), Some(0));
        assert_eq!(l.pop_back(), Some(3));
        assert_eq!(l.pop_back(), Some(2));
        assert_eq!(l.pop_front(), Some(1));
    }

    #[test]
    fn test_drop() {
        let mut l = List::new();
        l.push_front(1);
        l.push_back(2);
        l.push_front(0);
        l.push_back(3);
    }
}
