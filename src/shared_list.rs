use std::cell::{Ref, RefCell, RefMut};
use std::rc::Rc;

#[derive(Debug)]
pub struct SharedList<T> {
    head: Link<T>,
}

type Link<T> = Option<Rc<RefCell<Node<T>>>>;

#[derive(Debug)]
struct Node<T> {
    elem: T,
    next: Link<T>,
}

impl<T> Node<T> {
    fn new(elem: T) -> Rc<RefCell<Self>> {
        Rc::new(RefCell::new(Node {
            elem: elem,
            next: None,
        }))
    }
}

impl<T> Clone for SharedList<T> {
    fn clone(&self) -> SharedList<T> {
        match &self.head {
            None => SharedList::new(),
            Some(head) => SharedList {
                head: Some(Rc::clone(head)),
            },
        }
    }
}

impl<T> SharedList<T> {
    pub fn new() -> Self {
        SharedList { head: None }
    }

    pub fn push(&mut self, elem: T) {
        let new_head = Node::new(elem);
        if let Some(old_head) = self.head.take() {
            new_head.borrow_mut().next = Some(old_head);
        }
        self.head = Some(new_head);
    }

    pub fn peek(&self) -> Option<Ref<T>> {
        self.head
            .as_ref()
            .map(|node| Ref::map(node.borrow(), |node| &node.elem))
    }

    pub fn peek_mut(&mut self) -> Option<RefMut<T>> {
        self.head
            .as_ref()
            .map(|node| RefMut::map(node.borrow_mut(), |node| &mut node.elem))
    }

    pub fn pop(&mut self) {
        self.head.take().map(|old_head| {
            if let Some(new_head) = old_head.borrow_mut().next.clone() {
                self.head = Some(new_head);
            }
        });
    }

    pub fn tail(&self) -> SharedList<T> {
        SharedList {
            head: self
                .head
                .as_ref()
                .and_then(|old_head| old_head.borrow().next.clone()),
        }
    }

    pub fn empty(&self) -> bool {
        self.head.is_none()
    }

    pub fn equals(&self, other: &SharedList<T>) -> bool {
        match &self.head {
            None => match other.head {
                None => true,
                Some(_) => false,
            },
            Some(l) => match &other.head {
                None => false,
                Some(r) => Rc::ptr_eq(l, r),
            },
        }
    }
}

impl<T> Drop for SharedList<T> {
    fn drop(&mut self) {
        while !self.empty() {
            self.pop();
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::shared_list::SharedList;

    #[test]
    fn push_peek_pop() {
        let mut tree: SharedList<i32> = SharedList::new();
        assert!(tree.empty());
        assert!(tree.peek().is_none());
        tree.pop();
        assert!(tree.peek().is_none());
        tree.push(1);
        assert!(!tree.empty());
        assert!(tree.peek().is_some());
        assert_eq!(*tree.peek().unwrap(), 1);
        *tree.peek_mut().unwrap() = 2;
        assert_eq!(*tree.peek().unwrap(), 2);
        tree.pop();
        assert!(tree.empty());
        assert!(tree.peek().is_none());
        tree.push(1);
        tree.push(2);
        assert!(!tree.empty());
        assert_eq!(*tree.peek().unwrap(), 2);
        tree.pop();
        assert!(!tree.empty());
        assert_eq!(*tree.peek().unwrap(), 1);
        tree.pop();
        assert!(tree.empty());
        assert!(tree.peek().is_none());
    }

    #[test]
    fn sharing() {
        let mut tree: SharedList<i32> = SharedList::new();
        tree.push(1);
        tree.push(2);
        let copy = tree.clone();
        assert_eq!(*tree.peek().unwrap(), 2);
        assert_eq!(*copy.peek().unwrap(), 2);
        assert_eq!(*copy.tail().peek().unwrap(), 1);
        assert!(copy.tail().tail().peek().is_none());
        tree.pop();
        assert_eq!(*tree.peek().unwrap(), 1);
        assert_eq!(*copy.peek().unwrap(), 2);
        assert_eq!(*copy.tail().peek().unwrap(), 1);
        assert!(copy.tail().tail().peek().is_none());
        tree.pop();
        assert!(tree.peek().is_none());
        assert_eq!(*copy.peek().unwrap(), 2);
        assert_eq!(*copy.tail().peek().unwrap(), 1);
        assert!(copy.tail().tail().peek().is_none());
        tree.pop();
        assert_eq!(*copy.peek().unwrap(), 2);
        assert_eq!(*copy.tail().peek().unwrap(), 1);
        assert!(copy.tail().tail().peek().is_none());
    }

    #[test]
    fn mutation() {
        let mut tree: SharedList<i32> = SharedList::new();
        tree.push(1);
        let copy = tree.clone();
        assert_eq!(*copy.peek().unwrap(), 1);
        *tree.peek_mut().unwrap() = 2;
        assert_eq!(*copy.peek().unwrap(), 2);
        tree.push(7);
        assert_eq!(*copy.peek().unwrap(), 2);
        assert!(copy.tail().peek().is_none());
    }
}
