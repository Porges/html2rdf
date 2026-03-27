use std::{cell::RefCell, collections::BTreeMap, rc::Rc};

use oxrdf::Term;
use tracing::trace;

pub type SharedList = RefCell<Vec<Rc<Term>>>;

#[derive(Default)]
pub struct ListMapping {
    pub lists: BTreeMap<oxrdf::NamedNode, Rc<SharedList>>,
}

impl ListMapping {
    pub fn ensure_list(&mut self, predicate: oxrdf::NamedNode) -> Rc<SharedList> {
        if let Some(list) = self.lists.get(&predicate) {
            return list.clone();
        }

        trace!(%predicate, "created new list for predicate");
        let shared_list: Rc<SharedList> = Rc::default();
        let replaced = self.lists.insert(predicate, shared_list.clone());
        debug_assert!(replaced.is_none());
        shared_list
    }

    pub fn insert_value(&mut self, predicate: oxrdf::NamedNode, term: Rc<oxrdf::Term>) {
        trace!(%predicate, %term, "inserting term into list");
        let list = if let Some(list) = self.lists.get(&predicate) {
            list
        } else {
            trace!(%predicate, "created new list for predicate");
            self.lists.entry(predicate).or_default()
        };

        list.borrow_mut().push(term);
    }
}
