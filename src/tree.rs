use crate::{InsertError, MatchError, Params};

use std::cell::UnsafeCell;
use std::cmp::min;
use std::mem;
use std::ops::{Deref, DerefMut};
use std::str;

/// A successful match consisting of the registered value and the URL parameters, returned by
/// [`Node::at`](crate::Node::at).
#[derive(Debug)]
pub struct Match<'k, 'v, V> {
    /// The value stored under the matched node.
    pub value: V,
    /// The route parameters. See [parameters](crate#parameters) for more details.
    pub params: Params<'k, 'v>,
}

/// The types of nodes the tree can hold
#[derive(PartialEq, Eq, PartialOrd, Ord, Debug, Clone, Copy)]
enum NodeType {
    /// The root path
    Root,
    /// A URL parameter, ex: `/:id`. See `Param`
    Param,
    /// A wilcard parameter, ex: `/*static`
    CatchAll,
    /// Anything else
    Static,
}

#[derive(Clone)]
pub struct Root<T> {
    node: Node<T>,
    paths: Vec<(String, *const UnsafeCell<T>)>,
}

impl<T> Default for Root<T> {
    fn default() -> Self {
        Self {
            node: Node::default(),
            paths: Vec::new(),
        }
    }
}

impl<T> Root<T> {
    /// Construct a new `Root`.
    pub fn new() -> Self {
        Self::default()
    }

    /// Register a value in the tree under the given route.
    ///
    /// ```rust
    /// # use matchit::Root;
    /// # fn main() -> Result<(), Box<dyn std::error::Error>> {
    /// let mut matcher = Root::new();
    /// matcher.insert("/home", "Welcome!")?;
    /// matcher.insert("/users/:id", "A User")?;
    /// # Ok(())
    /// # }
    /// ```
    pub fn insert(&mut self, path: impl Into<String>, value: T) -> Result<(), InsertError> {
        let path = path.into();
        // this ensures that the stored pointer is initialized
        let cell_ref = self.deref_mut().insert_inner(path.clone(), value)?;
        let cell_pointer: *const UnsafeCell<T> = cell_ref;
        self.paths.push((path, cell_pointer));
        Ok(())
    }

    /// Get the routes contained in the tree along with an immutable reference
    /// to it's respective value.
    ///
    /// ```rust
    /// # use matchit::Root;
    /// # fn main() -> Result<(), Box<dyn std::error::Error>> {
    /// let mut matcher = Root::new();
    /// matcher.insert("/home", "Welcome!")?;
    /// matcher.insert("/users/:id", "A User")?;
    /// for (path, value) in matcher.routes() {
    ///     // println!("route '{}': {}", path, value);
    /// }
    /// # Ok(())
    /// # }
    /// ```
    pub fn routes(&self) -> impl Iterator<Item = (&str, &T)> {
        // SAFETY: this is safe because we take a &self
        self.routes_inner()
            .map(|(path, value)| (path, unsafe { &*value }))
    }

    /// Get the routes contained in the tree along with a mutable reference
    /// to it's respective value.
    ///
    /// ```rust
    /// # use matchit::Root;
    /// # fn main() -> Result<(), Box<dyn std::error::Error>> {
    /// let mut matcher = Root::new();
    /// matcher.insert("/home", "Welcome!")?;
    /// matcher.insert("/users/:id", "A User".to_string())?;
    /// for (path, value) in unsafe { matcher.routes_mut() } {
    ///     *value = "asdf".to_string();
    /// }
    /// # Ok(())
    /// # }
    /// ```
    pub fn routes_mut(&mut self) -> impl Iterator<Item = (&str, &mut T)> {
        // SAFETY: this is safe because we take a &mut self
        self.routes_inner()
            .map(|(path, value)| (path, unsafe { &mut *value }))
    }

    // This is needed to abstract over mutability.
    //
    // Out stored UnsafeCell's need to be wrapped in a Box in order to
    // be able to safely expose them. Otherwise assigning a value to the
    // generated mutable references will cause UB.
    fn routes_inner(&self) -> impl Iterator<Item = (&str, *mut T)> {
        self.paths.iter().map(move |(path, value)| {
            // SAFETY: this is safe because
            // - lifetimes are bound to &self, which is correct
            // - the pointer will always be initialized, because we cast a reference to a pointer
            // in `insert`
            // - the pointer cannot be null because we don't drop it or move it (if it's dropped, it means
            // self is also dropped, due to lifetimes being bound to self and that node is stored in self)
            (path.as_str(), UnsafeCell::raw_get(*value))
        })
    }
}

impl<T> Deref for Root<T> {
    type Target = Node<T>;

    fn deref(&self) -> &Self::Target {
        &self.node
    }
}

impl<T> DerefMut for Root<T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.node
    }
}

/// A radix tree used for URL path matching.
///
/// See [the crate documentation](crate) for details.
pub struct Node<T> {
    priority: u32,
    wild_child: bool,
    indices: Vec<u8>,
    node_type: NodeType,
    // See `at_inner` for why an unsafe cell is needed.
    // See `Root::routes_inner` for why Box is needed.
    value: Option<Box<UnsafeCell<T>>>,
    pub(crate) prefix: Vec<u8>,
    pub(crate) children: Vec<Self>,
}

// SAFETY: we expose `value` per rust's usual borrowing rules, so we can just delegate these traits
unsafe impl<T: Send> Send for Node<T> {}
unsafe impl<T: Sync> Sync for Node<T> {}

impl<T> Clone for Node<T>
where
    T: Clone,
{
    fn clone(&self) -> Self {
        Self {
            prefix: self.prefix.clone(),
            wild_child: self.wild_child,
            node_type: self.node_type,
            indices: self.indices.clone(),
            children: self.children.clone(),
            // SAFETY: We only expose &mut T through &mut self
            value: self
                .value
                .as_ref()
                .map(|val| Box::new(UnsafeCell::new(unsafe { &*val.get() }.clone()))),
            priority: self.priority,
        }
    }
}

impl<T> Default for Node<T> {
    fn default() -> Self {
        Self {
            prefix: Vec::new(),
            wild_child: false,
            node_type: NodeType::Static,
            indices: Vec::new(),
            children: Vec::new(),
            value: None,
            priority: 0,
        }
    }
}

impl<T> Node<T> {
    /// Construct a new `Node`.
    pub fn new() -> Self {
        Self::default()
    }

    /// Register a value in the tree under the given route.
    /// ```rust
    /// # use matchit::Node;
    /// # fn main() -> Result<(), Box<dyn std::error::Error>> {
    /// let mut matcher = Node::new();
    /// matcher.insert("/home", "Welcome!")?;
    /// matcher.insert("/users/:id", "A User")?;
    /// # Ok(())
    /// # }
    /// ```
    pub fn insert(&mut self, route: impl Into<String>, val: T) -> Result<(), InsertError> {
        self.insert_inner(route, val).map(|_| ())
    }

    fn insert_inner(
        &mut self,
        route: impl Into<String>,
        val: T,
    ) -> Result<&UnsafeCell<T>, InsertError> {
        let route = route.into().into_bytes();
        let mut prefix = route.as_ref();

        self.priority += 1;

        // Empty tree
        if self.prefix.is_empty() && self.children.is_empty() {
            self.node_type = NodeType::Root;
            let cell_ref = self.insert_child(&prefix, &route, val)?;
            return Ok(cell_ref);
        }

        let mut current = self;

        'walk: loop {
            // Find the longest common prefix.
            // This also implies that the common prefix contains no ':' or '*'
            // since the existing key can't contain those chars.
            let mut i = 0;
            let max = min(prefix.len(), current.prefix.len());

            while i < max && prefix[i] == current.prefix[i] {
                i += 1;
            }

            // Split edge
            if i < current.prefix.len() {
                let mut child = Self {
                    prefix: current.prefix[i..].to_owned(),
                    wild_child: current.wild_child,
                    indices: current.indices.to_owned(),
                    value: current.value.take(),
                    priority: current.priority - 1,
                    ..Self::default()
                };

                mem::swap(&mut current.children, &mut child.children);

                current.children = vec![child];
                current.indices = current.prefix[i..=i].to_owned();
                current.prefix = prefix[..i].to_owned();
                current.wild_child = false;
            }

            // Make new node a child of this node
            if prefix.len() > i {
                prefix = &prefix[i..];

                let idxc = prefix[0];

                // `/` after param
                if current.node_type == NodeType::Param
                    && idxc == b'/'
                    && current.children.len() == 1
                {
                    current = &mut current.children[0];
                    current.priority += 1;

                    continue 'walk;
                }

                // Check if a child with the next path byte exists
                for mut i in 0..current.indices.len() {
                    if idxc == current.indices[i] {
                        i = current.update_child_priority(i);
                        current = &mut current.children[i];

                        continue 'walk;
                    }
                }

                if idxc != b':' && idxc != b'*' && current.node_type != NodeType::CatchAll {
                    current.indices.push(idxc);
                    let child = current.add_child(Self::default());
                    current.update_child_priority(current.indices.len() - 1);
                    current = &mut current.children[child];
                } else if current.wild_child {
                    // inserting a wildcard node, need to check if it conflicts with the existing wildcard
                    current = current.children.last_mut().unwrap();
                    current.priority += 1;

                    // Check if the wildcard matches
                    if prefix.len() >= current.prefix.len()
                    && current.prefix == prefix[..current.prefix.len()]
                    // Adding a child to a CatchAll Node is not possible
                    && current.node_type != NodeType::CatchAll
                    // Check for longer wildcard, e.g. :name and :names
                    && (current.prefix.len() >= prefix.len() || prefix[current.prefix.len()] == b'/')
                    {
                        continue 'walk;
                    }

                    return Err(InsertError::conflict(&route, &prefix, &current));
                }

                return current.insert_child(prefix, &route, val);
            }

            // Otherwise add value to current node
            if current.value.is_some() {
                return Err(InsertError::conflict(&route, &prefix, &current));
            }

            current.value = Some(Box::new(UnsafeCell::new(val)));

            return Ok(current.value.as_ref().unwrap());
        }
    }

    // add a child node, keeping wildcards at the end
    fn add_child(&mut self, child: Node<T>) -> usize {
        let len = self.children.len();

        if self.wild_child && len > 0 {
            self.children.insert(len - 1, child);
            return len - 1;
        } else {
            self.children.push(child);
            return len;
        }
    }

    // Increments priority of the given child and reorders if necessary
    // returns the new position (index) of the child
    fn update_child_priority(&mut self, pos: usize) -> usize {
        self.children[pos].priority += 1;
        let prio = self.children[pos].priority;
        // adjust position (move to front)
        let mut new_pos = pos;

        while new_pos > 0 && self.children[new_pos - 1].priority < prio {
            // swap node positions
            self.children.swap(new_pos - 1, new_pos);
            new_pos -= 1;
        }

        // build new index char string
        if new_pos != pos {
            self.indices = [
                &self.indices[..new_pos],    // unchanged prefix, might be empty
                &self.indices[pos..=pos],    // the index char we move
                &self.indices[new_pos..pos], // rest without char at 'pos'
                &self.indices[pos + 1..],
            ]
            .concat();
        }

        new_pos
    }

    fn insert_child(
        &mut self,
        mut prefix: &[u8],
        route: &[u8],
        val: T,
    ) -> Result<&UnsafeCell<T>, InsertError> {
        let mut current = self;

        loop {
            let (wildcard, wildcard_index, valid) = find_wildcard(&prefix);

            if wildcard_index.is_none() {
                current.value = Some(Box::new(UnsafeCell::new(val)));
                current.prefix = prefix.to_owned();
                return Ok(current.value.as_ref().unwrap());
            };

            let mut wildcard_index = wildcard_index.unwrap();
            let wildcard = wildcard.unwrap();

            // the wildcard name must not contain ':' and '*'
            if !valid {
                return Err(InsertError::TooManyParams);
            };

            // check if the wildcard has a name
            if wildcard.len() < 2 {
                return Err(InsertError::UnnamedParam);
            }

            // Param
            if wildcard[0] == b':' {
                // Insert prefix before the current wildcard
                if wildcard_index > 0 {
                    current.prefix = prefix[..wildcard_index].to_owned();
                    prefix = &prefix[wildcard_index..];
                }

                let child = Self {
                    node_type: NodeType::Param,
                    prefix: wildcard.to_owned(),
                    ..Self::default()
                };

                let child = current.add_child(child);
                current.wild_child = true;
                current = &mut current.children[child];
                current.priority += 1;

                // If the route doesn't end with the wildcard, then there
                // will be another non-wildcard subroute starting with '/'
                if wildcard.len() < prefix.len() {
                    prefix = &prefix[wildcard.len()..];
                    let child = Self {
                        priority: 1,
                        ..Self::default()
                    };

                    let child = current.add_child(child);
                    current = &mut current.children[child];
                    continue;
                }

                // Otherwise we're done. Insert the value in the new leaf
                current.value = Some(Box::new(UnsafeCell::new(val)));
                return Ok(current.value.as_ref().unwrap());
            }

            // catch all
            if wildcard_index + wildcard.len() != prefix.len() {
                return Err(InsertError::InvalidCatchAll);
            }

            if !current.prefix.is_empty() && current.prefix.last().copied() == Some(b'/') {
                return Err(InsertError::conflict(&route, &prefix, &current));
            }

            // Currently fixed width 1 for '/'
            wildcard_index = wildcard_index
                .checked_sub(1)
                .ok_or(InsertError::MalformedPath)?;

            if prefix[wildcard_index] != b'/' {
                return Err(InsertError::InvalidCatchAll);
            }

            current.prefix = prefix[..wildcard_index].to_owned();
            current.indices = vec![b'/'];

            // first node: CatchAll Node with empty route
            let child = Self {
                wild_child: true,
                node_type: NodeType::CatchAll,
                ..Self::default()
            };

            let child = current.add_child(child);
            current = &mut current.children[child];
            current.priority += 1;

            // Second node: node holding the variable
            let child = Self {
                prefix: prefix[wildcard_index..].to_owned(),
                node_type: NodeType::CatchAll,
                value: Some(Box::new(UnsafeCell::new(val))),
                priority: 1,
                ..Self::default()
            };

            current.children = vec![child];

            return Ok(current.children[0].value.as_ref().unwrap());
        }
    }

    /// Tries to find a value in the router matching the given path.
    /// If no value can be found it returns a trailing slash redirect recommendation, see [`tsr`](crate::MatchError::tsr).
    /// ```rust
    /// # use matchit::Node;
    /// # fn main() -> Result<(), Box<dyn std::error::Error>> {
    /// let mut matcher = Node::new();
    /// matcher.insert("/home", "Welcome!")?;
    ///
    /// let matched = matcher.at("/home").unwrap();
    /// assert_eq!(*matched.value, "Welcome!");
    /// # Ok(())
    /// # }
    /// ```
    pub fn at<'n, 'p>(&'n self, path: &'p str) -> Result<Match<'n, 'p, &'n T>, MatchError> {
        match self.at_inner(path.as_bytes()) {
            Ok(v) => Ok(Match {
                // SAFETY: We only expose &mut T through &mut self
                value: unsafe { &*v.value.get() },
                params: v.params,
            }),
            Err(e) => Err(e),
        }
    }

    /// Tries to find a value in the router matching the given path, and returns a mutable
    /// reference to it. If no value can be found it returns a trailing slash redirect
    /// recommendation, see [`tsr`](crate::MatchError::tsr).
    pub fn at_mut<'n, 'p>(
        &'n mut self,
        path: &'p str,
    ) -> Result<Match<'n, 'p, &'n mut T>, MatchError> {
        match self.at_inner(path.as_bytes()) {
            Ok(v) => Ok(Match {
                // SAFETY: We have &mut self
                value: unsafe { &mut *v.value.get() },
                params: v.params,
            }),
            Err(e) => Err(e),
        }
    }
}

struct Skipped<'n, 'p, T> {
    path: &'p [u8],
    node: &'n Node<T>,
    params: usize,
}

#[rustfmt::skip]
macro_rules! backtracker {
    ($skipped_nodes:ident, $path:ident, $current:ident, $params:ident, $backtracking:ident, $walk:lifetime) => {
        macro_rules! backtrack {
            () => {
                while let Some(skipped) = $skipped_nodes.pop() {
                    if skipped.path.ends_with($path) {
                        $path = skipped.path;
                        $current = &skipped.node;
                        $params.truncate(skipped.params);

                        $backtracking = true;
                        continue $walk;
                    }
                }
            };
        }
    };
}

impl<T> Node<T> {
    // It's a bit sad that we have to introduce unsafe here, but rust doesn't really have a way
    // to abstract over mutability, so it avoids having to duplicate logic between `at` and
    // `at_mut`.
    fn at_inner<'n, 'p>(
        &'n self,
        path: &'p [u8],
    ) -> Result<Match<'n, 'p, &'n UnsafeCell<T>>, MatchError> {
        let full_path = path;

        let mut current = self;
        let mut path = full_path;
        let mut backtracking = false;
        let mut params = Params::new();
        let mut skipped_nodes: Vec<Skipped<'_, '_, _>> = Vec::new();

        'walk: loop {
            backtracker!(skipped_nodes, path, current, params, backtracking, 'walk);

            if path.len() > current.prefix.len() {
                if current.prefix == &path[..current.prefix.len()] {
                    path = &path[current.prefix.len()..];

                    let idx = path[0];

                    // Try all the non-wildcard children first by matching the indices
                    // unless we are currently backtracking
                    if !backtracking {
                        if let Some(i) = current.indices.iter().position(|&c| c == idx) {
                            if current.wild_child {
                                skipped_nodes.push(Skipped {
                                    path: &full_path
                                        [full_path.len() - (current.prefix.len() + path.len())..],
                                    node: &current,
                                    params: params.len(),
                                });
                            }
                            current = &current.children[i];
                            continue 'walk;
                        }
                    }

                    if !current.wild_child {
                        // If the path at the end of the loop is not '/', and the
                        // current node has no child nodes, we need to backtrack
                        if path != b"/" {
                            backtrack!();
                        }

                        // Nothing found.
                        // We can recommend to redirect to the same URL without a
                        // trailing slash if a leaf exists for that path.
                        let tsr = path == b"/" && current.value.is_some();
                        return Err(MatchError::new(tsr));
                    }

                    // Handle wildcard child, which is always at the end of the array
                    current = current.children.last().unwrap();

                    match current.node_type {
                        NodeType::Param => {
                            // find param end (either '/' or path end)
                            let end = path.iter().position(|&c| c == b'/').unwrap_or(path.len());

                            params.push(&current.prefix[1..], &path[..end]);

                            // we need to go deeper!
                            if end < path.len() {
                                // ... but we can't
                                if current.children.is_empty() {
                                    let tsr = path.len() == end + 1;
                                    return Err(MatchError::new(tsr));
                                }

                                path = &path[end..];
                                current = &current.children[0];

                                backtracking = false;
                                continue 'walk;
                            }

                            if let Some(value) = current.value.as_ref() {
                                return Ok(Match { value, params });
                            } else if current.children.len() == 1 {
                                current = &current.children[0];

                                // No value found. Check if a value for this path + a
                                // trailing slash exists for TSR recommendation
                                let tsr = (current.prefix == b"/" && current.value.is_some())
                                    || (current.prefix.is_empty() && (current.indices == b"/"));
                                return Err(MatchError::new(tsr));
                            }

                            return Err(MatchError::new(false));
                        }
                        NodeType::CatchAll => {
                            params.push(&current.prefix[2..], path);

                            return match current.value.as_ref() {
                                Some(value) => Ok(Match { value, params }),
                                None => Err(MatchError::new(false)),
                            };
                        }
                        _ => unreachable!(),
                    }
                }
            }

            if path == current.prefix {
                // If the current path is not '/', the node does not have a value,
                // and the most recently matched node has a child node, we need
                // to backtrack
                if current.value.is_none() && path != b"/" {
                    backtrack!();
                }

                // We should have reached the node containing the value.
                // Check if this node has a value registered.
                if let Some(value) = current.value.as_ref() {
                    return Ok(Match { value, params });
                }

                // If there is no value for this route, but this route has a
                // wildcard child, there must be a value for this path with an
                // additional trailing slash
                if path == b"/" && current.wild_child && current.node_type != NodeType::Root {
                    return Err(MatchError::new(true));
                }

                // No value found. Check if a value for this path + a
                // trailing slash exists for trailing slash recommendation
                if !backtracking {
                    if let Some(i) = current.indices.iter().position(|&c| c == b'/') {
                        current = &current.children[i];
                        let tsr = (current.prefix.len() == 1 && current.value.is_some())
                            || (current.node_type == NodeType::CatchAll
                                && current.children[0].value.is_some());
                        return Err(MatchError::new(tsr));
                    }
                }

                return Err(MatchError::new(false));
            }

            // Nothing found. We can recommend to redirect to the same URL with an
            // extra trailing slash if a leaf exists for that path
            let tsr = (path == b"/" && full_path != b"/")
                || (current.prefix.len() == path.len() + 1
                    && current.prefix[path.len()] == b'/'
                    && path == &current.prefix[..current.prefix.len() - 1]
                    && current.value.is_some());

            // If there is no tsr, try backtracking.
            if !tsr && path != b"/" {
                backtrack!();
            }

            return Err(MatchError::new(tsr));
        }
    }

    /// Makes a case-insensitive match of the given path and tries to find a handler.
    /// It can optionally also fix trailing slashes.
    /// If the match is successful, it returns the case corrected path.
    /// ```rust
    /// # use matchit::Node;
    ///
    /// # fn main() -> Result<(), Box<dyn std::error::Error>> {
    /// let mut matcher = Node::new();
    /// matcher.insert("/home", "Welcome!")?;
    ///
    /// let path = matcher.path_ignore_case("/HoMe/", true).unwrap();
    /// assert_eq!(path, "/home");
    /// # Ok(())
    /// # }
    /// ````
    pub fn path_ignore_case(
        &self,
        path: impl AsRef<str>,
        fix_trailing_slash: bool,
    ) -> Option<String> {
        let path = path.as_ref();
        let mut insensitive_path = Vec::with_capacity(path.len() + 1);
        let found = self.path_ignore_case_helper(
            path.as_bytes(),
            &mut insensitive_path,
            [0; 4],
            fix_trailing_slash,
        );
        if found {
            Some(String::from_utf8(insensitive_path).unwrap())
        } else {
            None
        }
    }

    fn path_ignore_case_helper(
        &self,
        mut path: &[u8],
        insensitive_path: &mut Vec<u8>,
        mut buf: [u8; 4],
        fix_trailing_slash: bool,
    ) -> bool {
        let lower_path: &[u8] = &path.to_ascii_lowercase();
        if lower_path.len() >= self.prefix.len()
            && (self.prefix.is_empty()
                || lower_path[1..self.prefix.len()].eq_ignore_ascii_case(&self.prefix[1..]))
        {
            insensitive_path.extend_from_slice(&self.prefix);

            path = &path[self.prefix.len()..];

            if !path.is_empty() {
                let cached_lower_path = <&[u8]>::clone(&lower_path);

                // If this node does not have a wildcard (param or catchAll) child,
                // we can just look up the next child node and continue to walk down
                // the tree
                if !self.wild_child {
                    // skip char bytes already processed
                    buf = shift_n_bytes(buf, self.prefix.len());

                    if buf[0] == 0 {
                        // process a new char
                        let mut current_char = 0 as char;

                        // find char start
                        // chars are up to 4 byte long,
                        // -4 would definitely be another char
                        let mut off = 0;
                        for j in 0..min(self.prefix.len(), 3) {
                            let i = self.prefix.len() - j;
                            if char_start(cached_lower_path[i]) {
                                // read char from cached path
                                current_char = str::from_utf8(&cached_lower_path[i..])
                                    .unwrap()
                                    .chars()
                                    .next()
                                    .unwrap();
                                off = j;
                                break;
                            }
                        }

                        current_char.encode_utf8(&mut buf);

                        // skip already processed bytes
                        buf = shift_n_bytes(buf, off);

                        for i in 0..self.indices.len() {
                            // lowercase matches
                            if self.indices[i] == buf[0] {
                                // must use a recursive approach since both the
                                // uppercase byte and the lowercase byte might exist
                                // as an index
                                if self.children[i].path_ignore_case_helper(
                                    path,
                                    insensitive_path,
                                    buf,
                                    fix_trailing_slash,
                                ) {
                                    return true;
                                }

                                if insensitive_path.len() > self.children[i].prefix.len() {
                                    let prev_len =
                                        insensitive_path.len() - self.children[i].prefix.len();
                                    insensitive_path.truncate(prev_len);
                                }

                                break;
                            }
                        }

                        // same for uppercase char, if it differs
                        let up = current_char.to_ascii_uppercase();
                        if up != current_char {
                            up.encode_utf8(&mut buf);
                            buf = shift_n_bytes(buf, off);

                            for i in 0..self.indices.len() {
                                if self.indices[i] == buf[0] {
                                    return self.children[i].path_ignore_case_helper(
                                        path,
                                        insensitive_path,
                                        buf,
                                        fix_trailing_slash,
                                    );
                                }
                            }
                        }
                    } else {
                        // old char not finished
                        for i in 0..self.indices.len() {
                            if self.indices[i] == buf[0] {
                                // continue with child node
                                return self.children[i].path_ignore_case_helper(
                                    path,
                                    insensitive_path,
                                    buf,
                                    fix_trailing_slash,
                                );
                            }
                        }
                    }

                    // Nothing found. We can recommend to redirect to the same URL
                    // without a trailing slash if a leaf exists for that path
                    return fix_trailing_slash && path == [b'/'] && self.value.is_some();
                }

                return self.children[0].path_ignore_case_match_helper(
                    path,
                    insensitive_path,
                    buf,
                    fix_trailing_slash,
                );
            } else {
                // We should have reached the node containing the value.
                // Check if this node has a value registered.
                if self.value.is_some() {
                    return true;
                }

                // No value found.
                // Try to fix the path by adding a trailing slash
                if fix_trailing_slash {
                    for i in 0..self.indices.len() {
                        if self.indices[i] == b'/' {
                            if (self.children[i].prefix.len() == 1
                                && self.children[i].value.is_some())
                                || (self.children[i].node_type == NodeType::CatchAll
                                    && self.children[i].children[0].value.is_some())
                            {
                                insensitive_path.push(b'/');
                                return true;
                            }
                            return false;
                        }
                    }
                }
                return false;
            }
        }

        // Nothing found.
        // Try to fix the path by adding / removing a trailing slash
        if fix_trailing_slash {
            if path == [b'/'] {
                return true;
            }
            if lower_path.len() + 1 == self.prefix.len()
                && self.prefix[lower_path.len()] == b'/'
                && lower_path[1..].eq_ignore_ascii_case(&self.prefix[1..lower_path.len()])
                && self.value.is_some()
            {
                insensitive_path.extend_from_slice(&self.prefix);
                return true;
            }
        }

        false
    }

    fn path_ignore_case_match_helper(
        &self,
        mut path: &[u8],
        insensitive_path: &mut Vec<u8>,
        buf: [u8; 4],
        fix_trailing_slash: bool,
    ) -> bool {
        match self.node_type {
            NodeType::Param => {
                let mut end = 0;

                while end < path.len() && path[end] != b'/' {
                    end += 1;
                }

                insensitive_path.extend_from_slice(&path[..end]);

                if end < path.len() {
                    if !self.children.is_empty() {
                        path = &path[end..];

                        return self.children[0].path_ignore_case_helper(
                            path,
                            insensitive_path,
                            buf,
                            fix_trailing_slash,
                        );
                    }

                    // ... but we can't
                    if fix_trailing_slash && path.len() == end + 1 {
                        return true;
                    }
                    return false;
                }

                if self.value.is_some() {
                    return true;
                } else if fix_trailing_slash
                    && self.children.len() == 1
                    && self.children[0].prefix == [b'/']
                    && self.children[0].value.is_some()
                {
                    // No value found. Check if a value for this path + a
                    // trailing slash exists
                    insensitive_path.push(b'/');
                    return true;
                }

                false
            }
            NodeType::CatchAll => {
                insensitive_path.extend_from_slice(path);
                true
            }
            _ => unreachable!(),
        }
    }

    #[cfg(feature = "__test_helpers")]
    pub fn check_priorities(&self) -> Result<u32, (u32, u32)> {
        let mut priority: u32 = 0;
        for child in &self.children {
            priority += child.check_priorities()?;
        }

        if self.value.is_some() {
            priority += 1;
        }

        if self.priority != priority {
            return Err((self.priority, priority));
        }

        Ok(priority)
    }
}

// Shift bytes in array by n bytes left
pub const fn shift_n_bytes(bytes: [u8; 4], n: usize) -> [u8; 4] {
    match u32::from_ne_bytes(bytes).overflowing_shr((n * 8) as u32) {
        (_, true) => [0; 4],
        (shifted, false) => shifted.to_ne_bytes(),
    }
}

// Reports whether the byte could be the first byte of a `char`.
const fn char_start(b: u8) -> bool {
    b & 0xC0 != 0x80
}

// Search for a wildcard segment and check the name for invalid characters.
fn find_wildcard(path: &[u8]) -> (Option<&[u8]>, Option<usize>, bool) {
    // Find start
    for (start, &c) in path.iter().enumerate() {
        // A wildcard starts with ':' (param) or '*' (catch-all)
        if c != b':' && c != b'*' {
            continue;
        };

        // Find end and check for invalid characters
        let mut valid = true;

        for (end, &c) in path[start + 1..].iter().enumerate() {
            match c {
                b'/' => return (Some(&path[start..start + 1 + end]), Some(start), valid),
                b':' | b'*' => valid = false,
                _ => (),
            };
        }

        return (Some(&path[start..]), Some(start), valid);
    }
    (None, None, false)
}

#[cfg(test)]
mod tests {
    use super::Root;

    #[test]
    fn test_tree_routes() {
        let mut tree = Root::new();

        let routes = [
            ("/test", "a"),
            ("/test2", "b"),
            ("/test/sub", "c"),
            ("/test2/sub", "d"),
        ];

        for (path, value) in routes {
            tree.insert(path, value).unwrap();
        }

        let new_routes = tree.routes().collect::<Vec<_>>();

        let are_routes_correct = routes.iter().all(|(p, v)| {
            new_routes
                .iter()
                .find(|(op, _)| p == op)
                .map_or(false, |(_, ov)| v == *ov)
        });

        assert!(are_routes_correct);
    }

    #[test]
    fn test_tree_routes_mut() {
        let mut tree = Root::new();

        let routes = [
            ("/test", "a"),
            ("/test2", "b"),
            ("/test/sub", "c"),
            ("/test2/sub", "d"),
        ];

        for (path, value) in routes {
            tree.insert(path, value.to_string()).unwrap();
        }

        let replace_with = [
            ("/test", "hello"),
            ("/test2", "world"),
            ("/test/sub", "!"),
            ("/test2/sub", "?"),
        ];

        for (path, value) in tree.routes_mut() {
            if let Some((_, replace_value)) = replace_with.iter().find(|(op, _)| path == *op) {
                *value = replace_value.to_string();
            }
        }

        let new_routes = tree.routes().collect::<Vec<_>>();

        let replaced_correctly = replace_with.iter().all(|(p, v)| {
            new_routes
                .iter()
                .find(|(op, _)| p == op)
                .map_or(false, |(_, ov)| v == ov)
        });

        assert!(replaced_correctly);
    }
}
