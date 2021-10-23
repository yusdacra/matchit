use crate::{InsertError, MatchError, Params};

use std::cell::UnsafeCell;
use std::cmp::min;
use std::mem;
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

/// A node in a radix tree ordered by priority.
///
/// Priority is just the number of values registered in sub nodes
/// (children, grandchildren, and so on..).
pub struct Node<T> {
    prefix: Vec<u8>,
    wild_child: bool,
    node_type: NodeType,
    indices: Vec<u8>,
    children: Vec<Self>,
    // See `at_inner` for why an unsafe cell is needed.
    value: Option<UnsafeCell<T>>,
    priority: u32,
}

// SAFETY: we expose `value` per rust's usual borrowing rules, so we can just delegate these traits
unsafe impl<T: Send> Send for Node<T> {}
unsafe impl<T: Sync> Sync for Node<T> {}

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

struct Skipped<'n, 'p, T> {
    path: &'p [u8],
    node: &'n Node<T>,
    params: usize,
}

impl<T> Node<T> {
    /// Construct a new `Node`.
    pub fn new() -> Self {
        Self::default()
    }

    /// Register a value in the tree under the given path.
    /// ```rust
    /// # use matchit::Node;
    /// # fn main() -> Result<(), Box<dyn std::error::Error>> {
    /// let mut matcher = Node::new();
    /// matcher.insert("/home", "Welcome!")?;
    /// matcher.insert("/users/:id", "A User")?;
    /// # Ok(())
    /// # }
    /// ```
    pub fn insert(&mut self, path: impl Into<String>, val: T) -> Result<(), InsertError> {
        let path = path.into().into_bytes();
        let mut prefix = path.as_ref();

        self.priority += 1;

        // Empty tree
        if self.prefix.is_empty() && self.children.is_empty() {
            self.insert_child(&prefix, &path, val)?;
            self.node_type = NodeType::Root;
            return Ok(());
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

                    return Err(InsertError::conflict(&path, &prefix, &current.prefix));
                }

                return current.insert_child(prefix, &path, val);
            }

            // Otherwise add value to current node
            if current.value.is_some() {
                return Err(InsertError::conflict(&path, &prefix, &current.prefix));
            }

            current.value = Some(UnsafeCell::new(val));

            return Ok(());
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
            // TODO: len + 1
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
        full_path: &[u8],
        val: T,
    ) -> Result<(), InsertError> {
        let mut current = self;

        loop {
            let (wildcard, wildcard_index, valid) = find_wildcard(&prefix);

            if wildcard_index.is_none() {
                current.value = Some(UnsafeCell::new(val));
                current.prefix = prefix.to_owned();
                return Ok(());
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

                // If the path doesn't end with the wildcard, then there
                // will be another non-wildcard subpath starting with '/'
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
                current.value = Some(UnsafeCell::new(val));
                return Ok(());
            }

            // catch all
            if wildcard_index + wildcard.len() != prefix.len() {
                return Err(InsertError::InvalidCatchAll);
            }

            if !current.prefix.is_empty() && current.prefix.last().copied() == Some(b'/') {
                return Err(InsertError::conflict(&full_path, &prefix, &current.prefix));
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

            // first node: CatchAll Node with empty path
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
                value: Some(UnsafeCell::new(val)),
                priority: 1,
                ..Self::default()
            };

            current.children = vec![child];

            return Ok(());
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
                // SAFETY: We only expose unique references to value through &mut self
                // and pointer is always non-null
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
                // SAFETY: We have a unique reference to self
                value: unsafe { &mut *v.value.get() },
                params: v.params,
            }),
            Err(e) => Err(e),
        }
    }

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
        let mut skipped_nodes: Option<Vec<Skipped<'_, '_, T>>> = None;

        'walk: loop {
            let prefix = &current.prefix;
            if path.len() > prefix.len() {
                if prefix == &path[..prefix.len()] {
                    path = &path[prefix.len()..];

                    let idx = path[0];

                    // skip indices check when backtracking
                    if !backtracking {
                        if let Some(i) = current.indices.iter().position(|&c| c == idx) {
                            if current.wild_child {
                                skipped_nodes
                                    .get_or_insert_with(Default::default)
                                    .push(Skipped {
                                        path: &full_path
                                            [full_path.len() - (prefix.len() + path.len())..],
                                        node: &current,
                                        params: params.len(),
                                    });
                            }
                            current = &current.children[i];
                            continue 'walk;
                        }
                    }

                    if path != b"/" && !current.wild_child {
                        if let Some(skipped_nodes) = &mut skipped_nodes {
                            while let Some(skipped) = skipped_nodes.pop() {
                                if skipped.path.ends_with(path) {
                                    path = skipped.path;
                                    current = &skipped.node;
                                    params.truncate(skipped.params);

                                    backtracking = true;
                                    continue 'walk;
                                }
                            }
                        }
                    }

                    // If there is no wildcard pattern, recommend a redirection
                    if !current.wild_child {
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

            if path == prefix {
                if current.value.is_none() && path != b"/" {
                    if let Some(skipped_nodes) = &mut skipped_nodes {
                        while let Some(skipped) = skipped_nodes.pop() {
                            if skipped.path.ends_with(path) {
                                path = skipped.path;
                                current = &skipped.node;
                                params.truncate(skipped.params);

                                backtracking = true;
                                continue 'walk;
                            }
                        }
                    }
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

            if path != b"/" {
                if let Some(skipped_nodes) = &mut skipped_nodes {
                    while let Some(skipped) = skipped_nodes.pop() {
                        if skipped.path.ends_with(path) {
                            path = skipped.path;
                            current = &skipped.node;
                            params.truncate(skipped.params);

                            backtracking = true;
                            continue 'walk;
                        }
                    }
                }
            }

            // Nothing found. We can recommend to redirect to the same URL with an
            // extra trailing slash if a leaf exists for that path
            let tsr = (path == b"/") || (prefix.len() == path.len() + 1 && current.value.is_some());
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
}

// Shift bytes in array by n bytes left
pub const fn shift_n_bytes(bytes: [u8; 4], n: usize) -> [u8; 4] {
    match u32::from_ne_bytes(bytes).overflowing_shr((n * 8) as u32) {
        (_, true) => [0; 4],
        (shifted, false) => shifted.to_ne_bytes(),
    }
}

// This function is ported from go.
// Reports whether the byte could be the first byte of an encoded,
// possibly invalid char. Second and subsequent bytes always have
// the top two bits set to 10.
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
    use super::*;

    fn params(vec: Vec<(&'static str, &'static str)>) -> Params<'static, 'static> {
        let mut params = Params::new();
        for (key, value) in vec {
            params.push(key.as_bytes(), value.as_bytes());
        }
        params
    }

    struct TestRequest {
        path: &'static str,
        route: &'static str,
        params: Option<Params<'static, 'static>>,
    }

    fn req(
        path: &'static str,
        route: &'static str,
        vec: Option<Vec<(&'static str, &'static str)>>,
    ) -> TestRequest {
        TestRequest {
            path,
            route,
            params: vec.map(params),
        }
    }

    fn check_requests(tree: &mut Node<String>, requests: Vec<TestRequest>) {
        for request in requests {
            let res = tree.at(request.path);

            match res {
                Err(_) => {
                    if request.params.is_some() {
                        panic!("Expected non-nil value for route '{}'", request.path);
                    }
                }
                Ok(result) => {
                    match request.params {
                        Some(expected_params) => {
                            if result.value != request.route {
                                panic!(
                                    "Wrong value for route '{}'. Expected '{}', found '{}')",
                                    request.path, result.value, request.route
                                );
                            }

                            assert_eq!(
                                result.params, expected_params,
                                "Wrong params for route '{}'",
                                request.path
                            );

                            // test at_mut
                            let res_mut = tree.at_mut(request.path).unwrap();
                            res_mut.value.push_str("CHECKED");

                            let res = tree.at(request.path).unwrap();
                            assert!(res.value.contains("CHECKED"));

                            let res_mut = tree.at_mut(request.path).unwrap();
                            *res_mut.value = res_mut.value.replace("CHECKED", "");
                        }

                        None => {
                            panic!(
                                "Expected nil value for route '{}', got: {:?}",
                                request.path,
                                result.params.iter().collect::<Vec<_>>()
                            );
                        }
                    }
                }
            };
        }
    }

    fn check_priorities(n: &mut Node<String>) -> u32 {
        let mut prio: u32 = 0;
        for i in 0..n.children.len() {
            prio += check_priorities(&mut n.children[i]);
        }

        if n.value.is_some() {
            prio += 1;
        }

        if n.priority != prio {
            panic!(
                "priority mismatch for node '{}': found '{}', expected '{}'",
                str::from_utf8(&n.prefix).unwrap(),
                n.priority,
                prio
            )
        }

        prio
    }

    #[test]
    fn test_tree_add_and_get() {
        let mut tree = Node::new();

        let routes = vec![
            "/hi",
            "/contact",
            "/co",
            "/c",
            "/a",
            "/ab",
            "/doc/",
            "/doc/go_faq.html",
            "/doc/go1.html",
            "/ʯ",
            "/β",
        ];

        for route in routes {
            tree.insert(route, route.to_owned()).unwrap();
        }

        check_requests(
            &mut tree,
            vec![
                req("/a", "/a", Some(vec![])),
                req("/", "", None),
                req("/hi", "/hi", Some(vec![])),
                req("/contact", "/contact", Some(vec![])),
                req("/co", "/co", Some(vec![])),
                req("/con", "", None),  // key mismatch
                req("/cona", "", None), // key mismatch
                req("/no", "", None),   // no matching child
                req("/ab", "/ab", Some(vec![])),
                req("/ʯ", "/ʯ", Some(vec![])),
                req("/β", "/β", Some(vec![])),
            ],
        );

        check_priorities(&mut tree);
    }

    #[test]
    fn test_tree_wildcard() {
        let mut tree = Node::new();

        let routes = vec![
            "/",
            "/cmd/:tool/",
            "/cmd/:tool/:sub",
            "/cmd/whoami",
            "/cmd/whoami/root",
            "/cmd/whoami/root/",
            "/src/*filepath",
            "/search/",
            "/search/:query",
            "/search/gin-gonic",
            "/search/google",
            "/user_:name",
            "/user_:name/about",
            "/files/:dir/*filepath",
            "/doc/",
            "/doc/go_faq.html",
            "/doc/go1.html",
            "/info/:user/public",
            "/info/:user/project/:project",
            "/info/:user/project/golang",
            "/aa/*xx",
            "/ab/*xx",
            "/:cc",
            "/c1/:dd/e",
            "/c1/:dd/e1",
            "/:cc/cc",
            "/:cc/:dd/ee",
            "/:cc/:dd/:ee/ff",
            "/:cc/:dd/:ee/:ff/gg",
            "/:cc/:dd/:ee/:ff/:gg/hh",
            "/get/test/abc/",
            "/get/:param/abc/",
            "/something/:paramname/thirdthing",
            "/something/secondthing/test",
            "/get/abc",
            "/get/:param",
            "/get/abc/123abc",
            "/get/abc/:param",
            "/get/abc/123abc/xxx8",
            "/get/abc/123abc/:param",
            "/get/abc/123abc/xxx8/1234",
            "/get/abc/123abc/xxx8/:param",
            "/get/abc/123abc/xxx8/1234/ffas",
            "/get/abc/123abc/xxx8/1234/:param",
            "/get/abc/123abc/xxx8/1234/kkdd/12c",
            "/get/abc/123abc/xxx8/1234/kkdd/:param",
            "/get/abc/:param/test",
            "/get/abc/123abd/:param",
            "/get/abc/123abddd/:param",
            "/get/abc/123/:param",
            "/get/abc/123abg/:param",
            "/get/abc/123abf/:param",
            "/get/abc/123abfff/:param",
        ];

        for route in routes {
            tree.insert(route, route.to_owned()).unwrap();
        }

        check_requests(
            &mut tree,
            vec![
                req("/", "/", Some(vec![])),
                req("/cmd/test", "/cmd/:tool/", None),
                req("/cmd/test/", "/cmd/:tool/", Some(vec![("tool", "test")])),
                req(
                    "/cmd/test/3",
                    "/cmd/:tool/:sub",
                    Some(vec![("tool", "test"), ("sub", "3")]),
                ),
                req("/cmd/who", "/cmd/:tool/", None),
                req("/cmd/who/", "/cmd/:tool/", Some(vec![("tool", "who")])),
                req("/cmd/whoami", "/cmd/whoami", Some(vec![])),
                req("/cmd/whoami/", "/cmd/whoami", None),
                req(
                    "/cmd/whoami/r",
                    "/cmd/:tool/:sub",
                    Some(vec![("tool", "whoami"), ("sub", "r")]),
                ),
                req("/cmd/whoami/r/", "/cmd/:tool/:sub", None),
                req("/cmd/whoami/root", "/cmd/whoami/root", Some(vec![])),
                req("/cmd/whoami/root/", "/cmd/whoami/root/", Some(vec![])),
                req("/src/", "/src/*filepath", Some(vec![("filepath", "/")])),
                req(
                    "/src/some/file.png",
                    "/src/*filepath",
                    Some(vec![("filepath", "/some/file.png")]),
                ),
                req("/search/", "/search/", Some(vec![])),
                req(
                    "/search/someth!ng+in+ünìcodé",
                    "/search/:query",
                    Some(vec![("query", "someth!ng+in+ünìcodé")]),
                ),
                req("/search/someth!ng+in+ünìcodé/", "", None),
                req(
                    "/user_rustacean",
                    "/user_:name",
                    Some(vec![("name", "rustacean")]),
                ),
                req(
                    "/user_rustacean/about",
                    "/user_:name/about",
                    Some(vec![("name", "rustacean")]),
                ),
                req(
                    "/files/js/inc/framework.js",
                    "/files/:dir/*filepath",
                    Some(vec![("dir", "js"), ("filepath", "/inc/framework.js")]),
                ),
                req(
                    "/info/gordon/public",
                    "/info/:user/public",
                    Some(vec![("user", "gordon")]),
                ),
                req(
                    "/info/gordon/project/go",
                    "/info/:user/project/:project",
                    Some(vec![("user", "gordon"), ("project", "go")]),
                ),
                req(
                    "/info/gordon/project/golang",
                    "/info/:user/project/golang",
                    Some(vec![("user", "gordon")]),
                ),
                req("/aa/aa", "/aa/*xx", Some(vec![("xx", "/aa")])),
                req("/ab/ab", "/ab/*xx", Some(vec![("xx", "/ab")])),
                req("/a", "/:cc", Some(vec![("cc", "a")])),
                req("/all", "/:cc", Some(vec![("cc", "all")])),
                req("/d", "/:cc", Some(vec![("cc", "d")])),
                req("/ad", "/:cc", Some(vec![("cc", "ad")])),
                req("/dd", "/:cc", Some(vec![("cc", "dd")])),
                req("/dddaa", "/:cc", Some(vec![("cc", "dddaa")])),
                req("/aa", "/:cc", Some(vec![("cc", "aa")])),
                req("/aaa", "/:cc", Some(vec![("cc", "aaa")])),
                req("/aaa/cc", "/:cc/cc", Some(vec![("cc", "aaa")])),
                req("/ab", "/:cc", Some(vec![("cc", "ab")])),
                req("/abb", "/:cc", Some(vec![("cc", "abb")])),
                req("/abb/cc", "/:cc/cc", Some(vec![("cc", "abb")])),
                req("/allxxxx", "/:cc", Some(vec![("cc", "allxxxx")])),
                req("/alldd", "/:cc", Some(vec![("cc", "alldd")])),
                req("/all/cc", "/:cc/cc", Some(vec![("cc", "all")])),
                req("/a/cc", "/:cc/cc", Some(vec![("cc", "a")])),
                req("/c1/d/e", "/c1/:dd/e", Some(vec![("dd", "d")])),
                req("/c1/d/e1", "/c1/:dd/e1", Some(vec![("dd", "d")])),
                req(
                    "/c1/d/ee",
                    "/:cc/:dd/ee",
                    Some(vec![("cc", "c1"), ("dd", "d")]),
                ),
                req("/cc/cc", "/:cc/cc", Some(vec![("cc", "cc")])),
                req("/ccc/cc", "/:cc/cc", Some(vec![("cc", "ccc")])),
                req("/deedwjfs/cc", "/:cc/cc", Some(vec![("cc", "deedwjfs")])),
                req("/acllcc/cc", "/:cc/cc", Some(vec![("cc", "acllcc")])),
                req("/get/test/abc/", "/get/test/abc/", Some(vec![])),
                req(
                    "/get/te/abc/",
                    "/get/:param/abc/",
                    Some(vec![("param", "te")]),
                ),
                req(
                    "/get/testaa/abc/",
                    "/get/:param/abc/",
                    Some(vec![("param", "testaa")]),
                ),
                req(
                    "/get/xx/abc/",
                    "/get/:param/abc/",
                    Some(vec![("param", "xx")]),
                ),
                req(
                    "/get/tt/abc/",
                    "/get/:param/abc/",
                    Some(vec![("param", "tt")]),
                ),
                req(
                    "/get/a/abc/",
                    "/get/:param/abc/",
                    Some(vec![("param", "a")]),
                ),
                req(
                    "/get/t/abc/",
                    "/get/:param/abc/",
                    Some(vec![("param", "t")]),
                ),
                req(
                    "/get/aa/abc/",
                    "/get/:param/abc/",
                    Some(vec![("param", "aa")]),
                ),
                req(
                    "/get/abas/abc/",
                    "/get/:param/abc/",
                    Some(vec![("param", "abas")]),
                ),
                req(
                    "/something/secondthing/test",
                    "/something/secondthing/test",
                    Some(vec![]),
                ),
                req(
                    "/something/abcdad/thirdthing",
                    "/something/:paramname/thirdthing",
                    Some(vec![("paramname", "abcdad")]),
                ),
                req(
                    "/something/secondthingaaaa/thirdthing",
                    "/something/:paramname/thirdthing",
                    Some(vec![("paramname", "secondthingaaaa")]),
                ),
                req(
                    "/something/se/thirdthing",
                    "/something/:paramname/thirdthing",
                    Some(vec![("paramname", "se")]),
                ),
                req(
                    "/something/s/thirdthing",
                    "/something/:paramname/thirdthing",
                    Some(vec![("paramname", "s")]),
                ),
                req(
                    "/c/d/ee",
                    "/:cc/:dd/ee",
                    Some(vec![("cc", "c"), ("dd", "d")]),
                ),
                req(
                    "/c/d/e/ff",
                    "/:cc/:dd/:ee/ff",
                    Some(vec![("cc", "c"), ("dd", "d"), ("ee", "e")]),
                ),
                req(
                    "/c/d/e/f/gg",
                    "/:cc/:dd/:ee/:ff/gg",
                    Some(vec![("cc", "c"), ("dd", "d"), ("ee", "e"), ("ff", "f")]),
                ),
                req(
                    "/c/d/e/f/g/hh",
                    "/:cc/:dd/:ee/:ff/:gg/hh",
                    Some(vec![
                        ("cc", "c"),
                        ("dd", "d"),
                        ("ee", "e"),
                        ("ff", "f"),
                        ("gg", "g"),
                    ]),
                ),
                req(
                    "/cc/dd/ee/ff/gg/hh",
                    "/:cc/:dd/:ee/:ff/:gg/hh",
                    Some(vec![
                        ("cc", "cc"),
                        ("dd", "dd"),
                        ("ee", "ee"),
                        ("ff", "ff"),
                        ("gg", "gg"),
                    ]),
                ),
                req("/get/abc", "/get/abc", Some(vec![])),
                req("/get/a", "/get/:param", Some(vec![("param", "a")])),
                req("/get/abz", "/get/:param", Some(vec![("param", "abz")])),
                req("/get/12a", "/get/:param", Some(vec![("param", "12a")])),
                req("/get/abcd", "/get/:param", Some(vec![("param", "abcd")])),
                req("/get/abc/123abc", "/get/abc/123abc", Some(vec![])),
                req(
                    "/get/abc/12",
                    "/get/abc/:param",
                    Some(vec![("param", "12")]),
                ),
                req(
                    "/get/abc/123ab",
                    "/get/abc/:param",
                    Some(vec![("param", "123ab")]),
                ),
                req(
                    "/get/abc/xyz",
                    "/get/abc/:param",
                    Some(vec![("param", "xyz")]),
                ),
                req(
                    "/get/abc/123abcddxx",
                    "/get/abc/:param",
                    Some(vec![("param", "123abcddxx")]),
                ),
                req("/get/abc/123abc/xxx8", "/get/abc/123abc/xxx8", Some(vec![])),
                req(
                    "/get/abc/123abc/x",
                    "/get/abc/123abc/:param",
                    Some(vec![("param", "x")]),
                ),
                req(
                    "/get/abc/123abc/xxx",
                    "/get/abc/123abc/:param",
                    Some(vec![("param", "xxx")]),
                ),
                req(
                    "/get/abc/123abc/abc",
                    "/get/abc/123abc/:param",
                    Some(vec![("param", "abc")]),
                ),
                req(
                    "/get/abc/123abc/xxx8xxas",
                    "/get/abc/123abc/:param",
                    Some(vec![("param", "xxx8xxas")]),
                ),
                req(
                    "/get/abc/123abc/xxx8/1234",
                    "/get/abc/123abc/xxx8/1234",
                    Some(vec![]),
                ),
                req(
                    "/get/abc/123abc/xxx8/1",
                    "/get/abc/123abc/xxx8/:param",
                    Some(vec![("param", "1")]),
                ),
                req(
                    "/get/abc/123abc/xxx8/123",
                    "/get/abc/123abc/xxx8/:param",
                    Some(vec![("param", "123")]),
                ),
                req(
                    "/get/abc/123abc/xxx8/78k",
                    "/get/abc/123abc/xxx8/:param",
                    Some(vec![("param", "78k")]),
                ),
                req(
                    "/get/abc/123abc/xxx8/1234xxxd",
                    "/get/abc/123abc/xxx8/:param",
                    Some(vec![("param", "1234xxxd")]),
                ),
                req(
                    "/get/abc/123abc/xxx8/1234/ffas",
                    "/get/abc/123abc/xxx8/1234/ffas",
                    Some(vec![]),
                ),
                req(
                    "/get/abc/123abc/xxx8/1234/f",
                    "/get/abc/123abc/xxx8/1234/:param",
                    Some(vec![("param", "f")]),
                ),
                req(
                    "/get/abc/123abc/xxx8/1234/ffa",
                    "/get/abc/123abc/xxx8/1234/:param",
                    Some(vec![("param", "ffa")]),
                ),
                req(
                    "/get/abc/123abc/xxx8/1234/kka",
                    "/get/abc/123abc/xxx8/1234/:param",
                    Some(vec![("param", "kka")]),
                ),
                req(
                    "/get/abc/123abc/xxx8/1234/ffas321",
                    "/get/abc/123abc/xxx8/1234/:param",
                    Some(vec![("param", "ffas321")]),
                ),
                req(
                    "/get/abc/123abc/xxx8/1234/kkdd/12c",
                    "/get/abc/123abc/xxx8/1234/kkdd/12c",
                    Some(vec![]),
                ),
                req(
                    "/get/abc/123abc/xxx8/1234/kkdd/1",
                    "/get/abc/123abc/xxx8/1234/kkdd/:param",
                    Some(vec![("param", "1")]),
                ),
                req(
                    "/get/abc/123abc/xxx8/1234/kkdd/12",
                    "/get/abc/123abc/xxx8/1234/kkdd/:param",
                    Some(vec![("param", "12")]),
                ),
                req(
                    "/get/abc/123abc/xxx8/1234/kkdd/12b",
                    "/get/abc/123abc/xxx8/1234/kkdd/:param",
                    Some(vec![("param", "12b")]),
                ),
                req(
                    "/get/abc/123abc/xxx8/1234/kkdd/34",
                    "/get/abc/123abc/xxx8/1234/kkdd/:param",
                    Some(vec![("param", "34")]),
                ),
                req(
                    "/get/abc/123abc/xxx8/1234/kkdd/12c2e3",
                    "/get/abc/123abc/xxx8/1234/kkdd/:param",
                    Some(vec![("param", "12c2e3")]),
                ),
                req(
                    "/get/abc/12/test",
                    "/get/abc/:param/test",
                    Some(vec![("param", "12")]),
                ),
                req(
                    "/get/abc/123abdd/test",
                    "/get/abc/:param/test",
                    Some(vec![("param", "123abdd")]),
                ),
                req(
                    "/get/abc/123abdddf/test",
                    "/get/abc/:param/test",
                    Some(vec![("param", "123abdddf")]),
                ),
                req(
                    "/get/abc/123ab/test",
                    "/get/abc/:param/test",
                    Some(vec![("param", "123ab")]),
                ),
                req(
                    "/get/abc/123abgg/test",
                    "/get/abc/:param/test",
                    Some(vec![("param", "123abgg")]),
                ),
                req(
                    "/get/abc/123abff/test",
                    "/get/abc/:param/test",
                    Some(vec![("param", "123abff")]),
                ),
                req(
                    "/get/abc/123abffff/test",
                    "/get/abc/:param/test",
                    Some(vec![("param", "123abffff")]),
                ),
                req(
                    "/get/abc/123abd/test",
                    "/get/abc/123abd/:param",
                    Some(vec![("param", "test")]),
                ),
                req(
                    "/get/abc/123abddd/test",
                    "/get/abc/123abddd/:param",
                    Some(vec![("param", "test")]),
                ),
                req(
                    "/get/abc/123/test22",
                    "/get/abc/123/:param",
                    Some(vec![("param", "test22")]),
                ),
                req(
                    "/get/abc/123abg/test",
                    "/get/abc/123abg/:param",
                    Some(vec![("param", "test")]),
                ),
                req(
                    "/get/abc/123abf/testss",
                    "/get/abc/123abf/:param",
                    Some(vec![("param", "testss")]),
                ),
                req(
                    "/get/abc/123abfff/te",
                    "/get/abc/123abfff/:param",
                    Some(vec![("param", "te")]),
                ),
            ],
        );

        check_priorities(&mut tree);
    }

    type TestRoute = (&'static str, bool);

    fn test_routes(routes: Vec<TestRoute>) {
        let mut tree = Node::new();

        for route in routes {
            let res = tree.insert(route.0, ());

            if route.1 {
                if res.is_ok() {
                    panic!("no panic for conflicting route '{}'", route.1);
                }
            } else if res.is_err() {
                panic!("unexpected panic for route '{}': {:?}", route.0, res);
            }
        }
    }

    #[test]
    fn test_tree_wildcard_conflict() {
        let routes = vec![
            ("/cmd/:tool/:sub", false),
            ("/cmd/vet", false),
            ("/foo/bar", false),
            ("/foo/:name", false),
            ("/foo/:names", true),
            ("/cmd/*path", true),
            ("/cmd/:badvar", true),
            ("/cmd/:tool/names", false),
            ("/cmd/:tool/:badsub/details", true),
            ("/src/*filepath", false),
            ("/src/:file", true),
            ("/src/static.json", true),
            ("/src/*filepathx", true),
            ("/src/", true),
            ("/src/foo/bar", true),
            ("/src1/", false),
            ("/src1/*filepath", true),
            ("/src2*filepath", true),
            ("/src2/*filepath", false),
            ("/search/:query", false),
            ("/search/valid", false),
            ("/user_:name", false),
            ("/user_x", false),
            ("/user_:name", true),
            ("/id:id", false),
            ("/id/:id", false),
        ];

        test_routes(routes);
    }

    #[test]
    fn test_catchall_after_slash() {
        let routes = vec![("/non-leading-*catchall", true)];

        test_routes(routes);
    }

    #[test]
    fn test_tree_child_conflict() {
        let routes = vec![
            ("/cmd/vet", false),
            ("/cmd/:tool", false),
            ("/cmd/:tool/:sub", false),
            ("/cmd/:tool/misc", false),
            ("/cmd/:tool/:othersub", true),
            ("/src/AUTHORS", false),
            ("/src/*filepath", true),
            ("/user_x", false),
            ("/user_:name", false),
            ("/id/:id", false),
            ("/id:id", false),
            ("/:id", false),
            ("/*filepath", true),
        ];

        test_routes(routes);
    }

    #[test]
    fn test_tree_duplicate_path() {
        let mut tree = Node::new();

        let routes = vec![
            "/",
            "/doc/",
            "/src/*filepath",
            "/search/:query",
            "/user_:name",
        ];

        for route in routes {
            tree.insert(route, route.to_owned()).unwrap();
            let res = tree.insert(route, route.to_owned());
            assert_eq!(res, Err(InsertError::Conflict { with: route.into() }));
        }

        check_requests(
            &mut tree,
            vec![
                req("/", "/", Some(vec![])),
                req("/doc/", "/doc/", Some(vec![])),
                req(
                    "/src/some/file.png",
                    "/src/*filepath",
                    Some(vec![("filepath", "/some/file.png")]),
                ),
                req(
                    "/search/someth!ng+in+ünìcodé",
                    "/search/:query",
                    Some(vec![("query", "someth!ng+in+ünìcodé")]),
                ),
                req(
                    "/user_rustacean",
                    "/user_:name",
                    Some(vec![("name", "rustacean")]),
                ),
            ],
        );
    }

    #[test]
    fn test_empty_wildcard_name() {
        let mut tree = Node::new();
        let routes = vec!["/user:", "/user:/", "/cmd/:/", "/src/*"];

        for route in routes {
            assert_eq!(
                tree.insert(route, route.to_owned()),
                Err(InsertError::UnnamedParam)
            );
        }
    }

    #[test]
    fn test_tree_catch_all_conflict() {
        let routes = vec![
            ("/src/*filepath/x", true),
            ("/src2/", false),
            ("/src2/*filepath/x", true),
        ];

        test_routes(routes);
    }

    #[test]
    fn test_tree_catch_all_conflict_root() {
        let routes = vec![("/", false), ("/*filepath", true)];

        test_routes(routes);
    }

    #[test]
    fn test_tree_double_wildcard() {
        let routes = vec!["/:foo:bar", "/:foo:bar/", "/:foo*bar"];

        for route in routes {
            let mut tree = Node::new();
            let res = tree.insert(route, route.to_owned());
            assert_eq!(res, Err(InsertError::TooManyParams));
        }
    }

    #[test]
    fn test_tree_trailing_slash_redirect() {
        let mut tree = Node::new();
        let routes = vec![
            "/hi",
            "/b/",
            "/search/:query",
            "/cmd/:tool/",
            "/src/*filepath",
            "/x",
            "/x/y",
            "/y/",
            "/y/z",
            "/0/:id",
            "/0/:id/1",
            "/1/:id/",
            "/1/:id/2",
            "/aa",
            "/a/",
            "/admin",
            "/admin/:category",
            "/admin/:category/:page",
            "/doc",
            "/doc/go_faq.html",
            "/doc/go1.html",
            "/no/a",
            "/no/b",
            "/api/hello/:name",
        ];

        for route in routes {
            let res = tree.insert(route, route.to_owned());

            if res.is_err() {
                panic!("panic inserting route '{}': {:?}", route, res);
            }
        }

        let tsr_routes = vec![
            "/hi/",
            "/b",
            "/search/rustacean/",
            "/cmd/vet",
            "/src",
            "/x/",
            "/y",
            "/0/go/",
            "/1/go",
            "/a",
            "/admin/",
            "/admin/config/",
            "/admin/config/permissions/",
            "/doc/",
        ];

        for route in tsr_routes {
            let res = tree.at(route);

            match res {
                Ok(_) => {
                    panic!("non-nil value for TSR route '{}'", route);
                }
                Err(err) => {
                    if !err.tsr() {
                        panic!("expected TSR recommendation for route '{}'", route);
                    }
                }
            }
        }

        let no_tsr_routes = vec!["/", "/no", "/no/", "/_", "/_/", "/api/world/abc"];

        for route in no_tsr_routes {
            let res = tree.at(route);

            match res {
                Ok(_) => {
                    panic!("non-nil value for TSR route '{}'", route);
                }
                Err(err) => {
                    if err.tsr() {
                        panic!("expected no TSR recommendation for route '{}'", route);
                    }
                }
            }
        }
    }

    #[test]
    fn test_tree_root_trailing_slash_redirect() {
        let mut tree = Node::new();

        tree.insert("/:test", "/:test".to_owned()).unwrap();

        let res = tree.at("/");

        match res {
            Ok(_) => {
                panic!("non-nil value for route '/'");
            }
            Err(err) => {
                if err.tsr() {
                    panic!("expected no TSR recommendation for route '/'");
                }
            }
        }
    }

    #[test]
    fn test_tree_find_case_insensitive_path() {
        let mut tree = Node::new();

        let routes = vec![
            "/hi",
            "/b/",
            "/ABC/",
            "/search/:query",
            "/cmd/:tool/",
            "/src/*filepath",
            "/x",
            "/x/y",
            "/y/",
            "/y/z",
            "/0/:id",
            "/0/:id/1",
            "/1/:id/",
            "/1/:id/2",
            "/aa",
            "/a/",
            "/doc",
            "/doc/go_faq.html",
            "/doc/go1.html",
            "/doc/go/away",
            "/no/a",
            "/no/b",
            "/Π",
            "/u/apfêl/",
            "/u/äpfêl/",
            "/u/öpfêl",
            "/v/Äpfêl/",
            "/v/Öpfêl",
            "/w/♬",
            "/w/♭/",
            "/w/𠜎",
            "/w/𠜏/",
            "/loooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooong",
    ];

        for route in &routes {
            tree.insert(*route, route.to_owned()).unwrap();
        }

        // Check out == in for all registered routes
        // With fixTrailingSlash = true
        for route in &routes {
            let out = tree.path_ignore_case(route, true);
            match out {
                None => panic!("Route '{}' not found!", route),
                Some(out) => {
                    if out != *route {
                        panic!("Wrong result for route '{}': {}", route, out);
                    }
                }
            };
        }

        // With fixTrailingSlash = false
        for route in &routes {
            let out = tree.path_ignore_case(route, false);
            match out {
                None => panic!("Route '{}' not found!", route),
                Some(out) => {
                    if out != *route {
                        panic!("Wrong result for route '{}': {}", route, out);
                    }
                }
            };
        }

        let tests = vec![
            ("/HI", "/hi", false),
            ("/HI/", "/hi", true),
            ("/B", "/b/", true),
            ("/B/", "/b/", false),
            ("/abc", "/ABC/", true),
            ("/abc/", "/ABC/", false),
            ("/aBc", "/ABC/", true),
            ("/aBc/", "/ABC/", false),
            ("/abC", "/ABC/", true),
            ("/abC/", "/ABC/", false),
            ("/SEARCH/QUERY", "/search/QUERY", false),
            ("/SEARCH/QUERY/", "/search/QUERY", true),
            ("/CMD/TOOL/", "/cmd/TOOL/", false),
            ("/CMD/TOOL", "/cmd/TOOL/", true),
            ("/SRC/FILE/PATH", "/src/FILE/PATH", false),
            ("/x/Y", "/x/y", false),
            ("/x/Y/", "/x/y", true),
            ("/X/y", "/x/y", false),
            ("/X/y/", "/x/y", true),
            ("/X/Y", "/x/y", false),
            ("/X/Y/", "/x/y", true),
            ("/Y/", "/y/", false),
            ("/Y", "/y/", true),
            ("/Y/z", "/y/z", false),
            ("/Y/z/", "/y/z", true),
            ("/Y/Z", "/y/z", false),
            ("/Y/Z/", "/y/z", true),
            ("/y/Z", "/y/z", false),
            ("/y/Z/", "/y/z", true),
            ("/Aa", "/aa", false),
            ("/Aa/", "/aa", true),
            ("/AA", "/aa", false),
            ("/AA/", "/aa", true),
            ("/aA", "/aa", false),
            ("/aA/", "/aa", true),
            ("/A/", "/a/", false),
            ("/A", "/a/", true),
            ("/DOC", "/doc", false),
            ("/DOC/", "/doc", true),
            ("/NO", "", true),
            ("/DOC/GO", "", true),
            // [TODO] unicode vs ascii case sensitivity
            // ("/π", "/Π", false),
            // ("/π/", "/Π", true),
            // ("/u/ÄPFÊL/", "/u/äpfêl/", false),
            // ("/u/ÄPFÊL", "/u/äpfêl/", true),
            // ("/u/ÖPFÊL/", "/u/öpfêl", true),
            // ("/u/ÖPFÊL", "/u/öpfêl", false),
            // ("/v/äpfêL/", "/v/Äpfêl/", false),
            // ("/v/äpfêL", "/v/Äpfêl/", true),
            // ("/v/öpfêL/", "/v/Öpfêl", true),
            // ("/v/öpfêL", "/v/Öpfêl", false),
            ("/w/♬/", "/w/♬", true),
            ("/w/♭", "/w/♭/", true),
            ("/w/𠜎/", "/w/𠜎", true),
            ("/w/𠜏", "/w/𠜏/", true),
            (
                "/lOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOng/",
                "/loooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooong",
                true
            ),
    ];

        struct Test {
            inn: &'static str,
            out: &'static str,
            slash: bool,
        }

        let tests: Vec<Test> = tests
            .into_iter()
            .map(|test| Test {
                inn: test.0,
                out: test.1,
                slash: test.2,
            })
            .collect();

        // With fixTrailingSlash = true
        for test in &tests {
            let res = tree.path_ignore_case(test.inn, true).unwrap_or_default();
            if res != test.out {
                panic!("Wrong result for route '{}': {}", res, test.out);
            }
        }

        // without fix trailing slash = false
        for test in &tests {
            let res = tree.path_ignore_case(test.inn, false);
            match res {
                None => (),
                Some(res) => {
                    if test.slash {
                        // test needs a trailing slash fix. It must not be found!
                        panic!(
                            "Found without fix_trailing_slash: {}; got {}",
                            test.inn, res
                        );
                    }
                    if res != test.out {
                        panic!("Wrong result for route '{}': {}", res, test.out);
                    }
                }
            };
        }
    }

    #[test]
    fn test_tree_wildcard_conflict_ex() {
        let conflicts = vec![
            ("/who/are/foo", "/who/are/*you"),
            ("/who/are/foo/", "/who/are/*you"),
            ("/who/are/foo/bar", "/who/are/*you"),
            ("/con:nection", "/con:tact"),
            ("/whose/:users/:user", "/whose/:users/:name"),
        ];

        for conflict in conflicts {
            let mut tree = Node::new();

            let routes = vec![
                "/con:tact",
                "/who/are/*you",
                "/who/foo/hello",
                "/whose/:users/:name",
            ];

            for route in routes {
                tree.insert(route, route.to_owned()).unwrap();
            }

            let res = tree.insert(conflict.0, conflict.0.to_owned());
            assert_eq!(
                res,
                Err(InsertError::Conflict {
                    with: conflict.1.into()
                })
            );
        }
    }

    #[test]
    fn malformed_path() {
        let mut tree = Node::new();
        assert_eq!(tree.insert("*x", 1), Err(InsertError::MalformedPath));
    }
}
