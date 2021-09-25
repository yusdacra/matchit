use std::iter;
use std::mem;
use std::slice;

/// A single URL parameter, consisting of a key and a value.
#[derive(Debug, PartialEq, Eq, Ord, PartialOrd, Default, Copy, Clone)]
struct Param<'a> {
    key: &'a [u8],
    value: &'a [u8],
}

impl<'a> Param<'a> {
    // this could be from_utf8_unchecked, but we'll keep this safe for now
    fn key_str(&self) -> &'a str {
        std::str::from_utf8(self.key).unwrap()
    }

    fn value_str(&self) -> &'a str {
        std::str::from_utf8(self.value).unwrap()
    }
}

/// A list of parameters returned by a route match.
///
/// ```rust
/// # fn main() -> Result<(), Box<dyn std::error::Error>> {
/// # let mut matcher = matchit::Node::new();
/// # matcher.insert("/users/:id", true).unwrap();
/// let matched = matcher.at("/users/1")?;
///
/// // you can iterate through the keys and values
/// for (key, value) in matched.params.iter() {
///     println!("key: {}, value: {}", key, value);
/// }
///
/// // or get a specific value by key
/// let id = matched.params.get("id");
/// assert_eq!(id, Some("1"));
/// # Ok(())
/// # }
/// ```
#[derive(Debug, PartialEq, Eq, Ord, PartialOrd, Clone)]
pub struct Params<'a> {
    kind: ParamsKind<'a>,
}

// most routes have 1-3 dynamic parameters, so we can avoid a heap allocation in common cases.
const SMALL: usize = 3;

#[derive(Debug, PartialEq, Eq, Ord, PartialOrd, Clone)]
enum ParamsKind<'a> {
    None,
    Small([Param<'a>; SMALL], usize),
    Large(Vec<Param<'a>>),
}

impl<'a> Params<'a> {
    pub(crate) fn new() -> Self {
        let kind = ParamsKind::None;
        Self { kind }
    }

    /// Returns the value of the first parameter registered under the given key.
    pub fn get(&self, key: impl AsRef<str>) -> Option<&'a str> {
        match &self.kind {
            ParamsKind::None => None,
            ParamsKind::Small(arr, len) => arr
                .iter()
                .take(*len)
                .find(|param| param.key == key.as_ref().as_bytes())
                .map(|param| param.value_str()),
            ParamsKind::Large(vec) => vec
                .iter()
                .find(|param| param.key == key.as_ref().as_bytes())
                .map(|param| param.value_str()),
        }
    }

    /// Returns an iterator over the parameters in the list.
    pub fn iter(&self) -> ParamsIter<'_, 'a> {
        ParamsIter::new(self)
    }

    /// Returns `true` if there are no parameters in the list.
    pub fn is_empty(&self) -> bool {
        match &self.kind {
            ParamsKind::None => true,
            ParamsKind::Small(_, len) => *len == 0,
            ParamsKind::Large(vec) => vec.is_empty(),
        }
    }

    /// Inserts a key value parameter pair into the list.
    pub(crate) fn push(&mut self, key: &'a [u8], value: &'a [u8]) {
        #[cold]
        fn drain_to_vec<T: Default>(len: usize, elem: T, arr: &mut [T; SMALL]) -> Vec<T> {
            let mut vec = Vec::with_capacity(len + 1);
            vec.extend(arr.iter_mut().map(mem::take));
            vec.push(elem);
            vec
        }

        let param = Param { key, value };
        match &mut self.kind {
            ParamsKind::None => {
                self.kind = ParamsKind::Small([param, Default::default(), Default::default()], 1);
            }
            ParamsKind::Small(arr, len) => {
                if *len == SMALL {
                    self.kind = ParamsKind::Large(drain_to_vec(*len, param, arr));
                    return;
                }
                arr[*len] = param;
                *len += 1;
            }
            ParamsKind::Large(vec) => vec.push(param),
        }
    }
}

/// An iterator over the keys and values of a route's [parameters](crate::Params).
pub struct ParamsIter<'ps, 'a> {
    kind: ParamsIterKind<'ps, 'a>,
}

impl<'ps, 'a> ParamsIter<'ps, 'a> {
    fn new(params: &'ps Params<'a>) -> Self {
        let kind = match &params.kind {
            ParamsKind::None => ParamsIterKind::None,
            ParamsKind::Small(arr, len) => ParamsIterKind::Small(arr.iter().take(*len)),
            ParamsKind::Large(vec) => ParamsIterKind::Large(vec.iter()),
        };
        Self { kind }
    }
}

enum ParamsIterKind<'ps, 'a> {
    None,
    Small(iter::Take<slice::Iter<'ps, Param<'a>>>),
    Large(slice::Iter<'ps, Param<'a>>),
}

impl<'ps, 'a> Iterator for ParamsIter<'ps, 'a> {
    type Item = (&'a str, &'a str);

    fn next(&mut self) -> Option<Self::Item> {
        match self.kind {
            ParamsIterKind::None => None,
            ParamsIterKind::Small(ref mut iter) => {
                iter.next().map(|p| (p.key_str(), p.value_str()))
            }
            ParamsIterKind::Large(ref mut iter) => {
                iter.next().map(|p| (p.key_str(), p.value_str()))
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn no_alloc() {
        assert_eq!(Params::new().kind, ParamsKind::None);
    }

    #[test]
    fn heap_alloc() {
        let vec = vec![
            ("hello", "hello"),
            ("world", "world"),
            ("foo", "foo"),
            ("bar", "bar"),
            ("baz", "baz"),
        ];

        let mut params = Params::new();
        for (key, value) in vec.clone() {
            params.push(key.as_bytes(), value.as_bytes());
            assert_eq!(params.get(key), Some(value));
        }

        match params.kind {
            ParamsKind::Large(..) => {}
            _ => panic!(),
        }

        assert!(params.iter().eq(vec.clone()));
    }

    #[test]
    fn stack_alloc() {
        let vec = vec![("hello", "hello"), ("world", "world"), ("baz", "baz")];

        let mut params = Params::new();
        for (key, value) in vec.clone() {
            params.push(key.as_bytes(), value.as_bytes());
            assert_eq!(params.get(key), Some(value));
        }

        match params.kind {
            ParamsKind::Small(..) => {}
            _ => panic!(),
        }

        assert!(params.iter().eq(vec.clone()));
    }

    #[test]
    fn ignore_array_default() {
        let params = Params::new();
        assert!(params.get("").is_none());
    }
}
