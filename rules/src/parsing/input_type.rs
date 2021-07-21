use std::{
    fmt::{Debug, Display},
    ops::{Deref, RangeBounds},
    str::FromStr,
};

use arcstr::{ArcStr, Substr};
use nom::{
    AsBytes, Compare, ExtendInto, FindSubstring, FindToken, InputIter, InputLength, InputTake,
    InputTakeAtPosition, Needed, Offset, ParseTo, Slice,
};

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Input(pub Substr);

impl Input {
    pub fn substr<R: RangeBounds<usize>>(&self, range: R) -> Self {
        let start = match range.start_bound() {
            std::ops::Bound::Included(x) => *x,
            std::ops::Bound::Excluded(x) => *x + 1,
            std::ops::Bound::Unbounded => 0,
        };
        let end = match range.end_bound() {
            std::ops::Bound::Included(x) => *x + 1,
            std::ops::Bound::Excluded(x) => *x,
            std::ops::Bound::Unbounded => self.0.len(),
        };

        // in the case of an empty substr, we want to create a blank reference
        // to a new str, with the empty range
        if start == end && start <= self.0.len() {
            let idx = self.0.range().start + start;
            Input(unsafe { Substr::from_parts_unchecked(self.0.parent().clone(), idx..idx) })
        } else {
            Input(self.0.substr(range))
        }
    }
}

impl Display for Input {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl Deref for Input {
    type Target = Substr;
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl AsBytes for Input {
    fn as_bytes(&self) -> &[u8] {
        self.0.as_bytes()
    }
}

impl Compare<&str> for Input {
    fn compare(&self, t: &str) -> nom::CompareResult {
        self.as_str().compare(t)
    }

    fn compare_no_case(&self, t: &str) -> nom::CompareResult {
        self.as_str().compare_no_case(t)
    }
}

impl ExtendInto for Input {
    type Item = char;
    type Extender = String;

    fn new_builder(&self) -> Self::Extender {
        self.as_str().new_builder()
    }

    fn extend_into(&self, acc: &mut Self::Extender) {
        self.as_str().extend_into(acc)
    }
}

impl FindSubstring<&str> for Input {
    fn find_substring(&self, substr: &str) -> Option<usize> {
        self.as_str().find_substring(substr)
    }
}
impl FindToken<u8> for Input {
    fn find_token(&self, token: u8) -> bool {
        self.as_str().find_token(token)
    }
}

impl FindToken<char> for Input {
    fn find_token(&self, token: char) -> bool {
        self.as_str().find_token(token)
    }
}

impl FindToken<&u8> for Input {
    fn find_token(&self, token: &u8) -> bool {
        self.as_str().find_token(token)
    }
}

pub struct CharIndices {
    inner: Substr,
    current: usize,
}
impl Iterator for CharIndices {
    type Item = (usize, char);

    fn next(&mut self) -> Option<Self::Item> {
        let ret = self.inner.char_indices().nth(self.current);
        self.current += 1;
        ret
    }
}

pub struct Chars {
    inner: Substr,
    current: usize,
}

impl Iterator for Chars {
    type Item = char;

    fn next(&mut self) -> Option<Self::Item> {
        let ret = self.inner.chars().nth(self.current);
        self.current += 1;
        ret
    }
}

impl InputIter for Input {
    type Item = char;

    type Iter = CharIndices;

    type IterElem = Chars;

    fn iter_indices(&self) -> Self::Iter {
        CharIndices {
            inner: self.0.clone(),
            current: 0,
        }
    }

    fn iter_elements(&self) -> Self::IterElem {
        Chars {
            inner: self.0.clone(),
            current: 0,
        }
    }

    fn position<P>(&self, predicate: P) -> Option<usize>
    where
        P: Fn(Self::Item) -> bool,
    {
        self.as_str().position(predicate)
    }

    fn slice_index(&self, count: usize) -> Result<usize, nom::Needed> {
        self.as_str().slice_index(count)
    }
}

impl InputLength for Input {
    fn input_len(&self) -> usize {
        self.as_str().input_len()
    }
}

impl InputTake for Input {
    fn take(&self, count: usize) -> Self {
        self.substr(..count)
    }

    fn take_split(&self, count: usize) -> (Self, Self) {
        (self.substr(count..), self.substr(..count))
    }
}

impl InputTakeAtPosition for Input {
    type Item = char;

    fn split_at_position<P, E: nom::error::ParseError<Self>>(
        &self,
        predicate: P,
    ) -> nom::IResult<Self, Self, E>
    where
        P: Fn(Self::Item) -> bool,
    {
        match self.0.as_str().find(predicate) {
            Some(i) => Ok(self.take_split(i)),
            None => Err(nom::Err::Incomplete(Needed::new(1))),
        }
    }

    fn split_at_position1<P, E: nom::error::ParseError<Self>>(
        &self,
        predicate: P,
        e: nom::error::ErrorKind,
    ) -> nom::IResult<Self, Self, E>
    where
        P: Fn(Self::Item) -> bool,
    {
        match self.0.as_str().find(predicate) {
            Some(0) => Err(nom::Err::Error(E::from_error_kind(self.clone(), e))),
            Some(i) => Ok(self.take_split(i)),
            None => Err(nom::Err::Incomplete(Needed::new(1))),
        }
    }

    fn split_at_position_complete<P, E: nom::error::ParseError<Self>>(
        &self,
        predicate: P,
    ) -> nom::IResult<Self, Self, E>
    where
        P: Fn(Self::Item) -> bool,
    {
        match self.0.as_str().find(predicate) {
            Some(i) => Ok(self.take_split(i)),
            None => Ok(self.take_split(self.input_len())),
        }
    }

    fn split_at_position1_complete<P, E: nom::error::ParseError<Self>>(
        &self,
        predicate: P,
        e: nom::error::ErrorKind,
    ) -> nom::IResult<Self, Self, E>
    where
        P: Fn(Self::Item) -> bool,
    {
        match self.find(predicate) {
            Some(0) => Err(nom::Err::Error(E::from_error_kind(self.clone(), e))),
            Some(i) => Ok(self.take_split(i)),
            None => {
                if self.is_empty() {
                    Err(nom::Err::Error(E::from_error_kind(self.clone(), e)))
                } else {
                    Ok(self.take_split(self.input_len()))
                }
            }
        }
    }
}

impl Offset for Input {
    fn offset(&self, second: &Self) -> usize {
        second.0.range().start - self.0.range().start
    }
}

impl<R: FromStr> ParseTo<R> for Input {
    fn parse_to(&self) -> std::option::Option<R> {
        self.as_str().parse_to()
    }
}

impl<T> Slice<T> for Input
where
    T: RangeBounds<usize> + Debug,
{
    fn slice(&self, range: T) -> Self {
        self.substr(range)
    }
}

#[cfg(test)]
mod test {

    use nom::{bytes::complete::tag, multi::many1};

    use crate::parsing::{input, IResult, Span};

    use super::*;

    #[test]
    fn test_substr() {
        let input = Input(Substr::full(ArcStr::from("012345")));

        assert_eq!("012345", input.substr(..).as_str());
        assert_eq!("12345", input.substr(1..).as_str());
        assert_eq!("01234", input.substr(..5).as_str());
        assert_eq!("1234", input.substr(1..5).as_str());
        assert_eq!("", input.substr(1..1).as_str());
        assert_eq!(1..1, input.substr(1..1).range());
    }
}
