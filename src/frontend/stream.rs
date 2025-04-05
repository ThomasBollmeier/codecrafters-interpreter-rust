use std::collections::VecDeque;

pub trait Stream<T, E> {
    fn next(&mut self) -> Result<Option<T>, E>;
}

pub struct BufferedStream<T, E> {
    stream: Box<dyn Stream<T, E>>,
    buffer: VecDeque<T>,
}

impl<T, E> BufferedStream<T, E> {
    pub fn new(stream: Box<dyn Stream<T, E>>) -> BufferedStream<T, E> {
        BufferedStream {
            stream,
            buffer: VecDeque::new(),
        }
    }

    pub fn advance(&mut self) -> Result<Option<T>, E> {
        if !self.buffer.is_empty() {
            return Ok(self.buffer.pop_front());
        }
        self.stream.next()
    }

    pub fn peek(&mut self) -> Option<&T> {
        let items = self.peek_many(1);
        if !items.is_empty() {
            Some(&items[0])
        } else {
            None
        }
    }

    pub fn peek_many(&mut self, n: usize) -> Vec<&T> {
        while self.buffer.len() < n {
            if let Ok(Some(item)) = self.stream.next() {
                self.buffer.push_back(item);
            } else {
                break;
            }
        }
        self.buffer.iter().take(n).collect()
    }
}

pub struct CharStream {
    chars: Vec<char>,
    idx: usize,
}

impl CharStream {
    pub fn new(text: String) -> CharStream {
        CharStream {
            chars: text.chars().collect(),
            idx: 0,
        }
    }
}

impl Stream<char, ()> for CharStream {
    fn next(&mut self) -> Result<Option<char>, ()> {
        if self.idx >= self.chars.len() {
            return Ok(None);
        }
        let result = self.chars[self.idx];
        self.idx += 1;
        Ok(Some(result))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    
    fn create_buffered_stream(text: &str) -> BufferedStream<char, ()> {
        let char_stream = CharStream::new(text.to_string());
        BufferedStream::new(Box::new(char_stream))
    }
    
    #[test]
    fn test_buffered_stream_advance() {
        let mut stream = create_buffered_stream("hello");
        assert_eq!(stream.advance(), Ok(Some('h')));
        assert_eq!(stream.advance(), Ok(Some('e')));
        assert_eq!(stream.advance(), Ok(Some('l')));
        assert_eq!(stream.advance(), Ok(Some('l')));
        assert_eq!(stream.advance(), Ok(Some('o')));
        assert_eq!(stream.advance(), Ok(None));
    }

    #[test]
    fn test_buffered_stream_peek() {
        let mut stream = create_buffered_stream("hello");
        assert_eq!(stream.peek(), Some(&'h'));
        assert_eq!(stream.advance(), Ok(Some('h')));
        assert_eq!(stream.peek(), Some(&'e'));
        assert_eq!(stream.peek(), Some(&'e'));
        assert_eq!(stream.advance(), Ok(Some('e')));
    }

    #[test]
    fn test_buffered_stream_peek_many() {
        let mut stream = create_buffered_stream("hello");
        assert_eq!(stream.advance(), Ok(Some('h')));
        assert_eq!(stream.peek_many(5), vec![&'e', &'l', &'l', &'o']);
    }
}
