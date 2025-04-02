use std::collections::VecDeque;

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

impl Iterator for CharStream {
    type Item = char;
    fn next(&mut self) -> Option<char> {
        if self.idx >= self.chars.len() {
            return None;
        }
        let result = self.chars[self.idx];
        self.idx += 1;
        Some(result)    
    }
}

pub struct BufferedStream<T> {
    stream: Box<dyn Iterator<Item=T>>,
    buffer: VecDeque<T>,
}

impl<T> BufferedStream<T> {
    pub fn new(stream: Box<dyn Iterator<Item=T>>) -> BufferedStream<T> {
        BufferedStream { 
            stream,
            buffer: VecDeque::new(),
        }
    }
    
    pub fn advance(&mut self) -> Option<T> {
        if !self.buffer.is_empty() {
            return self.buffer.pop_front();
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
            if let Some(item) = self.stream.next() {
                self.buffer.push_back(item);
            } else {
                break;
            }
        }
        self.buffer.iter().take(n).collect()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    
    fn create_buffered_stream(text: &str) -> BufferedStream<char> {
        let char_stream = CharStream::new(text.to_string());
        BufferedStream::new(Box::new(char_stream))
    }
    
    #[test]
    fn test_buffered_stream_advance() {
        let mut stream = create_buffered_stream("hello");
        assert_eq!(stream.advance(), Some('h'));
        assert_eq!(stream.advance(), Some('e'));
        assert_eq!(stream.advance(), Some('l'));
        assert_eq!(stream.advance(), Some('l'));
        assert_eq!(stream.advance(), Some('o'));
        assert_eq!(stream.advance(), None);
    }

    #[test]
    fn test_buffered_stream_peek() {
        let mut stream = create_buffered_stream("hello");
        assert_eq!(stream.peek(), Some(&'h'));
        assert_eq!(stream.advance(), Some('h'));
        assert_eq!(stream.peek(), Some(&'e'));
        assert_eq!(stream.peek(), Some(&'e'));
        assert_eq!(stream.advance(), Some('e'));
    }

    #[test]
    fn test_buffered_stream_peek_many() {
        let mut stream = create_buffered_stream("hello");
        assert_eq!(stream.advance(), Some('h'));
        assert_eq!(stream.peek_many(5), vec![&'e', &'l', &'l', &'o']);
    }
}
