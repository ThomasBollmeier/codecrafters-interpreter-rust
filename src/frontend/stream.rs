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
