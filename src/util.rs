static mut COUNTER: u32 = 0;

pub fn add_unique_suffix(word: &str) -> String {
    let mut word = word.to_string();
    let suffix: String = generate_suffix();

    word.push_str("_");
    word.push_str(&suffix);
    word
}

fn generate_suffix() -> String {
    let n: u32;

    unsafe {
        n = COUNTER;
        COUNTER += 1;
    }

    n.to_string()
}
