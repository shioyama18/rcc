static mut COUNTER: u32 = 0;

pub fn add_suffix(word: &str, suffix: &str) -> String {
    let mut word = word.to_string();
    word.push_str(suffix);
    word
}

pub fn unique_suffix() -> String {
    let count: String = generate_suffix();
    let mut suffix = "_".to_string();
    suffix.push_str(&count);

    suffix
}

fn generate_suffix() -> String {
    let n: u32;

    unsafe {
        n = COUNTER;
        COUNTER += 1;
    }

    n.to_string()
}
