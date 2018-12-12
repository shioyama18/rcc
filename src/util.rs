use rand::{thread_rng, Rng};
use rand::distributions::Alphanumeric;

/// Function to apply a unique suffix to identifier
/// TODO: Keep track of used identifier with set
pub fn unique(word: &str) -> String {
    let mut word = word.to_string();
    let rand_suffix: String = thread_rng()
        .sample_iter(&Alphanumeric)
        .take(10)
        .collect();

    word.push_str(&rand_suffix);
    word
}
