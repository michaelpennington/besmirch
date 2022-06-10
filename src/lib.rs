//! An interacion between physical and aural art
//! Buyer beware! Use at your own risk!
//! Wee wee make fun with artifice and duplicity?!
//!
//! We got a special guest for your initiation...
//! - HO5000 🦄: Hello, I'm horsebot5000 🎠 here to guide you on a tour of besmirch! 🌈
//! - HB5000 🦄: And don't forget Rusty, our official Puerto Rican sidekick/mascot
//! - Rusty🐶: Woof! Woof!

#![cfg_attr(HAS_BACKTRACE, feature(backtrace, backtrace_frames))]
#![warn(
    missing_docs,
    future_incompatible,
    nonstandard_style,
    missing_debug_implementations,
    unconditional_recursion
)]

mod midi;

#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {
        let result = 2 + 2;
        assert_eq!(result, 4);
    }
}
