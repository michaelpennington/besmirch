use rustc_version::{version_meta, Channel};

fn main() {
    match version_meta().unwrap().channel {
        Channel::Stable => {
            println!("cargo:rustc-cfg=RUSTC_IS_STABLE")
        }
        Channel::Beta => {
            println!("cargo:rustc-cfg=RUSTC_IS_BETA")
        }
        Channel::Nightly => {
            println!("cargo:rustc-cfg=RUSTC_IS_NIGHTLY");
            println!("cargo:rustc-cfg=HAS_BACKTRACE")
        }
        Channel::Dev => {
            println!("cargo:rustc-cfg=RUSTC_IS_DEV");
            println!("cargo:rustc-cfg=HAS_BACKTRACE")
        }
    }
}
