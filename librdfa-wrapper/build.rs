#[cfg(not(target_os = "windows"))]
fn main() {
    use std::env;
    use std::path::PathBuf;

    // Tell cargo to look for shared libraries in the specified directory
    println!("cargo:rustc-link-search=/usr/local/lib/");

    // Tell cargo to tell rustc to link the system rdfa
    // shared library.
    println!("cargo:rustc-link-lib=rdfa");

    // The bindgen::Builder is the main entry point
    // to bindgen, and lets you build up options for
    // the resulting bindings.
    let bindings = bindgen::Builder::default()
        .headers([
            "/usr/local/include/rdfa.h",
            "/usr/local/include/rdfa_utils.h",
        ])
        //.clang_arg("-DLIBRDFA_IN_RAPTOR")
        //.clang_arg("-I/usr/include/raptor2/")
        .clang_arg("-I/usr/include/libxml2/")
        // Tell cargo to invalidate the built crate whenever any of the
        // included header files changed.
        .parse_callbacks(Box::new(bindgen::CargoCallbacks::new()))
        // Finish the builder and generate the bindings.
        .generate()
        // Unwrap the Result and panic on failure.
        .expect("Unable to generate bindings");

    // Write the bindings to the $OUT_DIR/bindings.rs file.
    let out_path = PathBuf::from(env::var("OUT_DIR").unwrap());
    bindings
        .write_to_file(out_path.join("bindings.rs"))
        .expect("Couldn't write bindings!");
}

#[cfg(target_os = "windows")]
fn main() {
    // not available
}
