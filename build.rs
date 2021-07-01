fn main() {
    println!("cargo:rerun-if-changed=src/hello.c");
    cc::Build::new()
        .file("src/backends/aarch64/m1_jit.c")
        .pic(true)
        .compile("m1_jit");
}
