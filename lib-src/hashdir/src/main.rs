extern crate base16;
extern crate ignore;
extern crate sha2;
use sha2::Digest;

fn process_path<'a>(path: &str) -> String {
    let path = std::path::Path::new("../src").join(path);
    let buf = std::fs::canonicalize(path).unwrap();
    buf.to_string_lossy().to_string()
}

fn main() {
    let cflags = std::env::var_os("EMACS_CFLAGS").unwrap();
    let cflags_str = cflags.to_string_lossy();
    let mut processed_paths: Vec<String> = Vec::new();
    // we're running clang from a different directory, so we have to adjust any relative include paths
    for path in cflags_str.split(' ') {
        if path.starts_with("-I") {
            processed_paths.push(process_path(path.get(2..).unwrap()))
        };
    }
    if cfg!(target_os = "windows") {
        processed_paths.push(process_path("../nt/inc"));
    }
    let mut hash_input: String = String::with_capacity(1024);
    for path in processed_paths {
        for entry in ignore::Walk::new(path)
            .into_iter()
            .filter(|e| {
                let entry = e.as_ref().unwrap();
                let filename = entry.file_name().to_string_lossy();
                entry.file_type().unwrap().is_dir() || filename.ends_with(".c")
                    || filename.ends_with(".h") || filename.ends_with(".rs")
            })
            .filter_map(|e| e.ok())
        {
            if entry.file_type().unwrap().is_file() {
                hash_input.push_str(&entry.path().to_string_lossy().to_string());
                let metadata = entry.metadata().ok().unwrap();
                let modified = metadata.modified().ok().unwrap();
                // this is possibly the dumbest string encoding of a timestep ever, but it hardly matters.
                hash_input.push_str(&format!("{:?}", modified));
            }
        }
    }
    println!(
        "{}",
        base16::encode_lower(&sha2::Sha256::digest(hash_input.as_bytes()))
    );
}
