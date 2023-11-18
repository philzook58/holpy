use std::path::Path;

fn main() {
    lalrpop::process_root().unwrap();
    let mut config = lalrpop::Configuration::new();
    // config.set_out_dir(".");
    config.always_use_colors();
    config
        .process_file(Path::new("src/parser/parser.lalrpop"))
        .unwrap();
}
