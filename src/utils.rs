pub fn join(vec: &[impl ToString]) -> String {
    vec.iter()
        .map(ToString::to_string)
        .collect::<Vec<_>>()
        .join(" ")
}

pub fn join_lines(vec: &[impl ToString]) -> String {
    vec.iter()
        .map(ToString::to_string)
        .collect::<Vec<_>>()
        .join("\n")
}

pub fn join_sep(vec: &[impl ToString], sep: &str) -> String {
    vec.iter()
        .map(ToString::to_string)
        .collect::<Vec<_>>()
        .join(sep)
}
