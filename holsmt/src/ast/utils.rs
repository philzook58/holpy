/// creating new name.
pub fn get_variant_name(nm: &str, prevs: &Vec<String>) -> String {
    if !prevs.contains(&nm.to_string()) {
        return nm.to_string();
    }

    let mut i = 1;
    loop {
        let new_name = format!("{}{}", nm, i);
        if !prevs.contains(&new_name) {
            return new_name;
        }
        i += 1;
    }
}

/// creating new names.
pub fn get_variant_names(nms: &Vec<String>, prevs: &Vec<String>) -> Vec<String> {
    if nms.is_empty() {
        return Vec::new();
    }
    let nm = get_variant_name(&nms[0], prevs);
    let mut new_prevs = prevs.clone();
    new_prevs.push(nm.clone());
    let mut nm_rest = get_variant_names(&nms[1..].to_vec(), &new_prevs);
    nm_rest.insert(0, nm);
    nm_rest
}
