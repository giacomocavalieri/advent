import filepath
import global_value
import simplifile

pub fn find_root() -> String {
  global_value.create_with_unique_name("project_root", fn() {
    find_root_loop("./")
  })
}

fn find_root_loop(path: String) -> String {
  case simplifile.is_file(filepath.join(path, "gleam.toml")) {
    Ok(True) -> path
    Ok(False) | Error(_) -> find_root_loop(filepath.join(path, "../"))
  }
}
