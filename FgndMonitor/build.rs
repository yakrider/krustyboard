extern crate winres;

fn main() {
    let mut res = winres::WindowsResource::new();
    res.set_icon("fg_mon.ico");
    res.compile().unwrap();
}
