extern crate winres;

fn main() {
    let mut res = winres::WindowsResource::new();
    res.set_icon("kbd_evs.ico");
    res.compile().unwrap();
}
