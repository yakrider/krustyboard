
// slightly unconventional organization here ..
//.. we wanted to keep the main module file inside the module dir, so we're just declaring all our modules as
//.. mostly empty wrappers here, that re-export the _module_name file inside the module folders
//.. (along with any other re-exports we want to do)

// in general, we'll re-export everything from the shadowed module file in the module dir
// then we could selectively re-export to module-top-level from other submodules under it ..
// however, with decent encapsulation, it seems ok so far to just re-export everything toplevel in those sub-modules too
//.. (if we do this (everywhere) we wont have to import any of our high-level mod constructs once we do 'use crate::*')
//.. (hopefully any name-space contention will be minimal and where occurs, avoidable via explicit path specification)



// first our single-file sub-modules
// .. currently we dont have any more of these!



// then our sub-modules with folders (but no main module file) with shadowed module file in the folders (where useful)

/// inputs .. keyboard-keys, mouse-btns/wheel/pointer, and binding/processing them
pub mod inputs {
    // sub-modules in module directory
    pub mod kbd;
    pub mod kbd_codes;
    pub mod kbd_bindings;
    pub mod mouse;
    pub mod mouse_bindings;
    pub mod input_proc;

    // and our (selective or wholesale) sub-module re-exports
    pub use self::kbd::*;
    pub use self::kbd_codes::*;
    pub use self::kbd_bindings::*;
    pub use self::mouse::*;
    pub use self::mouse_bindings::*;
    pub use self::input_proc::*;
}


/// krusty-board state, tracking modifier keys, combo states, key/mod-key/mode-state combo-maps etc
/// the krusty_app and krusty_mouse impl the actual setup the  krusty-configuration application
pub mod krusty {
    // shadowed module file that we'll re-export from here
    mod _krusty;
    pub use self::_krusty::*;

    // other sub-modules in module directory
    pub mod mod_keys;
    pub mod mode_states;
    pub mod combo_maps;
    pub mod krusty_mouse;
    pub mod krusty_app;

    // and our (selective or wholesale) sub-module re-exports
    pub use self::mod_keys::*;
    pub use self::mode_states::*;
    pub use self::combo_maps::*;
    pub use self::krusty_mouse::*;
    pub use self::krusty_app::*;

}


/// bunch of utility helpers for manipulating screen brighness, launching processes, window-dimensions etc
pub mod utils {
    // sub-modules in module directory
    pub mod brightness;
    pub mod process_utils;
    pub mod windows_utils;

    // and our (selective or wholesale) sub-module re-exports
    pub use self::brightness::*;
    pub use self::process_utils::*;
    pub use self::windows_utils::*;
}


// and finally our krustyboard (lib) level re-exports
//.. here again, instead of only re-exporting selective essential constructs, we'll try and re-export heavily first,
//.. then where appropriate, circling back to leave some out if it gets too polluting!
pub use crate::inputs::*;
pub use crate::krusty::*;
//pub use crate::utils::*;
// ^^ lets not do the internals of utils .. we can just use 'utils::' when needed
