

### Krusty - <small>A Fast Easy Keyboard Scripting Framework (for Windows)</small>

Krustyboard is designed to be a more capable and advanced replacment for keyboard/mouse automation tools like AutoHotKey for those who would rather write the scripting directly in a modern language like Rust and have full backing of a powerful, versatile, but ergonomic API/library enabling them .. (and first and foremost, for my own heavy daily use).

The primary motivation was how slow and buggy Autohotkey was getting with the extensive remapping and automations I had set up with it .. which itself was motivated by re-imagining the keyboard to avoid/ameliorate RSI by taking the load away from un-ergonomic keys and combos into a much more flexible and powerful but ergonomic setup.

Krusty makes keyboard remapping and automation very straightforward with fluent builder apis for defining hotkey combos and the action(s) to execute when they are triggered.

Consider a simple example :

```rust
let combo  = k.ks.cg().k(L).m(caps).m(alt);
let action = k.ks.ag().k(F20).m(alt).m(shift);
k .cm .add_combo (combo, action);
```

The above uses a Combo-Generator (cg) and an Action-Generator (ag) from KrustyBoard (k) state (ks) to build a combo (Capslock+Alt+L) and its action (send out Alt+Shift+F20), and register them to our combos-mapping table (cm). 

When using with an IDE, the API methods will show up with doc-comments explaining their usage and capabilities, making even complex tasks accessible, easy, quick, yet still robust and reliable. 

The API further enables one to .. 
- Directly define arbitrary code fragments to trigger upon combo activation.
- Conditional combos that only trigger on satisfying arbitrary conditions
- Various 'mode-states' that can further specify the modes during which certain combos are to be enabled
- Specify wildcards for various modifier keys (e.g. ctrl, alt, shift, win) that can be optional for the combo
- Support for defining combos and actions for mouse button and wheel events, and for double-taps on keys/buttons etc
- Framework for directly binding handling for any of the key/mouse input events, further upstream than the combo registrations (if so desired), which is specially useful for managing actions on actual modifier keys themselves (like Capslock, Alt, Ctrl etc).
- Framework and examples on setting up various mode-states that allow for second/third/fourth etc layers of hotkey functionality layered on the keyboard
- A set of utility functions for interacting with and managing .. windows, volume, brightness, media player, alt-tab, window-groups etc etc

To make things even more accessible, a comprehensive snapshot of remappings I used personally is included in krusty_app.rs.

The general idea is that anyone wanting to do similar would either just modify the file, or create a separate one modeled after that for themselves to build/run for their own use. 

(In that sense, this isnt intended to be used as a typical library, more like a living automation scaffolding for someone who wants to fork and dive into creating their own version of remappings for themselves, while having access to the guts to get full control and power of the underlying machinery. On the plus side, this means you retain full control of every line of code, and the executable you generate, therefore limiting security risks of having to run something external that intercepts and modifies low level input events.)

Among the numerous things implemented in the demonstration code/script, here are some quick highlights :

- Capslock is transformed to a general 'magic-key' that can be used context-dependently as shift, ctrl, and/or as a free-use combo modifier key.
- A layer-two functionality is implemented that transforms right hand central keys [I,J,K,L,M etc] to text navigation keys, with their functionality and mode switched by left-hand central keys [E,D,F,R etc] to enable various editing modes like caret movement, selection, word nav, faster nav, deletion, line duplication/movement, copy/pasting etc etc .. (most of which will work identically across all applications, editors, IDEs, browsers etc seamlessly)
- (And numerous other setups for flexible layer-3 / layer-4 etc usage of those core ergonomic keys)
- Remappings to use mouse wheel with various mode-keys (and capslock key) for things like controling volume, brightness, switching tabs, windows, or desktops, horiz/vertical caret motions, navigating through code in IDE, moving windows around the screen etc etc
- Remappings to use mouse drag (with combo keys) for things like ergonomically dragging or resizing windows (or window-groups) around etc, while convenient window-snap functionality is implemented to make aligning and placing windows etc effortless.
- Remapping keys (e.g. Left-Win key) to switch its typical OS reserved combo functionality to double-tap-mode (i.e. double-press-win + keys), while repurposing single press win combos to a whole host of uses for ourselves (with auto fallback for combos we havent overriden).
- Latch states that essentially can remap the full keyboard in a toggleable manner (while behavior undefined in the new toggled layer/mode continue to fallback to normal behavior) .. for example for gaming sessions etc.

As for a quick word on what it is not .. Krusty does not have any support for creating UI etc that Autohotkey does. It focuses only on the keyboard/mouse remapping/automation portions.

A final warning, that like many who have tried it out for a bit, you might quickly find yourself almost incapable of using anyone elses keyboard (without your krusty enhancements) because of how easy it is to get addicted to having so much power in your fingertips, even for mundane tasks like text editing, or manipulating things like tabs, windows, desktops, volume, brightness, IDE, games and so on, just with mouse wheel combos etc!

So dive in, take a look at krusty_app.rs for examples, maybe scan through the doc-comments to get a sense of the capabilities, and get started re-imagining your reborn keyboard experience that is at once, more powerful, flexible, ergonomic, and easily customizable!
