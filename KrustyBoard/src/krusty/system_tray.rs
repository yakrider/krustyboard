
#![allow(unused)]


use image::ImageFormat;
use tao::event_loop::{ControlFlow, EventLoopBuilder};
use tray_icon::{Icon, TrayIconBuilder, TrayIconEvent};
use tray_icon::menu::{Menu, MenuEvent, MenuItem};

use crate::{InputProcessor, KrustyState};

fn get_icon() -> Icon {
    let icon_str = {
        //"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"
    } .to_string();
    use base64::Engine;
    let icon_data = base64::engine::general_purpose::STANDARD.decode(&icon_str).unwrap();
    let icon = image::io::Reader::with_format (std::io::Cursor::new(icon_data), ImageFormat::Png).decode().unwrap();
    Icon::from_rgba (icon.into_bytes(), 44, 44).unwrap()
}

pub fn start_system_tray_monitor() {

    let event_loop = EventLoopBuilder::new().build();

    let tray_menu = Menu::new();
    let suspend = MenuItem::new ("Suspend", true, None);
    let reload  = MenuItem::new ("Reload",  true, None);
    let quit    = MenuItem::new ("Quit",    true, None);
    tray_menu .append_items ( &[ &suspend, &reload, &quit ] );

    let mut tray_icon = None;

    let menu_channel = MenuEvent::receiver();
    let tray_channel = TrayIconEvent::receiver();

    event_loop .run ( move |event, _, control_flow| {

        *control_flow = ControlFlow::Wait;

        if let tao::event::Event::NewEvents(tao::event::StartCause::Init) = event {
            //let icon = load_icon(std::path::Path::new(path));
            tray_icon = Some(
                TrayIconBuilder::new()
                    .with_menu (Box::new (tray_menu.clone()))
                    .with_tooltip ("KrustyBoard")
                    .with_icon (get_icon())
                    .build()
                    .unwrap(),
            );
        }

        if let Ok(event) = menu_channel.try_recv() {
            //println!("menu_channel: {event:?}");
            if event.id == quit.id() {
                tray_icon.take();
                *control_flow = ControlFlow::Exit;
            }
            else if event.id == reload.id() {
                KrustyState::instance().unstick_all();
                suspend.set_text("Suspend");
            }
            else if event.id == suspend.id() {
                if InputProcessor::instance().are_hooks_set() {
                    InputProcessor::instance().stop_input_processing();
                    suspend.set_text ("Resume");
                } else {
                    InputProcessor::instance().begin_input_processing();
                    suspend.set_text ("Suspend");
                }
            }
        }

        if let Ok(event) = tray_channel.try_recv() {
            //println!("tray_channel: {event:?}");
        }

    })

}
