#![allow (non_snake_case)]

use std::ops::Not;
use std::path::{Path, PathBuf};
use std::str::FromStr;
use std::sync::{OnceLock, RwLock};
use std::{fs, io};

use tracing::{info, warn, Level};
use tracing::metadata::LevelFilter;
use tracing_appender::non_blocking;
use tracing_appender::non_blocking::WorkerGuard;
use tracing_appender::rolling::{RollingFileAppender, Rotation};
use tracing_subscriber::fmt::time::LocalTime;
use tracing_subscriber::prelude::*;
use tracing_subscriber::reload::Handle;
use tracing_subscriber::{reload, Layer, Registry};

use toml_edit::DocumentMut;



#[derive (Debug)]
pub struct Config {
    pub toml     : RwLock <Option <DocumentMut>>,
    pub default  : DocumentMut,
    pub loglevel : RwLock <Option <Handle <LevelFilter, Registry>>>,
}





// first some module level helper functions ..
/// Returns the directory of the currently running executable
fn get_app_dir () -> Option<PathBuf> {
    std::env::current_exe().ok() .and_then (|p| p.parent() .map (|p| p.to_path_buf()))
}

/// Checks whether a path is readonly .. note: this is not a good indicator of whether the path is writeable by a user
fn _is_writeable (path: &Path) -> bool {
    if let Ok(metadata) = fs::metadata(path) {
        metadata.permissions().readonly().not()
        // ^^ doesnt work .. its a direct read-only flag check, orthogonal to user-role based permissions
    } else { false }
}

/// Checks whether a path is writeable by the current user by attempting to open/create a file in write mode
fn is_writeable (path: &Path) -> bool {
    fs::OpenOptions::new().write(true).create(true).truncate(false).open(path).is_ok()
    // note that ^^ this is similar to 'touch' and will create an empty file if it doesnt exist
}


impl Config {

    pub fn instance () -> &'static Config {
        static INSTANCE: OnceLock <Config> = OnceLock::new();
        INSTANCE .get_or_init ( || {
            let conf = Config {
                toml    : RwLock::new (None),
                default : DocumentMut::from_str (include_str!("KrustyBoard.conf.toml")).unwrap(),
                // ^^ our KrustyBoard.conf.toml is at root of project, the include_str macro will load the contents at compile time
                loglevel : RwLock::new (None),
            };
            conf.load();
            conf
        } )
    }

    pub const CONF_FILE_NAME  : &'static str = "KrustyBoard.conf.toml";


    fn get_config_file (&self) -> Option<PathBuf> {
        let app_dir_loc = get_app_dir() .map (|p| p.join (Self::CONF_FILE_NAME));
        //println! ("app_dir_loc: {:?}", app_dir_loc);
        //win_apis::write_win_dbg_string (&format!("KRUSTY : app_dir_loc: {:?}", &app_dir_loc));
        if app_dir_loc.as_ref() .is_some_and (|p| is_writeable(p)) {
            return app_dir_loc;
        }
        let app_data_dir = dirs::data_local_dir() .map (|p| p.join ("KrustyBoard"));
        if app_data_dir .as_ref() .is_some_and (|p| !p.exists()) {
            let _ = fs::create_dir (app_data_dir.as_ref().unwrap());
        }
        let app_data_dir_loc = app_data_dir .map (|p| p.join (Self::CONF_FILE_NAME));
        //println! ("app_data_dir_loc: {:?}", app_data_dir_loc);
        //win_apis::write_win_dbg_string (&format!("KRUSTYBOARD : app_data_dir_loc: {:?}", &app_data_dir_loc));

        if app_data_dir_loc .as_ref() .is_some_and (|p| is_writeable(p)) {
            return app_data_dir_loc;
        }
        None
    }
    pub fn get_config_loc (&self) -> Option<PathBuf> {
        if let Some (conf_path) = self.get_config_file() {
            if let Some (conf_loc) = conf_path.parent() {
                return Some (conf_loc.to_path_buf());
        } }
        None
    }


    pub fn trigger_config_loc_open (&'static self) {
        if let Some(conf_loc) = self.get_config_loc() {
            let _ = std::process::Command::new("cmd").arg("/c").arg("start").arg(conf_loc).spawn();
        }
    }
    pub fn trigger_config_file_edit (&'static self) {
        if let Some(conf_path) = self.get_config_file() {
            let _ = std::process::Command::new("cmd").arg("/c").arg("start").arg(conf_path).spawn();
        }
    }
    pub fn trigger_config_file_reset (&self) {
        self.toml.write().unwrap() .replace (self.default.clone());
        self.write_back_toml();
    }


    pub fn load (&self) {
        if let Some(conf_path) = self.get_config_file().as_ref() {
            if let Ok(cfg_str) = fs::read_to_string(conf_path) {
                if !cfg_str.trim().is_empty() {
                    if let Ok(toml) = DocumentMut::from_str(&cfg_str) {
                        // successfully read and parsed a writeable non-empty toml, we'll use that
                        self.toml.write().unwrap().replace(toml);
                        return
        }   }   }  }
        // there's no writeable location, or the file was empty, or we failed to read or parse it .. load default and write back
        self.trigger_config_file_reset();
    }

    pub fn _reload_log_level (&self) {
        // we'll revisit log-subscriber setup in case there was a switch from disabled to enabled
        // (but note we still want to have the initial setup_log_subscriber called direct from main first to keep around the flush guard)
        let _ = self.setup_log_subscriber();
        let log_level = self.get_log_level();
        warn! ("Setting log-level to {:?}", log_level.into_level());
        self.loglevel.write().unwrap().as_ref() .map (|h| {
            h.modify (|f| *f = log_level)
        } );
    }

    // ^^ todo .. this is weird .. looks like it was setup so logging could be switched to enabled later, but doing so doesnt save the guard
    // .. either check to ensure all's still well, or make it such that 'disabled' simply means log-level  error, or disallow runtime changes

    #[ allow (clippy::result_unit_err) ]
    pub fn setup_log_subscriber (&self) -> Result <WorkerGuard, ()> {
        // todo .. ^^ prob use actual errors, though little utility here

        if self.check_flag__logging_enabled().not() ||  self.loglevel.read().unwrap().is_some() {
            return Err(())
        }

        if let Some(log_loc) = self.get_config_loc() {

            let log_appender = RollingFileAppender::builder()
                .rotation(Rotation::DAILY)
                .filename_prefix("KrustyBoard_log_")
                .filename_suffix("log")
                .latest_symlink ("KrustyBoard_log_latest_.log")
                .max_log_files(7)
                .build(log_loc)
                .map_err (|_e| ())?;

            let (nb_log_appender, guard) = non_blocking (log_appender);

            let (level_filter, filter_handle) = reload::Layer::new(self.get_log_level());

            *self.loglevel.write().unwrap() = Some(filter_handle.clone());

            let timer = LocalTime::new ( ::time::format_description::parse_borrowed::<3> (
                "[year]-[month]-[day] [hour]:[minute]:[second].[subsecond digits:3]"
            ).unwrap() );

            let console_subscriber = tracing_subscriber::fmt::Layer::new()
                .with_ansi(true) .with_timer(timer.clone()) //.with_filter(filter_handle)
                .with_writer ( io::stderr.with_min_level(Level::WARN) .or_else(io::stdout) );

            let file_subscriber = tracing_subscriber::fmt::Layer::new()
                .with_writer(nb_log_appender)
                .with_ansi(false) .with_timer(timer.clone()) .with_filter(level_filter);

            tracing_subscriber::registry() .with (file_subscriber) .with (console_subscriber) .init();

            return Ok(guard)
        }
        Err(())
    }


    fn write_back_toml (&self) {
        //tracing::debug! ("write_back_toml");
        let conf_path = self.get_config_file();
        if conf_path.is_none() { return }
        let _ = fs::write (
            conf_path.as_ref().unwrap(),
            self.toml.read().unwrap().as_ref() .map (|d| d.to_string()).unwrap_or_default()
        );
    }
    #[allow (dead_code)]
    fn write_back_toml_if_changed (&'static self) {
        let conf_path = self.get_config_file();
        if conf_path.is_none() { return }
        let toml_str = self.toml.read().unwrap().as_ref() .map (|d| d.to_string()) .unwrap_or_default();
        let old_toml_str = fs::read_to_string (conf_path.as_ref().unwrap()) .unwrap_or_default();
        if toml_str != old_toml_str {
            let _ = fs::write (conf_path.as_ref().unwrap(), toml_str);
        }
    }



    fn check_flag (&self, flag_name:&str) -> bool {
        self.toml.read().unwrap().as_ref()
            .and_then (|t| t.get(flag_name))
            .and_then (|t| t.as_bool())
            .unwrap_or (self.default.get(flag_name).unwrap().as_bool().unwrap())
    }

    fn get_string (&self, key:&str) -> String {
        self.toml.read().unwrap().as_ref()
            .and_then (|t| t.get(key))
            .and_then (|t| t.as_str()) .map (|s| s.to_string())
            .unwrap_or ( self.default.get(key) .and_then (|t| t.as_str()) .map (|s| s.to_string()) .unwrap_or_default() )
    }


    pub fn check_krusty_conf_version_match (&self) -> bool {
        let conf_version = self.toml.read().unwrap().as_ref().unwrap() .get("krusty_conf_version")
            .and_then (|t| t.as_float().map(|n| n as f32)) .unwrap_or_default();
        info! ("Using user conf KrustyBoard.conf.toml with version string : {:?}", conf_version);

        let default_conf_version = self.default.get("krusty_conf_version")
            .and_then (|t| t.as_float().map(|n| n as f32)) .unwrap_or_default();

        if conf_version != default_conf_version {
            warn! (" !!! WARNING !!! CONF VERSION MISMATCH : Expected {:?} .. Found {:?}", default_conf_version, conf_version);
            return false
        }
        true
    }


    pub fn check_flag__logging_enabled (&self) -> bool {
        self.check_flag ("logging_enabled")
    }
    pub fn get_log_level (&self) -> LevelFilter {
        if !self.check_flag__logging_enabled() {
            return LevelFilter::OFF;
        }
        match self.get_string("logging_level").as_str() {
            "TRACE" => LevelFilter::TRACE,
            "DEBUG" => LevelFilter::DEBUG,
            "WARN"  => LevelFilter::WARN,
            "ERROR" => LevelFilter::ERROR,
            "OFF"   => LevelFilter::OFF,
        //  "INFO"  => LevelFilter::INFO,
            _       => LevelFilter::INFO,
        }
    }


}
