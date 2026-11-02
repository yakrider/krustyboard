
use std::thread;
use std::error::Error;
use std::sync::{Mutex, OnceLock};
use std::time::{Duration, Instant};

use cpal::{Devices, OutputDevices, Stream, Sample};
use cpal::traits::{DeviceTrait, HostTrait, StreamTrait};


// turns out a started stream does resume back ok when machine is directly put to sleep and awakened (eg via power btn)
// .. i.e sleep happens, the stream stops during sleep, and auto resumes when woken up (tested w a tone instead of silence)
// .. however, there are instances when upon resuming from suspend etc, the stream is gone .. (hence impld the refreshing on that sys-event)


const RESTART_GRACE_DUR : Duration = Duration::from_secs(5);


pub struct AudioMemGuard {
    stream : Mutex <Option <Stream>>,
    t_next : Mutex <Option <Instant>>,
}


impl AudioMemGuard {

    pub fn instance () -> &'static AudioMemGuard {
        static INSTANCE : OnceLock <AudioMemGuard> = OnceLock::new();
        INSTANCE .get_or_init ( || {
            AudioMemGuard {
                stream : Mutex::new (None),
                t_next : Mutex::new (None),
            }
        } )
    }


    pub fn refresh (&'static self) {
        if let Ok (pending) = self.t_next.lock() {
            if pending.is_some() {
                return;
            }
        }

        let due_at = Instant::now() + RESTART_GRACE_DUR;
        if let Ok (mut pending) = self.t_next.lock() {
            *pending = Some(due_at);
        }

        thread::spawn (move || {
            thread::sleep (RESTART_GRACE_DUR);
            let guard = AudioMemGuard::instance();
            if let Ok (mut pending) = guard.t_next.lock() {
                if *pending != Some(due_at) {
                    return;
                }
                *pending = None;
            }
            if let Err(err) = guard.restart_stream() {
                tracing::error! ("Audio stream restart failed: {err}");
            }
        });
    }


    pub fn disable (&self) {
        if let Ok(mut pending) = self.t_next.lock() {
            *pending = None;
        }
        let Ok(mut stream_lock) = self.stream.lock() else {
            tracing::error! ("Audio stream mutex is poisoned.");
            return;
        };
        if let Some(stream) = stream_lock.take() {
            let _ = stream.pause();
            drop (stream);
            tracing::info! ("Intel SST Anti-leak stream: STOPPED.");
        }
    }


    fn restart_stream (&'static self) -> Result <(), Box<dyn Error>> {

        let mut stream_lock = self .stream .lock() .map_err (|e|
            format!("Audio stream mutex poisoned: {e}")
        )?;

        if let Some(stream) = stream_lock.take() {
            let _ = stream.pause();
            drop(stream);
            tracing::info! ("Intel SST Anti-leak stream: STOPPED.");
        }

        let host = cpal::default_host();

        let mut devices: OutputDevices<Devices> = host.output_devices()?;

        let device = devices .find (|dvc| {
            dvc.description().ok() .is_some_and (|dvc| {
                let name = dvc.name().to_lowercase();
                name .contains("speaker") // || name.contains("sst")
            })
        }) .or_else (|| host.default_output_device()) .ok_or ("Could not find audio device")?;

        let config = device.default_output_config()?;


        //// test code path to stream a sinewave tone instead of silence
        //let sample_rate = config.sample_rate() as f32;
        //let channels = config.channels() as usize;
        //let mut sample_clock = 0f32;
        //let mut next_value = move || {
        //    sample_clock = (sample_clock + 1.0) % sample_rate;
        //    (sample_clock * 440.0 * 2.0 * std::f32::consts::PI / sample_rate).sin()
        //};

        let stream = device.build_output_stream (
            config.clone().into(),
            move |data: &mut [f32], _: &cpal::OutputCallbackInfo| {
                data.fill(0.0);   // silence
                //Self::write_data (data, channels, &mut next_value)   // sinewave tone
            },
            |err| {
                tracing::error! ("SST audio pump stream error: {err}");
                // if we were actively streaming, and ran into an error, its worth a refresh attempt!
                AudioMemGuard::instance().refresh();
            },
            None,
        )?;

        stream.play()?;
        *stream_lock = Some(stream);

        tracing::info! ("Intel SST Anti-leak stream: STARTED.");
        Ok(())
    }


    #[allow (dead_code)]
    pub fn is_active(&self) -> bool {
        self.stream .lock() .map (|s| s.is_some()) .unwrap_or(false)
    }


    #[allow (dead_code)]
    fn write_data (output: &mut[f32], channels: usize, next_sample: &mut dyn FnMut() -> f32) {
        for frame in output.chunks_mut(channels) {
            let value: f32 = f32::from_sample(next_sample());
            for sample in frame.iter_mut() {
                *sample = value;
            }
        }
    }

}
