use anyhow::Context;
use anyhow::Result;
use cpal::traits::{DeviceTrait, HostTrait, StreamTrait};

enum MidiMessage {
    Unknown(Vec<u8>),
    ControlChange { ch: u8, num: u8, value: u8 },
}
#[derive(Debug)]
struct MidiMessageParseError {}
fn get_at(value: &[u8], index: usize) -> std::result::Result<u8, MidiMessageParseError> {
    if index < value.len() {
        Ok(value[index])
    } else {
        Err(MidiMessageParseError {})
    }
}
impl std::convert::TryFrom<&[u8]> for MidiMessage {
    type Error = MidiMessageParseError;
    fn try_from(value: &[u8]) -> std::result::Result<Self, Self::Error> {
        let status = get_at(value, 0)?;
        let kind = status & 0xF0;
        let ch = status & 0x0F;
        match kind {
            #[allow(clippy::if_same_then_else)]
            0xB0 => {
                let control = get_at(value, 1)?;
                if control <= 0x77 {
                    // control change
                    let control_value = get_at(value, 2)?;
                    Ok(MidiMessage::ControlChange {
                        ch,
                        num: control,
                        value: control_value,
                    })
                } else if control <= 0x7F {
                    // channel message
                    Ok(MidiMessage::Unknown(value.to_vec()))
                } else {
                    // ???
                    Ok(MidiMessage::Unknown(value.to_vec()))
                }
            }
            _ => Ok(MidiMessage::Unknown(value.to_vec())),
        }
    }
}
impl std::fmt::Debug for MidiMessage {
    fn fmt(&self, fmt: &mut std::fmt::Formatter<'_>) -> std::result::Result<(), std::fmt::Error> {
        match self {
            MidiMessage::Unknown(value) => fmt.write_fmt(format_args!("Unknown({:02X?})", value)),
            MidiMessage::ControlChange { ch, num, value } => fmt
                .debug_struct("ControlChange")
                .field("ch", ch)
                .field("num", num)
                .field("value", value)
                .finish(),
        }
    }
}

fn main() -> Result<()> {
    // sine_wave()?;
    midi_input()?;
    Ok(())
}

#[derive(Debug)]
struct SyncError<T: std::error::Error>(std::sync::Mutex<T>);
impl<T: std::error::Error> SyncError<T> {
    fn new(value: T) -> SyncError<T> {
        SyncError(std::sync::Mutex::new(value))
    }
}
impl<T: std::error::Error + std::fmt::Display> std::fmt::Display for SyncError<T> {
    fn fmt(&self, fmt: &mut std::fmt::Formatter<'_>) -> std::result::Result<(), std::fmt::Error> {
        match self.0.lock() {
            Ok(value) => {
                value.fmt(fmt)?;
            }
            Err(err) => {
                panic!("Can't acquire lock: {}", err);
            }
        }
        Ok(())
    }
}
impl<T: std::error::Error> std::error::Error for SyncError<T> {}

fn midi_input() -> Result<()> {
    let mut input = midir::MidiInput::new("midi_input")?;
    input.ignore(midir::Ignore::None);
    println!("Available ports:");
    for port in input.ports() {
        println!("* {}", input.port_name(&port)?);
    }
    let port = &input.ports()[0];
    let port_name = input.port_name(port)?;
    let _con = input
        .connect(
            port,
            &port_name,
            |stamp, message, _| {
                print!("{:10}", stamp);
                let message = MidiMessage::try_from(message);
                match message {
                    Ok(message) => println!("Message: {:?}", message),
                    Err(err) => println!("Error: {:?}", err),
                };
            },
            (),
        )
        .map_err(SyncError::new)?;
    std::thread::sleep(std::time::Duration::from_millis(5 * 1000));
    Ok(())
}

fn sine_wave() -> Result<()> {
    let host = cpal::default_host();
    println!("Avaliable devices:");
    for device in host.output_devices()? {
        println!("* {}", device.name()?);
    }

    let device = host
        .default_output_device()
        .context("Default output device not found")?;
    println!("Using device {}", device.name()?);

    let config = device.default_output_config()?;
    println!("Config: {:?}", config);
    match config.sample_format() {
        cpal::SampleFormat::F32 => run::<f32>(&device, &config.into()),
        cpal::SampleFormat::I16 => run::<i16>(&device, &config.into()),
        cpal::SampleFormat::U16 => run::<u16>(&device, &config.into()),
    }
}

fn run<T>(device: &cpal::Device, config: &cpal::StreamConfig) -> Result<()>
where
    T: cpal::Sample,
{
    let sample_rate = config.sample_rate.0 as f32;
    let channels = config.channels as usize;
    let mut sample_clock = 0f32;
    let mut next_value = move || {
        sample_clock = (sample_clock + 1.0) % sample_rate;
        (sample_clock * 440.0 * 2.0 * std::f32::consts::PI / sample_rate).sin()
    };

    let output_stream = device.build_output_stream(
        config,
        move |data: &mut [T], _: &cpal::OutputCallbackInfo| {
            write_data(data, channels, &mut next_value);
        },
        |err| eprintln!("An error occured on stream {}", err),
    )?;
    output_stream.play()?;
    std::thread::sleep(std::time::Duration::from_millis(1000));
    Ok(())
}

fn write_data<T, F>(output: &mut [T], channels: usize, next_sample: &mut F)
where
    T: cpal::Sample,
    F: FnMut() -> f32,
{
    for frame in output.chunks_mut(channels) {
        let value: T = cpal::Sample::from::<f32>(&next_sample());
        for sample in frame.iter_mut() {
            *sample = value;
        }
    }
}
