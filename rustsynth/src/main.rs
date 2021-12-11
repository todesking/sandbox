use anyhow::Context;
use anyhow::Result;
use cpal::traits::{DeviceTrait, HostTrait, StreamTrait};

enum MidiMessage {
    Unknown(Vec<u8>),
    ControlChange { ch: u8, num: u8, value: u8 },
    SysEx(Vec<u8>),
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
            0xF0 => {
                if value[value.len() - 1] == 0xF7 {
                    Ok(MidiMessage::SysEx(value[1..value.len() - 1].to_vec()))
                } else {
                    Ok(MidiMessage::SysEx(value[1..].to_vec()))
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
            MidiMessage::SysEx(value) => fmt.write_fmt(format_args!("SysEx({:02X?})", value)),
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
    // midi_input()?;
    // midi_comm()?;
    run_synth()?;
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

fn list_available_ports<T: midir::MidiIO>(io: &T, kind: &str) -> Result<()> {
    println!("Available {} ports:", kind);
    for port in io.ports() {
        println!("* {}", io.port_name(&port)?);
    }
    Ok(())
}

fn run_synth() -> Result<()> {
    let output = midir::MidiOutput::new("midir")?;
    list_available_ports(&output, "output")?;
    let port = &output.ports()[0];
    let port_name = output.port_name(port)?;
    let out_con = output.connect(port, &port_name).map_err(SyncError::new)?;

    let mut input = midir::MidiInput::new("midi_input")?;
    list_available_ports(&input, "input")?;
    input.ignore(midir::Ignore::None);
    run_synth_main(input, out_con)?;
    Ok(())
}

fn run_synth_main(midi_in: midir::MidiInput, midi_out: midir::MidiOutputConnection) -> Result<()> {
    let input = std::sync::Arc::new(std::sync::Mutex::new(rustsynth::Input {
        ..Default::default()
    }));
    let port = &midi_in.ports()[0];
    let port_name = midi_in.port_name(port)?;
    println!("Connect to {}", &port_name);
    let _in_con = midi_in
        .connect(
            port,
            &port_name,
            {
                let input = std::sync::Arc::clone(&input);
                move |stamp, message, _| {
                    print!("{:10}", stamp);
                    let message = MidiMessage::try_from(message);
                    match message {
                        Ok(message) => {
                            println!("Message: {:0X?}", message);
                            if let MidiMessage::ControlChange { ch: 0, num, value } = message {
                                let value = value as f32 / 127.0;
                                let mut input = input.lock().unwrap();
                                match num {
                                    0x00 => (*input).lfo1_freq = value,
                                    0x01 => (*input).vco1_freq = value,
                                    0x10 => (*input).vco1_lfo1_amount = value,
                                    _ => {
                                        if value > 0.5 {
                                            match num {
                                                0x20 => {
                                                    (*input).lfo1_waveform =
                                                        rustsynth::WaveForm::Sine
                                                }
                                                0x30 => {
                                                    (*input).lfo1_waveform =
                                                        rustsynth::WaveForm::Sawtooth
                                                }
                                                0x40 => {
                                                    if (*input).lfo1_waveform
                                                        == rustsynth::WaveForm::Square
                                                    {
                                                        (*input).lfo1_waveform =
                                                            rustsynth::WaveForm::Triangle
                                                    } else {
                                                        (*input).lfo1_waveform =
                                                            rustsynth::WaveForm::Square
                                                    }
                                                }
                                                0x21 => {
                                                    (*input).vco1_waveform =
                                                        rustsynth::WaveForm::Sine
                                                }
                                                0x31 => {
                                                    (*input).vco1_waveform =
                                                        rustsynth::WaveForm::Sawtooth
                                                }
                                                0x41 => {
                                                    if (*input).vco1_waveform
                                                        == rustsynth::WaveForm::Square
                                                    {
                                                        (*input).vco1_waveform =
                                                            rustsynth::WaveForm::Triangle
                                                    } else {
                                                        (*input).vco1_waveform =
                                                            rustsynth::WaveForm::Square
                                                    }
                                                }
                                                _ => {}
                                            }
                                        }
                                    }
                                }
                            }
                        }
                        Err(err) => println!("Error: {:?}", err),
                    };
                }
            },
            (),
        )
        .map_err(SyncError::new)?;

    let host = cpal::default_host();
    println!("Avaliable devices:");
    for device in host.output_devices()? {
        println!("* {}", device.name()?);
    }

    let device = host
        .default_output_device()
        .context("Default output device not found")?;
    println!("Using device {}", device.name()?);

    println!("Available output config:");
    for config in device.supported_output_configs()? {
        println!("* {:?}", config);
    }
    let output_available = device.supported_output_configs()?.any(|c| {
        c.sample_format() == cpal::SampleFormat::F32
            && c.channels() == 2
            && c.min_sample_rate() <= cpal::SampleRate(44_100)
            && c.max_sample_rate() >= cpal::SampleRate(44_100)
            && match c.buffer_size() {
                cpal::SupportedBufferSize::Range { min, max } => min <= &441 && &441 <= max,
                _ => false,
            }
    });
    if !output_available {
        panic!("No suitable output available")
    }
    let config = cpal::StreamConfig {
        channels: 2,
        sample_rate: cpal::SampleRate(44_100),
        buffer_size: cpal::BufferSize::Fixed(441),
    };
    let mut rack = rustsynth::new_my_rack();
    let stream = device.build_output_stream(
        &config,
        {
            let input = std::sync::Arc::clone(&input);
            move |data: &mut [f32], _| {
                let input = input.lock().unwrap();
                let input = &*input;
                for frame in data.chunks_mut(2) {
                    rustsynth::update_all(&mut rack, input);
                    let value = rack.vco1.borrow().out;
                    for sample in frame.iter_mut() {
                        *sample = value;
                    }
                }
            }
        },
        |err| {
            println!("Device output error: {}", err);
        },
    )?;
    stream.play()?;

    loop {
        std::thread::sleep(std::time::Duration::from_millis(2000));
        let _ = dbg!(input.lock());
    }
}
