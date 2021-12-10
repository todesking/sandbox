use anyhow::Context;
use anyhow::Result;
use cpal::traits::{DeviceTrait, HostTrait, StreamTrait};

enum MidiMessage {
    Unknown(Vec<u8>),
    ControlChange { ch: u8, num: u8, value: u8 },
    SysEx(Vec<u8>),
}

enum IOSelect {
    In,
    Out,
}
enum ModeDataMode {
    Normal,
    Native,
}

struct DataDump {}

enum NanoKontrol2SysEx {
    /// section 2-1
    /// ch: 00-0F of 7F(any)
    Inquiry {
        ch: u8,
    },
    // section 2-2 is cryptic, detail is in section 3
    // 3-3
    SearchDevice {
        echo_back_id: u8,
    },
    CurrentSceneDataDumpReq {
        ch: u8,
    },
    SceneWriteReq {
        ch: u8,
    },
    NativeModeIOReq {
        ch: u8,
        io: IOSelect,
    },
    ModeReq {
        ch: u8,
    },
    CurrentSceneDataDmp {
        ch: u8,
        data: DataDump,
    },
    DataLoadCompleted {
        ch: u8,
    },
    DataLoadError {
        ch: u8,
    },
    WriteCompleted {
        ch: u8,
    },
    WriteError {
        ch: u8,
    },
    NativeModeIO {
        ch: u8,
        io: IOSelect,
    },
    ModeData {
        ch: u8,
        mode: ModeDataMode,
    },
}

enum Button {
    Cycle,
    Rew,
    FF,
    Stop,
    Play,
    Rec,
    TrackRew,
    TrackFF,
    MarkerSet,
    MarkerRew,
    MarkerFF,
    Solo(u8),
    Mute(u8),
    GRec(u8),
}

impl NanoKontrol2SysEx {
    // fn to_midi_message(&self) -> MidiMessage {
    //     match self {
    //         NanoKontrol2SysEx::Inquiry { ch } => {
    //             let data = vec![0x7E, *ch, 0x06, 0x01];
    //             MidiMessage::SysEx(data)
    //         },
    //         NanoKontrol2SysEx::SearchDevice{echo_back_id} => {
    //             let data = vec![0x42, 0x50, 0x00, *echo_back_id];
    //             MidiMessage::SysEx(data)
    //         }
    //     }
    // }
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
                            match message {
                                MidiMessage::ControlChange { ch: 0, num, value } => {
                                    let value = value as f32 / 127.0;
                                    let mut input = input.lock().unwrap();
                                    match num {
                                        0x00 => (*input).lfo1_freq = value,
                                        0x01 => (*input).vco1_freq = value,
                                        0x10 => (*input).vco1_lfo1_amount = value,
                                        _ => {}
                                    }
                                }
                                _ => {}
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
        dbg!(input.lock());
    }
}

fn midi_comm() -> Result<()> {
    let output = midir::MidiOutput::new("midir")?;
    list_available_ports(&output, "output")?;
    println!("Available output ports:");
    for port in output.ports() {
        println!("* {}", output.port_name(&port)?);
    }
    let port = &output.ports()[0];
    let port_name = output.port_name(port)?;
    let mut out_con = output.connect(port, &port_name).map_err(SyncError::new)?;

    let mut input = midir::MidiInput::new("midi_input")?;
    list_available_ports(&input, "input")?;
    input.ignore(midir::Ignore::None);
    let port = &input.ports()[0];
    let port_name = input.port_name(port)?;
    let _in_con = input
        .connect(
            port,
            &port_name,
            |stamp, message, _| {
                print!("{:10}", stamp);
                let message = MidiMessage::try_from(message);
                match message {
                    Ok(message) => {
                        println!("Message: {:0X?}", message);
                        match message {
                            MidiMessage::SysEx(data) => {
                                if data.len() == 400
                                    && &data[..12]
                                        == [
                                            0x42, 0x40, 0x00, 0x01, 0x13, 0x00, 0x7F, 0x7F, 0x02,
                                            0x03, 0x05, 0x40,
                                        ]
                                {
                                    let dump =
                                        rustsynth::nanokontrol2::SceneData::from_encoded_bytes(
                                            &data[12..(388 + 12)],
                                        );
                                    println!("Dump: {:#?}", dump);
                                }
                            }
                            _ => {}
                        }
                    }
                    Err(err) => println!("Error: {:?}", err),
                };
            },
            (),
        )
        .map_err(SyncError::new)?;
    // out_con.send(&[0xF0, 0x7E, 0x7F, 0x06, 0x01, 0xF7])?;
    out_con.send(&[
        0xF0, 0x42, 0x40, 0x00, 0x01, 0x13, 0x00, 0x1F, 0x10, 0x00, 0xF7,
    ])?;

    let off = 0x00u8;
    let on = 0x7Fu8;
    for _ in 0..10 {
        out_con.send(&[0xB0, 0x2E, off])?;
        out_con.send(&[0xB0, 0x2B, on])?;
        std::thread::sleep(std::time::Duration::from_millis(500));
        out_con.send(&[0xB0, 0x2E, on])?;
        out_con.send(&[0xB0, 0x2B, off])?;
        std::thread::sleep(std::time::Duration::from_millis(500));
    }

    out_con.send(&[0xBF, 0x2E, 0xFF])?;
    std::thread::sleep(std::time::Duration::from_millis(5 * 1000));
    Ok(())
}

fn midi_input() -> Result<()> {
    let mut input = midir::MidiInput::new("midi_input")?;
    input.ignore(midir::Ignore::None);
    println!("Available input ports:");
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
