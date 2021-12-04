use anyhow::Context;
use anyhow::Result;
use cpal::traits::{DeviceTrait, HostTrait, StreamTrait};

fn main() -> Result<()> {
    sine_wave()?;
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
