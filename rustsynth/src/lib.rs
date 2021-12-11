pub mod nanokontrol2;

use std::cell::RefCell;
use std::marker::PhantomData;

pub trait Rack {
    type Input;
}
pub trait Module<R: Rack> {
    fn update(&mut self, rack: &R, input: &R::Input);
}

const SAMPLES_PER_SEC: u32 = 44_100;

pub trait InPort<R: Rack, T>: Fn(&R, &R::Input) -> T + std::marker::Send {}
impl<R: Rack, T, F: Fn(&R, &R::Input) -> T + std::marker::Send> InPort<R, T> for F {}

pub struct VCO<R: Rack> {
    _rack: PhantomData<R>,
    // range: 0.0 - 1.0 ( freq_min Hz - freq_max Hz )
    in_freq: Box<dyn InPort<R, f32>>,
    in_waveform: Box<dyn InPort<R, WaveForm>>,
    phase: f32,
    freq_min: f32,
    freq_max: f32,
    pub out: f32,
}
impl<R: Rack> Module<R> for VCO<R> {
    fn update(&mut self, rack: &R, input: &R::Input) {
        let in_freq = (self.in_freq)(rack, input);
        let pi: f32 = std::f32::consts::PI;
        let pi2: f32 = pi * 2.0;
        let pi12: f32 = pi / 2.0;
        let pi32: f32 = pi12 * 3.0;
        let freq = (self.freq_min.ln() + in_freq * (self.freq_max.ln() - self.freq_min.ln())).exp();
        self.phase += freq * pi2 / SAMPLES_PER_SEC as f32;
        self.phase %= pi2;
        let wf = (self.in_waveform)(rack, input);
        self.out = match wf {
            WaveForm::Sine => (self.phase).sin(),
            WaveForm::Sawtooth => {
                if self.phase < pi {
                    self.phase / pi
                } else {
                    (self.phase - pi) / pi - 1.0
                }
            }
            WaveForm::Triangle => {
                if self.phase < pi12 {
                    self.phase / pi12
                } else if self.phase < pi32 {
                    1.0 - (self.phase - pi12) / pi12
                } else {
                    (self.phase - pi32) / pi12 - 1.0
                }
            }
            WaveForm::Square => {
                if self.phase < pi {
                    1.0
                } else {
                    -1.0
                }
            }
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum WaveForm {
    Sine,
    Sawtooth,
    Triangle,
    Square,
}
impl Default for WaveForm {
    fn default() -> WaveForm {
        WaveForm::Sine
    }
}

#[derive(Default, Debug, Clone)]
pub struct Input {
    pub vco1_freq: f32,
    pub vco1_waveform: WaveForm,
    pub lfo1_freq: f32,
    pub lfo1_waveform: WaveForm,
    pub vco1_lfo1_amount: f32,
}

pub struct MyRack<In> {
    pub lfo1: RefCell<VCO<MyRack<In>>>,
    pub vco1: RefCell<VCO<MyRack<In>>>,
}
impl<In> Rack for MyRack<In> {
    type Input = In;
}

pub fn new_my_rack() -> MyRack<Input> {
    MyRack {
        lfo1: RefCell::new(VCO {
            _rack: PhantomData,
            in_freq: Box::new(|_, input| input.lfo1_freq as f32),
            in_waveform: Box::new(|_, input| input.lfo1_waveform),
            freq_min: 0.1,
            freq_max: 100.0,
            phase: 0.0,
            out: 0.0,
        }),
        vco1: RefCell::new(VCO {
            _rack: PhantomData,
            in_freq: Box::new(|rack, input| {
                rack.lfo1.borrow().out * input.vco1_lfo1_amount + input.vco1_freq
            }),
            in_waveform: Box::new(|_, input| input.vco1_waveform),
            freq_min: 200.0,
            freq_max: 15000.0,
            phase: 0.0,
            out: 0.0,
        }),
    }
}

pub fn update_all<In>(rack: &mut MyRack<In>, input: &In) {
    rack.lfo1.borrow_mut().update(rack, input);
    rack.vco1.borrow_mut().update(rack, input);
}
