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
    phase: f32,
    freq_min: f32,
    freq_max: f32,
    pub out: f32,
}
impl<R: Rack> Module<R> for VCO<R> {
    fn update(&mut self, rack: &R, input: &R::Input) {
        let in_freq = (self.in_freq)(rack, input);
        let pi2 = std::f32::consts::PI * 2.0;
        let freq = (self.freq_min.ln() + in_freq * (self.freq_max.ln() - self.freq_min.ln())).exp();
        self.phase += freq * pi2 / SAMPLES_PER_SEC as f32;
        self.phase %= pi2;
        self.out = (self.phase).sin();
    }
}

#[derive(Default, Debug)]
pub struct Input {
    pub vco1_freq: f32,
    pub lfo1_freq: f32,
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
