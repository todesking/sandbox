pub mod nanokontrol2;

use std::cell::RefCell;
use std::marker::PhantomData;

trait Rack {
    type Input;
}
trait Module<R: Rack> {
    fn update(&mut self, rack: &R, input: &R::Input);
}

const SAMPLES_PER_SEC: u32 = 44_100_000;

struct VCO<R: Rack> {
    _rack: PhantomData<R>,
    // range: 0.0 - 1.0 (0.1Hz - 20.0KHz)
    in_freq: Box<dyn Fn(&R, &R::Input) -> f32>,
    phase: f32,
    out: f32,
}
impl<R: Rack> Module<R> for VCO<R> {
    fn update(&mut self, rack: &R, input: &R::Input) {
        let in_freq = (self.in_freq)(rack, input);
        let a = (20_000.0f32 - 0.1f32).ln() - 1.0;
        let freq = (1.0 + in_freq * a).exp();
        let pi2 = std::f32::consts::PI * 2.0;
        let phase_per_sample = freq / SAMPLES_PER_SEC as f32 * pi2;
        self.phase += phase_per_sample;
        self.phase %= pi2;
        self.out = self.phase.sin();
    }
}

pub struct MyRack<In> {
    vco1: RefCell<VCO<MyRack<In>>>,
}
impl<In> Rack for MyRack<In> {
    type Input = In;
}

pub struct Input {
    pub slider1: f32,
}

pub fn new_my_rack() -> MyRack<Input> {
    MyRack {
        vco1: RefCell::new(VCO {
            _rack: PhantomData,
            in_freq: Box::new(|_, input| input.slider1 as f32 / 127.0),
            phase: 0.0,
            out: 0.0,
        }),
    }
}

pub fn update_all<In>(rack: &mut MyRack<In>, input: &In) {
    rack.vco1.borrow_mut().update(rack, input);
}
