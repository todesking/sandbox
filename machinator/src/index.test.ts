import { Machine } from "./machine";
import * as E from "./expr"

describe("Machine", () => {
  test("resource, sink, transfer, tick", () => {
    const m = new Machine();

    const  food  = m.resource('food', 2)
    const  wood  = m.resource('wood')
    const  sink  = m.sink('sink')
    m.transfer(food, sink, 1)

    const state = m.run()

    expect(state.value(food)).toBe(2)
    expect(state.value(wood)).toBe(0)
    expect(state.step).toBe(0)

    state.tick()

    expect(state.value(food)).toBe(1)
    expect(state.value(wood)).toBe(0)
    expect(state.step).toBe(1)

    state.tick()

    expect(state.value(food)).toBe(0)
    expect(state.step).toBe(2)

    state.tick()

    expect(state.value(food)).toBe(0)
    expect(state.step).toBe(3)
  });
  test("example: woodcutter", () => {
    const m = new Machine();
    const wood = m.resource('wood')
    const wood_worker = m.resource('wood_worker')
    const source = m.source('source')

    m.transfer(source, wood, E.mul(wood_worker, 3))

    const s = m.run()
    expect(s.value(wood)).toBe(0)

    s.tick()
    expect(s.value(wood)).toBe(0)

    s.set(wood_worker, 1)
    s.tick()
    expect(s.value(wood)).toBe(3)

    s.set(wood_worker, 2)
    s.tick()
    expect(s.value(wood)).toBe(9)

    s.set(wood_worker, 0)
    s.tick()
    expect(s.value(wood)).toBe(9)
  })
});
