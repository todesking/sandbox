import { Machine } from "./machine";

describe("Machine", () => {
  test("should have resources", () => {
    const m = new Machine();

    const { food } = m.resources("food");
    expect(m.value(food)).toBe(0);
  });
});
