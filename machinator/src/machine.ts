export type ResourceID = string & { _resourceIDBrand: never };

function error(msg: string): never {
  throw new Error(msg);
}

function newResourceID(name: string): ResourceID {
  return name as ResourceID;
}

export class Machine {
  #values: Map<ResourceID, number>;
  constructor() {
    this.#values = new Map();
  }

  resources<S extends string>(...names: S[]): { [K in S]: ResourceID } {
    const obj: Record<string, ResourceID> = {};
    for (const name of names) {
      const id = newResourceID(name);
      obj[name] = id;
      this.#values.set(id, 0);
    }
    return obj as { [K in S]: ResourceID };
  }

  value(id: ResourceID): number {
    console.log(this.#values);
    return this.#values.get(id) || error(`ID not found: ${id}`);
  }
}
