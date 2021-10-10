import {Value, evalValue} from "./expr"

export type EntityID = string & { _entityIDBrand: never };


export type Connection = {
  from: EntityID,
  to: EntityID,
  amount: Value,
}

function error(msg: string): never {
  throw new Error(msg);
}

function newEntityID(name: string): EntityID {
  return name as EntityID;
}

export class Machine {
  initialValues: Map<EntityID, number> = new Map()
  connections: Connection[] = []

  resource(name: string, initial?: number): EntityID {
    const id = newEntityID(name)
    this.initialValues.set(id, initial ?? 0)
    return id
  }

  sink(name: string): EntityID {
    const id = newEntityID(name)
    return id
  }

  source(name: string): EntityID {
    const id = newEntityID(name)
    return id
  }

  transfer(from: EntityID, to: EntityID, amount: Value) {
    this.connections.push({from, to, amount})
  }

  run(): State {
    return new State(this)
  }
}

export class State {
  #machine: Machine
  #values: Map<EntityID, number> = new Map()
  step = 0
  constructor(machine: Machine) {
    this.#machine = machine
    for(const [id, v] of this.#machine.initialValues) {
      this.#values.set(id, v)
    }
  }

  value(id: EntityID): number {
    return this.#values.get(id) ?? error(`Entity not found: ${id}`)
  }

  tick() {
    for(const con of this.enabledConnections()) {
      const amount = evalValue(con.amount, this.#values)
      this.modifyResource(con.from, -amount)
      this.modifyResource(con.to, amount)
    }
    this.step++
  }

  enabledConnections(): Iterable<Connection> {
    return this.#machine.connections
  }

  modifyResource(id: EntityID, amount: number) {
    const prev = this.#values.get(id)
    if(prev === undefined) return
    const next = prev + amount < 0 ? 0 : prev + amount
    this.#values.set(id, next)
  }

  set(id: EntityID, value: number) {
    if(!this.#values.has(id)) throw new Error(`Value undefined: ${id}`)
    if(value < 0) throw new Error(`Value should positive: ${id} = ${value}`)
    this.#values.set(id, value)
  }
}
