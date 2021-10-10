import { EntityID } from "./machine"

export type Value = number | EntityID | {type: 'add', lhs: Value, rhs: Value} | {type: 'mul', lhs: Value, rhs: Value}

export function add(lhs:Value, rhs: Value): Value {
  return {type: 'add', lhs, rhs}
}

export function mul(lhs: Value, rhs: Value): Value {
  return {type: 'mul', lhs, rhs}
}

function error(msg: string): never {
  throw new Error(msg)
}

export function evalValue(value: Value, env: Map<EntityID, number>): number {
  if(typeof value ===  'number') return value
  else if(typeof value ===  'string') return env.get(value) ?? error(`Entity value not defined: ${value}`)
  else {
    switch(value.type) {
      case 'add': return evalValue(value.lhs, env) + evalValue(value.rhs, env); break
      case 'mul': return evalValue(value.lhs, env) * evalValue(value.rhs, env); break
    }
  }
}
