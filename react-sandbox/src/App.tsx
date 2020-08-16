import React, {
  useRef, useState, useEffect, forwardRef, useImperativeHandle,
} from 'react';
import './App.css';

function Todo() {
  const [items, setItems] = useState<string[]>([]);
  function onAdd(title: string) {
    setItems((old) => [...old, title]);
  }
  return (
    <div>
      <TodoInput onAdd={onAdd} />
      <TodoList items={items} />
    </div>
  );
}

function TodoInput(props: {onAdd: (title: string)=>void}) {
  const { onAdd } = props;
  const input = useRef<HTMLInputElement|null>(null);
  function onClick() {
    if (input.current) onAdd(input.current.value);
  }
  return (
    <div>
      <input type='text' ref={input} />
      <button type='button' onClick={onClick}>Add</button>
    </div>
  );
}

function TodoList({ items }: {items: string[]}) {
  return (
    <ul>
      {
        items.map((item) => <li>{item}</li>)
      }
    </ul>
  );
}

function Clock() {
  const [now, setNow] = useState(new Date());
  useEffect(() => {
    const timerId = setInterval(() => {
      setNow(new Date());
    }, 100);
    return () => {
      clearInterval(timerId);
    };
  }, []);
  return (
    <p>
      Now:
      {now.toLocaleTimeString()}
    </p>
  );
}

function nextState(width: number, height: number, active: boolean[][]): boolean[][] {
  const newState: boolean[][] = [];
  for (let y = 0; y < height; y += 1) {
    const newRow: boolean[] = [];
    for (let x = 0; x < width; x += 1) {
      let nactive = 0;
      for (let dy = -1; dy <= 1; dy += 1) {
        for (let dx = -1; dx <= 1; dx += 1) {
          if (dy === 0 && dx === 0) continue;
          if (active[(y + dy + height) % height][(x + dx + width) % width]) nactive += 1;
        }
      }
      if (active[y][x]) {
        if (nactive === 2 || nactive === 3) {
          newRow.push(true);
        } else {
          newRow.push(false);
        }
      } else if (nactive === 3) {
        newRow.push(true);
      } else {
        newRow.push(false);
      }
    }
    newState.push(newRow);
  }
  return newState;
}

interface WorldAPI {
  tick: () => void
}
const World = forwardRef(({ width, height }: {
  width: number,
  height: number
}, ref: React.Ref<WorldAPI>) => {
  const [active, setActive] = useState<boolean[][]>(() => {
    const rows: boolean[][] = [];
    for (let y = 0; y < height; y += 1) {
      const cols: boolean[] = [];
      for (let x = 0; x < width; x += 1) {
        cols.push(Math.random() < 0.33);
      }
      rows.push(cols);
    }
    return rows;
  });
  useImperativeHandle(ref, () => ({
    tick() {
      setActive((old) => nextState(width, height, old));
    },
  }), [width, height]);
  return (
    <table>
      <tbody>
        {
        active.map((row: boolean[]) => (
          <tr>
            {
            row.map((col) => <td>{col ? '□' : '■'}</td>)
          }
          </tr>
        ))
      }
      </tbody>
    </table>
  );
});

function LifeGame({ width, height }: {
  width: number,
  height: number
}) {
  const worldRef = useRef<WorldAPI>(null);
  const handleStart = () => {
    worldRef.current?.tick();
  };
  return (
    <div>
      <World width={width} height={height} ref={worldRef} />
      <button type='button' onClick={handleStart}>Start</button>
    </div>
  );
}

function App(): JSX.Element {
  return (
    <div className='App'>
      <Todo />
      <Clock />
      <Clock />
      <LifeGame width={20} height={20} />
    </div>
  );
}

export default App;
