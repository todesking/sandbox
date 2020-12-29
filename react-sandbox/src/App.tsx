import React, {
  useRef, useState, useEffect, forwardRef, useImperativeHandle, useLayoutEffect,
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

type BoardComponentType = (props: {active: boolean[][]}) => JSX.Element;

function TableTextBoard({ active }: {
  active: boolean[][]
}) {
  return (
    <table>
      <tbody>
        {
        active.map((row: boolean[], y) => (
          <tr
            /* eslint-disable-next-line react/no-array-index-key */
            key={y}
          >
            {
                /* eslint-disable-next-line react/no-array-index-key */
                row.map((col, x) => <td key={x}>{col ? '□' : '■'}</td>)
              }
          </tr>
        ))
      }
      </tbody>
    </table>
  );
}

function TableCssBoard({ active }: {
  active: boolean[][]
}) {
  return (
    <table
      className='table-color-board'
      style={{
        tableLayout: 'fixed',
        borderCollapse: 'collapse',
      }}
    >
      <tbody>
        {
        active.map((row: boolean[], y) => (
          <tr
            /* eslint-disable-next-line react/no-array-index-key */
            key={y}
          >
            {
              row.map((col, x) => (
                <td
                  /* eslint-disable-next-line react/no-array-index-key */
                  key={x}
                  style={{
                    margin: '0px',
                    padding: '0px',
                    width: '10px',
                    height: '10px',
                    backgroundColor: col ? 'white' : 'black',
                  }}
                />
              ))
            }
          </tr>
        ))
      }
      </tbody>
    </table>
  );
}

function DivBoard({ active }: {
  active: boolean[][]
}) {
  // reference: https://www.freecodecamp.org/news/create-gameoflife-with-react-in-one-hour-8e686a410174/
  const width = active[0].length;
  const height = active.length;
  const cellSize = 10;
  const cells: JSX.Element[] = [];
  for (let y = 0; y < height; y += 1) {
    for (let x = 0; x < width; x += 1) {
      if (active[y][x]) {
        cells.push(
          <div
            key={`${x},${y}`}
            style={{
              position: 'absolute',
              left: `${cellSize * x + 1}px`,
              top: `${cellSize * y + 1}px`,
              width: `${cellSize - 1}px`,
              height: `${cellSize - 1}px`,
              backgroundColor: '#ccc',
            }}
          />,
        );
      }
    }
  }
  return (
    <div style={{
      backgroundColor: 'black',
      width: `${cellSize * width}px`,
      height: `${cellSize * height}px`,
      backgroundSize: `${cellSize}px ${cellSize}px`,
      backgroundImage: 'linear-gradient(#333 1px, transparent 1px), linear-gradient(90deg, #333 1px, transparent 1px)',
      position: 'relative',
    }}
    >
      {cells}
    </div>
  );
}

interface WorldAPI {
  tick: () => void
}
const World = forwardRef(({ width, height, boardComponent }: {
  width: number,
  height: number,
  boardComponent: BoardComponentType
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
  const Board = boardComponent;
  return (
    <Board active={active} />
  );
});

function BoardSizeInput({ initialWidth, initialHeight, onApply }: {
  initialWidth: number,
  initialHeight: number,
  onApply: (w: number, h: number) => void
}) {
  const [width, setWidth] = useState(initialWidth.toString());
  const [height, setHeight] = useState(initialHeight.toString());
  return (
    <span>
      <input type='text' value={width} onChange={(e) => setWidth(e.target.value)} />
      x
      <input type='text' value={height} onChange={(e) => setHeight(e.target.value)} />
      <button type='button'>Set board size</button>
    </span>
  );
}

function LifeGame({ initialWidth, initialHeight }: {
  initialWidth: number,
  initialHeight: number
}) {
  const [[width, height], setBoardSize] = useState([initialWidth, initialHeight]);
  const [running, setRunning] = useState<boolean>(false);
  const worldRef = useRef<WorldAPI>(null);
  const handleStart = () => {
    setRunning((old) => !old);
  };
  const handleStep = () => {
    if (worldRef.current) worldRef.current.tick();
  };
  useLayoutEffect(() => {
    if (running) {
      let id: number;
      const f = () => {
        if (worldRef.current) worldRef.current.tick();
        id = requestAnimationFrame(f);
      };
      id = requestAnimationFrame(f);
      return () => cancelAnimationFrame(id);
    }
    return undefined;
  }, [running]);
  const renderMethodOrder = ['tableText', 'tableCss', 'div'];
  const renderMethods: {[key: string]: [string, BoardComponentType] } = {
    tableText: ['Table(text)', TableTextBoard],
    tableCss: ['Table(CSS)', TableCssBoard],
    div: ['Div', DivBoard],
  };
  const [renderMethod, setRenderMethod] = useState(renderMethodOrder[1]);
  const handleChange = (e: React.ChangeEvent<HTMLSelectElement>) => {
    setRenderMethod(e.target.value);
  };
  const boardComponent = renderMethods[renderMethod][1];
  const handleBoardSizeChange = (w: number, h: number) => setBoardSize([w, h]);
  return (
    <div>
      <World width={width} height={height} ref={worldRef} boardComponent={boardComponent} />
      <button type='button' onClick={handleStart}>{running ? 'Pause' : 'Start'}</button>
      <button type='button' onClick={handleStep} disabled={running}>Step</button>
      <select value={renderMethod} onChange={handleChange}>
        {
          renderMethodOrder.map((name) => <option value={name}>{renderMethods[name][0]}</option>)
        }
      </select>
      <BoardSizeInput initialWidth={width} initialHeight={height} onApply={handleBoardSizeChange} />
    </div>
  );
}

function App(): JSX.Element {
  return (
    <div className='App'>
      <Todo />
      <Clock />
      <Clock />
      <LifeGame initialWidth={100} initialHeight={100} />
    </div>
  );
}

export default App;
