import { Link } from "react-router-dom";
import { useWasm } from "./Wasm";
import { useMemo } from "react";

const days = Array.from({ length: 25 }, (_, i) => i + 1);

function DayButton({ day }: { day: number }) {
    const wasm = useWasm();

    const dayImplemented = useMemo(() => {
        if(!wasm) {
            return false;
        }

        const dayName = `day${day.toString().padStart(2, "0")}`;
        const funcName = `${dayName}_part1`;
        return !!wasm[funcName];
    }, [wasm, day]);

    if(dayImplemented) {
        return <Link to={`/days/${day}`} className="bg-green-100 hover:ring-blue-500 hover:ring-2 active:bg-white rounded p-2 m-2 text-center">
            Day {day}
        </Link>
    }

    return <button disabled={true} className="bg-gray-100 rounded p-2 m-2">
        Day {day}
    </button>
}

export default function Home() {
    return <div>
        <p className="p-2">Select a day below to begin.</p>
        <div className="grid grid-cols-5 grid-flow-row justify-stretch">
            {days.map((day) => <DayButton key={day} day={day} />)}
        </div>
    </div>
}