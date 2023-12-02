import { Link } from "react-router-dom";

const days = Array.from({ length: 25 }, (_, i) => i + 1);

function dayImplemented(day: number) {
    // TODO: Get this from the Rust side
    return day <= 3;
}

function DayButton({ day }: { day: number }) {
    if(dayImplemented(day)) {
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