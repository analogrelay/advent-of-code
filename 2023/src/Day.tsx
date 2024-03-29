import { ChangeEvent, useCallback, useRef, DragEvent, useState } from "react"
import { Link } from "react-router-dom"
import { useWasm } from "./Wasm";

export interface DayProps {
    day: number
}

interface FileUploadProps {
    onFileSelect: (file: File) => void
}

function FileUpload({onFileSelect}: FileUploadProps) {
    const fileInput = useRef<HTMLInputElement>(null);
    const [ activeFile, setActiveFile ] = useState<File | null>(null);
    const [ fileHover, setFileHover ] = useState(false);

    const handleClick = useCallback(() => {
        if(fileInput.current) {
            fileInput.current.click();
        }
    }, [fileInput]);

    const handleDrop = useCallback((evt: DragEvent<HTMLButtonElement>) => {
        evt.preventDefault();
        const file = evt.dataTransfer.files?.[0];
        if(!file) {
            return;
        }

        setActiveFile(file);
        onFileSelect(file);
        setFileHover(false);
    }, [onFileSelect]);

    const handleFileSelect = useCallback((evt: ChangeEvent<HTMLInputElement>) => {
        const file = evt.target.files?.[0];
        if(!file) {
            return;
        }
        setActiveFile(file);
        onFileSelect(file);
        setFileHover(false);
    }, [onFileSelect]);

    return <div>
        <button 
            className={`border-2 border-blue-800 active:bg-white/30 ${fileHover && "bg-white/30"} border-dashed rounded-lg p-4 w-full h-full`}
            onDrop={handleDrop}
            onDragOver={(evt) => evt.preventDefault()}
            onDragEnter={() => setFileHover(true)}
            onDragLeave={() => setFileHover(false)}
            onClick={handleClick}>

            {activeFile
                ? <p className="text-center">Selected file: {activeFile.name}</p>
                : <p>Drag and drop a file here, or click to upload</p>}
            <input type="file" hidden={true} ref={fileInput} onChange={handleFileSelect} />
        </button>
    </div>
}

interface PartProps {
    day: number,
    part: number
}

function StandardDayPart({day, part}: PartProps) {
    const [ result, setResult ] = useState<string | null>(null);
    const [ file, setFile ] = useState<File | null>(null);
    const [ input, setInput ] = useState<string | null>(null);
    const wasm = useWasm();

    const handleFileSelect = useCallback((file: File) => {
        setFile(file);
    }, [setFile]);

    const handleTextInput = useCallback((evt: ChangeEvent<HTMLTextAreaElement>) => {
        setInput(evt.target.value);
    }, [setInput]);

    const handleSolve = useCallback(async () => {
        if(!wasm) {
            return;
        }

        // Get an array buffer with either the file or input text, preferring the input text
        const puzzleInput = input 
            ? input
            : await file?.text();
        if(!puzzleInput) {
            return;
        }

        // Get the function to call based on the day and part
        const dayName = `day${day.toString().padStart(2, "0")}`;
        const partName = `part${part}`;
        const funcName = `${dayName}_${partName}`;
        const func = wasm[funcName];
        if(!func) {
            setResult(`Function ${funcName} not found in WASM module`);
        }
        else {
            const result = func(puzzleInput);
            setResult(result);
        }
    }, [wasm, day, part, file, input]);

    const disabledReason = useCallback(() => {
        if(!wasm) {
            return "(loading WASM...)";
        }

    if(!file && !input) {
            return "(no input provided)";
        }

        return "";
    }, [wasm, file, input]);

    return <div className="text-center flex flex-col gap-2">
        <h3 className="font-semibold text-lg">Part {part}</h3>
        <p>Upload or enter your input (entered input has priority)</p>
        <div className="grid grid-cols-2 gap-2">
            <FileUpload onFileSelect={handleFileSelect} />
            <textarea 
                placeholder="Enter input here" 
                className="border-2 rounded-lg p-4 w-full h-96" 
                onChange={handleTextInput} />
        </div>
        {wasm && (file || input)
            ? <button onClick={handleSolve} className="bg-green-100 hover:ring-blue-500 hover:ring-2 active:bg-white rounded p-2 m-2 text-center">
                Solve
            </button>
            : <button disabled={true} className="bg-gray-100 rounded p-2 m-2">
                Solve {disabledReason()}
            </button>}
        {result && <>
            <h3 className="font-semibold text-lg">Result</h3>
            <pre className="bg-white border border-gray-400 bg-gray-200 rounded m-4 p-4">{result}</pre>
        </>}
    </div>
}

export default function Day({day}: DayProps) {
    return <div>
        <Link to="/" className="text-sm text-blue-900 active:text-blue-700">
            ← Back to days list
        </Link>
        <div className="mt-2">
            <h2 className="font-semibold text-xl text-center">Day {day}</h2>

            <div className="grid grid-cols-2 mt-4 gap-2 grid-flow-row justify-stretch">
                {/* We can support custom day part UI here if needed */}
                <StandardDayPart day={day} part={1} />
                <StandardDayPart day={day} part={2} />
            </div>
        </div>
    </div>
}