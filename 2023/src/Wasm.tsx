import init, { type InitOutput } from "aoc2023";
import { PropsWithChildren, createContext, useContext, useEffect, useState } from "react";

export interface WasmContextState {
    wasm: InitOutput | null;
}

export const WasmContext = createContext<WasmContextState | null>(null);

export function useWasm() {
    const state = useContext(WasmContext);
    return state?.wasm;
}

export default function WasmProvider({ children }: PropsWithChildren) {
    const [ wasm, setWasm ] = useState<InitOutput | null>(null);

    useEffect(() => {
        (async function initWasm() {
            console.log("Loading WASM Module...");
            const module = await init();
            console.log("WASM Module loaded!");
            setWasm(module);
        })();
    }, [wasm, setWasm]);

    const state = {
        wasm,
    };

    return <WasmContext.Provider value={state}>
        {children}
    </WasmContext.Provider>
}