import init, * as Wasm from "aoc2023";
import { PropsWithChildren, createContext, useContext, useEffect, useState } from "react";

export type WasmInterface = {
    [key: string]: (input: string) => string;
}

export interface WasmContextState {
    wasm: WasmInterface | null;
}

export const WasmContext = createContext<WasmContextState | null>(null);

export function useWasm() {
    const state = useContext(WasmContext);
    return state?.wasm;
}

export default function WasmProvider({ children }: PropsWithChildren) {
    const [ wasm, setWasm ] = useState<WasmInterface | null>(null);

    useEffect(() => {
        (async function initWasm() {
            console.log("Loading WASM Module...");
            await init();
            console.log("WASM Module loaded!");
            setWasm(Wasm as any);
        })();
    }, [wasm, setWasm]);

    const state = {
        wasm,
    };

    return <WasmContext.Provider value={state}>
        {children}
    </WasmContext.Provider>
}