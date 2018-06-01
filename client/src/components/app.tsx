import xs, { Stream } from 'xstream';
import { VNode } from '@cycle/dom';
import { StateSource } from 'cycle-onionify';
import isolate from '@cycle/isolate';
import { extractSinks } from 'cyclejs-utils';

import { driverNames } from '../drivers';
import { BaseSources, BaseSinks } from '../interfaces';

export interface Sources extends BaseSources {
    onion: StateSource<State>;
}
export interface Sinks extends BaseSinks {
    onion?: Stream<Reducer>;
}

// State
export interface State {}
export const defaultState: State = {};
export type Reducer = (prev?: State) => State | undefined;

export function App(sources: Sources): Sinks {
    const initReducer$ = xs.of<Reducer>(() => ({}));

    const vdom$ = xs.of(
        <div>
            <h1>Media Goggler</h1>
            <video
                autoplay
                controls
                src="/api/files/3413857a-0bd0-4335-8757-863261c69dc6/raw"
                height="720"
            />
        </div>
    );

    return {
        DOM: vdom$
    };
}
