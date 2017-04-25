// @flow

import API from "../api";

declare type ExtraArgument = {
    api: typeof API
};

declare type GetState = () => State;

declare type Dispatch = (
    action:
        | Action
        | ((
            dispatch: Dispatch,
            getState: GetState,
            arg: ExtraArgument
        ) => void | Promise<void>)
) => void;
