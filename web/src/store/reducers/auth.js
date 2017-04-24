// @flow

import { combineReducers } from "redux";

type StatusState = {
    loading: boolean,
    error: ?string
};

type DataState = {
    user?: User,
    token?: string
};

export type AuthState = {
    status: StatusState,
    data: DataState
};

const statusState = (): StatusState => ({ loading: false, error: undefined });

const status = (
    state: StatusState = statusState(),
    action: Action
): StatusState => {
    switch (action.type) {
        case "REGISTER_START":
        case "LOGIN_START":
            return {
                ...state,
                error: undefined,
                loading: true
            };
        case "REGISTER_SUCCESS":
        case "LOGIN_SUCCESS":
            return {
                ...state,
                error: undefined,
                loading: false
            };
        case "REGISTER_ERROR":
        case "LOGIN_ERROR":
            return {
                ...state,
                error: action.payload || "something went wrong",
                loading: false
            };
        default:
            return state;
    }
};

const data = (state: DataState = {}, action: Action): DataState => {
    switch (action.type) {
        case "LOGIN_SUCCESS":
            return {
                ...state,
                ...action.payload
            };
        case "LOGOUT":
            return {};
        default:
            return state;
    }
};

export default combineReducers({
    status,
    data
});

export const getStatus = (state: State) => state.status;

export const getData = (state: State) => state.data;
