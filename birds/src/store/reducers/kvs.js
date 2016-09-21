import { combineReducers } from "redux";

const kvs = (prefix) => (state = {}, action) => {
    if (action.error) {
        return state;
    }
    switch (action.type) {
        case `${prefix}_SET`:
            return {
                ...state,
                ...action.payload,
            };
        case `${prefix}_RESET`:
            return {};
        default:
            return state;
    }
};

const error = (prefix) => (state = {}, action) => {
    if (!action.error) {
        return state;
    }
    switch (action.type) {
        case `${prefix}_SET`:
            return {
                ...state,
                error: action.payload
            };
        case `${prefix}_RESET`:
            return {
                ...state,
                error: action.payload
            };
        default:
            return state;
    }
};


export default (prefix) =>
    combineReducers({
        kvs: kvs(prefix),
        error: error(prefix)
    });

export const getStore = (store) => store.kvs;

export const getError = (store) => store.error;
