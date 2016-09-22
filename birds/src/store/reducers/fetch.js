import { combineReducers } from "redux";

const fetchData = (prefix) => (state = {}, action) => {
    switch (action.type) {
        case `${prefix}_SUCCESS`:
            return {
                ...state,
                ...action.payload
            };
        default:
            return state;
    }
};

const fetchStatus = (prefix) => (state = {}, action) => {
    switch (action.type) {
        case `${prefix}_SUCCESS`:
            return {
                ...state,
                error: null,
                fetching: false
            };
        case `${prefix}_FAILURE`:
            return {
                ...state,
                error: action.payload,
                fetching: false
            };
        case `${prefix}_LOAD`:
            return {
                ...state,
                error: null,
                fetching: true
            };
        default:
            return state;
    }
};

const fetch = (prefix) => combineReducers({
    status: fetchStatus(prefix),
    data: fetchData(prefix)
});

export const getStatus = (state) => state.status;
export const getData = (state) => state.data;

export default fetch;
