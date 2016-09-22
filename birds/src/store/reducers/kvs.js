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

export default kvs;

export const getStore = (store) => store;
