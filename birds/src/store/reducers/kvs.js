const kvs = (prefix) => (state = {}, action) => {
  if (action.error) {
    console.error(action.eror);
    return state;
  }
  switch (action.type) {
    case `${prefix}_SET`:
      return {
        ...state,
        [action.payload.key]: action.payload.value
      }
    default:
      return state;
  }
}

export default kvs;

export const getStore = (store) => store;
