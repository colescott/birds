const kvs = (prefix) => (state = {}, action) => {
  if (action.error) {
    console.error(action.eror);
    return state;
  }
  switch (action.type) {
    case `${prefix}_SET`:
      return {
        ...state,
        [action.key]: action.value
      }
    default:
      return state;
  }
}

export default kvs;

export const getStore = (store) => store;
