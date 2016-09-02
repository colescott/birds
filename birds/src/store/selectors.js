import { createSelector } from "reselect";

import { getStore } from "./reducers/kvs.js";

export const getRegisterForm = createSelector(
  (state) => state.registerForm,
  getStore
);
