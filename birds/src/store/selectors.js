import { createSelector } from "reselect";

import { getStore } from "./reducers/kvs.js";

export const getRegisterForm = createSelector(
  (state) => state.registerForm,
  getStore
);

export const getAuth = createSelector(
    (state) => state.auth,
    getStore
);

export const getLoginForm = createSelector(
    (state) => state.loginForm,
    getStore
);

export const getTeamForm = createSelector(
    (state) => state.teamForm,
    getStore
);
