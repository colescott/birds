// @flow

import * as auth from "./reducers/auth";

export const getAuthData = (state: State) => auth.getData(state.auth);
export const getAuthStatus = (state: State) => auth.getStatus(state.auth);
