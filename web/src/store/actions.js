// @flow

import API from "../api";

export const loginStart = (): LOGIN_START => ({
    type: "LOGIN_START"
});

export const loginError = (error: string): LOGIN_ERROR => ({
    type: "LOGIN_ERROR",
    payload: error
});

export const loginSuccess = (user: User, token: string): LOGIN_SUCCESS => ({
    type: "LOGIN_SUCCESS",
    payload: {
        user,
        token
    }
});

export const registerStart = (): REGISTER_START => ({
    type: "REGISTER_START"
});

export const registerSuccess = (user: User): REGISTER_SUCCESS => ({
    type: "REGISTER_SUCCESS",
    payload: user
});

export const registerError = (error: string): REGISTER_ERROR => ({
    type: "REGISTER_ERROR",
    payload: error
});

export const logout = (): LOGOUT => ({
    type: "LOGOUT"
});

export const register = (data: RegisterData) => async (
    dispatch: Dispatch,
    getState: GetState,
    { api }: { api: typeof API }
) => {
    dispatch(registerStart());
    const user = await api.users.register(data);
    dispatch(registerSuccess(user));
};

export const login = (email: string, password: string) => async (
    dispatch: Dispatch,
    getState: GetState,
    { api }: { api: typeof API }
) => {
    dispatch(loginStart());
    const { user, token } = await api.users.login(email, password);
    dispatch(loginSuccess(user, token));
};
