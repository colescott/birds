// @flow

declare type ServerError = {
    code: number,
    error: string,
    message?: string
};

declare type User = {
    id: string,
    email: string,
    firstname: string,
    lastname: string,
    teamnumber?: number
};

declare type RegisterData = {
    email: string;
    firstname: string;
    lastname: string;
    password: string;
};
