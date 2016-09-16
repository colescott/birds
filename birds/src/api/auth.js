import xr from "xr";

const urlPrefix = (typeof URL_PREFIX !== "undefined" && URL_PREFIX) || "";

const auth = {
    login: (email, password) =>
        xr.post(`${urlPrefix}/api/v1/auth/login`, { email, password })
            .then(res => res.data),
    register: (user) =>
        xr.post(`${urlPrefix}/api/v1/users`, user)
            .then(res => res.data),
    getUser: (id, token) =>
        xr.get(`${urlPrefix}/api/v1/users/${id}`, {}, {
            headers: {
                Authorization: `Bearer ${token}`
            }
        })
            .then(res => res.data)
};

export default auth;
