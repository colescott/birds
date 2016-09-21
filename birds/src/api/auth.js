import xr from "xr";

const urlPrefix = (typeof URL_PREFIX !== "undefined" && URL_PREFIX) || "";

const auth = {
    login: (email, password) =>
        xr.post(`${urlPrefix}/api/v1/auth/login`, { email, password })
            .then(res => res.data)
            .catch(() => ({
                error: {
                    message: "Network Error"
                }
            })),
    register: (user) =>
        xr.post(`${urlPrefix}/api/v1/users`, user)
            .then(res => res.data)
            .catch(() => ({
                error: {
                    message: "Network Error"
                }
            })),
    getUser: (id, token) =>
        xr.get(`${urlPrefix}/api/v1/users/${id}`, {}, {
            headers: {
                Authorization: `Bearer ${token}`
            }
        })
            .then(res => res.data)
            .catch(() => ({
                error: {
                    message: "Network Error"
                }
            }))
};

export default auth;
