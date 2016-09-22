import xr from "xr";

const urlPrefix = (typeof URL_PREFIX !== "undefined" && URL_PREFIX) || "";

const auth = {
    login: (email, password) =>
        xr.post(`${urlPrefix}/api/v1/auth/login`, { email, password })
            .then(res => res.data)
            .catch(handleNetworkError),
    register: (user) =>
        xr.post(`${urlPrefix}/api/v1/users`, user)
            .then(res => res.data)
            .catch(handleNetworkError),
    getUser: (id, token) =>
        xr.get(`${urlPrefix}/api/v1/users/${id}`, {}, {
            headers: {
                Authorization: `Bearer ${token}`
            }
        })
            .then(res => res.data)
            .catch(handleNetworkError)
};

export const handleNetworkError = (e) => {
    try {
        const res = JSON.parse(e.response);
        return res;
    } catch (parseError) {
        return {
            error: {
                message: "Network Error"
            }
        };
    }
};

export default auth;
