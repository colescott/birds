import xr from "xr";

const urlPrefix = (typeof URL_PREFIX !== "undefined" && URL_PREFIX) || "";

const lessons = {
    create: (title, branch, data, token) => {
        return xr.post(`${urlPrefix}/api/v1/lessons`, { title, branch, data }, {
            headers: {
                Authorization: `Bearer ${token}`,
                "Content-Type": "application/json"
            }
        })
            .then(res => res.data);
    },
    get: (id) => {
        return xr.get(`${urlPrefix}/api/v1/lessons/${id}`, { }, {
            headers: {
                "Content-Type": "application/json"
            }
        })
            .then(res => res.data);
    },
    update: (id, title, branch, data, token) => {
        return xr.put(`${urlPrefix}/api/v1/lessons/${id}`, { title, branch, data }, {
            headers: {
                Authorization: `Bearer ${token}`,
                "Content-Type": "application/json"
            }
        })
            .then(res => res.data);
    }
};

export default lessons;
