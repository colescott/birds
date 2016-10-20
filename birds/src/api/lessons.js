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
            .then(res => res.data)
            .catch(handleNetworkError);
    },
    get: (id) => {
        return xr.get(`${urlPrefix}/api/v1/lessons/${id}`, { }, {
            headers: {
                "Content-Type": "application/json"
            }
        })
            .then(res => res.data)
            .catch(handleNetworkError);
    },
    update: (id, title, branch, data, token) => {
        return xr.put(`${urlPrefix}/api/v1/lessons/${id}`, { title, branch, data }, {
            headers: {
                Authorization: `Bearer ${token}`,
                "Content-Type": "application/json"
            }
        })
            .then(res => res.data)
            .catch(handleNetworkError);
    }
};

export const handleNetworkError = (e) => {
    try {
        const res = JSON.parse(e.response);
        if (typeof res.error.message.message === "string") {
            res.error.message = res.error.message.message;
        }
        return res;
    } catch (parseError) {
        return {
            error: {
                message: "Network Error"
            }
        };
    }
};

export default lessons;
