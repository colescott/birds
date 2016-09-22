import React from "react";

import Header from "./header";
import Error from "./error";

const Main = ({ children }) => {
    return (
        <div className="wrapper">
            <div className="main">
                <Header />
                <Error />
                {
                    children
                }
            </div>
        </div>
  );
};

export default Main;
