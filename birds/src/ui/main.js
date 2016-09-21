import React from "react";

import Header from "./header";

const Main = ({ children }) => {
    return (
        <div className="wrapper">
            <div className="main">
                <Header />
                {
                    children
                }
            </div>
        </div>
  );
};

export default Main;
