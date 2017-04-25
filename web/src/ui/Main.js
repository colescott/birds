import React from "react";

import Header from "./Header";

import "./main.css";

export default ({ children }) => (
    <div className="container" id="main">
        <Header />
        <div id="content">
            {children}
        </div>
    </div>
);
