import React from "react";
import { connect } from "react-redux";

import NavBar from "./navBar";

const Main = ({ children }) => {
    return (
        <div className="wrapper">
            <div className="main">
                <NavBar />
                {
                    children
                }
            </div>
        </div>
  );
};

const mapStateToProps = () => ({});

const mapDispatchToProps = () => ({});

export default connect(mapStateToProps, mapDispatchToProps)(Main);
