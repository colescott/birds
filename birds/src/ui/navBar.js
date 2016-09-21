import React from "react";
import { Toolbar, ToolbarGroup, ToolbarTitle } from "material-ui/Toolbar";

const NavBar = (props) => {
    return (
        <Toolbar
            style={{
                display: "flex",
                alignItems: "space-between"
            }}
        >
            <ToolbarGroup>
                <ToolbarTitle text={props.title} />
            </ToolbarGroup>
                <ToolbarGroup>
                    <ToolbarTitle text={props.status} />
                    {
                        props.children
                    }
                </ToolbarGroup>
        </Toolbar>
    );
};

export default NavBar;
