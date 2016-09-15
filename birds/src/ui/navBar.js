import React from "react";
import { Link } from "react-router";

import AppBar from "material-ui/AppBar";
import IconButton from "material-ui/IconButton";
import IconMenu from "material-ui/IconMenu";
import MenuItem from "material-ui/MenuItem";
import MoreVertIcon from "material-ui/svg-icons/navigation/more-vert";

const NavBar = ({ title, links, user}) => (
    <AppBar
    title={`${title} - ${user}`}
    iconElementRight={
        <IconMenu
            iconButtonElement={
                <IconButton><MoreVertIcon /></IconButton>
            }
            targetOrigin={{ horizontal: "right", vertical: "top" }}
            anchorOrigin={{ horizontal: "right", vertical: "top" }}
        >
            {
                links.map((v, i) => (
                    <Link to={v.to} key={i} style={{ textDecoration: "none" }}>
                        <MenuItem primaryText={v.name}/>
                    </Link>
                ))
            }
        </IconMenu>
    }
    >
    </AppBar>
);

export default NavBar;
