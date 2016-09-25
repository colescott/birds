import React from "react";
import { connect } from "react-redux";

import * as a from "../../store/actions.js";
import * as s from "../../store/selectors.js";

import { Card, CardHeader, CardText } from "material-ui/Card";
import FlatButton from "material-ui/FlatButton";
import TextField from "material-ui/TextField";

import ReactMarkdown from "react-markdown";

const styles = {
    div: {
        display: "flex",
        flexDirection: "column",
        padding: 10,
    },
    paperLeft: {
        flex: 1,
        margin: 10,
    },
    paperRight: {
        flex: 1,
        margin: 10,
    }
};

const LessonEditor = (props) => {
    try {
        console.log(props.getEditor.editor);
    } catch (e) {
        console.log(e);
    }

    return (
        <Card>
            <div style={styles.div}>
                <Card zDepth={1} style={styles.paperLeft}>
                    <CardHeader
                        title="Editor"
                    />
                    <CardText>
                        <TextField
                            floatingLabelText="Markdown"
                            type="text"
                            onChange={props.updatePreview()}
                            value={props.getEditor.editor}
                            multiLine={true}
                            rows={10}
                            rowsMax={15}
                            fullWidth={true}
                        />
                    </CardText>
                </Card>
                <Card zDepth={1} style={styles.paperRight}>
                    <CardHeader
                        title="Preview"
                    />
                    <CardText>
                        <ReactMarkdown source={props.getEditor.editor || "# Dank Memez 2k16"} />
                    </CardText>
                </Card>
            </div>
        </Card>
    );
};

const mapStateToProps = (state) => ({
    getEditor: s.getLessonEditor(state)
});

const mapDispatchToProps = (dispatch) => ({
    updatePreview: () => (e, value) => dispatch(a.setLessonEditor({ editor: value }))
});

export default connect(mapStateToProps, mapDispatchToProps)(LessonEditor);
